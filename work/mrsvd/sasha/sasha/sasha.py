#!/usr/bin/python

#
# python sasha.py local localhost 5000
#
import zmq, sys, re, job, os
import time, job, imp, yaml
import numpy as np, random
from threading import Thread

__REDUCER__ = ":_r_:" # code indicating data is for reducer
__COMMAND__ = ":_c_:" # code indicting data is a command

'''
Main loop that waits for data from socket, processes or delegates its
data to another (or this) node.
'''
def server_loop():
    
    conf_block =  sys.argv[1]; # the yaml block to use from .sasha.conf
    host =  sys.argv[2] # host for this process
    port =  int(sys.argv[3]) # port for this process
    
    # start server on your own port
    context = zmq.Context()
    socket = context.socket(zmq.PULL)
    socket.bind('tcp://*:%d' % port)
    print 'started', host, port    
    out_conns = get_cluster_connections(context, conf_block)

    my_data_dir, server_i = get_my_info(context, conf_block, host, port)
    os.chdir(my_data_dir) # otherwise python module import will not work
    print 'working dir', my_data_dir, "server no", server_i

    output_file = None    
    reducer_objs = None
    mappers_finished = None
    f_output_file = None
    reducer_queue = []
    klass = None

    def reduce_line(id, value, mod):
        # if not exists, create reducer
        if id not in reducer_objs:
             reducer_objs[id] = eval("mod.%s()" % klass)
             reducer_objs[id].reducer_key = id
        reducer_objs[id].reducer(value)            
        # if the 'complete' flag is set, we can flush this reducer's
        # output to file right now, without having to wait. So we dump
        # its output to file and remove it from the dictionary.
        if reducer_objs[id].is_complete:
            for curr_res in reducer_objs[id].result():
                if curr_res:
                    res = '\t'.join(map(str,[id,curr_res]))
                    f_output_file.write(res + '\n')
                    f_output_file.flush()
            del reducer_objs[id]

    def map_line(data):
        # if there is a key in there, get it
        if '\t' in data: [id, line] = data.split('\t')
        # if not assign a dummy value for key, doesnt matter
        else: id = "1"; line = data
        for key, value in mapper_obj.mapper(id, line):
            if key == None: continue
            # the _and_ below is needed otherwise hash calculates
            # different values for x32 and x64.
            h =  hash(key) & 0xffffffff 
            to_where = h % len(out_conns)
            # check if we can by-pass sending a reducer message through a
            # queeue if the reduce work is meant for this server, then
            # make a local reduce call.
            if to_where == server_i:
                reducer_queue.append((key, value))
            else:
                out_conns[to_where].send(__REDUCER__ + key + '\t' + str(value))

    '''
    all reduce work will be placed in a queue and this function is the
    only function that can process this queue, controlled by a single
    thread. we created this local-queue-based architecture because
    reduce actions arriving both from files and from other servers
    (through 0mq) was causing conflicts. for example the (output) file
    is shared and we saw sometimes the output file being corrupted due
    to multiple threads accessing it, or reducer objects were being
    accessed incorrectly, so this structure is created.
    '''
    def process_reduce_queue(mod):
        while True:
            # no new work will arrive from any other server, and i
            # finished all work in my reduce queue, by definition, i
            # am done.
            if np.all(mappers_finished) and len(reducer_queue) == 0:
                break
            # if nothing to process sleep for a while. this gives some
            # breathing room to other threads which actually makes a big
            # difference, perf increased twofold with the introduction of
            # this sleep. 
            if np.all(mappers_finished) == False and len(reducer_queue) == 0:
                time.sleep(1.0)
            elif len(reducer_queue) > 0:
                id,val = reducer_queue.pop()
                reduce_line(id,val,mod)
            
        # being here means all mappers from other (and this process)
        # sent their 'finished' signal here, and we are done with all
        # reduce (micro) work, so reducers can now be
        # "finalized". They've have been performing small computations
        # until now, in a piecemeal fashion, however for a final
        # computation, we do the following.
        reducers_to_output(f_output_file, reducer_objs)
        response_socket = context.socket(zmq.PUSH)
        response_socket.connect(response_address)
        response_socket.send("SERVER=%d;KLASS=%s" % (server_i,klass))
        f_output_file.close()
                
    while True:
        data = socket.recv()
        # check if data is a reducer
        if __REDUCER__ in data[:10]:            
            res = data.replace(__REDUCER__, '')
            [id, value] = res.split('\t')
            #reduce_line(id, value)
            reducer_queue.append((id,value))
                
        # check if data is a command
        elif __COMMAND__ in data[:10]:
            res = re.search('^' + __COMMAND__+'(.*)', data).group(1)
            print res
            # reset the system
            if "RESET" in res:
                # initialize / cleanup
                reducer_objs = {}
                mappers_finished = [False for x in range(len(out_conns))]
                
            # Initialize the system by loading the job class, from its file
            elif "INIT=" in res:                
                # read init params
                res = re.search('INIT=(.*)$', data).group(1)
                [job_file, klass] = res.split(" ")
                sys.path.append(os.getcwd())
                mod = __import__(job_file)
                print "loaded", job_file, klass
                mapper_obj = eval("mod.%s()" % klass) # does i.e. mod.WordFreqCount()
                
            # response queue, we send notification here once done
            elif "RESPONSE_ADDRESS=" in res:
                response_address = re.search('RESPONSE_ADDRESS=(.*)$', data).group(1)

            # name of the output file, goes to data dir
            elif "OUTPUT_FILE=" in res:
                output_file = re.search('OUTPUT_FILE=(.*)$', data).group(1)
                f_output_file = open(my_data_dir + "/" + output_file, "w")    

            # external files, written under data dir
            elif "FILE=" in res:
                file_name = re.search('FILE=(.*)CONTENT=', data).group(1)
                content = re.search('FILE=.*CONTENT=(.*)$', data, re.MULTILINE | re.DOTALL).group(1)
                fout = open(my_data_dir + "/" + os.path.basename(file_name), "w")
                fout.write(content)
                fout.close()
                print "file saved", file_name
                
            # input file will be assumed it is under data dir folder
            # of each server 
            elif "INPUT=" in res:
                input_files = re.search('INPUT=(.*)$', data).group(1)
                input_files = input_files.split(",")
                def stream():
                    # read multiple files all at once, reading
                    # one line out of each at each iteration, and
                    # feeding them one by one to the system
                    fins = [iter(open(f)) for f in input_files]
                    while True:
                        for i,fin in enumerate(fins):
                            # this funkyness is necessary to detect end of
                            # file, whichever iteration ends, is removed
                            # from the list of files to be iterated.
                            try:
                                line =  next(fin).strip()
                                map_line(line)
                            except StopIteration:
                                fin.close()
                                fins.remove(fin)
                        if len(fins) == 0: break

                    # done with processing files, do the map final
                    for key, value in mapper_obj.mapper_final():
                        # the _and_ below is needed otherwise hash calculates
                        # different values for x32 and x64.
                        h =  hash(key) & 0xffffffff
                        to_where = h % len(out_conns)
                        out_conns[to_where].send(__REDUCER__ + key + '\t' + str(value))

                    # Send all hosts (reducers) a message saying we
                    # are done with our mapping phase. on the
                    # receiving end each server keeps track of which
                    # other server sent this "mapping done" message,
                    # when all servers send it, the receiving server
                    # can continue to reduce phase.  this is the only
                    # way of a server can make sure no other server
                    # (reducer) will send any more mapper data, hence
                    # the content of the reducer objects will not
                    # change anymore.
                    for i in range(len(out_conns)):
                        out_conns[i].send(__COMMAND__ + "MAPPER_DONE=%d" % server_i)
                        
                Thread(target=stream).start()
                Thread(target=process_reduce_queue, args=(mod,)).start()
                                
            # receiving mapper done signal from another server
            elif "MAPPER_DONE=" in res:                
                mapper_server = re.search('MAPPER_DONE=(\d*)$', data).group(1)
                mappers_finished[int(mapper_server)] = True
                

'''
Connect to all nodes, return connections as 0mq sockets
in an array
'''
def get_cluster_connections(context, conf_block):
    
    # read the configuration
    stram = open("%s/.sasha.conf" % os.environ['HOME'], "r")
    conf = yaml.load(stram)
    cluster = [tuple(x.split(':')) for x in conf[conf_block]['cluster']]
    
    print "cluster_size", len(cluster)
    print "connecting..."
    
    out_conns = [None for i in range(len(cluster))]    
    for i, (h,p,wd) in enumerate(cluster):
        tmp = context.socket(zmq.PUSH)
        tmp.connect('tcp://%s:%d' % (h,int(p)))
        out_conns[i] = tmp
        print "connected",h,p,wd
        
    return out_conns

'''
find the data dir, index no for _this_ process
'''
def get_my_info(context, conf_block, host, port):
    # read the configuration
    stram = open("%s/.sasha.conf" % os.environ['HOME'], "r")
    conf = yaml.load(stram)
    cluster = [tuple(x.split(':')) for x in conf[conf_block]['cluster']]
    for i,(h,p,wd) in enumerate(cluster):
        if h == host and int(p) == port: return wd, i
    # should not get here
    raise RuntimeError("Must have own host port in conf file")

'''
Dump all reducer output to file
'''
def reducers_to_output(f_output_file, reducer_objs):
    for rkey in reducer_objs.keys():
        for curr_res in reducer_objs[rkey].result():
            res = '\t'.join(map(str,[rkey,curr_res]))
            f_output_file.write(res + '\n')
            f_output_file.flush()


if __name__ == "__main__":    
    server_loop()

                
