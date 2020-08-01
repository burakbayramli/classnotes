from optparse import OptionParser
import zmq, time, sys, sasha
import inspect, os, re, numpy as np

class SashaJob:
    
    def __init__(self):
        self.reducer_key = None
        self.is_complete = False

    def mapper(self, id, value):
        yield id, value
        
    def reducer(self, value):
        self.value = value # identityreducer
        self.is_complete = True

    def mapper_final(self):
        return iter([])
                
    def result(self):
        yield self.value # identityreducer
        
    @classmethod
    def run(self):        
        parser = OptionParser()        
        parser.add_option("-i", "--input", dest="input")
        parser.add_option("-o", "--output", dest="output")
        parser.add_option("-f", "--file", dest="file")
        parser.add_option("-r", "--response", dest="response")
        (options, args) = parser.parse_args()
        print options.output
        print options.file
        print options.input
        print options.response
        
        conf_block =  sys.argv[1]
        
        context = zmq.Context()
        out_conns = sasha.get_cluster_connections(context, conf_block)
        job_file = sys.argv[0]
        job_file_py = sys.argv[0].replace(".py","")
        klass = self.__name__        
        job_file_cnt = open(job_file).read()

        # reset all servers, do these before all the rest
        for i in range(len(out_conns)):
            out_conns[i].send(sasha.__COMMAND__ + "RESET")
            if options.file:
                file_cnt = open(options.file).read()
                out_conns[i].send(sasha.__COMMAND__ + "FILE=%sCONTENT=%s" % (options.file, file_cnt))
            if options.output: out_conns[i].send(sasha.__COMMAND__ + "OUTPUT_FILE=%s" % options.output )
            out_conns[i].send(sasha.__COMMAND__ + "FILE=%sCONTENT=%s" % (job_file, job_file_cnt))
            out_conns[i].send(sasha.__COMMAND__ + "INIT=%s %s" % (job_file_py, klass))
            
        # wait for some time here, give some time for the output file
        # to be opened, otherwise there are attempts to write to a closed
        # output file, some servers finish init too fast, sending reducer
        # messages already for which some servers might not be ready for
        time.sleep(2)
            
        # specifiy the input file - this also starts streaming that file
        # into that server.
        if options.input:
            for i in range(len(out_conns)):
                out_conns[i].send(sasha.__COMMAND__ + "INPUT=%s" % options.input)
                
        if options.response:
            response = context.socket(zmq.PULL)
            rport = re.search(':(\d+)',options.response).groups(1)
            rport = int(rport[0])
            for i in range(len(out_conns)):
                out_conns[i].send(sasha.__COMMAND__ + "RESPONSE_ADDRESS=%s" % options.response)
            response.bind('tcp://*:%d' % rport)
            server_responses = [None for x in range(len(out_conns))]
            while True:
                data = response.recv()
                print data
                resp_server = re.search("SERVER=(\d*)",data).groups(1)[0]
                resp_klass = re.search("KLASS=(.*)",data).groups(1)[0]
                print resp_server, resp_klass
                server_responses[int(resp_server)] = True
                print 'server %s for klass %s' % (resp_server,resp_klass), "is done"
                if np.all(server_responses): exit(0)
                
