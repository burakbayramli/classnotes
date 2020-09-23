# Small helper utility to start, stop cluster split, copy to/from
# cluster and combine results.  it will remotely login to all machines
# using ssh and execute commands there. so passwordless ssh and scp
# must be setup for all machines for $USER. 

import yaml, os, sys, glob

stram = open("%s/.sasha.conf" % os.environ['HOME'], "r")
conf = yaml.load(stram)
cluster = [tuple(x.split(':')) for x in conf[sys.argv[2]]['cluster']]

# servers.py stop local2
if sys.argv[1] == 'stop':
    for (h,p,wd) in cluster:
        print 'stopping..', h,p
        cmd = "ssh %s -l %s 'pkill -f sasha'" % (h,os.environ['USER'])
        os.system(cmd); print cmd
        
# servers.py start local2        
if sys.argv[1] == 'start':
    for i,(h,p,wd) in enumerate(cluster):
        print 'starting..', h,p
        cmd = "ssh %s -l %s 'cd %s/sasha; nohup python sasha.py %s %s %d > /tmp/sasha-%d.out 2> /tmp/sasha-%d.err < /dev/null &' " % (h,os.environ['USER'],conf['sasha.dir'],sys.argv[2],h,int(p),i,i) 
        os.system(cmd); print cmd

# splits local file into equal chunks, sends one piece to each cluster
# example: servers.py split-copy local2 [data file].csv [to file].csv
if sys.argv[1] == 'split-copy':
    os.system("split %s -d -n l/%d /tmp/x-" % (sys.argv[3], len(cluster)) )
    for i,x in enumerate(glob.glob("/tmp/x-*")):
        print "copying", x
        (h,p,wd) = cluster[i]
        cmd = "scp %s %s@%s:%s/%s" % (x,os.environ['USER'],h,wd,sys.argv[4])
        print cmd
        os.system(cmd)
        
# example: servers.py pull-combine local2 A.dat
# pulls file from each server, combines them locally under /tmp
if sys.argv[1] == 'pull-combine':
    os.system("rm /tmp/%s" % sys.argv[3])
    for (h,p,wd) in cluster:
        cmd = "scp %s@%s:%s/%s /tmp/s-out-combine" % (os.environ['USER'],h,wd,sys.argv[3])
        os.system(cmd); print cmd
        os.system("cat /tmp/s-out-combine >> /tmp/%s" % sys.argv[3])
                
