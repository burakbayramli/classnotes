import os, sys, glob

fout = "%s/Downloads/netflix/download/netflix1_reorg.csv" % os.environ['HOME']
for i,f in enumerate(glob.glob("/home/burak/Downloads/netflix/download/training_set/*")):
    cmd = "cat %s | python reorg.py >> %s" % (f,fout)
    os.system(cmd)
    # quit after few lines if options passed is 'test'
    # we can create a smaller dataset this way
    # otherwise whole netflix data is processed
    if len(sys.argv) > 1 and sys.argv[1] == 'test' and i == 8: break
