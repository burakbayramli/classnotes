import os, sys, re, codecs, shutil, markdown
import glob, os, sys

TARGET_DIR = "/home/burak/Documents/dersblog/sk"

if __name__ == "__main__": 
 
    if len(sys.argv) == 1: exit()

    if sys.argv[1] == 'html':
        
        fr = os.getcwd()
        cmd = "python /home/burak/Documents/kod/rsync.py '%s' '%s'  --delete 1 --ignore-list=.git,.zip,.pdf" % (fr, TARGET_DIR)
        print (cmd)
        os.system(cmd)

    
        
