import os, sys, re, codecs, shutil, markdown
import glob, os, sys

TARGET_DIR = "/home/burak/Documents/dersblog/sk"

if __name__ == "__main__": 
 
    if len(sys.argv) < 2:
        print ("options: html | years ")
    exit()  

    if sys.argv[1] == 'html':

        shutil.copy("_config.yml","/home/burak/Documents/dersblog")

        ignore = " --ignore-list=.git,.zip,.pdf,out.html,.apk,.stl,.key"
        fr = os.getcwd()
        cmd = "python /home/burak/Documents/kod/rsync.py '%s' '%s'  --delete 1" % (fr, TARGET_DIR)
        cmd += ignore
        print (cmd)
        os.system(cmd)

    if sys.argv[1] == 'years':
        for year in range(2000,2023):
            if year == 2007: continue
            os.system("echo '# %d\n' > %d/index.md" % (year,year))
            os.system("python -u gen.py %d >> %d/index.md" % (year,year))

