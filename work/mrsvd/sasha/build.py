import sys, os
if sys.argv[1] == 'deploy':
    os.system("scp -r /home/burak/Documents/sasha burak@host2:/home/burak/Documents")
    os.system("scp -r /home/burak/Documents/sasha burak@host1:/home/burak/Documents")
if sys.argv[1] == 'zip':    
    cmd = "zip ~/Downloads/sasha-0.1.7.zip -r * --exclude=.git/* "
    os.system(cmd)    
if sys.argv[1] == 'dep':    
    os.system("python setup.py sdist")
    os.system("gksudo python setup.py install")
    
