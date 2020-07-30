# -*- coding: utf-8 -*-

#
# __TODO__: for html, put article title inside <title></title>
#
import os, sys

if __name__ == "__main__": 
 
    if sys.argv[1] == 'send':
        os.system("scp *.py pi@192.168.43.89:/home/pi/Documents/pi1")
        
