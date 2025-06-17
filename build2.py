import codecs, re, os, sys

if __name__ == "__main__":
    
    if len(sys.argv) < 2: exit()
    
    if sys.argv[1] == "pdf": 
        currfull = os.getcwd()
        currd = os.path.dirname(currfull)
        currf = os.path.basename(currfull)
        print (currf, currd)
    
