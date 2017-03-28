import os, sys, shutil

cfg = """
\\Preamble{xhtml}
\\Configure{HColor}{DarkGray}{\\#1A1A1A}
\\begin{document}
\\EndPreamble
"""
def getstatusoutput(cmd):
    """Return (status, output) of executing cmd in a shell."""
    mswindows = (sys.platform == "win32")
    if not mswindows:
        return commands.getstatusoutput(cmd)
    pipe = os.popen(cmd + ' 2>&1', 'r')
    text = pipe.read()
    sts = pipe.close()
    if sts is None: sts = 0
    if text[-1:] == '\n': text = text[:-1]
    return sts, text

def deleteDir(path):
    """deletes the path entirely"""
    mswindows = (sys.platform == "win32")
    if mswindows: 
        cmd = "RMDIR "+ path +" /s /q"
    else:
        cmd = "rm -rf "+path
    result = getstatusoutput(cmd)
    if(result[0]!=0):
        raise RuntimeError(result[1])

if __name__ == "__main__": 
 
    if len(sys.argv) == 1: exit()

    if sys.argv[1] == 'clean':
        os.system("find . -name _region_* | xargs rm -rf ")
        os.system("find . -name '*.log' | xargs rm -rf ")
        os.system("find . -name '*.aux' | xargs rm -rf ")
        os.system("find . -name '*.out' | xargs rm -rf ")
        os.system("find . -name '_minted-*' | xargs rm -rf ")
        os.system("find . -name '_preview*' | xargs rm -rf ")

    if sys.argv[1] == 'all':
        for x in os.listdir("."):
            if ".git" in x: continue
            if os.path.isdir(x):
                os.chdir(x)
                print "building", x
                os.system("python build.py")
                os.chdir("..")

    if sys.argv[1] == 'html':
        # htlatex dosya.tex "dosya" "" "" -shell-escape
        tgt = "%s/classnotes" % os.environ['TEMP']
        #print tgt
        #deleteDir(tgt)
        #shutil.copytree(".",  tgt, ignore=shutil.ignore_patterns(".git"))
        files = []
        for root, directories, filenames in os.walk(tgt):
            for filename in filenames: 
                path = os.path.join(root,filename)
                if ".tex" in path:
                    dir = os.path.dirname(os.path.abspath(path))
                    base = os.path.basename(path).replace(".tex","")
                    print dir, base
                    os.chdir(dir)
                    out = open(base + ".cfg", "w")
                    out.write(cfg)
                    out.close()
                    cmd = 'htlatex %s.tex "%s" "" "" -shell-escape' % (base,base)                    
                    print cmd
                    os.system(cmd)
                    exit()
