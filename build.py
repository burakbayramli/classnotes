import os, sys, shutil

cfg = """
\\Preamble{xhtml}
\\Configure{HColor}{DarkGray}{\\#1A1A1A}
\\begin{document}
\\EndPreamble
"""

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
        tgt = "/home/burak/Downloads/classnotes_html"
        #cmd = "python /home/burak/Documents/kod/rsync.py '%s' '%s' --delete" % (os.getcwd(), tgt)
        #print cmd
        #os.system(cmd)
        for topdir in ['algs','calc_multi','chaos','compscieng',
                       'func_analysis','linear','ode','pde','stat',
                       'tser','vision']:
            print 'main',topdir
            dir = tgt + "/" + topdir
            print 'dir',dir
            for subdir in os.listdir(dir):
                print 'subdir',subdir
                print dir + "/" + subdir
                # read tex file, get header
                                
            break


        # for root, directories, filenames in os.walk(tgt):
        #     print root, directories, filenames
        #     for filename in filenames:
        #         print filename
        #         path = os.path.join(root,filename)
        #         if ".tex" in path:
        #             dir = os.path.dirname(os.path.abspath(path))
        #             if "00" in dir: continue
        #             base = os.path.basename(path).replace(".tex","")
        #             print dir, base
        #             os.chdir(dir)
        #             out = open(base + ".cfg", "w")
        #             out.write(cfg)
        #             out.close()
        #             cmd = 'htlatex %s.tex "%s" "" "" -shell-escape' % (base,base)     
        #             print cmd
        #             os.system(cmd)
        #             break
        #         break
            
