# -*- coding: utf-8 -*-
import os, sys, re, codecs

cfg = """
\\Preamble{xhtml}
\\Configure{HColor}{DarkGray}{\\#1A1A1A}
\\begin{document}
\\EndPreamble
"""

def translit_low(c):
    res = c.lower()
    res = res.replace(u'ğ','g')
    res = res.replace(u'ş','s')
    res = res.replace(u'ı','i')
    res = res.replace(u'ç','c')
    return res

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
            fout = codecs.open(dir + "/index.html",mode="w",encoding="utf-8")
            for subdir in os.listdir(dir):
                print 'subdir',subdir
                print dir + "/" + subdir
                # read tex file, get header
                fin = open(dir + "/" + subdir + "/" + subdir + ".tex")
                content = fin.read()
                #print content
                title = re.findall(u"begin.*?document.*?\n(.*?)\n",content.decode('latin5'),re.DOTALL)[0]
                url = translit_low(title)
                url = url.replace(" ","_")
                url = url.replace("(","_")
                url = url.replace(")","_")
                url = url.replace("-","")
                url = url.replace(",","")
                url = url + ".html"
                line = "<a href='%s'>%s</a>" % (url, title)
                fout.write(line)
                fout.write("\n")
                fin.close()
                break
            fout.close()
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
            
