# -*- coding: utf-8 -*-
import os, sys, re, codecs, shutil

cfg = """
\\Preamble{xhtml}
\\Configure{HColor}{DarkGray}{\\#1A1A1A}
\\begin{document}
\\EndPreamble
"""

ad = '''<p>Your ad code goes here</p>
'''

def translit_low(c):
    res = c.lower()
    res = res.replace(u'ğ','g')
    res = res.replace(u'ş','s')
    res = res.replace(u'ı','i')
    res = res.replace(u'ç','c')
    res = res.replace(u'ü','u')
    res = res.replace(u'ö','o')
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
        
        fr = os.getcwd() + "/static_files"
        tgt = "/home/burak/Downloads/classnotes_html/scripts"
        cmd = "python /home/burak/Documents/kod/rsync.py '%s' '%s'" % (fr, tgt)
        print cmd
        os.system(cmd)

        fr = os.getcwd()
        tgt = tgt + "/static/classnotes"
        cmd = "python /home/burak/Documents/kod/rsync.py '%s' '%s'" % (fr, tgt)
        print cmd
        os.system(cmd)
        
        for topdir in ['algs','calc_multi','chaos','compscieng',
                       'func_analysis','linear','ode','pde','stat',
                       'tser','vision']:
            print 'main',topdir
            dir = tgt + "/" + topdir
            print 'dir',dir
            fout = codecs.open(dir + "/index.html",mode="w",encoding="utf-8")
            fout.write("<html>\n")
            for subdir in sorted(os.listdir(dir)):
                if not os.path.isdir(dir + "/" + subdir): continue
                print 'subdir',subdir
                if "cover" in subdir or "00" in subdir or "dict" in subdir: continue
                print dir + "/" + subdir
                # read tex file, get header
                fin = open(dir + "/" + subdir + "/" + subdir + ".tex")
                content = fin.read()
                title = re.findall(u"begin.*?document.*?\n(.*?)\n",content.decode('latin5'),re.DOTALL)[0]
                url = translit_low(title)
                url = url.replace(" ","_")
                url = url.replace("(","_")
                url = url.replace(")","_")
                url = url.replace("/","_")
                url = url.replace("-","")
                url = url.replace(",","")
                url = url + ".html"
                print 'the url is', subdir + "/" + url
                line = "<a href='%s'>%s</a><br/><br/>" % (subdir + "/" + url, title)
                fout.write(line)
                fout.write("\n")
                fin.close()

                print 'chdir', dir + "/" + subdir
                os.chdir(dir + "/" + subdir)

                if os.path.isfile(subdir + ".html"): 
                    textime = os.path.getmtime(subdir + ".tex")
                    htmltime = os.path.getmtime(subdir + ".html")
                    if htmltime > textime:
                        print "HTML exists.. skipping"
                        continue

                ocfg = open(subdir + ".cfg", "w")
                ocfg.write(cfg)
                ocfg.close()
                cmd = 'make4ht -u %s.tex -s ' % subdir
                os.system(cmd)

                # read 16 lines, insert ad, then continue
                fin1 = open(subdir + ".html")
                fout1 = codecs.open(url, "w")
                for i,line in enumerate(fin1.readlines()):
                    fout1.write(line)
                    if i == 15:  fout1.write(ad)
                fout1.close()                                
                #break
            fout.write("</html>\n")
            fout.close()
            #break
                      
