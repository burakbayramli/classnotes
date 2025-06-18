import codecs, re, os, sys, shutil, md1

dirs = ['algs','calc_multi','chaos','compscieng',
        'func_analysis','linear','ode', 'stat',
        'tser','vision','phy']

def doc_dirs(topdirs):
    curr = os.getcwd()
    print (curr)
    for topdir in topdirs:
        print (topdir)
        for subdir in sorted(os.listdir(curr + "/" + topdir)):
            print (subdir)
            if not os.path.isdir(curr + "/" + topdir + "/" + subdir): continue
            if "cover" in subdir or "000" in subdir : continue
            #print (topdir, subdir)
            os.chdir(curr + "/" + topdir + "/" + subdir)
            mdfile = curr + "/" + topdir + "/" + subdir + "/" + subdir + ".md"
            shutil.copy(mdfile,"/tmp/out.md")
            cmd = "pandoc  /home/burak/Documents/cl3/metadata.yaml --standalone --mathjax -f markdown -t html /tmp/out.md -o /tmp/out.html" 
            os.system(cmd)
            cmd = "pandoc %s /home/burak/Documents/cl3/metadata.yaml -t latex  -fmarkdown-implicit_figures -o %s" % ("/tmp/out.md","/tmp/out.pdf")
            os.system(cmd)            
            pdffile = curr + "/" + topdir + "/" + subdir + "/" + subdir + ".pdf"
            htmlfile = curr + "/" + topdir + "/" + subdir + "/" + subdir + ".html"
            print ("copying to", mdfile)            
            shutil.copy("/tmp/out.pdf", pdffile) 
            shutil.copy("/tmp/out.html", htmlfile) 
            break
        
if __name__ == "__main__": 

    if sys.argv[1] == "doc":
        if len(sys.argv) < 3:
            currfile = os.path.basename(os.getcwd())
            currdir = os.path.dirname(os.getcwd())
            print (currdir, currfile)
            exit()
        if sys.argv[2] == "all":
            doc_dirs(dirs)
        else:
            doc_dirs([sys.argv[2]])
    
