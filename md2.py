import codecs, re, os, sys, shutil, md1

topdirs = ['algs','calc_multi','chaos','compscieng',
           'func_analysis','linear','ode', 'stat',
           'tser','vision','phy']

#topdirs = ['chaos']

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
        texfile = subdir + ".tex"
        print (texfile)
        md1.conv(texfile)
        mdfile = curr + "/" + topdir + "/" + subdir + "/" + subdir + ".md"
        pdffile = curr + "/" + topdir + "/" + subdir + "/" + subdir + ".pdf"
        htmlfile = curr + "/" + topdir + "/" + subdir + "/" + subdir + ".html"
        print ("copying to", mdfile)
        shutil.copy("/tmp/out.md", mdfile) 
        #shutil.copy("/tmp/out.pdf", pdffile) 
        #shutil.copy("/tmp/out.html", htmlfile) 
        
