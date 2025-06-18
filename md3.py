import codecs, re, os, sys, shutil, md1

topdirs = ['algs','calc_multi','chaos','compscieng',
           'func_analysis','linear','ode', 'stat',
           'tser','vision','phy']

curr = os.getcwd()
print (curr)
for topdir in topdirs:
    print (topdir)
    for subdir in sorted(os.listdir(curr + "/" + topdir)):
        if not os.path.isdir(curr + "/" + topdir + "/" + subdir): continue
        if "cover" in subdir or "000" in subdir : continue
        if "dict" in subdir: continue
        os.chdir(curr + "/" + topdir + "/" + subdir)
        texfile = curr + "/" + topdir + "/" + subdir + "/" + subdir + ".out"
        print (texfile)
        #if os.path.exists(texfile): os.remove(texfile)
        
