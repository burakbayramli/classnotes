import os, sys, glob

pdfs = " ".join(sorted(list(glob.glob('./*/*.pdf'))))
d = os.environ['HOME']
if len(sys.argv) == 1 :
    cmd = "pdfunite %s " + d + "/Downloads/stat.pdf"
    os.system(cmd % pdfs)
    exit()
    
elif sys.argv[1] == 'all':
    for a in sorted(glob.glob("stat*")):
        os.chdir(a)
        os.system("pdflatex -shell-escape stat*.tex")    
        os.chdir("..")

elif sys.argv[1] == 'clean':
    os.system("find . -name '_region_*' | xargs rm  -rf")
    os.system("find . -name '_minted-*' | xargs rm  -rf")
        
elif sys.argv[1] == 'tex':
    file = glob.glob('stat_*.tex')
    os.system("pdflatex -shell-escape %s" % file[0])
    
    
