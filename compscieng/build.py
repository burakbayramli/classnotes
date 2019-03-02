import os, sys, glob, shutil

pdfs = " ".join(sorted(list(glob.glob('./*/*.pdf'))))

if len(sys.argv) == 1:
    os.system("pdfunite %s  /data/data/com.termux/files/home/Downloads/compscieng.pdf" % pdfs)
    exit()    

elif sys.argv[1] == 'all':
    for a in sorted(glob.glob("compscieng*")):
        os.chdir(a)
        os.system("pdflatex -shell-escape compscieng*.tex")    
        os.chdir("..")
            
elif sys.argv[1] == 'clean':
    os.system("find . -name '_region_*' | xargs rm  -rf")
    os.system("find . -name '_minted-*' | xargs rm  -rf")

elif sys.argv[1] == 'tex':
    file = glob.glob('compscieng_*.tex')
    os.system("pdflatex -shell-escape %s" % file[0])
    d = "/data/data/com.termux/files/home/storage/downloads"
    if os.path.isdir(d):
        ff = file[0].replace(".tex",".pdf")
        shutil.copy(ff,d)
    
    
