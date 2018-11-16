import os, sys, glob, shutil

pdfs = " ".join(sorted(list(glob.glob('./*/*.pdf'))))

if len(sys.argv) == 1:
    os.system("pdftk %s output ../../Dropbox/Public/skfiles/elecmag.pdf" % pdfs)
    exit()    
elif sys.argv[1] == 'all':
    for a in sorted(glob.glob("elecmag*")):
        os.chdir(a)
        os.system("pdflatex -shell-escape %s" % glob.glob("elecmag_*.tex")[0] )
        os.chdir("..")
elif sys.argv[1] == 'clean':
    os.system("find . -name '_region_*' | xargs rm  -rf")
    os.system("find . -name '_minted-*' | xargs rm  -rf")
    os.system("find . -name '*.log' | xargs rm  -rf")
    os.system("find . -name '*.aux' | xargs rm  -rf")
    os.system("find . -name '*.out' | xargs rm  -rf")

elif sys.argv[1] == 'tex':
    file = glob.glob('elecmag_*.tex')
    print (file[0])
    os.system("pdflatex -shell-escape %s" % file[0])
    d = "/data/data/com.termux/files/home/storage/downloads"
    if os.path.isdir(d):
        ff = file[0].replace(".tex",".pdf")
        shutil.copy(ff,d)
    
    
