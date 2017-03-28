import os, sys, glob

pdfs = " ".join(sorted(list(glob.glob('./*/*.pdf'))))

if len(sys.argv) == 1:
    os.system("pdftk %s output ../../Dropbox/Public/skfiles/linear_strang.pdf" % pdfs)
    exit()
    
elif sys.argv[1] == 'all':
    for a in sorted(glob.glob("linear*")):
        os.chdir(a)
        os.system("pdflatex -shell-escape *.tex")    
        os.chdir("..")
        
elif sys.argv[1] == 'clean':
    os.system("find . -name '_region_*' | xargs rm  -rf")
    os.system("find . -name '_minted-*' | xargs rm  -rf")
        
elif sys.argv[1] == 'tex':
    file = glob.glob('linear_*.tex')
    os.system("pdflatex -shell-escape %s" % file[0])
    
