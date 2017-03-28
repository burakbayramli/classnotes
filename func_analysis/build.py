import os, sys, glob

pdfs = " ".join(sorted(list(glob.glob('./*/*.pdf'))))

if len(sys.argv) == 1 :
    os.system("pdftk %s output ../../Dropbox/Public/skfiles/functional_analysis.pdf" % pdfs)
    exit()
    
elif sys.argv[1] == 'all':
    for a in sorted(glob.glob("func*")):
        os.chdir(a)
        os.system("pdflatex -shell-escape func*.tex")    
        os.chdir("..")

elif sys.argv[1] == 'clean':
    os.system("find . -name '_region_*' | xargs rm  -rf")
    os.system("find . -name '_minted-*' | xargs rm  -rf")
        
elif sys.argv[1] == 'tex':
    file = glob.glob('func_*.tex')
    os.system("pdflatex -shell-escape %s" % file[0])
    
    
