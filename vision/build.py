import os, sys, glob

pdfs = " ".join(sorted(list(glob.glob('./*/*.pdf'))))
d = os.environ['HOME']
if len(sys.argv) == 1:
    cmd = "pdfunite %s " + d + "/Downloads/vision.pdf"
    os.system(cmd % pdfs)
    exit()
    
elif sys.argv[1] == 'all':
    for a in sorted(glob.glob("vision_*")):
        os.chdir(a)
        os.system("pdflatex -shell-escape %s" % a) 
        os.chdir("..")
            
elif sys.argv[1] == 'clean':
    os.system("find . -name '_region_*' | xargs rm  -rf")
    os.system("find . -name '_minted-*' | xargs rm  -rf")
    os.system("find . -name '*.log' | xargs rm  -rf")
    os.system("find . -name '*.aux' | xargs rm  -rf")
    os.system("find . -name '*.out' | xargs rm  -rf")

elif sys.argv[1] == 'tex':
    file = glob.glob('vision_*.tex')
    os.system("pdflatex -shell-escape %s" % file[0])
    
    
