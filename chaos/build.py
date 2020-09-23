import os, sys, glob

pdfs = " ".join(sorted(list(glob.glob('./*/*.pdf'))))
d = os.environ['HOME']

if len(sys.argv) == 1 :
    cmd = "pdfunite " + pdfs + " " + d + "/Downloads/chaos.pdf"
    print (cmd)
    os.system(cmd)
    exit()    
elif sys.argv[1] == 'all':
    for a in sorted(glob.glob("chaos_*")):
        os.chdir(a)
        os.system("pdflatex -shell-escape chaos*.tex") 
        os.chdir("..")            
elif sys.argv[1] == 'clean':
    os.system("find . -name '_region_*' | xargs rm  -rf")
    os.system("find . -name '*.log' | xargs rm  -rf")
elif sys.argv[1] == 'tex':
    file = glob.glob('chaos_*.tex')
    os.system("pdflatex -shell-escape %s" % file[0])
        
