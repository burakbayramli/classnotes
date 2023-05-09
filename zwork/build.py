import os, sys, glob

pdfs = " ".join(sorted(list(glob.glob('./*/*.pdf'))))
d = os.environ['HOME']
if sys.argv[1] == 'tex':
    file = glob.glob('*.tex')
    os.system("pdflatex -shell-escape %s" % file[0])
    
    
