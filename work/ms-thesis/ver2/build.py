import os, sys

if len(sys.argv) == 1 or sys.argv[1] == 'tex':
    os.system("pdflatex -shell-escape ms-thesis.tex")
    os.system("cp ms-thesis.pdf $HOME/Dropbox/Public/skfiles/")
    os.system("evince ms-the*.pdf")
    exit()
    
