import os, sys

if len(sys.argv) == 1 or sys.argv[1] == 'tex':
    os.system("pdflatex xform1.tex")
if len(sys.argv) == 1 or sys.argv[1] == 'zip':
    os.system("zip -r /home/burak/Downloads/xform2.zip PRIMEarxiv.sty xform1.tex kf-out-50.jpg kf-out-70.jpg sudoku81.jpg out2.png kf3.jpg kf4.jpg")
       
