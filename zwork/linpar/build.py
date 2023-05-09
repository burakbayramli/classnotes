import os, sys

if len(sys.argv) > 1 and sys.argv[1] == 'tex':
    os.system("pdflatex linpar6.tex")
if len(sys.argv) > 1 and sys.argv[1] == 'zip':
    os.system("zip -r /home/burak/Downloads/linpar8.zip PRIMEarxiv.sty linpar6.tex mapreduce1.jpg mult1.jpg splitprocess.jpg")
       
