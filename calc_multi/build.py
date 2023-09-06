import os, sys, glob

pdfs = " ".join(sorted(list(glob.glob('./*/*.pdf'))))
d = os.environ['HOME']

if len(sys.argv) == 1:
    cmd = "pdfunite " + pdfs + " " + d + "/Downloads/multivar_calculus.pdf"
    os.system(cmd)
    exit()    
elif sys.argv[1] == 'all':
    for a in sorted(glob.glob("calc*")):
        os.chdir(a)
        os.system("pdflatex -shell-escape %s" % glob.glob("calc_*.tex")[0] )
        os.chdir("..")
        
elif sys.argv[1] == 'clean':
    os.system("find . -name '_region_*' | xargs rm  -rf")
    os.system("find . -name '_minted-*' | xargs rm  -rf")
    os.system("find . -name '*.log' | xargs rm  -rf")
    os.system("find . -name '*.aux' | xargs rm  -rf")
    os.system("find . -name '*.out' | xargs rm  -rf")

elif sys.argv[1] == 'tex':
    file = glob.glob('calc_multi_*.tex')
    os.system("pdflatex -shell-escape %s" % file[0])
        
elif sys.argv[1] == 'md':
    sys.path.append("../.."); import util
    file = glob.glob('*.tex')
    ftex = file[0]; path = os.getcwd()
    util.tex_mathjax_html(path + "/" + ftex, path + "/" + "out.html","adkasdf")
    
