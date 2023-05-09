import os, sys, re, codecs, shutil, markdown
import glob, os, sys

TARGET_DIR = "/home/burak/Documents/dersblog/sk"

if __name__ == "__main__": 
 
    if len(sys.argv) < 2:
        print ("options: html | pdf | pdf-unite | git-change ")
        exit()

    if sys.argv[1] == 'pdf':
        retpath = os.getcwd()
        files = glob.glob("**/**/*.md")
        files = sorted(files)
        for i,file in enumerate(files):
            f = os.path.basename(file).replace(".md",".pdf")
            dir = os.path.dirname(file)
            res = re.findall("(\d\d\d\d\/\d\d)",file)[0]
            i = int(res.replace("/",""))
            f = "/opt/Downloads/skpdf/%04d-%s" % (i,f)
            os.chdir(dir)
            cmd = "pandoc %s --latex-engine=xelatex -fmarkdown-implicit_figures -o %s" % (os.path.basename(file),f)
            if not os.path.isfile(f): 
                print (cmd)                
                os.system(cmd)
            os.chdir(retpath)

    if sys.argv[1] == 'pdf-unite':
        os.system("pdfunite /opt/Downloads/skpdf/*.pdf ~/Downloads/sk-blog-all.pdf")
                
    if sys.argv[1] == 'git-change':    
        os.system('git log --since="7 day ago" --name-only --pretty=format: | sort | uniq')
