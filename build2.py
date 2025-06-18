import codecs, re, os, sys, shutil, md1, json
import util2, markdown2, glob

dirs = ['algs','calc_multi','chaos','compscieng',
        'func_analysis','linear','ode', 'stat',
        'tser','vision','phy']

def doc_dirs(topdirs):
    curr = os.getcwd()
    print (curr)
    for topdir in topdirs:
        print (topdir)
        for subdir in sorted(os.listdir(curr + "/" + topdir)):
            print (subdir)
            if not os.path.isdir(curr + "/" + topdir + "/" + subdir): continue
            if "cover" in subdir or "000" in subdir : continue
            #print (topdir, subdir)
            os.chdir(curr + "/" + topdir + "/" + subdir)
            mdfile = curr + "/" + topdir + "/" + subdir + "/" + subdir + ".md"
            shutil.copy(mdfile,"/tmp/out.md")
            cmd = "pandoc  /home/burak/Documents/cl3/metadata.yaml --standalone --mathjax -f markdown -t html /tmp/out.md -o /tmp/out.html" 
            os.system(cmd)
            cmd = "pandoc %s /home/burak/Documents/cl3/metadata.yaml -t latex  -fmarkdown-implicit_figures -o %s" % ("/tmp/out.md","/tmp/out.pdf")
            os.system(cmd)            
            pdffile = curr + "/" + topdir + "/" + subdir + "/" + subdir + ".pdf"
            htmlfile = curr + "/" + topdir + "/" + subdir + "/" + subdir + ".html"
            print ("copying to", mdfile)            
            shutil.copy("/tmp/out.pdf", pdffile) 
            shutil.copy("/tmp/out.html", htmlfile) 
            #break

def copy_files_and_dirs(fr,to,ignore_list):
    if ignore_list == None:
        ignore_list = []
    else:
        ignore_list = ignore_list.split(',')
    frdirs,frfiles =  ls(fr,ignore_list)
    todirs,tofiles = ls(to)

    tofilesdict = dict(tofiles)
    print ('create dirs')
    todirs_tmp = dict([(x.replace(fr,to),0) for x in todirs])
    diff = [x for x in frdirs if x.replace(fr,to) not in todirs_tmp]
    for x in diff:
        x=x.replace(fr,to)
        if os.path.exists(x) == False:            
            os.mkdir(x)

    print ('1 a files not in b')
    for (x,size) in frfiles:
        x_to=x.replace(fr,to)
        if x_to in tofilesdict and tofilesdict[x_to] != size: 
            print ('copying %s %s' % (x,x_to))
            shutil.copy(x,x_to)
        elif x_to not in tofilesdict: 
            print ('copying %s %s' % (x,x_to))
            shutil.copy(x,x_to)
            
    return frdirs, todirs

def ls(d,ignore_list=[]):
    print ('ls ignore lst', ignore_list)
    dirs = []; files = []
    for root, directories, filenames in os.walk(d):
        for directory in directories:
            path = os.path.join(root, directory)
            do_add = True
            for ignore in ignore_list:
                if ignore in path:
                    print ('ignoring', path); do_add = False
            if do_add: dirs.append(path)
        for filename in filenames: 
            path = os.path.join(root,filename)
            do_add = True
            for ignore in ignore_list:
                if ignore in path: do_add = False
            if do_add: files.append((path, os.path.getsize(path)))
    return dirs, files

def get_title_from_md(f):
    fin = codecs.open(f, encoding='utf8')
    line = fin.readline()
    fin.close()
    return line[2:].strip()

def title_sci(to):
    lecs = json.loads(open("lecs.conf").read())
    for dir in dirs:
        out = to + "/" + dir + "/index.html"
        fout = codecs.open(out,mode="w",encoding="utf-8")
        fout.write(util2.html_head.replace("[title]",lecs['titles'][dir]))
        fout.write("<a href='%s'>PDF</a><br/><br/>" % lecs['pdfs'][dir])
        for subdir in sorted(os.listdir(dir)):
            if not os.path.isdir(dir + "/" + subdir): continue
            if "cover" in subdir or "000" in subdir : continue
            # read tex file, get header
            md = subdir + ".md"
            title = get_title_from_md(dir + "/" + subdir + "/" + md)
            html = subdir + "/" + util2.filename_from_title(title) + ".html"        
            fout.write("<p><a href='%s'>%s</a><p/>" % (html,title))
        fout.write("<br/><a href='../index.html'>Yukarı</a><br/>")


def gen_html_sk():
    dirs, files = ls(os.getcwd() + "/sk")
    for (f,size) in files:
        if ".md" in f:
            path = os.path.dirname(f)
            fmd = os.path.basename(f)
            fhtml = os.path.basename(f).replace(".md",".html")
            update = True
            if os.path.isfile(path + "/" + fhtml):
                mdtime = os.path.getmtime(path + "/" + fmd)
                htmltime = os.path.getmtime(path + "/" + fhtml)
                if htmltime > mdtime: update = False
            if update:
                print ('Generating html for', f)
                title = get_title_from_md(f)
                content = open(path + "/" + fmd).read()
                res = util2.html_head.replace("[title]","")
                res += markdown2.markdown(content, extras=['fenced-code-blocks'])
                res += util2.bottom
                fout = open(path + "/" + fhtml, "w")
                fout.write(res)
                fout.close()
        
def title_sk(to):
    d = "sk"
    for year in range(2000,2026):
        if year == 2007: continue
        out = d + "/" + str(year) + "/index.md"
        fout = codecs.open(out,mode="w",encoding="utf-8")
        fout.write("# %s\n\n" % str(year))
        for f in sorted(glob.glob(d + "/" + str(year) + "/*/*.md")):
            fin = open(f)
            for line in fin.readlines():
                ff = f.replace(d, '')[1:]
                ff = ff.replace(str(year) + "/", "")
                ff = ff.replace(".md",".html")
                fout.write ("[%s](%s)\n\n" % (line[2:].strip(), ff))
                break
        fout.close()

            
if __name__ == "__main__": 

    fr = os.getcwd()
    to = os.environ['HOME'] + "/Documents/dersblog"
    
    if sys.argv[1] == "doc":
        if len(sys.argv) < 3:
            # for single file, command run inside working dir
            currfile = os.path.basename(os.getcwd())
            currdir = os.path.dirname(os.getcwd())
            mdfile = currdir + "/" + currfile + "/" + currfile + ".md"
            htmlfile = currdir + "/" + currfile + "/" + currfile + ".html"
            pdffile = currdir + "/" + currfile + "/" + currfile + ".pdf"
            shutil.copy(mdfile,"/tmp/out.md")
            cmd = "pandoc  /home/burak/Documents/cl3/metadata.yaml --standalone --mathjax -f markdown -t html /tmp/out.md -o /tmp/out.html" 
            os.system(cmd)
            cmd = "pandoc %s /home/burak/Documents/cl3/metadata.yaml -t latex  -fmarkdown-implicit_figures -o %s" % ("/tmp/out.md","/tmp/out.pdf")
            os.system(cmd) 
            shutil.copy("/tmp/out.pdf", pdffile) 
            shutil.copy("/tmp/out.html", htmlfile)
            exit()
        if sys.argv[2] == "all":
            # for all documents
            doc_dirs(dirs)
            exit()
        else:
            # for selected subdirectory
            doc_dirs([sys.argv[2]])
            exit()
    
    elif sys.argv[1] == "push":
        print ('title')
        frdirs, todirs = copy_files_and_dirs(fr, to, ".git,.pdf,_minted,zwork")
        os.chdir(to)
        title_sci(to)
        for topdir in dirs:
            print (topdir)
            for subdir in sorted(os.listdir(to + "/" + topdir)):
                if not os.path.isdir(to + "/" + topdir + "/" + subdir): continue
                if "cover" in subdir or "000" in subdir : continue
                htmlfile = to + "/" + topdir + "/" + subdir + "/" + subdir + ".html"
                mdfile = to + "/" + topdir + "/" + subdir + "/" + subdir + ".md"
                title = get_title_from_md(mdfile)
                html_long = to + "/" + topdir + "/" + subdir + "/" + util2.filename_from_title(title) + ".html"                
                print (htmlfile, html_long)
                shutil.copy(htmlfile, html_long)
                
        title_sk(to)
        gen_html_sk()
        exit()

    elif sys.argv[1] == "comb":
        pdfdir = sys.argv[2]
        pdfs = " ".join(sorted(list(glob.glob(pdfdir + '/*/*.pdf'))))
        print (pdfs)
        home = os.environ['HOME']
        cmd = "pdfunite " + pdfs + " " + home + "/Downloads/" + pdfdir + ".pdf"
        os.system(cmd)
