#!/usr/bin/env python
# -*- coding: utf-8 -*- 
import sys, glob, os, shutil, re, argparse, json
import markdown, markdown2, os, sys, util, codecs

topdirs = ['algs','calc_multi','chaos','compscieng',
           'func_analysis','linear','ode', 'stat',
           'tser','vision','phy']

def deleteDir(path):
    mswindows = (sys.platform == "win32")
    if mswindows: 
        cmd = "RMDIR "+ path +" /s /q"
    else:
        cmd = "rm -rf "+path
    result = getstatusoutput(cmd)
    if(result[0]!=0):
        raise RuntimeError(result[1])

def deleteFile(path):
    print (path)
    mswindows = (sys.platform == "win32")
    if mswindows: 
        cmd = 'DEL /F /S /Q "%s"' % path
    else:
        cmd = 'rm -rf "' + path + '"'
    result = getstatusoutput(cmd)
    if(result[0]!=0):
        raise RuntimeError(result[1])

def getstatusoutput(cmd):
    pipe = os.popen(cmd + ' 2>&1', 'r')
    text = pipe.read()
    sts = pipe.close()
    if sts is None: sts = 0
    if text[-1:] == '\n': text = text[:-1]
    return sts, text
        
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
                
def clean(to):
    print ('deleting ..')
    for top in topdirs:
        d = to + "/" + top
        deleteDir(d)
    d = to + "/sk"
    deleteDir(d)

def title_tex(to):
    lecs = json.loads(open("lecs.conf").read())
    for dir in topdirs:
        out = to + "/" + dir + "/index.html"
        fout = codecs.open(out,mode="w",encoding="utf-8")
        fout.write(util.html_head.replace("[title]",lecs['titles'][dir]))
        fout.write("<a href='%s'>PDF</a><br/><br/>" % lecs['pdfs'][dir])
        for subdir in sorted(os.listdir(dir)):
            if not os.path.isdir(dir + "/" + subdir): continue
            if "cover" in subdir or "000" in subdir : continue
            # read tex file, get header
            tex = subdir + ".tex"
            title = util.get_title_from_tex(dir + "/" + subdir + "/" + tex)
            html = subdir + "/" + util.filename_from_title(title) + ".html"        
            fout.write("<p><a href='%s'>%s</a><p/>" % (html,title))
        fout.write("<br/><a href='../index.html'>Yukarı</a><br/>")

def title_sk(to):
    d = "sk"
    for year in range(2000,2024):
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

def get_title_from_md(f):
    fin = codecs.open(f, encoding='utf8')
    line = fin.readline()
    fin.close()
    return line[2:].strip()

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
                res = util.html_head.replace("[title]","")
                res += markdown2.markdown(content, extras=['fenced-code-blocks'])
                res += util.bottom
                fout = open(path + "/" + fhtml, "w")
                fout.write(res)
                fout.close()

def gen_html_cl():
    basedir = os.getcwd()
    for dir in topdirs:
        for subdir in sorted(os.listdir(dir)):
            if not os.path.isdir(dir + "/" + subdir): continue
            if "cover" in subdir or "000" in subdir : continue
            # read tex file, get header
            target_dir = basedir + "/" + dir + "/" + subdir
            os.chdir(target_dir)        
            tex = subdir + ".tex"
            title = util.get_title_from_tex(tex)
            html = util.filename_from_title(title) + ".html"
            update = True
            if os.path.isfile(html):
                textime = os.path.getmtime(tex)
                htmltime = os.path.getmtime(html)
                if htmltime > textime: update = False
            if update:
                print ('Generating html for', tex)
                util.tex_mathjax_html(tex, html, title)
            os.chdir(basedir)


if __name__ == "__main__":
    
    fr = os.getcwd()
    to = os.environ['HOME'] + "/Documents/dersblog"
    #to = os.environ['HOME'] + "/Documents/repos/burakbayramli.github.com/scitech"
 
    if len(sys.argv) < 2:
      print ("options: all | clean | html ")
      exit()  
      
    if sys.argv[1] == 'title':
        frdirs, todirs = copy_files_and_dirs(fr, to, ".git,.pdf,_minted,zwork")
        os.chdir(to)
        title_tex(to)
        os.chdir(to)
        title_sk(to)
        
    if sys.argv[1] == 'html': 
        frdirs, todirs = copy_files_and_dirs(fr, to, ".git,.pdf,_minted,zwork,README.md")
        os.chdir(to)
        gen_html_sk()
        os.chdir(to)
        gen_html_cl()
        
    if sys.argv[1] == 'clean': 
        clean(to)
