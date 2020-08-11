# -*- coding: utf-8 -*-

#
# __TODO__: for html, put article title inside <title></title>
#
import os, sys, re, codecs, shutil, markdown

topdirs = ['algs','calc_multi','chaos','compscieng',
           'func_analysis','linear','ode','pde','stat',
           'tser','vision','phy']

cfg = """
\\Preamble{xhtml}
\\Configure{HColor}{DarkGray}{\\#1A1A1A}
\\begin{document}
\\EndPreamble
"""

ad = '''
<br/>
<a href='..'>Yukarı</a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
<a href='../..'>Ana Menü</a>
<br/>
'''

TARGET_DIR = "/home/burak/Documents/dersblog"

html_head = '''
<!DOCTYPE html>
<html>
  <head>
    <title>[title]</title>
    <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
    <script type="text/x-mathjax-config">
      MathJax.Hub.Config({
        tex2jax: {inlineMath: [["$","$"]]}
      });
    MathJax.Hub.Register.StartupHook("TeX Jax Ready",function () {
      MathJax.Hub.Insert(MathJax.InputJax.TeX.Definitions.macros,{
        cancel: ["Extension","cancel"],
        bcancel: ["Extension","cancel"],
        xcancel: ["Extension","cancel"],
        cancelto: ["Extension","cancel"]
      });
    });
    </script>
<script type="text/javascript"
   src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-AMS_HTML-full">
</script>
</head>
'''   

def tex_mathjax_html(texfile, htmlfile, title):

   fin = codecs.open(texfile, encoding='iso-8859-9')
   fout = codecs.open("/tmp/out.md",mode="w",encoding="utf-8")

   fin.readline()
   fin.readline()
   title = "# " + fin.readline()
   fout.write(title)

   head = html_head.replace("[title]",title.replace('# ',''))
   fout.write(head)

   for line in fin.readlines():
      line = line.replace("\\ud", "\\mathrm{d}")
      line = line.replace("^*", "@@RR@@@")
      line = line.replace("\\curl", "\\mathrm{curl}")
      line = line.replace("\\sinc", "\\mathrm{sinc}")
      line = line.replace("\\begin{quote}", "```")
      line = line.replace("\\end{quote}", "```")
      line = line.replace("\\bdiv", "\\mathrm{div}")
      line = line.replace("\\tr", "\\mathrm{tr}")
      line = line.replace("\\grad", "\\mathrm{grad}")
      line = line.replace("\\dom", "\\mathrm{dom}")
      line = line.replace("\\diag", "\\mathrm{diag}")
      line = line.replace("\\proj", "\\mathrm{proj}")
      line = line.replace("\\prox", "\\mathrm{prox}")
      line = line.replace("\\rank", "\\mathrm{rank}")
      line = line.replace("\\sign", "\\mathrm{sign}")
      line = line.replace("\\ddim", "\\mathrm{dim}")
      line = line.replace("\\begin{tabular}","\\begin{array}")
      line = line.replace("\\end{tabular}","\\end{array}")
      line = line.replace('``','"')
      line = line.replace("''",'"')
      line = line.replace("\$","\\$")
      line = line.replace("\\\\","\\\\\\\\")
      line = line.replace("\\left\{","\\left\\\\{")
      line = line.replace("\\right\}","\\right\\\\}")
      line = line.replace("\\bigg\{","\\bigg\\\\{")
      line = line.replace("\\bigg\}","\\bigg\\\\}")
      line = line.replace("\\bigg\]","\\bigg\\\\]")
      line = line.replace("\\bigg\[","\\bigg\\\\[")
      line = line.replace("\\big\[","\\big\\\\[")
      line = line.replace("\\big\]","\\big\\\\]")
      line = line.replace("\\big\{","\\big\\\\{")
      line = line.replace("\\big\}","\\big\\\\}")
      line = re.sub(r'\\hspace\{(.*?)\}', r'\n', line)
      line = line.replace("\\begin{center}","")
      line = line.replace("\\end{center}","")
      line = line.replace("\\begin{itemize}","")
      line = line.replace("\\end{itemize}","")
      line = line.replace("\\begin{enumerate}","")
      line = line.replace("\\end{enumerate}","")
      line = line.replace("\\item ","* ")
      line = re.sub(r'{\\em (.*?)}', r'*\1*', line)
      line = re.sub(r'\\textbf{(.*?)}', r'*\1*', line)
      line = re.sub(r'\\url\{(.*?)\}', r'<a href="\1">\1</a>', line)
      s = re.sub(r'verb!(.*?)!', r'`\1`', line)
      s = s.replace('\`','`')
      line = s
      
      if '\includegraphics' in line:
          gf = re.findall("includegraphics\[.*?\]\{(.*?)\}",line,re.DOTALL)[0]
          fout.write('![](' + gf + ')\n')
      elif '\\begin{minted}' in line:
          fout.write('```python\n')
      elif '\end{minted}' in line:
          fout.write('```\n')
      elif '\\newpage' in line:
          fout.write('<hr>\n')
      elif '\\begin{verbatim}' in line:
          fout.write('```\n')
      elif '\\end{verbatim}' in line:
          fout.write('```\n')          
      elif '\end{document}' in line:
          fout.write("\n")
      elif '\\inputminted' in line:
         pf = re.findall("inputminted.*?python\}\{(.*?)\}",line,re.DOTALL)[0]
         pfcontent = codecs.open(pf).read()
         fout.write("```python\n")
         fout.write(pfcontent)
         fout.write("```\n")
      elif '_' in line:
         line = line.replace("_","@@UUEEE@@") # special code
         fout.write(line)
      elif '\\mlabel' in line:
         line = re.sub(r'\\mlabel{(.*?)}', r'\qquad (\1)', line)
         fout.write(line)
      else:
          fout.write(line)
      fout.flush()
   fout.close()
   fin = codecs.open("/tmp/out.md",encoding="utf-8")
   fout = codecs.open(htmlfile, mode="w",encoding="utf-8")
   content=fin.read()
   res = markdown.markdown(content, extensions=['fenced_code'])
   res = res.replace("@@UUEEE@@","_")
   res = res.replace("@@RR@@@","^*")
   fout.write(res)
   fout.close()


def translit_low(c):
    res = c.lower()
    res = res.replace(u'ğ','g')
    res = res.replace(u'ş','s')
    res = res.replace(u'ı','i')
    res = res.replace(u'ç','c')
    res = res.replace(u'ü','u')
    res = res.replace(u'ö','o')
    return res

if __name__ == "__main__": 
 
    if len(sys.argv) == 1: exit()

    if sys.argv[1] == 'clean-light':
        os.system("find . -name _region_* | xargs rm -rf ")
        os.system("find . -name '*.log' | xargs rm -rf ")
        os.system("find . -name '*.aux' | xargs rm -rf ")
        os.system("find . -name '*.out' | xargs rm -rf ")
        os.system("find . -name '_minted*' | xargs rm -rf ")
        os.system("find . -name '_preview*' | xargs rm -rf ")
        os.system("find . -name '*ltxpng*' | xargs rm -rf ")

    if sys.argv[1] == 'clean':
        os.system("find . -name _region_* | xargs rm -rf ")
        os.system("find . -name '*.log' | xargs rm -rf ")
        os.system("find . -name '*.pdf' | xargs rm -rf ")
        os.system("find . -name '*.aux' | xargs rm -rf ")
        os.system("find . -name '*.out' | xargs rm -rf ")
        os.system("find . -name '_minted*' | xargs rm -rf ")
        os.system("find . -name '_preview*' | xargs rm -rf ")

    if sys.argv[1] == 'all':
        for x in topdirs:
            if ".git" in x: continue
            if os.path.isdir(x):
                os.chdir(x)
                print ("building", x)
                os.system("python build.py all")
                os.system("python build.py")
                os.chdir("..")

    if sys.argv[1] == 'html':
        
        fr = os.getcwd()
        cmd = "python /home/burak/Documents/kod/rsync.py '%s' '%s' --ignore-list=.md,.git,.zip,.pdf,.apk,.exe" % (fr, TARGET_DIR)
        print (cmd)
        os.system(cmd)
        shutil.copy(".gitignore", TARGET_DIR)
        
        for topdir in topdirs:
            print ('main',topdir)
            dir = TARGET_DIR + "/" + topdir
            print ('dir',dir)
            fout = codecs.open(dir + "/index.html",mode="w",encoding="utf-8")
            fout.write("<html>\n")
            fout.write(u"""
            <head>
            <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
            <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
            </head>  
            <body>                    
            <p>
            <a href='..'>Ana Menü</a>
            </p>
            """)
            for subdir in sorted(os.listdir(dir)):
                if not os.path.isdir(dir + "/" + subdir): continue
                print ('subdir',subdir)
                if "cover" in subdir or "000" in subdir : continue
                print (dir + "/" + subdir)
                # read tex file, get header
                ff = dir + "/" + subdir + "/" + subdir + ".tex"                
                fin = codecs.open(ff, encoding='iso-8859-9')
                content = fin.read()
                title = re.findall(u"begin.*?document.*?\n(.*?)\n",content,re.DOTALL)[0]
                url = translit_low(title)
                url = url.replace(" ","_")
                url = url.replace("(","_")
                url = url.replace("'","_")
                url = url.replace(")","_")
                url = url.replace("/","_")
                url = url.replace("-","")
                url = url.replace(",","")
                url = url.replace("?","")
                url = url.replace("̇","")
                url = url + ".html"
                print ('the url is', subdir + "/" + url)
                
                line = "<a href='%s'>%s</a><br/><br/>" % (subdir + "/" + url, title)
                print (line)
                fout.write(line)
                fout.write("\n")
                fin.close()
                
                print ('chdir', dir + "/" + subdir)                
                os.chdir(dir + "/" + subdir)

                if os.path.isfile(subdir + ".html"): 
                    textime = os.path.getmtime(subdir + ".tex")
                    htmltime = os.path.getmtime(subdir + ".html")
                    if htmltime > textime:
                        print ("HTML exists.. skipping")
                        continue

                texfile = subdir + '.tex'
                htmlfile = subdir + '.html'
                tex_mathjax_html(texfile, htmlfile, title)
                shutil.copy(htmlfile, url)
                                
            fout.write("</body>\n")
            fout.write("</html>\n")
            fout.close()
            #break
                      
