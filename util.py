#!/usr/bin/env python
# -*- coding: utf-8 -*- 
import os, sys, re, codecs, shutil, markdown, json, markdown2

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
   <link rel="stylesheet" type="text/css" media="screen" href="https://burakbayramli.github.io/css/style.css">
  <script async src="https://pagead2.googlesyndication.com/pagead/js/adsbygoogle.js?client=ca-pub-1548953794786292"
          crossorigin="anonymous"></script>  
  </head>
    <body>
      <div id="header_wrap" class="outer">
        <header class="inner">
          <h1 id="project_title">
            <a href="https://burakbayramli.github.io" style="text-decoration:none; color:inherit;">dersblog</a>
          </h1>
          <h2 id="project_tagline"></h2>          
        </header>
      </div>
      <div id="main_content_wrap" class="outer">        
        <section id="main_content" class="inner">
        <h1>[title]</h1>
'''   

bottom = """
          <br/><a href="../index.html">Yukarı</a>
        </section>          
      </div>
    </body>
</html>
"""

def translit_low(c):
    res = c.lower()
    res = res.replace(u'ğ','g')
    res = res.replace(u'ş','s')
    res = res.replace(u'ı','i')
    res = res.replace(u'ç','c')
    res = res.replace(u'ü','u')
    res = res.replace(u'ö','o')
    return res
                
def filename_from_title(title):                
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
    url = url.replace("#_","")
    return url


def get_title_from_tex(f):
    fin = codecs.open(f, encoding='utf8')
    content = fin.read()
    title = re.findall(u"begin.*?document.*?\n(.*?)\n",content,re.DOTALL)[0]
    return title

def tex_mathjax_html(texfile, htmlfile, title):
   tmpdir = '/tmp'
   if 'TMPDIR' in os.environ: tmpdir = os.environ['TMPDIR']
   fin = codecs.open(texfile, encoding='utf-8')
   fout = codecs.open(tmpdir + "/out.md",mode="w",encoding="utf-8")

   fin.readline()
   fin.readline()
   title = fin.readline()
   #fout.write(title)

   head = html_head.replace("[title]",title)
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
      line = line.replace("\\bprod", "\\prod")
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
      line = line.replace("\\begin{itemize}","")
      line = line.replace("\\end{itemize}","")
      line = line.replace("\\begin{enumerate}","")
      line = line.replace("\\end{enumerate}","")
      line = line.replace("\\item ","* ")
      line = line.replace("%<a name","<a name")
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
      elif 'renewcommand' in line and "arraystretch" in line:
          pass
      elif '\\inputminted' in line:
         pf = re.findall("inputminted.*?\}\{(.*?)\}",line,re.DOTALL)[0]
         pfcontent = codecs.open(pf).read()
         fout.write("```python\n")
         fout.write(pfcontent)
         fout.write("```\n")
      elif '_' in line:
         line = line.replace("_","@@UUEEE@@") # special code
         fout.write(line)
      elif '\\mlabel' in line:
         line = re.sub("\\\mlabel{(.*?)}", "\\\qquad (\\1)", line)
         fout.write(line)
      else:
          fout.write(line)
      fout.flush()
   fout.close()
   fin = codecs.open(tmpdir + "/out.md",encoding="utf-8")
   fout = codecs.open(htmlfile, mode="w",encoding="utf-8")
   content=fin.read()
   content = content.replace("@@UUEEE@@","_")
   content = content.replace("@@RR@@@","^*")
   res = markdown.markdown(content, extensions=['fenced_code'])
   #res = markdown2.markdown(content, extras=['fenced-code-blocks'])
   res = res.replace("<p><!DOCTYPE html>","")
   res = res.replace("}<em>{","}_{")
   res = res.replace("}</em>{","}_{")
   res = res.replace("}<em>","}_")
   res = res.replace("\\sum</em>{","\\sum_{")
   res = res.replace(")<em>{",")_{")
   fout.write(res)
   fout.write(bottom)   
   fout.close()
