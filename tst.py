# -*- coding: utf-8 -*-
import markdown, sys, os, codecs, re

html_head = '''
<!DOCTYPE html>
<html>
  <head>
    <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
    <script type="text/x-mathjax-config">
  MathJax.Hub.Config({
    tex2jax: {inlineMath: [["$","$"]]}
  });
</script>
<script type="text/javascript"
   src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-AMS_HTML-full">
</script>
</head>
'''   

def tex_mathjax_html(texfile, htmlfile):

   fin = codecs.open(texfile, encoding='iso-8859-9')
   fout = codecs.open("/tmp/out.md",mode="w",encoding="utf-8")

   fin.readline()
   fin.readline()
   title = "# " + fin.readline()
   fout.write(title)

   fout.write(html_head)

   for line in fin.readlines():
      line = line.replace("\\ud", "\\mathrm{d}")
      line = line.replace('``','"')
      line = line.replace("''",'"')
      s = re.sub(r'verb!(.*?)!', r'`\1`', line)
      s = s.replace('\`','`')
      line = s
      if '\includegraphics' in line:
          gf = re.findall("\includegraphics\[.*?\]\{(.*?)\}",line,re.DOTALL)[0]
          fout.write('![](' + gf + ')\n')
      elif '\\begin{minted}' in line:
          fout.write('```python\n')
      elif '\\inputminted' in line:
         pf = re.findall("inputminted.*?python\}\{(.*?\.py)\}",line,re.DOTALL)[0]
         pfcontent = codecs.open(pf).read()
         fout.write("```python")
         fout.write(pfcontent)
         fout.write("```") 
      elif '\end{minted}' in line:
          fout.write('```\n')
      elif '\\begin{verbatim}' in line:
          fout.write('```\n')
      elif '\\end{verbatim}' in line:
          fout.write('```\n')
      elif '\end{document}' in line:
          fout.write("\n")
      elif '\\mlabel' in line:
         label = re.findall(u"\mlabel\{(.*?)\}",line,re.DOTALL)[0]
         fout.write("\\qquad (" + label + ")")
      elif '\\url' in line:
         u = re.findall('url\{(.*?)\}',line,re.DOTALL)[0]
         s = "<a href='" + u + "'>" + u + "</a>"
         line = re.sub('url\{.*?\}', s, line)
         line = line.replace("\\http","http")
         fout.write(u + "\n")
      else:
          fout.write(line)
      fout.flush()
   fout.close()
   fin = codecs.open("/tmp/out.md",encoding="utf-8")
   fout = codecs.open(htmlfile, mode="w",encoding="utf-8")
   content=fin.read()
   res = markdown.markdown(content, extensions=['fenced_code'])
   fout.write(res)
   fout.close()

texfile = "/home/burak/Documents/classnotes/algs/convnet/convnet.tex"
tex_mathjax_html(texfile, "/tmp/out.html")   
