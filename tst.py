# -*- coding: utf-8 -*-
import markdown, sys, os, codecs, re
fin = codecs.open("/home/burak/Documents/classnotes/calc_multi/calc_multi_01/calc_multi_01.tex", encoding='iso-8859-9')
fout = codecs.open("/tmp/out.md",mode="w",encoding="utf-8")

fin.readline()
fin.readline()
title = "# " + fin.readline()
fout.write(title)

fout.write('''
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
''')

for line in fin.readlines():
   if '\includegraphics' in line:
       gf = re.findall(u"\includegraphics\[height.*?\]\{(.*?)\}",line,re.DOTALL)[0]
       fout.write('![](' + gf + ')\n')
   elif '\\begin{minted}' in line:
       fout.write('```python\n')
   elif '\end{minted}' in line:
       fout.write('```\n')
   elif '\end{document}' in line:
       fout.write("\n")
   elif '\\mlabel' in line:
      label = re.findall(u"\mlabel\{(.*?)\}",line,re.DOTALL)[0]
      fout.write("\\qquad (" + label + ")")
   else:
       fout.write(line)
   fout.flush()
fout.close()
fin = codecs.open("/tmp/out.md",encoding="utf-8")
fout = codecs.open("/tmp/out.html",mode="w",encoding="utf-8")
content=fin.read()
res = markdown.markdown(content, extensions=['fenced_code'])
fout.write(res)
fout.close()
