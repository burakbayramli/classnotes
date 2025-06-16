import codecs, re, os

texfile = "phy_005_basics_06.tex"

fin = codecs.open(texfile, encoding='utf-8')
fout = codecs.open("/tmp/out.md",mode="w",encoding="utf-8")

fin.readline()
fin.readline()
title = fin.readline()
fout.write("# " + title)

for line in fin.readlines():
   line = line.replace("\\ud", "\\mathrm{d}")
#   line = line.replace("^*", "@@RR@@@")
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
   #line = line.replace("''",'"')
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
   elif '\input{' in line:
      pf = re.findall("input\{(.*?)\}",line,re.DOTALL)[0]
      pfcontent = codecs.open(pf).read()
      #print (pfcontent)
      fout.write(pfcontent)
#   elif '_' in line:
#      line = line.replace("_","@@UUEEE@@") # special code
#      fout.write(line)
   elif '\\mlabel' in line:
      line = re.sub("\\\mlabel{(.*?)}", "\\\qquad (\\1)", line)
      fout.write(line)
   else:
       fout.write(line)

fout.close()

cmd = "pandoc %s --latex-engine=xelatex -fmarkdown-implicit_figures -o %s" % ("/tmp/out.md","/tmp/out.pdf")
os.system(cmd)
