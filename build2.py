# -*- coding: utf-8 -*-

#
# __TODO__: for html, put article title inside <title></title>
#
import os, sys, re, codecs, shutil, markdown

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

TARGET_DIR = "/home/burak/Documents/dersblog2"

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
      elif '\end{minted}' in line:
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

    if sys.argv[1] == 'clean':
        os.system("find . -name _region_* | xargs rm -rf ")
        os.system("find . -name '*.log' | xargs rm -rf ")
        os.system("find . -name '*.aux' | xargs rm -rf ")
        os.system("find . -name '*.out' | xargs rm -rf ")
        os.system("find . -name '_minted-*' | xargs rm -rf ")
        os.system("find . -name '_preview*' | xargs rm -rf ")

    if sys.argv[1] == 'all':
        for x in os.listdir("."):
            if ".git" in x: continue
            if os.path.isdir(x):
                os.chdir(x)
                print ("building", x)
                os.system("python build.py")
                os.chdir("..")

    if sys.argv[1] == 'html':
        
        fr = os.getcwd()
        cmd = "python /home/burak/Documents/kod/rsync.py '%s' '%s'" % (fr, TARGET_DIR)
        print (cmd)
        os.system(cmd)
        
        for topdir in ['algs','calc_multi','chaos','compscieng','elecmag',
                       'func_analysis','linear','ode','pde','stat',
                       'tser','vision','phy']:
            print ('main',topdir)
            dir = TARGET_DIR + "/" + topdir
            print ('dir',dir)
            fout = codecs.open(dir + "/index.html",mode="w",encoding="utf-8")
            fout.write("<html>\n")
            fout.write(u"""
            <p>
            <a href='..'>Ana Menü</a>
            </p>
            """)
            for subdir in sorted(os.listdir(dir)):
                if not os.path.isdir(dir + "/" + subdir): continue
                print ('subdir',subdir)
                if "cover" in subdir or "00" in subdir : continue
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
                tex_mathjax_html(texfile, htmlfile)

                #fout1.close()                                
                #break
            fout.write("</html>\n")
            fout.close()
            #break
                      
