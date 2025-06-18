#!/usr/bin/env python
# -*- coding: utf-8 -*- 
import os, sys, re, codecs, shutil, markdown, json, markdown2

head_insert = '''
    <meta name="generator" content="pandoc" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
    <script type="text/x-mathjax-config">
    MathJax.Hub.Register.StartupHook("TeX Jax Ready",function () {
      MathJax.Hub.Insert(MathJax.InputJax.TeX.Definitions.macros,{
        cancel: ["Extension","cancel"], cancelto: ["Extension","cancel"]
      });
    });
    </script>
'''

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
   <script async="async" data-cfasync="false" src="//pl22489825.profitablegatecpm.com/d84f574876e65b2d8f0c7bae784c22b3/invoke.js"></script>

   <link rel="stylesheet" type="text/css" media="screen" href="https://burakbayramli.github.io/css/style.css">
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
          <div id="container-d84f574876e65b2d8f0c7bae784c22b3"></div>

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

