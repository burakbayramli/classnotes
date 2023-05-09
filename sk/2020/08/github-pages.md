# Markdown Bazli Web Yayinciligi, Github Pages

Artık Github'ta içinde markdown dosyaları içeren bir depo direk Github
üzerinden blog tarzı Internet'te yayınlanır hale getirilebilir. Github
Pages teknolojisi arka planda Jekyll yazılımı üzerinen markdown
dosyalarını otomatik olarak HTML'e çeviriyor. 

Bu özelliğin aktif edilmesi için repoya gidip Settings'e tıklanır, ve
en altta `Github Pages` bölümü aktif edilir. Farklı temalar da buradan
seçilebilir. Bunlar yapılınca dizinde otomatik bir `_config.yml`
yaratılacak, bunun içine mesela

```
plugins:
  - jekyll-sitemap
```

eklersek Github Pages bizim için bir `sitemap.xml` üretir.

GP'ın güzel tarafı markdown ıcinde sanki HTML'den haberimiz yokmuş
gibi davranabiliriz, mesela ben alt dizin `şub1/şub2` içindeyim, ve
`şub3/şub4` içindeki bir `yazı.md` dosyasına bağlantı vermek
istiyorum, bunun için `[buraya tıkla](../../sub3/sub4/yazi.md)` yazmam
yeterli, bu bağlantı GP tarafından `https://github.io/../repo1/sub3/sub4/yazi.html` 
şeklindeki bir bağlantıya çevirilecektir .

### Matematik Formülleri

Eğer Github Pages kendi şablonu üzerinen, mesela Slate şablonu
üzerinden matematik formülleri servis etmek istersek, şurada anlatılıyor,

https://github.com/cjerdonek/gh-pages-theme-slate

ana dizinde `/_layouts/default.html` dosyasi yaratiriz, ve bu dosyaya

https://github.com/pages-themes/slate/blob/master/_layouts/default.html

görülen kodu koyarız. Bu kod mevcut Slate şablonu ile aynı kod, bu kodu
olduğu gibi kullanınca öncesi sonrası hiçbir değişiklik görmemeniz lazım. 
Sonra bu kod içine istediğimiz ekleri yaparız, mesela MathJax için

```
<script type="text/x-mathjax-config">
  MathJax.Hub.Config({
    tex2jax: {inlineMath: [["$","$"],["\\(","\\)"]]}
  });
</script>
<script type="text/javascript"
   src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-AMS_HTML-full">
</script>
```

`<head>` içine eklenir, bundan sonra Markdown dosyamızda iki `$$` arası formüller
gösterilecektir. 

### Jekyl Iptali

Github Pages aslında iki işi aynı anda yapıyor, hem arka planda HTML
üretiyor, hem de bildiğimiz Web servisi gibi bu HTML'leri servis
ediyor. Fakat eğer biz kendi HTML'imizi Markdown dosyalarından
üretirsek, Jekyl üretimini iptal edip kendi sayfalarımızı servis
edebiliriz.

İptal için en üst seviyede bir `.nojekyll` dosyası yaratmak lazım, içi
boş olabilir önemli değil, GH bunu görünce HTML üretimini iptal
edecek.

Şimdi kendi html dosyalarımızı markdown'dan üretmeye gelelim, alttaki
gibi bir kod olabilir,  

```python
import os, sys, re, shutil, markdown

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

def clean_html():
    if sys.argv[1] == 'clean-html2':
        dirs, files = ls(os.getcwd())
        for (f,size) in files:
            if ".md" in f:
                path = os.path.dirname(f)
                fmd = os.path.basename(f)
                fhtml = os.path.basename(f).replace(".md",".html")
                if os.path.isfile(path + "/" + fhtml):
                    print ('Erasing', path + "/" + fhtml)
                    os.remove(path + "/" + fhtml)

def gen_html():
        dirs, files = ls(os.getcwd())
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
                    content = open(path + "/" + fmd).read()
                    res = markdown.markdown(content, extensions=['fenced_code'])
                    fout = open(path + "/" + fhtml, "w")
                    fout.write(res)
                    fout.close()
```

Kod en üst dizin seviyesinden işletilir, ve gördüğü her .md dosyası için
aynı isimde bir .html dosyası üretir.


