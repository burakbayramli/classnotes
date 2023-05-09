# Web Sayfalarından Görünen Metni Kazımak (Scraping)

Bir web sayfasindaki Turkce, Ingilizce kelimeleri almak icin Python
uzerinde Beautiful Soup adinda bir paket var. "Gorunen metin" derken
bir sayfada okunabilir olan, HTML etiketleri haricindeki kelimeleri
kastediyoruz, istatistiki analiz icin mesela herhangi bir gunun
"kelime dagarcigini" cekip cikarmak icin boyle kodlar gerekebilir.

Öncelikle bir sayfayı script içinden okutup metnini alma komutunu görelim,

```python
import re, urllib.request as req, io
content = req.urlopen("https://www.cnn.com").read().decode('utf-8')
```

Artık `content` içinde içerik text olarak var.

```python
import re
import urllib
from bs4 import BeautifulSoup

keywords = ['script', '<b>', '\n']

def visible(element):
   if element.parent.name in ['style', 'script', '[document]', 'head', 'title']:
       return False
   elif re.match('<!--.*-->', str(element)):
       return False
   return True

def tokenize_site(url):
   html = urllib.urlopen(url).read()
   soup = BeautifulSoup.BeautifulSoup(html)
   texts = soup.findAll(text=True)
   visible_texts = filter(visible, texts)
   tmp = []
   for x in visible_texts:
       if (x not in keywords): tmp.append(x)
   res = []
   for x in tmp:
       for xx in x.split():
           res.append(xx)
   return res

if __name__ == "__main__":
   res = tokenize_site('[MEDYA SITE ISMI]')   
```

Python kütüphanelerinden urllib bu iş için kullanılır. Alttaki örnekte
Google İnsights for Search sayfalarından Google'da son 7 gün içinde en
çok aranan kelimelerin listesini almak için kullandığımız kodlar
bulunabilir. Aynı sayfalar üzerinde `wget` ise yaramadı, ürllib
FancyURLopener çalıştı.


```python
from urllib import FancyURLopener
myopener = FancyURLopener()
insightsURL = 'http://www.google.com/insights/search/overviewReport'
page = myopener.open(insightsURL + '?q=&date=today+7-d&cmpt=q')
print page.read()
```

FancyURLopener nereden geliyor? Google aramalarina scriptlemek istiyorsak, wget, hatta urllib ile ilk denememiz basarisiz olabilir. Anlasiliyor ki wget ve urllib baglantilari, kullanicilari Google'in izin verdigi robotlardan degil. O zaman baglananin 'kim oldugunu' degistirerek, yani Google'i yaniltarak, izin verilen bir robot ortaya cikartabiliriz. Alttaki 'version' tanimi bunu yapiyor. Sanki bir Windows makinasindan baglanan Firefox tarayicisi gibi gozukuyoruz.

```python
from urllib import FancyURLopener

class MyOpener(FancyURLopener):
 version = 'Mozilla/5.0 (Windows; U; Windows NT 5.1; it; ' +  \
     'rv:1.8.1.11) Gecko/20071127 Firefox/2.0.0.11'

myopener = MyOpener()
page = myopener.open('http://www.google.com.tr/search?q=tomatoes')
content = page.read()
print content
```

Eger orumcek (spider) usulu tum bir site icerigini almak istersek,

```python
# -*- coding: utf-8 -*-
from urllib.request import urlopen
from bs4 import BeautifulSoup
from urllib.request import urlretrieve
import codecs, os, re

base = 'http://sayilarvekuramlar.blogspot.com'
urls = ['/2018/11/tensorflowjs-javascript-ile-tensorflow.html',
        ...]

def get_article(url, local):
    fname = url[url.rfind('/')+1:]    
    subdir = local + url[0:url.rfind('/')]
    if not os.path.isdir(subdir):
        os.makedirs(subdir)
    md_file = subdir + "/" + fname.replace(".html",".md")
    if os.path.isfile(md_file):
        print ('Already downloaded', url)
        return
    fout = codecs.open(md_file, "w", encoding='utf-8')
    html = urlopen(base + url)
    bsObj = BeautifulSoup(html.read(),"lxml");
    title = bsObj.h3.get_text().strip()
    fout.write("# " + title + "\n")
    content = bsObj.get_text()
    
    imgs = bsObj.find_all("img")
    imgs = [x.get('src') for x in imgs if "bp.blogspot.com" in x.get('src')]
    tmp_img = []
    for img in imgs:
        print (img)
        imgname = img[img.rfind('/')+1:]
        urlretrieve(img, subdir + "/" + imgname)
        tmp_img.append(imgname)
    
    active = False
    for i,line in enumerate(content.split("\n")):
        if i==480: active = True
        if u'Gönderen' in line: active = False
        if active:
            fout.write(line)
            fout.write("\n")

    imgs = bsObj.find_all("img")
    for img in tmp_img:
        fout.write("![](%s)\n" % img)
        
    fout.close()

def articles():
    d = {}

    fin = open("/home/burak/Downloads/blog-11-25-2018.xml")
    content = fin.read()
    res = re.findall("sayilarvekuramlar.blogspot.com/(.*?.html)",
                     content,
                     re.DOTALL)

    count = 0
    for i,x in enumerate(res):
        if "feeds" in x: continue
        if "/html" in x: continue
        if len(x) < 150:
            count += 1
            d[x] = "1"
            print (x)

    print (len(d))
    #print (d)
    
        
if __name__ == "__main__":
    local = "/tmp/sk"
    res = articles()
    for x in res:
        print (x)
        try:
            get_article(x, local)
        except Exception as e:
            print ("cannot get article", x, repr(e))    
```

Twitter

Hangi tarayıcı tipinin taklit edildiği bağlanılan her siteye göre
değisebiliyor. Mesela Twitter'den bir kullanıcının mesajlarını kazımak
istediğimizde "Javascript'e sahip tarayıcı lazım", ya da "desteklenen
tarayıcıyı kullanın" gibi mesajlar alabiliyoruz, tüm bu hataların
üstesinden gelmek ve veriyi almak için alttaki gibi bir tarayıcı
beyani gerekti,


```python
import re, requests

headers = { 'User-Agent': 'UCWEB/2.0 (compatible; Googlebot/2.1; +google.com/bot.html)'}

def cleanhtml(raw_html):
  cleanr = re.compile('<.*?>')
  cleantext = re.sub(cleanr, '', raw_html)
  return cleantext

content = ""
for user in ['bbc','dw_turkce']:
    content += user + "\n\n"
    url_twitter = 'https://twitter.com/%s' % user
    resp = requests.get(url_twitter, headers=headers)  # Send request
    res = re.findall(r'<p class="TweetTextSize.*?tweet-text.*?>(.*?)</p>',resp.text)
    for x in res:
        x = cleanhtml(x)
        x = x.replace("&#39;","'")
        x = x.replace('&quot;','"')
        x = x.replace("&nbsp;"," ")
        content += x 
        content += "\n\n"
        content += "---"
        content += "\n\n"

print (content)
```

Imaj

Eger imaj toplamak istiyorsak, mesela Bing'den alttaki kod faydali,

[console.py](console.py)

[ping.py](ping.py)


```python
python -u console.py bing dog --limit 10 --json
```

10 tane kopek imaji indirilip dataset dizini altina yazilacak.


