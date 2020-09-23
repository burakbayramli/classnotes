# Web Sayfalarindan Gorunen Metni Kazimak (Scraping)

Bir web sayfasindaki Turkce, Ingilizce kelimeleri almak icin Python
uzerinde Beautiful Soup adinda bir paket var. "Gorunen metin" derken
bir sayfada okunabilir olan, HTML etiketleri haricindeki kelimeleri
kastediyoruz, istatistiki analiz icin mesela herhangi bir gunun
"kelime dagarcigini" cekip cikarmak icin boyle kodlar gerekebilir.

```python
import re
import urllib
import BeautifulSoup

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

Python kutuphanelerinden urllib bu is icin kullanilir. Alttaki ornekte
Google Insights for Search sayfalarindan Google'da son 7 gun icinde en
cok aranan kelimelerin listesini almak icin kullandigimiz kodlar
bulunabilir. Ayni sayfalar uzerinde wget ise yaramadi, urllib
FancyURLopener calisti.


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


Imaj

Eger imaj  toplamak istiyorsak, mesela Bing'den alttaki kod faydali,

console.py


```python
from pip import __main__

import sys, os, argparse, _bing
def main():
    # Parser
    parser = argparse.ArgumentParser(
        description="Scrape images from the internet.")
    parser.add_argument(
        "engine", help="Which search engine should be used? (Bing/Google)")
    parser.add_argument(
        "query", help="Query that should be used to scrape images.")
    parser.add_argument(
        "--limit", help="Amount of images to be scraped.", default=1000, required=False)
    parser.add_argument("--json", help="Should image metadata be downloaded?",
                        action='store_true', required=False)
    parser.add_argument(
        "--url", help="Google: Scrape images from a google image search link", required=False)  # Google Specific
    parser.add_argument("--adult-filter-off", help="Disable adult filter",
                        action='store_true', required=False)  # Bing Specific

    args = parser.parse_args()
    # Variables
    engine = args.engine.lower() if args.engine.lower() is not None else "google"
    query = urlparse.parse_qs(urlparse.urlparse(args.url).query)[
        'q'][0] if args.url is not None else args.query

    limit = args.limit
    metadata = args.json if args.json is not None else False
    adult = "off" if args.adult_filter_off else "on"
    if engine == "google" or engine == "g":
        engine = "google"
        url = args.url if args.url is not None else "https://www.google.com/search?q={}&source=lnms&tbm=isch".format(
            query)
    elif engine == "bing" or engine == "b":
        engine = "bing"
        url = "https://www.bing.com/images/async?q={}&first=0&adlt={}".format(
        str(query), adult)
    else:
    sys.exit("Invalid engine specified.")
    cwd = os.getcwd()
    # check directory and create if necessary
    if not os.path.isdir("{}/dataset/".format(cwd)):
        os.makedirs("{}/dataset/".format(cwd))
    if not os.path.isdir("{}/dataset/{}/{}".format(cwd, engine, query)):
        os.makedirs("{}/dataset/{}/{}".format(cwd, engine, query))
    if not os.path.isdir("{}/dataset/logs/{}/".format(cwd, engine, query)):
        os.makedirs("{}/dataset/logs/{}/".format(cwd, engine, query))
 
    if engine == "google":
        _google.google(url, metadata, query, limit)
    else:
        _bing.bing(url, metadata, query, limit, adult)

if __name__ == "__main__":
    main()
```

```python
#
# ping.py
#
import requests, time, shutil
import argparse, json
from bs4 import BeautifulSoup
from pathlib import Path
import lxml.html, os, sys
from fake_useragent import UserAgent
import urllib.parse as urlparse

def error(link, query):
    print("[!] Skipping {}. Can't download or no metadata.\n".format(link))
    file = Path("{}/dataset/logs/bing/errors.log".format(os.getcwd(), query))
    if file.is_file():
        with open("{}/dataset/logs/bing/errors.log".format(os.getcwd(), query), "a") as myfile:
            myfile.write(link + "\n")
    else:
        with open("{}/dataset/logs/bing/errors.log".format(os.getcwd(), query), "w+") as myfile:
            myfile.write(link + "\n")


def save_image(link, file_path):
    # Use a random user agent header for bot id
    ua = UserAgent(verify_ssl=False)
    headers = {"User-Agent": ua.random}
    r = requests.get(link, stream=True, headers=headers)
    if r.status_code == 200:
        with open(file_path, 'wb') as f:
            r.raw.decode_content = True
            shutil.copyfileobj(r.raw, f)
    else:
        raise Exception("Image returned a {} error.".format(r.status_code))

def download_image(link, image_data, metadata, query):
    download_image.delta += 1
    try:
        file_name = link.split("/")[-1]
        type = file_name.split(".")[-1]
        type = (type[:3]) if len(type) > 3 else type
        if type.lower() == "jpe":
            type = "jpeg"
        if type.lower() not in ["jpeg", "jfif", "exif", "tiff", "gif", "bmp", "png", "webp", "jpg"]:
            type = "jpg"

        print("[%] Downloading Image #{} from {}".format(
            download_image.delta, link))
        try:
            save_image(link, "{}/dataset/bing/{}/".format(os.getcwd(), query) +
                       "Scrapper_{}.{}".format(str(download_image.delta), type))
            print("[%] Downloaded File")
            if metadata:
                with open("{}/dataset/bing/{}/Scrapper_{}.json".format(os.getcwd(), query, str(download_image.delta)), "w") as outfile:
                    json.dump(image_data, outfile, indent=4)

        except Exception as e:
            download_image.delta -= 1
            print("[!] Issue Downloading: {}\n[!] Error: {}".format(link, e))
            error(link, query)
    except Exception as e:
        download_image.delta -= 1
        print("[!] Issue getting: {}\n[!] Error:: {}".format(link, e))
        error(link, query)


def bing(url, metadata, query, delta, adult):
    delta = int(delta)
    sys.setrecursionlimit(1000000)
    page_counter = 0
    link_counter = 0
    download_image.delta = 0
    while download_image.delta < delta:
        ua = UserAgent(verify_ssl=False)
        headers = {"User-Agent": ua.random}
        payload = (("q", str(query)), ("first", page_counter), ("adlt", adult))
        source = requests.get(
            "https://www.bing.com/images/async", params=payload, headers=headers).content
        soup = BeautifulSoup(str(source).replace('\r\n', ""), "html.parser")
        try:
            os.remove("dataset/logs/bing/errors.log")
        except OSError:
            pass

        links = [json.loads(i.get("m").replace('\r\n', ""))["murl"]
                 for i in soup.find_all("a", class_="iusc")]
        print("[%] Indexed {} Images on Page {}.".format(
            len(links), page_counter + 1))
        print("\n===============================================\n")
        print("[%] Getting Image Information.")
        images = {}
        for a in soup.find_all("a", class_="iusc"):
            if download_image.delta >= delta:
                break
            print("\n------------------------------------------")
            iusc = json.loads(a.get("m"))
            link = iusc["murl"]
            print("\n[%] Getting info on: {}".format(link))
            try:
                image_data = "bing", query, link, iusc["purl"], iusc["md5"]
                images[link] = image_data
                try:
                    download_image(link, images[link], metadata, query)
                except Exception as e:
                    error(link, query)

            except Exception as e:
                images[link] = image_data
                print("[!] Issue getting data: {}\n[!] Error: {}".format(rg_meta, e))
            link_counter += 1

        page_counter += 1

    print("\n\n[%] Done. Downloaded {} images.".format(download_image.delta))
    print("\n===============================================\n")

```

```python
python -u console.py bing dog --limit 10 --json
```

10 tane kopek imaji indirilip dataset dizini altina yazilacak.

