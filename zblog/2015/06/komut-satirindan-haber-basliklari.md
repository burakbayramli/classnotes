# Komut Satirindan Haber Basliklari


Komut Satirindan Haber Basliklari




Haberler, ozellikle "en son ne oldu" seviyesinde aranan haberler nihayetinde metin bazli bilgiler - komut satirindan Ubuntu uzerinden bu haberleri almak icin newsbeuter var.

apt-get install newsbeuter

Ardindan $HOME/.newsbeuter/urls 

dosyasi icinde RSS haber vericiler eklenir, mesela

http://www.ft.com/rss/worldhttp://www.theguardian.com/world/rsshttp://news.google.co.uk/news?pz=1&cf=all&ned=uk&hl=en&output=rsshttp://newsrss.bbc.co.uk/rss/newsonline_world_edition/front_page/rss.xmlhttp://feeds.reuters.com/reuters/topNewshttp://www.huffingtonpost.com/feeds/verticals/world/index.xmlhttp://www.spiegel.de/international/index.rss

Program newsbeuter ile baslatilir. Oklar ile kaynak uzerine gidilir, R ile kaynagin haber icerigi guncellenir, Enter ile kaynaga bakilir. Bazi renk ve dis tarayici ayarlamak icin ./newsbeuter/config icinde

color background   default   defaultcolor listnormal   default   defaultcolor listfocus    black     yellowcolor info         default   blackcolor article      default   default# highlightshighlight article "^(Title):.*$" blue defaulthighlight article "https?://[^ ]+" red defaulthighlight article "\\[image\\ [0-9]+\\]" green defaultbrowser "w3m %u"

Bazi anahtar kelimelere gore haber filtrelemek istenirse, mesela basliginda ve icinde Turkey kelimesi gecen tum haberler,

ignore-article "*" "content =~ \"Turkey\""
ignore-mode "download"
ignore-article "*" "title =~ \"Turkey\""
ignore-mode "download"

Eger sudo apt-get install w3m ile dis tarayici olarak w3m kurarsaniz, bir haberi okurken "o" tusuna basinca metin bazli olarak bu haber gosterilecektir. Eger konsol programinizda renkler hala rahatsiz ediyorsa, menuden

Edit | Profile Preferences | Colors | Built-in schemes | Linux Console 

seciniz.





