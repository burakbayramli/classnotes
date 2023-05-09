# Github Usulu Dokuman Yazmak, Markdown

Github üzerinde mesela README.md (ya da diğer md ile biten)
dosyalarında markdown formatı kullanılıyor, başlık için #, altbaşlık
için ##, maddeler (bullet) * ile, resim, bağlantı eklemek çok basit,
vs. Biz mesela artık bu blog'u markdown ile yazıp HTML'e çevirip
sonucu blogger'a veriyoruz.

Bağlantılar

```
[Bir baglanti](https://www.google.com)
```

```
[![IMAGE ALT TEXT HERE](http://img.youtube.com/vi/YOUTUBE ID/0.jpg)](http://www.youtube.com/watch?v=YOUTUBE ID)
```

Bir diğer araç

Markdown'dan ve HTML'e geçişte en rahat araç pandoc. 

```
pandoc dosya.md > out.html
```

Komut satiri araci grip faydali olabilir.

```
sudo pip install grip
```

Dokümanı derlemek için

```
grip README.md --export out.html
```

Artık out.html içinde markdown dokümanının görüntüsü var. 

Not: Bir diğer araç, markdown, pek iyi işlemiyor, mesela ``` arasında
kod göstermeyi tam yapamıyor.

Bir baska yontem `markdown` adli bir paketin python icinden yukleyip
kullanmak. Kurmak icin `pip install markdown`, sonra mesela soyle bir
md icin,

```
import markdown, sys, os
content=open('dosya.md').read()
print (markdown.markdown(content, extensions=['fenced_code']))
```

`fenced_code` kullanimi birden fazla satira yayilan kodlari halletmek icin.

