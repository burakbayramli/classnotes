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
istiyorum, bunun için `[buraya tıkla](../../şub3/şub4/yazı.md)` yazmam
yeterli, bu bağlantı GP tarafından `https://github.iö/../repo1/şub3/şub4/yazı.html` 
şeklindeki bir bağlantıya çevirilecektir .

Matematik Formülleri

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
