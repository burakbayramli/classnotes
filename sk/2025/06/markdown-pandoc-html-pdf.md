# Markdown ile PDF ve HTML Üretmek, Pandoc

Markdown formatı basit şekilde doküman yazmamıza izin veriyor,
başlıklar tek `#` alt başlıklar `##` ile belirtilebilir, listeler,
yana yatik fontlar için basit metin bazlı komutları vardır, ve bu
metin bazlı komutları içeren belgeler derlenip PDF ya da HTML
üretilebilir.  Mesela tıklanabilir bağlantılar,

```
[Bir baglanti](https://www.google.com)
```

Resimler,

```
![](resim.jpg)
```

Tıklanabilir ve tarif edilmiş resim için

```
[![IMAGE ALT TEXT HERE](http://img.youtube.com/vi/YOUTUBE ID/0.jpg)](http://www.youtube.com/watch?v=YOUTUBE ID)
```

komutlarını kullanabiliriz.

Biz bu blog'u markdown ile yazıp HTML'e çevirip sonucu blogger'a
veriyoruz. Matematiksel belgeler yine markdown formatindan hem PDF hem
HTML'e cevriliyorlar.

Çevrimi yapmak için bazı araçlar var.

## Pandoc

En basit kullanım,

```
pandoc dosya.md > out.html
```

Daha çetrefil bir örnek, MathJax içeren markdown dosyasından HTML
üretmek için

```
pandoc --mathjax -f markdown -t html doc.md -o out.html
```

Üstteki tanımla markdown ıcinde `$$` ve `$` arasındaki metinler
matematik formülü olarak kabul edilir ve Javascript üzerinden HTML
içinde MathJax ile ekrana basılır. Eğer HTML üretimini bir şablon
(template) üzerinden yapmak istersek önce `pandoc`'un kendi kullandığı
olağan şablona bakabiliriz,

```
pandoc -D > template.html
```

Sonra gereken eklerimizi bu şablon içinde yapabiliriz. Sıfırdan yazmak
ta mümkün tabii. Şablon hazırlandıktan sonra

```
pandoc --template=my_template.html --mathjax -f markdown -t html doc.md -o out.html
```

Not: Üretilen şablon dosyasına yakından bakarsak orada pek çok
değişken kullanıldığını görebiliriz, mesela `<title>$title</title>`
tanımı var, yani `title` adlı değişken gösterilen etiketler içine
yazılıyor. Bu değişkenleri dışarıdan `pandoc` komutuna parametre
olarak geçmek mümkün, mesela `-M title=12345` ile değişkene `12345`
değerini geçmiş olduk. Bu değer üretilen HTML içinde gerekli yere
konacaktır.

Not: En son Pandoc 3.1.3 için MathJax'in nereden okunması gerektiğini
direk belirtmek gerekebiliyor, mesela bizim son kullanımda

```
--mathjax=https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-AMS_HTML-full
```

gibi bir parametre vermek gerekti, bu verilmediği zaman `pandoc` yerel bir dizinde
MathJax arıyordu.

PDF uretmek icin

```
pandoc doc.md metadata.yaml -t latex  -fmarkdown-implicit_figures -o out.pdf
```

Dosya `metadata.yaml` içinde LaTeX'e gereken ek kütüphaneler tanımlanabilir. Mesela
bizim tanım,

```
---
header-includes:
- \usepackage{cancel}
---
```

Not: Ek kütüphaneleri HTML'e vermek yine şablonla mümkün, mesela
şablonda `head` kısmına

```
<script type="text/x-mathjax-config">
MathJax.Hub.Register.StartupHook("TeX Jax Ready",function () {
  MathJax.Hub.Insert(MathJax.InputJax.TeX.Definitions.macros,{
    cancel: ["Extension","cancel"], cancelto: ["Extension","cancel"]
  });
});
</script>
```

eklersek `pandoc` üretim sırasında bu şablon üzerinden gerekli
referansları dahil edecektir.

Not: PDF üretimi için `pandoc` bir noktada sisteminizdeki LaTeX
komutunu kullanır, bunun için minimal bir LaTeX kurulumu olmalıdır,
mesela Ubuntu üzerinde baz seviyede

```
sudo apt install texlive-latex-base texlive-formats-extra
```

kurulumu gerekebilir.

## Grip

```
sudo pip install grip
```

Dokümanı derlemek için

```
grip README.md --export out.html
```

Artık out.html içinde markdown dokümanının görüntüsü var. 

## Python Paketi markdown

`markdown` adli bir paketin python icinden yukleyip
kullanabiliriz. Kurmak icin `pip install markdown`, sonra mesela soyle
bir md icin,

```
import markdown, sys, os
content=open('dosya.md').read()
print (markdown.markdown(content, extensions=['fenced_code']))
```

`fenced_code` kullanimi birden fazla satira yayilan kodlari halletmek icin.


