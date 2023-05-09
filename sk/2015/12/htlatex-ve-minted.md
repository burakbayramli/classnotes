# htlatex ve minted, make4ht


TeX dosyalarindan HTML uretmek icin 

```
htlatex dosya.tex
```

ifadesi kullanilir. Kurulu degilse

```
sudo apt-get install  tex4ht-common
```

Fakat icinde minted paketine referans olan tex dosyalarinda problem
cikabilir; "-shell-escape secenegi kullanilmamis" hatasi verilecek,
fakat bu secenek eklense bile problem devam ediyor. 

Cozum, 

```
htlatex dosya.tex "dosya" "" "" -shell-escape
```

kullanimi, bunun icin ayrica dosya.cfg ayni dizinde olmali, icerigi 

```
\Preamble{xhtml}

\Configure{HColor}{DarkGray}{\#1A1A1A}

\begin{document}

\EndPreamble
```

Fakat ustteki program karakter kodlamasi (encoding) icin problem
cikartabiliyor. Her seyi utf-8 bazli yapmak istiyorsak make4ht
programi daha kullanisli olabilir,

```
make4ht -u dosya.tex -s 
```


Kaynak

http://tex.stackexchange.com/questions/225763/latex-to-html-using-htlatex-background-problem

