# Tex Paketleri Eklemek

TeX'e ek islevsellik kazandiran stil dosyalari (style files) kurmak
icin ne yapilir? Bu dosyalar .sty soneki tasirlar. Kurmak icin .ins
ile biten bir dosya uzerinde latex isletmek gerekebilir, ve .sty
dosyasi uretilir. Ubuntu uzerinde bu dosyalar alinir ve

```
/usr/local/share/texmf/tex/latex
```

altinda pakete tekabul eden bir dizin mkdir ile yaratilip oraya
kopyalanir. Daha sonra iki ust dizine cikilir, orada ls-R adinda bir
dosya var. Orada

```
./tex/latex:[yeni paket]

..../tex/latex/[yeni paket]:
[yeni paket].sty
```

Ekleri yapilir. Artik `\usepackage` ile bu paketi kullanabilirsiniz.

Tabii tum bunlari kolaylastiracak bir script soyle,

```
import ospkgs = ['paket1','paket2']
fout = open("/tmp/ls-R","w")
fout.write("% ls-R -- filename database for kpathsea; do not change this line.")
fout.write("\n")
fout.write("./tex/latex:\n")for x in pkgs:
fout.write(x+"\n")
    fout.write("\n")
for x in pkgs:
    fout.write("./tex/latex/%s:\n" % x)
    fout.write("%s.sty\n" % x)
    fout.write("%s.tex\n" % x)
for x in pkgs:
    os.system("mkdir /usr/local/share/texmf/tex/latex/%s" % x )
    os.system("cp %s.sty /usr/local/share/texmf/tex/latex/%s/" % (x,x) )
    os.system("cp %s.tex /usr/local/share/texmf/tex/latex/%s/" % (x,x) )
fout.close()os.system("cp /tmp/ls-R /usr/local/share/texmf/" )
```


Bu script ayni dizin icinde gerekli sty dosyalarinin oldugunu
varsayar. Script sudo olarak isletilmelidir.






