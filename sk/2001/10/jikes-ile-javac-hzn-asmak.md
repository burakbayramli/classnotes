# Jikes ile Javac Hızını Aşmak

Sun'ın Java paketinden çıkan derleyici program Javac, genel kullanım
için yeterli olsa da, büyük projelerde yetersiz kalabilmektedir. Bir
seferde 1000 Java dosyasının kodlanması derleyiciyi oldukça
uğraştırabilir. Ant ile sadece değişen Java dosyalarını derleyemek
suretiyle hızlandırma sâğlayabiliriz, fakat silbaştan derleme lâzım
olunca, derleyicimizin kendisinin hızlı olması işimize yarayacaktır.
Javac derleyicisi, tekrar Java kullanılarak yazılmıştır. Java dili,
yoğun hafıza ve disk erişimi gerektiren derleyici programları için
aşağı yukarı uygun olsa da, C++ dili ile yazılmış Jikes, bu işi daha
hızlı yapabilmektedir. Çok kodlu projeler için Jikes kullanmanızı
tavsiye ediyoruz.  İndirmek için IBM'in Jikes sayfasını ziyaret
edebilirsiniz.  Ant ve Jikes Ant içinde Jikes kullanabilmek için,
aşağıdaki gibi bir tanım yapmak yeterli.

```
<project name="proje1" default="filan" basedir="."><property
name="build.compiler" value="jikes"/>...
```

Jikes, Javac ile birebir uyumludur, yani, derleyicinin ürettiği .class
dosyaları, Javac ile üretilen .class dosyaları kadar geçerlidir.
Jikes'ı Emacs içinden çağırıyorsanız ve Jikes hata mesajlarının Emacs
tarafından yorumlanabilir formatta olmasını istiyorsanız, aşağıdaki
seçeneği aktif hale getirebilirsiniz.  ...

```
<property name=" build.compiler.emacs" value="true"/>..
```


![](jikes.jpg)

