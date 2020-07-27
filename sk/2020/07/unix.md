# Unix

İşletim sistemlerinin kralı tartışmasız Unix. Geliştirilmesi 70'li
yıllarda başlayan ve C programlama diline yakın oluşuyla daha da
yayılan bu sistem çekirdeksel olarak şu anda cep telefonlar dahil
olmak üzere milyarlarca makina üzerinde işliyor.

Surecler

Unix'i Unix yapan pek çok kavram var. Kullanıcıya pek gözükmeyen sahne
arkasındaki programları denetleyen, işleten çekirdek seviyesinde
yaptıkları var, dosya / dizin bazında kullanıcıya gözüken kısmı var.

Unix'te süreç kavramı önemli. Her işleyen program bir süreç içinde
işler, diğerlerinden ayrı çevre değişkenleri, yerel hafıza bloğu
vardır (global hafızaya özel programlama ile erişilebilir). O anda
bilgisayarınızdaki işleyen süreçleri görmek için `ps -eaf`
işletebiliriz, mesela bende

```
burak    15316 15314  0 11:21 pts/0    00:00:05 mplayer -quiet -playlist http://
burak    15400     1  2 11:24 tty2     00:01:43 /usr/bin/emacs25
burak    16320     1  1 11:47 tty2     00:00:49 /usr/lib/firefox/firefox -new-wi
burak    16487 16320  0 11:47 tty2     00:00:26 /usr/lib/firefox/firefox -conten
root     17133     2  0 11:54 ?        00:00:01 [kworker/u8:2+ev]
```

listesi var. Hakikaten şu anda Emacs içindeyim, arka planda Firefox
işliyor, vs. Bunların hepsi görülüyor. Bu surecler yokedilebilir, vs.,
bkz [Faydalı Unix Komutları](../../2012/04/faydali-unix-komutlari.md).

Komut Satırı, Kabuk

Unix'te pek çok şey komut satırı etrafında döner, en azından usta
admin, kullanıcılar onu tercih eder. Komut satırını başlattığımızda,
mesela Ubuntu Linux'ta Terminal programı ile, bir süreç
başlatılmıştır, ve bu süreç bir işler programın çağrılması ile
olmuştur. Başlatıyorum ve süreç listesine bakıyorum,

```
burak    14899 14890  0 11:20 pts/0    00:00:00 bash
```

görülüyor. Komut satırı "programı" bu işte. Tabii komut satırları tek
tip değil, pek çok farklı program var, üstteki `bash`, ama `sh` var,
`tsch` var, vs. Terminale gidip

```
echo $SHELL

```

deyince

```
/bin/bash
```

cevabi aliyorum. 




Referans

[Faydalı Unix Komutları](../../2012/04/faydali-unix-komutlari.md)

