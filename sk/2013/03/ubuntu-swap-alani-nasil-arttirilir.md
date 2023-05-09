# Ubuntu SWAP Alanı Nasıl Arttırılır?

SWAP alani bellek yetersiz oldugu zaman bellekteki bilginin bir
kismini belli zamanlarda diske yazilip geri alinabilmesini saglar,
boylece teorik olarak eldeki disk miktarindan cok daha fazla hafiza
"varmis gibi" yapabiliriz. Swap kelimesi degis-tokus anlaminda zaten,
yani hafiza ve disk arasinda bir degis-tokus oluyor. Tabii ki diske
cok fazla yazmak, oradan fazla bilgi okumak bellege gore daha
yavastir, vs. Yani bir muhendislik denge (trade-off) durumu.

Ubuntu'nun normal (kurulus aninda tanimlanan) swap buyuklugu bellek
kadar - eger 4 GB RAM var ise 4 GB swap yaratiliyor. Bunu sonradan
arttirmak icin, mesela 20 GB buyuklugunde swap icin, once swap'in
yasayacagi bir yer (dosya) yaratmak lazim.

```
sudo dd if=/dev/zero of=/swapfile.img bs=1024 count=20M
```

Swap dosyasinin nerede olacagini boylece tanimladik. Bu "yer" cok
hizli isleyen bir diskin baglanti (mount) noktasi olabilir mesela,
belki bir solid state disk orada duruyor. Sonra,

```
sudo mkswap /swapfile.img
```

Simdi sudo gedit `/etc/fstab` ile dosyaya girilir ve sonuna

```
/swapfile.img swap swap sw 0 0
```

eklenir. En son

```
sudo swapon /swapfile.img
```

ile swap aninda aktif hale getilir. /etc/fstab degisimi bilgisayar her
basladiginda swap'in otomatik olarak aktive edilmesi icin.






