# Sayfadan Video Indirme, videolectures.net, rtmpdump

Komut satiri araclari kullanarak mesela videolectures.net gibi bir
siteden nasil video indiririz? Once rtmpdump kurmak lazim,

```
sudo apt-get install rtmpdump
```

Bu aracin hangi adrese isaret edecegini bilmesi lazim. Fakat o gereken
adres web adresi degil, o web sayfasindan sonra arka planda baglanilan
baska bir adres. Bu adresi bulmak icin kendi network iletisiminizi
"takibe" alabilirsiniz, su komutu isletin,

```
sudo iptables -t nat -A OUTPUT -p tcp --dport 1935 -j REDIRECT && rtmpsrv
```

Bu bir servis baslatacak ve bekleyecek. Simdi web sayfasina girin,
mesela

http://videolectures.net/cikm08_elkan_llmacrf

Sayfaya girilip video baslayinca arkada isleyen servisin ciktisina
bakin. Buradaki cikti video indirmek icin gerekli rtmpdump komutunu
listelemis olacaktir.

Servisi Ctrl-C ile durdurun ve rtmpdump isleminizi baslatin. 

Eger indirme sirasinda baglanti koparsa, tekrar kalinan yerden
baslatmak icin komuta -e secenegi eklenebilir.

Not: Eger rtmpdump, hemen iptables ardindan baslamiyorsa, o zaman
makinanizi reboot edip tekrar deneyebilirsiniz.

Not: videolectures.net gibi siteler mesela bir dersi parca parca
verebiliyor, bu durumda ana adrese, mesela ikinci parca icin /video/2
ekini yapmak gerekiyor. Mesela

http://videolectures.net/cikm08_elkan_llmacrf/video/2

adresi icin su rtmpdump cikti

```
rtmpdump \-r "rtmp://hydro2.videolectures.net:1935/vod" \-a "vod" \-f "LNX 11,2,202,327" \-W "http://media.videolectures.net/rel.1383672220/common/swf/PlayerViidea.swf" \-p "http://videolectures.net" \-y "mp4:v00e/a3/unhk5n6cclzlkipr25rxyh5i2manetf7.mp4" \-o bir_dosya_ismi.flv
```

Not: rtmpdump bazen bazi bolumleri tekrar tekrar kayit
edebiliyor. Mesela 1:00 ve 1:05 arasi, 1:05 ve 1:10 arasinda
tekrarlanabiliyor. Cozum: video'yu goruntulemek icin VLC kullanirsaniz
bu program otomatik olarak tekrarlanan bolumleri atlamayi biliyor.

Kaynak






