# Firefox Icin SSH Tunelleme

Altta bahsedecegim tunelleme tekniginin 2 yarari var: Birincisi hicbir
Internet yasagina takilmadan, istediginiz siteyi
gezebilmeniz. Ikincisi, eristiginiz adres ve network trafiginizin
guvenli hale gelmesi; Mesela acik ve/ya size ait olmayan bir Internet
baglantisindan (cafe'lerdeki WiFi noktalarindan mesela) disari
cikiyorsaniz, bu teknik isinize yarayacak.Teknigin tek dezavantaji,
Internet yasagini delmek istiyorsaniz, ulke disinda bir Unix hesabi
gerektirmesi. Ama sadece Internet'i guvenli gezmek istiyorsaniz, ulke
icinde bir makina da olabilir.SSH tunelleme teknigi icin once Unix
makinaniza baglaniyorsunuz:

```
ssh -D 8080 -f -C -q -N kullanici@makina
```

Bunu islettikten sonra komut satiri hemen geri donecek, ama
baglantiniz arka planda yapilmis olacak. Ustte lokal makinanin 8080
portunu tunelleme portu olarak tanimladik. Simdi Firefox'a girin ve
tum Internet trafiginin bu port uzerinde disari cikmasi icin, gerekli
ayarlari yapin. Bunun icin URL kutusuna `about:config` yazin, bu size
gereken ayarlari getirecek.

Burada olmasi gerekenler soyle:

```
network.proxy.no_proxies_on : localhost, 127.0.0.1
network.proxy.socks : 127.0.0.1
network.proxy.socks_port : 8080
network.proxy.socks.remote_dns : true
network.proxy.socks_version : 5
network.proxy.type : 1
```

Bu kadar. Artik Firefox'un tum istekleri SSH uzerinden Unix makinasina
gidecek, ve Internet'te gezinmeyi aslinda bu makina yapacak, ve size
sayfa iceriklerini yine SSH uzerinden geri dondurecek. Eger
network'unuz "izleniyorsa" sadece sifrelenmis SSH trafigi gorecekler,
gittiginiz URL bile belli olmayacak.



