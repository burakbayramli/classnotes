# Unix Rsync - Makineler Arası Dosya Kopyalamanın Hızlı Yolu

Sitemiz için kullandığımız tekniklerden bir tanesi, geliştirme
ortamında yaratılan içerik .xml, .class, .jpg, .gif kayıtlarından
"sadece değişmiş olanların" sonuç ortamına kopyalanmasıdır. Kabul
edersiniz ki, üst üste yapılan ve çok sayıda dosya içeren bu şekilde
kopyalamalar için, sadece ve sadece yeni ve değişmiş dosyaların
kopyalanması zaman bakımından en verimli seçenektir.

Sitemizin ilk başlangıç yıllarında kullandığımız Unix scp programı,
her kayıtı sürekli sonuç ortamına kopyalıyordu. Örnek bir scp komutu
aşağıda gösteriliyor.

```
scp *.jsp kullanici@bizim.isp.makinasi.com:/usr/local/site/dizin/
```

Rsync

SSH ve CVS ilgili makalede, ssh (ve dolaylı olarak scp) komutu ile,
güvenli bir şekilde nasıl uzak herhangi bir makinada Unix komutu
işletebildiğimizi görmüştük. SSH'in öngördüğü anahtar dosyalar doğru
yerlere koyulduktan sonra,

```
ssh uzakmakina.com -l kullanici ls /usr/local/herhangibirdizin
```

..gibi bir komut, uzakmakina.com üzerindeki bir dizinin içeriğini dökebiliyordu! Hemde bunu yaparken, bildiğimiz Unix komutlarını uzaktaki bir makine üzerinde kullanabiliyorduk. Bu çok güçlü bir yöntemdir. Rsync programı da, arka planda kopyalamayı gerçekleştirmek için SSH'i kullanabiliyor. Güvenlik bakımından bu yöntemi seçmeniz tavsiye olunur.

SSH bağlantısı, rsync programına -e ssh ibaresini ile yapılıyor.

Aşağıda örnek bir rsync komutu görüyoruz.

```
rsync -av -e ssh /usr/local/dizin kullanici123@uzakmakina.com:/usr/local/filan/dizin
```

Bu komut, /usr/local/dizin dizini ile uzakmakina.com makinasındaki
/usr/local/filan/dizin dizinini kullanıcı123 adlı kullanıcı üzerinden
eşitliyor.

Not: Bu rsync komutunu ilk işlettiğinizde, bütün dosyaların bir kere
kopyalandığını göreceksiniz (eğer sonuç dizini tamamen boş ise). Fakat
ikinci sefer aynı rsync komutunu işletince, hiçbir kopyalanmanın
olmadığını görmeniz mümkün olacak. Çünkü ikinci sefer rsync, akıllı
bir şekilde hangi dosyaların değiştiğini ve eklendiğini anlamaya
çalıştı, ve sadece farkları göndermeye çalıştı.


üzerinde kurmak için, rsync tar dosyasını cygwin dizininize (mesela
c:/cygwin) bırakıp, tar xvf komutu ile açın.

Lokal Kopyalar İçin

Rsync'i iki diskiniz arasında yedekleme yapmak için de
kullanabilirsiniz. Bu gibi durumlarda -e seçeneğine ihtiyacınız
yoktur. Sadece -av ile rsync kullanmanız yeterli, mesela:


```
rsync -av  /cygdrive/c/Arsiv /cygdrive/f/Arsiv
```

Bu komut `c:` disk üzerindeki Arsiv dizinini `f:` adlı diske
taşıyacaktır. Tabii, tekrar söyleyelim, rsync'in cp komutuna göre
yararları şunlar; Aynen makinalararası durumda olduğu gibi, ilk
kopyalama tamamlandıktan sonra "ikinci rsync" işlemi sadece farkları
(birinci kopyalamadan sonra yeni olan dosyaları) gönderecektir. Bir
diğer seçenek --delete ile de "hedef dizininde olup, kaynak dizininde
olmayan dosyaları" bile otomatik olarak hedeften sildirebilirsiniz!
Rsync mükemmel bir eşitleme aracıdır (synchronization tool).


Gördüğümüz gibi bu tür güçlü bir araç için tıklama ile "güzel gözüken"
arayüzlü bir programı aramamıza hiç gerek yok. Unutmayın: Tıklamalar,
tekrar işletilemez, bu yüzden admin'lerin korkulu rüyasıdırlar.

Eger cetrefil bir ssh kullaniyorsaniz, mesela ssh'e -P ile port
vermeye mecbur kalmissaniz, vs

```
rsync --recursive -ve 'ssh -p [port]' /yerel/dizin kullanici@1.1.2.13:/uzak/dizin
```

Not: Bizim sifirdan Python uzerinde yazdigimiz [rsync.py](https://github.com/burakbayramli/kod/blob/master/rsync.py)
komutu da burada kullanilabilir.


