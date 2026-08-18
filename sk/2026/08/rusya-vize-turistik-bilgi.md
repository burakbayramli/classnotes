# Rusya'ya Gidiş, Vize, Turistik Bilgiler

### TR

Seyahat Sigortası

Vize başvurusu öncesi elde olması gereken tek belge sağlık
sigortası. Ben GöingRus sitesini kullandım, ücreti fazla değil, Web
üzerinden kredi kartı ödemesi yapılınca sigorta size email ile
gönderilir. O gönderilen PDF dosyasını bir yazıcıdan bastırmak iyi
olur.

[https://goingrus.com/en/insurance](https://goingrus.com/en/insurance)

Vize

Elektronik vize başvurusu kullanabilir, RU elçiliği ziyareti vs şart
değildir. Otel, uçak rezervasyonu yapılmış bile olması gerekmez
(sadece saglik sigortasi gerekli), formlar kalınacak adres sorunca
kalması planlanan otel ismi, adresi verilebilir.

Başvuru programı bir noktada vesika / yüz resmi, bir diğerinde
pasaport resmi isteyecektir. Vesika için biyometrik (tercihen
fotoğrafçıda çekilmiş olan) resimlerden birinini telefonla dijital
fotosunu alabilirsiniz. Bu fotoyu yanlardan traşlayarak kenarları tam
fiziksel fotoda olduğu hale getirilebilir (mesela Gimp programı ile
bölge seçimi yapıp 'crop to selection' ile).

Fakat iş hala bitmiş olmuyor, başvuru programı genişlik / yükseklik
oranı hakkında çok seçici imiş, muhakkak 35 / 45 oranı
gerekiyormuş. Bu durumda dijital fotoyu doğru orana getirmek için

```
convert input.jpg -resize 700x900^ -gravity center -extent 700x900 output.jpg
```

komutu kullanılır.

Pasaport için böyle bir şart yok, onu da benzer şekilde telefon ile
fotolayıp, o dijital resmi traşlayıp yükleyebiliriz. Pasaport resmi
icin tek yapilmasi gereken seri no'sunun yukleme programinin alt kare
bolgesine hedeflemek. Site kullaninca gorulecektir, pasaport resmi
hareket ettirilebiliyor, fotoyu kaydirip o hucre icine pasaport nosunu
hedefliyorsunuz. Bu kadar. 2-3 gün sonra vize kabulü size email ile
gönderilecek, onu PDF dosyasını da yazıcıdan bastırın.

Para

Rusya'da yabancı, TR çıkışlı kredi kartı, banka kartları çalışmıyor.
Nakit götürülmesi gerekir, oteli bile telefonla rezervasyon sonrası
ödemesini nakit ile orada yaptık. 10000 USD altındaki yolcunun yanında
taşıdığı nakitlerin deklere edilmesi gerekmez, en rahat seçenek budur.

TR Havalanı Çıkış

Çıkarken harç ödemesi lazım, bu mobil bankacılık ile yapılabilir
(Sabiha Gökçen'deki "harç kiosklarına" güvenilir degil, ben baktığımda
bu aletler çalışmıyordu). Programda "Ödemeler | Harç" seçilir, şehir
"İstanbul" seçilir, ilçe herhangi bir ilçe olabilir. Bu ay itibarı ile
1500 TL.  Eski günlerde "pul" satan birisi olurdu, o pul pasaport
içine konurdu, artık o günler kalmış ama hala bir harç alınıyor.

### RUSYA

Para

Gelir gelmez havaalanında döviz verip ruble alınır. 100-200'lük kağıt
para olması iyi olur, metro için gerekecek.

Not: Döviz değişimlerinde bir dükkan aynı kişi için bir günde sadece
400 Euro'lük değişim yapabilir. Daha fazlası gerekirse başka dükkana
gidilebilir tabii.

Metro

Seyahat kartı almak için kullanılacak aletin ekranı, başlangıç hali
böyle. 

![](metro-bilet-ekrani.jpg)

Sol üst köşedeki ilk iki seçenek 1 seyahat, 2 seyahat için, yani kart
/ bilet tek kullanımlık, ya da iki kullanımlık. Turistler için en
rahat olanlar bunlar... Birine basıyorsunuz, ardından para veriliyor
(ekranda ne yazdığına bakmayın bu noktada, basım ardından hemen para),
mesela 1 seyahat seçim sonrası 100 ruble alt sağ köşeden verince,
alttan (kağıt) kart ve para üstü alt kısımdaki bölmeye düşüyor. Diğer
seçenekler mesela 30 gün, 90 gün, vs. daha uzun süreli ziyaretçiler
bunlara bakabilir.

Telefon / Internet

Rusya savaş halinde bir ülke. Ukrayna'nın gönderdiği SİHA'ların GPS
kullanması sebebiyle Rusya büyük şehirlerde GPS sinyallerini
karıştırıcı teknoloji kullanıyor (jamming). GPS ile konum belirleme
büyük şehirlerde işlemiyor. Anladığım kadarıyla Rusya'nın kendi uydu
sistemleri GLONASS da bu blokeden nasibini almış, taksiciler, taşıyıcı
firmalar durumdan şikayetçi, onlar da konum bilgisine güvenemiyorlar.

Gitmeden önce Web'den baktık, "yapay zeka"'ya sorduk", "mobil gezici
(roaming) ayarını yapın, Internet kullanabilirsiniz" gibi yorumlar
vardı. Bunlara güvenmeyin. Roaming işlemiyor. Çoğu popüler İnternet
sitesi blok halinde, onlara da erişilemiyor. "VPN ile bloku aşarsınız"
deniyor, otelimizin Wifi'i üzerinden birkaç bedava VPN denedik, VPN
olmadı. Eğer RÜ telefon / mobil / Internet hattı olsa, onun üzerinden
cüzi para ödeyip VPN kurulması mümkün imiş (Red Shield?), ama turistik
seyahat için bunlarla uğraşmak istemedik. Blok edilmiş siteler,
uygulamaların bazıları YouTube, Whatsapp, Telegram, Google Gemini (ama
Google Arama, ve GMail işliyor).

RU on ödemeli (prepaid) SIM kartı alınması uzun iş,
geldiğimde havaalanından çıkarken turistler için "geçici" kartlar
satan birisini vardi, ama yabancıların SIM alması resmi prosedür
gerektiriyor, kart hemen açılıyor(muş) ama otelinizin gerekli
bakanlığa kaydınızı bildirmesi gerekiyor. Bu bildirim oteller için
kolay bir iş dediler, fakat geçici kart için ne kadar buna değer mi?

Harita

Benim en çok işime yarayan Maps.Me uygulaması oldu. Bu programın
Internet bağlantısına ihtiyacı yoktur, çevrimdışı (offline)
çalışabilir. Gitmeden öne TR'de iken gidilecek yerin tüm kısımlarına
tıklayıp o bolge bilgileri indirilmesi iyi olur. Böylece yer
bulunamasa bile önemli nokta, park, müze, cafe isimleri girerek
(maps.me arama özelliği) nerede olundugu bulunabilir. Uygulamanın sol
üst köşesinde katmanlar (layers) kısmında metro haritası katmanı
vardır, bu Moskova'da benim çok işime yaradı, tüm hatlar, bağlantılar
orada gösteriliyor. Şaka değil, kampçılıktan alışkanlık yanımda hep
taşıdığım pusula bile ise yaradı, GPS olmayınca güney, kuzey neresidir
diye bilmek gerekli olabiliyor.

