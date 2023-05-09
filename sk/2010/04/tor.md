# Tor

Tor programi, o programi isleten digerlerinin bilgisayarlarini
Internet uzerinden birlestirererek "guvenli" bir alternatif iletisim
agi olusturur. Isyerinizde belli bir site blok edilmis ise, Tor ile
bir "sanal aga" baglanip disari "cikarak" o istediginiz siteye
baglanabilirsiniz. Tabii sizin diger bilgisayarlari kullandigi gibi,
baskalari da sizin bilgisayarinizi diger noktalara erismek icin
kullaniyor olabilir - performans acisindan bunu da belirtelim. Yani
surekli Tor isletmek bilgisayar performansinizda negatif bir etkiye
sebep olabilir.Ubuntu uzerinde Tor kurmak icinsudo apt-get install
torsudo apt-get install privoxykomutlari yeterli.Not: Ubuntu 9.10 Tor
kurulumu icin surayi takip etmek lazim cunku bu durumda normal apt-get
islemiyor: /etc/apt/sources.list dosyasi icindedeb
http://deb.torproject.org/torproject.org karmic mainsatirlari en sona
eklenir vegpg --keyserver keys.gnupg.net --recv 886DDD89gpg --export
A3C4F0F979CAA22CDBA8F512EE8CBC9E886DDD89 | sudo apt-key add
-isletilir, sonraapt-get updateapt-get install tor tor-geoipdbPrivoxy
icin sudo nano /etc/privoxy/config ile dosyayi edit edin, ve icerik
olarakforward-socks4a / localhost:9050 .ekleyin. Baslatmak icinsudo
/etc/init.d/tor startsudo /etc/init.d/privoxy startSonra Firefox 3.0
uzerinde TorButton adli eklentiyi kurun.YouTube video'larini ve flash
icerik gorebilmek icin: Bu eklenti kurulduktan sonra Firefox sag alt
kosede Tor statusunu gosteren bir isaret cikacak. Bu isaret uzerinde
mouse sag dugme click yaparak "preferences" secenegine girin. Security
Preferences tab'inden "disable plugins during Tor usage" secenegini
de-aktive edin.Simdi mouse sol click ile Tor Disabled yazisi uzerine
tiklayin, bu yazi "Tor Enabled" gibi yesil bir yaziya donusecek. Bu
kadar. Artik istediginiz siteye
baglanabilirsiniz.Kaynaklarhttps://help.ubuntu.com/community/Tor?action=show&redirect=TORhttp://support.mozilla.com/tiki-view_forum_thread.php?locale=fi&comments_parentId=82766&forumId=1





