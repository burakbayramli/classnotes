# Ubuntu 11

Ubuntu 11 versiyonunu kurduk, bazi notlar:11.10Her Ubuntu surumunde
masaustu biraz daha degisiyor. 11.04 ve 11.10 arasinda bazi farklar
var, once 10'dan bahsedelim, sonra 04 okunabilir. Oncelikle kubuntu
ortami 04'ten farkli, ve artik bizim kullanabilecegimiz bir halde
degil. Onun yerine Gnome masaustu kullaniyoruz.sudo apt-get install
gnome-shellSonra Logout yapilir ve tekrar girmeden once tekerlek
sembolune tiklanir, ve Gnome secilir.Emacs ile Alt-Space cok
kullaniyoruz, ama Gnome'da bu kapilmis, iptal etmek icin System Tools
| System Settings | Keyboard. Oradan Keyboard, Shortcuts,
Windows. Activate the window menu secenegi Alt-Space
tanimlamis. Uzerine iki tiklayin, ve alakasiz baska bir tuslama
tanimlayin (backspace ile disable etmek ise yaramiyor).Sonrasudo
apt-get install gnome-tweak-toolgnome-tweak-toolBu program ile cop
sepeti, vs. gibi seyleri geri alabilirsiniz. "Have File Manager Handle
Desktop" On yapilmali. 11.04Bu versiyon kurulus sirasinda masaustu
(desktop) secenegi sunmuyor. Kurulum bittikten sonra ilk yuklenen
desktop, asiri buyuk dugmelerle, habire animasyon yapan Windows
Vista'ya benzer acaip bir ortam.

Buradan hemen kurtulmak icin soldaki program ikonlari (toolbar) icinde
+ isaretinden uygulamalara girilir, oradan "Ubuntu Software Center"
bulunur, ve center icinde "kubuntu-desktop" yazilir. Bu paket
Internet'ten indirilecek, ve kurulacak. Sonra classic desktop secerek
basit, sade bir desktop ortamina kavusabilirsiniz.Tex / Latex
icintetex-.. paketleri degil alttaki kullanilmalisudo apt-get install
texliveUbuntu 11 Emacs 23'u destekliyor, apt-get ile 22 alinamiyor,
apt-get ile 23 gelecek,sudo apt-get install emacsEmacs uzerindeki
preview-latex programini tekrar kurarken sunlari kullandiksudo apt-get
install emacs-extra emacs-goodies-el preview-latex dvipng.emacs
icindeki tanimlar su hale geldi:

(custom-set-variables..'(preview-scale-function
1.2)'(preview-image-type (quote dvipng))..)

Eger bunlara ragmen islemediyse, Auctex (preview-latex paketi artik bu
isimde) paketini kaynak olarak indirin, ve configure, make, sudo make
install ile kurun. .emacs icine (load "preview-latex.el" nil t t)
yazin ve hersey guzelce islemeye baslayacak.OpenCV 2.0Ubuntu paketleri
icinde OpenCV 2.1 icin destek var. Kurmak icin apt-get install
python-opencv. Bu gerekli ek paketleri indirip kuracaktir. Eger
webcam'e erisilemiyorsa, gstreamer-properties programindan WebCam
ismini direk secmek gerekebilir.Chat IkonuSag ust kosede bir mektup
zarfi ve balon sekillerinde gereksiz iki ikon var. Bunlar silmek
uzerine tiklayarak mumkun olmadi. Suraya gore apt-get kullanmak
lazim:sudo apt-get remove indicator-me
indicator-messagesMatplotlibshow() komutu en son kurulumda problem
cikardi. Cizim yaptirilan arka plandaki motorun (backend) secilmesi
gerekiyor. WX iyi bir secenek, kurmak icin

sudo apt-get install wxgtk2.8Matplotlib'in bundan haberdar olmasi icin

sudo gedit

/usr/local/lib/python2.7/dist-packages/matplotlib/mpl-data/matplotlibrcDosyadabackend
: WXAggyazilmasi yeterli. Bu satir buyuk bir ihtimalle sadece Agg
iceriyor olacakti.

Animasyonlar

Masaustunde pencere acma, kapama hareketlerinin animasyon yapmasini
durdurmak istiyorsaksudo apt-get install
compizconfig-settings-managerSonra menuden System | Settings | Compbiz
Settings Manager secilir ve "Effects" bolumunde Effects icin secim
kutusu iptal edilir.Torsudo apt-get install tor tor-geoipdbsudo
apt-get install privoxyile kurulur. Privoxy ayari icin sudo gedit
/etc/privoxy/config ile dosyayi edit edin, ve icerik
olarak

forward-socks4a / localhost:9050 .

Firefox 4 uzerinde TorButton icin

https://www.torproject.org/torbutton/Install Alpha baglantisindan xpi

dosyasi isletilebilir. YouTube video'larini ve flash icerik gorebilmek
icin eklenti kurulduktan sonra Firefox sol ustte Tor statusunu
gosteren bir isaret cikacak. Bu isaret uzerinde mouse sag dugme click
yaparak "preferences" secenegine girin. Security Preferences tab'inden
"disable plugins during Tor usage" secenegini de-aktive edin.Simdi
kirmizi xarpi isareti uzerine tiklayin, bu isaret yesile donusecek. Bu
kadar. Artik istediginiz siteye baglanabilirsiniz.Alt-TabMasaustu
uzerinde klavye kisayolu kullanarak programlar arasinda gecis yapmak,
Ubuntu 11 olagan ayarlarinda ayvayi yemis.

Genelde en son kullanilan ilk geri gelir kurali islemiyor. Duzeltmek
icin Sytem | Preferences | CompizConfig Settings Manager uzerinde
Window Management secilir ve Static Application Switcher yerine
Application Manager secilir.Ses EfektleriEger konsol icine mesela
gidilemeyecek durumda bile backspace yapilinca cikan bip, tan, tun
seslerini kapatmak istiyorsak System Tools | System Settings, oradan
Sound ve Sound Effects. Bir ses kontrol ayari var, onun yaninda Mute
secilirse artik uyarici sesleri cikmaz.SifreEger kullanicinizin
sifresini kisa yapmak isterseniz, passwd uzerinden gireceginiz sifre
Ubuntu tarafindan begenilmeyebilir (kisa, cetrefil olmayan sifreyi
guvensiz buluyor). Ama illa ki kisa / basit bir sifre vermek
isterseniz, o zaman "sudo passwd [kullanici isminiz]" ile istediginiz
sifreyi verebilirsiniz. Kendiniz super kullanici olmaniza ragmen sudo
uzerinden tekrar kendi kullanicinizi referans etmeniz garip olabilir,
ama kisa sifre bu sekilde memnun oluyor.


