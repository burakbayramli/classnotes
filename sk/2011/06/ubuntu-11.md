# Ubuntu 11

Ubuntu 11 versiyonunu kurduk, bazi notlar:

11.10

Her Ubuntu sürümünde masaüstü biraz daha değişiyor. 11.04 ve 11.10
arasında bazı farklar var, önce 10'dan bahsedelim, sonra 04
okunabilir. Öncelikle kubuntu ortamı 04'ten farklı, ve artık bizim
kullanabileceğimiz bir halde değil. Onun yerine Gnome masaüstü
kullanıyoruz.

```
sudo apt-get install gnome-shell
```

Sonra Logout yapılır ve tekrar girmeden önce tekerlek sembolüne
tiklanır, ve Gnome seçilir. Emacs ile Alt-Space çok kullanıyoruz, ama
Gnome'da bu kapılmış, iptal etmek için System Tools | System Settings
| Keyboard. Oradan Keyboard, Shortcuts, Windows. Activate the window
menü seçeneği Alt-Space tanımlamış. Üzerine iki tıklayın, ve alakasız
başka bir tuşlama tanımlayın (backspace ile disable etmek ise
yaramıyor). Sonra

```
sudo apt-get install gnome-tweak-toolgnome-tweak-tool
```

Bu program ile çöp sepeti, vs. gibi şeyleri geri alabilirsiniz. "Have
File Manager Handle Desktop" On yapılmalı.

11.04

Bu versiyon kuruluş sırasında masaüstü (desktop) seçeneği
sunmuyor. Kurulum bittikten sonra ilk yüklenen desktop, aşırı büyük
düğmelerle, habire animasyon yapan Windows Vista'ya benzer acaip bir
ortam.

Buradan hemen kurtulmak için soldaki program ikonları (toolbar) içinde +
işaretinden uygulamalara girilir, oradan "Ubuntu Software Center"
bulunur, ve center içinde "kubuntu-desktop" yazılır. Bu paket
İnternet'ten indirilecek, ve kurulacak. Sonra claşsiç desktop seçerek
basit, sade bir desktop ortamına kavuşabilirsiniz.Tex / Latex için
tetex-.. paketleri değil alttaki kullanılmalı

```
sudo apt-get install texlive
```

Ubuntu 11 Emacs 23'u destekliyor, apt-get ile 22 alinamiyor, apt-get
ile 23 gelecek, `sudo apt-get install emacs`

Emacs üzerindeki `preview-latex` programını tekrar kurarken şunları
kullandık

```
sudo apt-get install emacs-extra emacs-goodies-el preview-latex
```

dvipng.emacs icindeki tanimlar su hale geldi:

```
(custom-set-variables..'(preview-scale-function
1.2)'(preview-image-type (quote dvipng))..)
```

Eger bunlara ragmen islemediyse, Auctex (preview-latex paketi artik bu
isimde) paketini kaynak olarak indirin, ve configure, make, sudo make
install ile kurun. .emacs icine (load "preview-latex.el" nil t t)
yazin ve hersey guzelce islemeye baslayacak.OpenCV 2.0Ubuntu paketleri
icinde OpenCV 2.1 icin destek var. Kurmak icin apt-get install
python-opencv. Bu gerekli ek paketleri indirip kuracaktir. Eger
webcam'e erisilemiyorsa, gstreamer-properties programindan WebCam
ismini direk secmek gerekebilir.

Chat Ikonu

Sağ üst köşede bir mektup zarfı ve balon şekillerinde gereksiz iki
ikon var. Bunlar silmek üzerine tıklayarak mümkün olmadı. Şuraya göre
apt-get kullanmak lazım:

sudo apt-get remove indicator-me indicator-messages

Matplotlib

`show()` komutu en son kurulumda problem cikardi. Cizim yaptirilan arka
plandaki motorun (backend) secilmesi gerekiyor. WX iyi bir secenek,
kurmak icin

```
sudo apt-get install wxgtk2.8
```

Matplotlib'in bundan haberdar olmasi icin `sudo gedit /usr/local/lib/python2.7/dist-packages/matplotlib/mpl-data/matplotlibrc`


Dosyada

```
backend : WXAgg
```

yazilmasi yeterli. Bu satir buyuk bir ihtimalle sadece `Agg` iceriyor
olacakti.

Animasyonlar

Masaustunde pencere acma, kapama hareketlerinin animasyon yapmasini
durdurmak istiyorsaksudo apt-get install
compizconfig-settings-managerSonra menuden System | Settings | Compbiz
Settings Manager secilir ve "Effects" bolumunde Effects icin secim
kutusu iptal edilir.Torsudo apt-get install tor tor-geoipdbsudo
apt-get install privoxyile kurulur. Privoxy ayari icin `sudo gedit
/etc/privoxy/config` ile dosyayi edit edin, ve icerik
olarak

```
forward-socks4a / localhost:9050 .
```

Firefox 4 uzerinde TorButton icin

https://www.torproject.org/torbutton/Install Alpha baglantisindan xpi

dosyası işletilebilir. YouTube video'larını ve flash içerik görebilmek
için eklenti kurulduktan sonra Firefox sol üstte Tor statüsünü
gösteren bir işaret çıkacak. Bu işaret üzerinde mouse sağ düğme click
yaparak "preferences" seçeneğine girin. Security Preferences tab'inden
"dışable plugins during Tor uşage" seçeneğini de-aktıve edin.Şimdi
kırmızı xarpi işareti üzerine tıklayın, bu işaret yeşile dönüşecek. Bu
kadar. Artık istediğiniz siteye bağlanabilirsiniz.Alt-TabMasaüstü
üzerinde klavye kısayolu kullanarak programlar arasında geçiş yapmak,
Ubuntu 11 olağan ayarlarında ayvayı yemiş.

Genelde en son kullanılan ilk geri gelir kuralı işlemiyor. Düzeltmek
için Sytem | Preferences | CompizConfig Settings Manager üzerinde
Window Management seçilir ve Static Application Switcher yerine
Application Manager seçilir.Ses EfektleriEger konsol içine mesela
gidilemeyecek durumda bile backspace yapılınca çıkan bip, tan, tun
seslerini kapatmak istiyorsak System Tools | System Settings, oradan
Sound ve Sound Effects. Bir ses kontrol ayarı var, onun yaninda Müte
seçilirse artık uyarıcı sesleri çıkmaz.ŞifreEğer kullanıcınızın
şifresini kısa yapmak isterseniz, passwd üzerinden gireceğiniz şifre
Ubuntu tarafından beğenilmeyebilir (kısa, çetrefil olmayan şifreyi
güvensiz buluyor). Ama illa ki kısa / basit bir şifre vermek
isterseniz, o zaman "sudo passwd [kullanıcı isminiz]" ile istediğiniz
şifreyi verebilirsiniz. Kendiniz süper kullanıcı olmanıza rağmen sudo
üzerinden tekrar kendi kullanıcınızı referans etmeniz garip olabilir,
ama kısa şifre bu şekilde memnun oluyor.


