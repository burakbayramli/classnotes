# Ubuntu 12.04 LTS ve Acer Aspire S3

Acer Aspire S3 laptop uzerinde Ubuntu 12.04 LTS kullanmaya basladik.

Bu laptop 250 GB boyutunda solid state disk'e sahip. Solid state
teknolojisi ucucu bellek (RAM) teknolojisini sabit diskler icin
kullanan bir teknolojidir, eskiden pahaliydi, son zamanlarda dizustu
bilgisayarlarda gorulmeye basladi. Bazi yorumlara gore solid state
disk erisimi normal disklere oranla 10 kat daha hizlidir. SS manyetik
disk olmadigi icin oynayan bir okuyucu kafaya ihtiyaci yok, hata payi
daha az. Manyetik diskler olmadigi icin daha hafif. Mekanik islem
olmadigi icin ayrica daha az isiniyor. Her bakimdan tercih
edilebilecek bir teknoloji.

Solid state disk'e sahip bir dizustu bilgisayarlarin etiketinde
satildiklari dukkanlarda "SSD" ibaresi kullanilir genelde. Acer
Aspire'in parakende fiyati 1000 EUR'luk alet Internet erisim paketi
eklenince daha ucuza 500 EUR'a dusuyor (Berlin Saturn fiyati).

Ubuntu kurulusuna donelim: Kurmak icin USB flash disk / stick
kullandik, artik cogu laptop CD okuyucu dahil etmiyor. Aslinda boylesi
daha iyi, CD okuyucu kurulus haricinde baska bir ise yaramiyordu
zaten, fakat USB disk pek cok baska is icin de kullanilabilir.

Bilgisayari alinca Windows ile gelecek, tabii onu bir USB boot
edilebilir Ubuntu diski yaratmak haricinde baska bir sey icin
kullanmayacagiz, hemen silinecek [2] (baska bir Ubuntu uzerinden
USB'ye iso yakmak icin suraya bakilabilir). Once gerekli iso dosyasini
indirin (Aspire'in 64 bit kullanan bir makina oldugunu unutmayalim, 32
bit versiyonu degil 64'u indirin). Simdi hem USB'den direk
kullanilabilir bir Ubuntu 12 yaratacak hem de istenildigi zaman
Ubuntu'nun sabit diskinize yazilmasini saglayacak. Not: USB'den Ubuntu
yuklemek icin BIOS'unuzda araclarin "yuklenis sirasini" degistirmeyi
unutmayin. Cogunlukla ilk sirada sabit disk olur, siz bunu
degistirerek USB'yi en tepeye gonderin. Windows ile USB yaratmak icin

http://www.pendrivelinux.com/universal-usb-installer-easy-as-1-2-3/

Ozellikler

Acer Aspire uzerinde Ubuntu 12, touchpad uzerinde tek dokunus (tap)
sol fare dugmesi, iki parmak ile dokunus sag fare dugmesi tiklamasi
anlamina geliyor.

Programlar

Ubuntu hala Unity denen absurt bir GUI yapisinda israr ediyor. O
yuzden sisteme girer girmez

apt-get install gnome-shell

Logout edip tekrar girmeden once kullanici isminin girildigi kutunun
ustundeki tekerlek resmine tiklanir, ve Gnome Classic (No Effects)
secilir. Bu bizi klasik Gnome masaustune goturur ve rahat nefes
alinir.

Eger ust sag kosedeki mail ikonunu yoketmek istiyorsak 

sudo apt-get remove indicator-messages

Eger gnome panelden ikon silmek istiyorsak, Alt Super + sag mouse
tiklamasi yapilir ve "Remove from Panel" secilir. Ya da
`$HOME/.config/gnome-panel` altinda ikona tekabul eden dosya silinir.

Emacs uzerinde Alt-Space cok kullaniyoruz, fakat bu kombinasyon Ubuntu
tarafindan "kapilmis". Bu kombinasyonu Ubuntu seviyesinde iptal etmek
icin System Tools | System Settings | Keyboard ve Shortcuts | Windows
bolumunde "Activate the window menu" icin Alt+Space secilmis, bu
satira gidip cift tiklama yapin ve Silme (backspace) dugmesine basin.

Dil seciminde bazi hatalar var. Her ne kadar ne zaman sorulsa English
US secsek te, birden bire en ust sol kosedeki Applications diyen yer
ve Firefox ikonu (her nedense bunlar) bir Uzakdogu diline geciyor. Bu
hatayi duzeltmek icin /etc/default/locale sudo gedit ile acilir ve

LANG="en_US.UTF-8"LANGUAGE="en_US:en"LC_NUMERIC="en_US.UTF-8"LC_TIME="en_US.UTF-8"LC_MONETARY="en_US.UTF-8"LC_PAPER="en_US.UTF-8"LC_IDENTIFICATION="en_US.UTF-8"LC_NAME="en_US.UTF-8"LC_ADDRESS="en_US.UTF-8"LC_TELEPHONE="en_US.UTF-8"LC_MEASUREMENT="en_US.UTF-8"

yazilir. Tekrar sisteme girildiginde problem duzelmis olacaktir.

Eger alt-tab ile pencereler arasi gecis yapilamadigini farkederseniz,  

sudo apt-get install compiz compizconfig-settings-manager

Sonra Applications | System Settings | Preferences | Combiz Settings Manager

oradan Window Management ve Application Switcher secilir.

Pek cok kez Ubuntu kurduktan sonra bazi paketlerin surekli kurdugumuzu farkettik, gelistirme, analitik, veri isleme icin gerekli bu paketler sunlar (sudo apt-get ile)

```
build-essential emacs python-dev python-setuptools  python-numpy python-scipy python-matplotlib ipython-notebook texlive emacs-goodies-el preview-latex dvipng openjdk-7-jdk  libavformat-dev ffmpeg postgresql-contrib octave octave-image recoll libatlas-base-dev liblapack-dev gfortran python-sympy r-base-dev r-base python-rpy2 imagemagick gimp python-qt4 pdftk pdfjam gsl-bin xkbset python-pip nautilus-open-terminal nautilus-open-terminal python-markdown python-jinja2 wvdial mplayer vlc xkbset git cmake chromium-browser calibre

texlive-latex-extra [very large]

ubuntu-restricted-extras 

sudo pip install pyzmq tornado 
```

Skype icin

sudo apt-get install libasound2 libqt4-dbus libqt4-network libqt4-xml libqtcore4 libqtgui4 libqtwebkit4 libstdc++6 libxss1 libxv1 libssl1.0.0 

Ses Efektleri

Eger konsol icine mesela gidilemeyecek durumda bile backspace
yapilinca cikan bip, tan, tun seslerini kapatmak istiyorsak System
Tools | System Settings, oradan Sound ve Sound Effects. Bir ses
kontrol ayari var, onun yaninda Mute secilirse artik uyarici sesleri
cikmaz.

Ubuntu baslayinca calan davul sesinden kurtulmak istiyorsaniz

sudo rm /usr/share/sounds/ubuntu/stereo/system-ready.ogg

Clickpad

Acer Aspire S3 bir clickpad ile geliyor. Clickpad hem dokunus (tap)
hem de asagi yukari inebilen mekanik dugme gibi islev gorebilen bir
mekanizma. Bu clickpad'i kullanmanin farkli yollari var, paketten
cikan hali ile hem dugme olarak hem de tek, iki dokunuslar mouse
tiklamasi yapabiliyor. Klavyeyi kullanirken yanlislikla dokunusun
tiklama yapmasi rahatsizlik yaratiyorsa (mouse isareti baska bir
pencereye kaymis mesela, ve yazi yazarken yanlislikla tiklama odagi
istemeden o pencereye kaydiriyor), ve "klavye kullanirken clickpad'in
iptal edilmesi" secenegi ise yaramiyorsa, o zaman tum clickpad'i bir
mouse gibi isler hale getirebilirsiniz. Once System Tools | System
Settings | Mouse and Touchpad yapin ve "Enable mouseclicks with
touchpad" secenegini iptal edin. Simdi, alttaki cagrilari her
bilgisayar acildiginda cagrilacak bir yere yazin, mesela rc.local ya
da .bashrc gibi

synclient ClickPad=1
synclient RightButtonAreaLeft=1500
synclient RightButtonAreaRight=2846
synclient RightButtonAreaTop=101
synclient RightButtonAreaBottom=1771

Ustteki komutlar clickpad'i bolgelere ayirarak o bolgelerde olan
dokunuslari sag mouse tiklamasi gibi algilanmasini sagliyor. Ustteki
bolge sag tiklama icin tanimli. Ayrica dokusun tiklama olmasini iptal
ettik, boylece clickpad mouse gibi isler hale geldi.

Balon tavsiyelerini yoketmek icin

sudo mv /usr/share/dbus-1/services/org.freedesktop.Notifications.service
 
/usr/share/dbus-1/services/org.freedesktop.Notifications.service.disabled 

Klavye Tusunu Mouse Tiklamasi Haline Getirmek

OpenCV

Google arama sonuclarindaki URL'lere Google kendi eklerini yapiyor, bu sebeple kopyala, yapistir ile bu sonucu baska bir yere tasidiginizda karmakarisik bir URL ortaya cikiyor. Google eklerini yoketmek icin Firefox uzerinde Google / Yandex search link fix adli eklentiyi kurun. Bu eklenti arama sonuclarinizdaki URL'leri temizleyecektir.

Almanya'da USB Modem ile T-Mobile Internet Baglantisi

GPU

Eger GPU ile ilgili bir hata ortaya cikiyorsa,

sudo gksu gedit /etc/default/apport

ile ustteki dosya edit edilir, ve "enabled" diyen yer '1' degerinden
'0' degerine getirilir ve dosya kaydedilir.

Ust sag kosede olan Bluetooth ikonu Bluetooth iptal edilmis olsa bile
orada gozukmeye devam ediyor, ve kalabalik yaratiyor. Bu ikonu tamamen
yoketmek icin

gksu nautilus /etc/xdg/autostart 

ile dizini aciniz, ve bu dizindeki bluetooth-applet.desktop ve bluetooth-applet-unity.desktop dosyalarini siliniz. Makina tekrar baslatilinca ikon silinmis olacaktir.

Chrome Uzerinde Flash

Flash, mesela youtube gibi sitelerde, islemiyor ise, suradaki tavsiye takip edilir,

http://askubuntu.com/questions/449103/chromium-34-and-later-cannot-detect-flash-plugin

Komut satirindan

sudo apt-add-repository ppa:skunk/pepper-flash
sudo apt-get update
sudo apt-get install pepflashplugin-installer

Simdi

sudo gedit /etc/chromium-browser/default

ile dosyayi acin ve sonuna

. /usr/lib/pepflashplugin-installer/pepflashplayer.sh

ekleyin. Chrome'u tekrar baslatin (birkac kez f5 ile refresh yapmak gerekebilir), flash isleyecektir.

Ubuntu 13

sudo apt-get install gnome-session-fallback 

Caps control yapmak icin

setxkbmap -option ctrl:nocaps

Bu yaziya eklemeler olacak.

Kaynaklar

[1] http://askubuntu.com/questions/153016/apport-gpu-error-intel-py-crash 

[2]  Eger Windows'u silmiyorsaniz, o zaman bolum (partition) araci size bir ekranda diski tekrar bolmeniz ve yeni Unix disk bolume yaratmaniz icin yardim eder. Fakat bir problem bu ekran hangi tarafin Win hangi tarafin Unix oldugunu acikca soylemiyor. Soldaki kisim Windows. 
