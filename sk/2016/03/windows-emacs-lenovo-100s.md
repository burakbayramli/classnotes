# Windows, Emacs, Lenovo 100S

Yeni bir Lenovo 100S makinasi uzerinde Windows bazli kurulum, kullanim

Fonksiyon tuslarina, mesela F11 icin Fn+F11 ile erisilebiliyor,
fonksiyon tuslarina oldugu gibi basilirsa baska seyler yapiyorlar;
mesela ses acmak gibi. Eger baslangic sirasinda BIOS'a girilmek
istenirse Fn+F2 lazim. Ne yazik ki bu BIOS icinde boot siralamasi
secenegi yok; bu sebeple Ubuntu USB'den yuklemeyi zorlamayi set etmeyi
yapilamiyor. Emacs icin not: F tuslarina yapilan her atama yine
Fn+F[..] ile kullanilabilir.

Caps Tusu Kontrol

Standart numara - ctrl.reg dosyasi icinde

```
REGEDIT4

[HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\Keyboard Layout]
"Scancode Map"=hex:00,00,00,00,00,00,00,00,02,00,00,00,1d,00,3a,00,00,00,00,00
```

Sonra reg dosyasina iki tiklama yapilir, registry yuklemesi
olur. Bilgisayari kapatip, actiktan sonra Caps Ctrl olur.

Python

En iyisi Anaconda

https://www.continuum.io/downloads

Tum paket icinde Pandas bile var. Daha ufak bir kurulum mumkun, bunun
uzerine istenen ekleniyor.

Python 3

Genellikle Python 2 ana Python olarak kurulur, ardindan Python 3 bir
farkli gelistirme ortami olarak kurulur. Bunun icin

```
conda create --name=py3k python=3
```

Kurulus bitince ne zaman Python 3 isletmek istersek activate py3k
deriz, komut satiri ona gore degisir, ve artik python cagrisi Python 3
olur. Eger activate ile ugrasmak istemiyorsak where python ile Python
3 yorumlayicisinin nerede oldugunu ogrenebiliriz, genellikle
`[HOME]\Anaconda2\envs\py3k\python` altindadir, bu isler kodu direk
cagrirsak activate'e gerek yok. Not: Paket kurulumu farkli, onun icin
activate lazim; ayrica paket kurulumlarini activate sonrasi conda
install [paket] ile Python 3 icin ayri yapmak lazim, Python 2 icin
kurulan bir paket Python 3 tarafindan gorulemez. Ama paket kurulumlari
bitince ardindan kod isletimi icin activate gereksiz.

BitTorrent

https://sourceforge.net/projects/trqtw/files/latest/download

EPUB kitaplari okumak icin yeni program kurmak lazim; en iyi bilinen Calibre,

https://www.calibre-ebook.com/download_windows

LaTeX

Miktex icinde gerekli her sey var. Eksik sty paketlerini ihtiyac
oldukca (sorduktan sonra) Internet'ten otomatik indirme ozelligi cok
iyi.

http://miktex.org/download

Git

Komut satirindan Unix'te oldugu gibi Git kullanmak icin

https://github.com/git-for-windows/git/releases

Gerekli exe indirilir, kurulur (bir kere indirme islemi bana hata
verdi, ama ertesi gun duzeldi, bu durum olursa beklenebilir, Amazon
ile alakli bir durumdu galiba). Kurulum sirasinda olagan secenekler
birakilir. Bittikten sonra herhangi bir dizinde sag mouse tiklamasi
yapinca "burada Git baslat" secenegi gorulur, yani o dizinda Git komut
satiri acilabilir. Secin, komut satiri Unix kulanimi sagliyor, ve
burada git commit, vs cagrilari yapilabilir. Standart uyarilar
gelecek, email, isim vermek gibi,

```
 git config --global user.email "user@filan.com"
 git config --global user.name 'Isim'
```

ile bunlar halledilir. Eger Github'a baglanti gerekiyorsa, bir SSH
anahtari yaratmak lazim, 

ssh-keygen

Bu komut HOME/.ssh altinda id_rsa.pub yaratir, bu dosya icerigini
Github | Settings | Add SSH Keys uzerinden hesabimiza ekleriz, boylece
sifresiz commit yapabiliriz. Eger satir sonlari ile ilgili hatalari
gormek istemezsek, 

```
git config --global core.safecrlf false
```

PDF Okumak

PDF dosyalarini okumak icin evince var, MS dahil edilmis program Edge
yerine bu daha iyi ve hizli

https://wiki.gnome.org/Apps/Evince/Downloads

Tuslari Mouse Tiklamasi Yapmak

Autohotkey adli bir program indirelim

https://autohotkey.com/

Kurduktan sonra ahk ile biten dosyalar bir autohotkey script'i haline
gelir. Bir ornek

```
RAlt::LButton
LWin::LButton
AppsKey::RButton
```

Bu script sol Windows ve sag Alt tusuna sol mouse tiklamasi haline
getirir, Context Menu Key denen tusu (bir dikdortgen icinde alt alta
cizgilerin oldugu bir tus, hemen sag Control tusunun solunda) ise sag
mouse tiklamasi yapar. Bilgisayar basladiginda bu script'i isletmek
icin alta bakalim.

Baslangic Script'leri

Bir script baslangicta isletilsin istiyorsak, onu Startup dizini
altina koymak yeterli. Bu dizin Windows 10
icin

```
c:/Users/[kullanici]/AppData/Roaming/Microsoft/Windows/Start Menu/Programs/Startup
```

gibi bir yerde. Eger ustteki script baslangicta cagrilsin istiyorsak,
kodu mesela remap.ahk diye isimlendiriz, ve ustteki dizin icine
koyariz. Bu kadar.

Eger Excel dosyalarini goruntulemekte problem cikiyorsa, Microsoft
Office bazen islemiyor, o zaman Libre Office kurulabilir,

https://www.libreoffice.org/download/libreoffice-fresh/

PDFTK

https://www.pdflabs.com/tools/pdftk-the-pdf-toolkit/

ffplay

Komut satirindan muzik calmak icin ffplay lazim olabilir,

http://ffmpeg.zeranoe.com/builds/

Buradan statik Windows (32, ya da 64 bit) dosyalari
indirirsiniz. Kurulusa gerek yok, direk .exe cagrisi isliyor.

Imaj Dosyasi

PNG, JPEG gibi imaj dosyalarini hizli bir sekilde gosterebilmek icin
Faststone Image Viewer,

http://download.cnet.com/FastStone-Image-Viewer/3000-2192_4-10324485.html

Sesler

Windows bip, dan, dun gibi uyari seslerini kapatmak icin Start
uzerinde sag tiklama, Control Panel | Sounds | Sound her sesi None
yapariz.

Emacs

Indirmek

https://ftp.gnu.org/gnu/emacs/windows/

Oradan emacs-24.5-bin-i686-mingw32.zip dosyasi.

.emacs

Baslangic kodlari icin .emacs'in nereye gideceginden emin degilsek, o
zaman C-x f ile dosya acariz ve ~ dizinini seceriz; bu dizin her zaman
HOME'a gidecek sekilde ayarlidir; bu dizin altina .emacs dosyasini
yazariz. Dosya icinde

(load-file "C:/filan/falan/emacs-win.el")

gibi tek bir satir olabilir, bu satir esas ayar dosyasini yukler. Bir ornek surada.

Emacs icinde beep seslerini kapatmak

(setq ring-bell-function 'ignore)

Font listesi icin herhangi bir buffer icinde

(insert (prin1-to-string (x-list-fonts "*")))

yazin, ustteki kodu secip M-x eval-region ile kodu isletin. Font listesi gosterilecektir.

Ghostscript

http://www.ghostscript.com/download/gsdnld.html

Bu adresten Ghostscript 9.19 for Windows (32 bit) indirilir.

Indeksleme Kapatmak

Windows'un yerel dosyalari indeksleyen ve onlarin sonradan aranmasini
saglayan bir servisi var; bu servisi kullanmak istemiyorsak (diske
erisimi yavaslatabilir, belki aramayi baska yollarla yapiyoruz),
servisi kapatmak iyi olur. Start uzerinde sag tiklama sonrasi  Control
Panel | Administrative Toolsk | Services secelir, burada Windows
Search bulunur, bu servis "Disabled" konumuna getirilir. Artik
indeksleme olmayacak.

wget

Sitelerin icerigini gezerek yerel diske indirebilen bir arac,
wget. Windows icin isler kodlar,

http://gnuwin32.sourceforge.net/packages/wget.htm

Ekran Aydinligi

Win10 kafasina gore ekran aydinligini azaltiyor; bu "adaptive
brightness" denen bir ozellikmis - bunu iptal etmek icin Start
uzerinde sag tiklama, Power Options | Change advanced power settings |
Display | Enable adaptive brightness altindaki hersey off, ayrica
Dimmed display brightness altindaki her sayi 100% yapilmali.






