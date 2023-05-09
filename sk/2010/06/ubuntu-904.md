# Ubuntu 9.x


Ubuntu 9.04 versiyonu hakkinda bazi notlar:Daha once bahsettigimiz
Caps tusunu Ctrl yapma cozumu yerine, su kullanim yeterli: System |
Preferences | Keyboard'dan Layouts tab, oradan USA klavyesi (bizim
kullandigimiz) ve o listeden de Ctrl key position seceneginde "make
caps lock an additional ctrl".Wifi adaptoru icin bizim Toshiba
Satellite icin ve 9.04 kernel header (kafa!) dosyalari ile uyumlu olan
compat-wireless paketi 2.6.33. Ubuntu 9.10 uzerinde compat-wireless
paketine gerek yok.Python versiyonu degismis, 2.5 yerine 2.6; o
sebeple Sci/Numpy, ve diger bilimum paketleri tekrar kurmak
lazim. Dert degil.Eger 8 -> 9.04 -> 9.10 gecisini hep guncellemeler
uzerinden yaptiysaniz, makina kendini kaybedebilir. Aslinda en iyisi
onemli dosyalari yedekleyip 9.10 versionunu diskten sifirdan
kurmak.Ubuntu 9.x versiyonlarinda pil uzerinde calisiyorsaniz, sistem
"Battery Discharging" gibi bir mesaji rahatsiz edici bir popup,
dialog, vs. icinde surekli ekrana basiyor. Bundan kurtulmak icin
gconf-editor komut satirindan baslatilacak. Sonra gconf-editor > apps
> gnome-power-manager > notify'a gidip discharging kutusundaki isareti
iptal etmek gerekiyor. Sonra "threshold" altindaki percentage_critical
ve percentage low degerlerini '0' yapin, "general" altindaki
"use_time_for_policy" ve "use_profile_time" kutularini iptal
edin.Ubuntu baslayinca calan davul sesini kapatmak icin Preferences ->
Startup Applications'a gidin ve Gnome Login Sound secenegini bulun. Bu
ogenin sol tarafindaki secenegi iptal edin. Eger bu ise yaramazsa,
davul sesinin ses dosyasi
/usr/share/sounds/ubuntu/stereo/dialog-question.ogg. Bu dosyayi silip,
sonra "sudo touch dialog-question.ogg" komutu ile bos bir dosya
yaratabilirsiniz, bu dosyada hicbir ses olmadigi icin login esnasinda
hic ses cikmamis olur.Daha detay cozdukce buraya ekleyecegiz.


Ubuntu'da Internet'e baglanma, guc seviyesi, vs. ile alakali mesajlar
masaustunde balon mesajlar olarak cikiyor, ve kullanici dikkatini
cekmeye ugrasarak bazen rahatsiz edici olabiliyorlar. Balon
mesajlarini kaldirmak icin su yeterli:sudo mv
/usr/share/dbus-1/services/org.freedesktop.Notifications.service
/usr/share/dbus-1/services/org.freedesktop.Notifications.service.disabledBilgisayari
kapatip actiktan sonra mesajlarin bir daha gelmedigini goreceksiniz.


