# Ubuntu ve Masaustu Goruntu, Video Kaydi


Ubuntu ve Masaustu Goruntu, Video Kaydi




Ekranda olanlari, ozellikle belli bir pencerede olanlari, video olarak kaydetmek icin bir yontem

ffmpeg -video_size 1024x768 -framerate 25 -f x11grab -i :0.0+0,0 -f pulse -ac 2 -i default output.mkv

Bir baskasi vokoscreen, apt-get ile kurulur.

Bir digeri (bu eski)  xwininfo programini baslatin. Bu program ok isaretini bir arti haline donusturecek, ve bu arti isareti hangi diger pencere uzerine getirilirse o pencerenin bilgileri dokulecek. Bizim ihtiyacimiz olan "Window id" satirinin ne soyledigi. Burada mesela 0x4200003 gibi bir sayi var. Bu sayiyi alin ve

recordmydesktop --windowid=0x4200003


olarak kayit islemini baslatin. Istediginiz kadar kaydedince Ctrl-C ile cikin, kaydedilen her sey out.ogv adli bir video dosyasina yazilacak. Eger recordmydesktop kurulu degilse, su sekilde kurulabilir.


sudo apt-get install recordmydesktop zenity


Eger ogv dosyasini animasyonlu gif formatina cevirmek istiyorsak, su komut yeterli


ffmpeg -i out.ogv -loop_output 0 -pix_fmt rgb24 -r 5 -s 250x250 output.gif


-loop_output gif'in ne kadar tekrar edilecegini kontrol eder, 0 degeri sonsuza kadar demektir. -r secenegi bir saniye icinde kac kare (frame) gosterilecegi.


Tek Goruntu


Eger masaustundeki goruntuyu tek imaj olarak (screenshot) almak istiyorsak, secenekler sunlar. PRTSC, yani print screen dugmesi kullanilabilir. Bu ise yaramazsa, ImageMagick kurulur (apt-get install imagemagick), ve komut satirinda su girilir:


import [dosya.png]


Bu komutun hemen arkasindan ekrandaki isaret (cursor) arti isaretine donusecek. Bu isaret ile hangi pencereye tiklanirsa onun goruntusu dosya.png icine yazilir.


Ya da Applications | Accessories | Take Screenshot programi baslatilir.






