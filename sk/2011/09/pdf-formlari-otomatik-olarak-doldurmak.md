# PDF Formlarin Uzerine Yazi Yazmak


PDF Formlarin Uzerine Yazi Yazmak



Alttaki Python programi, biraz da Imagemagick yardimi ile bir PDF bazli formu otomatik olarak doldurma servisini sagliyor. PDF dokuman.pdf dosyasi isleniyor ve bu dosyanin 7. sayfasina bazi bilgiler eklenerek doldurulmus halde out7.jpg adli bir dosya yaratiliyor. Bu teknik basit bir ayar dosyasi ve biraz OCR yardimiyla herhangi bir PDF formunu doldurabilecek bir programa evrilebilir. Altta hem text hem de biraz imza jpg (signature.jpg) imajini belli noktalara koyuyoruz.Ilk once ImageMagick convert cagrisi ile PDF dosyasini alip eger birden fazla sayfa varsa, o kadar jpg dosyasina ceviriyoruz. Not: convert islemi biraz zaman alir, ve sadece bir kez yapilmasi yeter zaten, script'in diger kisimlarini ardi ardina test ederken convert satiri sonraki isletimlerde  iptal edilebilir.Imza imajini koyarken, paste kullaniyoruz. Bir imaji digerinin icine yapistirirken paste cagrisina bir "kutu (box)" parametresi verilir, bu dort ogeli bir Python tupu (tuple), yani (100,100,200,200) gibi bir deger. Ornekte verilen degerler sol ust kosesi 100,100'de sag alt kosesi (200,200)'de olan bir kutuyu tarif ediyor.Sonra, diyelim ki degistirilen dosya out7.jpg sadece, ama diger sayfalar hala oldugu gibi, yine ImageMagic convert kullanarak tum bu dosyalari degisen imaj ile beraber toparlayip tekrar bir PDF haline getirebiliriz.convert Dokuman-1.jpg Dokuman-2.jpg ... out7.jpg Dokuman-8.jpg Yeni_Dokuman.pdfgibi. Tek sayfa uzerinde degisim yapan script asagida.import os, sysimport PILfrom PIL import ImageFontfrom PIL import Imagefrom PIL import ImageDrawos.system("convert -density 200x200 -quality 60 ~/Desktop/Dokuman.pdf Dokuman.jpg")img = Image.open("Dokuman-7.jpg")sign = Image.open("signature.jpg")x = 900y = 1450sizex  = 350sizey  = 122img.paste(sign, (x,y,x+sizex,y+sizey))draw = ImageDraw.Draw(img)font = ImageFont.truetype("/usr/share/fonts/truetype/msttcorefonts/Times_New_Roman.ttf",27)draw.text((350, 1500), "Isim Soyad, Vesaire", font=font, fill='black')img.save("out7.jpg")




