# PDF Formlarin Uzerine Yazi Yazmak

Alttaki Python programı, biraz da İmagemagick yardımı ile bir PDF
bazlı formu otomatik olarak doldurma servisini sağlıyor. PDF
doküman.pdf dosyası işleniyor ve bu dosyanin 7. sayfasına bazı
bilgiler eklenerek doldurulmuş halde out7.jpg adlı bir dosya
yaratılıyor.

Bu teknik basit bir ayar dosyası ve biraz OCR yardımıyla herhangi bir
PDF formunu doldurabilecek bir programa evrilebilir. Altta hem text
hem de biraz imza jpg (sığnature.jpg) imajını belli noktalara
koyuyoruz.İlk önce İmageMagick convert çağrısı ile PDF dosyasını alıp
eğer birden fazla sayfa varsa, o kadar jpg dosyasına çeviriyoruz. Not:
convert işlemi biraz zaman alır, ve sadece bir kez yapılması yeter
zaten, script'in diğer kısımlarını ardı ardına test ederken convert
satırı sonraki işletimlerde iptal edilebilir.İmza imajını koyarken,
paste kullanıyoruz.

Bir imajı diğerinin içine yapıştırırken paste çağrısına bir "kutu
(box)" parametresi verilir, bu dört ögeli bir Python tüpü (tüple),
yani (100,100,200,200) gibi bir değer. Örnekte verilen değerler sol
üst köşesi 100,100'de sağ alt köşesi (200,200)'de olan bir kutuyu
tarif ediyor.Sonra, diyelim ki değiştirilen dosya out7.jpg sadece, ama
diğer sayfalar hala olduğu gibi, yine İmageMagic convert kullanarak
tüm bu dosyaları değisen imaj ile beraber toparlayıp tekrar bir PDF
haline getirebiliriz.convert Doküman-1.jpg Doküman-2.jpg ... out7.jpg
Doküman-8.jpg Yeni_Doküman.pdfğibi.

Tek sayfa uzerinde degisim yapan script asagida.

```
import os, sys
import PILfrom PIL import ImageFont
from PIL import Image
from PIL import ImageDraw

os.system("convert -density 200x200 -quality 60 ~/Desktop/Dokuman.pdf Dokuman.jpg")
img = Image.open("Dokuman-7.jpg")
sign = Image.open("signature.jpg")
x = 900y = 1450
sizex = 350
sizey = 122
img.paste(sign, (x,y,x+sizex,y+sizey))
draw = ImageDraw.Draw(img)
font = ImageFont.truetype("/usr/share/fonts/truetype/msttcorefonts/Times_New_Roman.ttf",27)
draw.text((350,1500), "Isim Soyad, Vesaire", font=font,fill='black')
img.save("out7.jpg")
```




