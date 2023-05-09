# Ubuntu ve Masaustu Goruntu, Video Kaydi

Ekranda olanları, özellikle belli bir pencerede olanları, video olarak
kaydetmek için bir yöntem

```
ffmpeg -video_size 1024x768 -framerate 25 -f x11grab -i :0.0+0,0 -f \
 pulse -ac 2 -i default output.mkv
```

Bir başkası `vokoscreen`, `apt-get` ile kurulur.

Bir diğeri (bu eski) `xwininfo` programını başlatın. Bu program ok
işaretini bir artı haline dönüştürecek, ve bu artı işareti hangi diğer
pencere üzerine getirilirse o pencerenin bilgileri dökülecek. Bizim
ihtiyacımız olan "Window id" satırının ne söylediği. Burada mesela
0x4200003 gibi bir sayı var. Bu sayıyı alın ve

```
recordmydesktop --windowid=0x4200003
```

olarak kayıt işlemini başlatın. İstediğiniz kadar kaydedince Ctrl-C
ile çıkın, kaydedilen her şey `out.ogv` adlı bir video dosyasına
yazılacak. Eğer recordmydesktop kurulu değilse, şu şekilde
kurulabilir.

```
sudo apt-get install recordmydesktop zenity
```

Eğer ogv dosyasını animasyonlu gif formatına çevirmek istiyorsak, şu
komut yeterli

```
ffmpeg -i out.ogv -loop_output 0 -pix_fmt rgb24 -r 5 -s 250x250 output.gif
```

`-loop_output` gif'in ne kadar tekrar edilecegini kontrol eder, 0
degeri sonsuza kadar demektir. `-r` secenegi bir saniye icinde kac kare
(frame) gosterilecegi.


Tek Görüntü (screenshot)

Eğer masaüstündeki görüntüyü tek imaj olarak (screenshot) almak
istiyorsak, seçenekler şunlar. PRTSC, yani print screen düğmesi
kullanılabilir. Bu ise yaramazsa, İmageMagick kurulur (apt-get ınstall
imagemagick), ve komut satırında şu girilir:

import [dosya.png]

Bu komutun hemen arkasından ekrandaki işaret (cursor) artı işaretine
dönüşecek. Bu işaret ile hangi pencereye tıklanırsa onun görüntüsü
dosya.png içine yazılır.

Ya da Applications | Accessories | Take Screenshot programi
baslatilir.

Python İçinden

Eğer bir Python programı içinde görüntü yaratıyorsak bu yaratılan
görüntüleri `pyglet` ile yakalayabiliriz. Kurmak için `pip
install`. Kod içinde

```
buffer = pyglet.image.get_buffer_manager().get_color_buffer()            
image_data = buffer.get_image_data()
image_data.save(filename='out.png')
```



