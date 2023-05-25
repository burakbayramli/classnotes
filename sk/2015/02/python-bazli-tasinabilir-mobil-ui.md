# Python Bazlı Taşınabilir Mobil UI Kodları - Kivy

Kivy bazli yazilan Python UI kodlari hem Android hem iOS uzerinde
isleyebiliyor. Android kodlamasi icin Java kullanmak istemeyenler icin
iyi bir secenek olabilir. Kodun telefona gonderilmesi icin birkac
yontem var, birisi Kivy "isletici" bir programi kurmak, bu isletici
programin kendi apk'si var, ya Google Play'den ya da Kivy sitesinden
bu apk indirilebilir.

Bu isletici programi telefonda baslatinca program dizin sisteminde 
/storage/emulated/0/kivy adli bir dizine bakacak, ve altinda gordugu
tum alt dizinleri Kivy programlari olarak kabul edecek. Bir Kivy
programinin olusturulmasi cok kolay, iki dosya yeterli. Biri
android.txt digeri main.py. Text dosyasi

```
title=Hello World
author=Burak Bayramli
orientation=portrait
```

Merhaba Dunya uygulamasi 

```python
from kivy.app
import App
from kivy.uix.button import Button

class TestApp(App):
   def build(self):
      return Button(text='Hello World')

TestApp().run()
```

Tabii her "uygulama" kivy dizini altinda ayri bir dizin olabilir, her
dizinde ayri bir android.txt dosyasi... Telefondak Kivy Launcher ana
dizinde gordugu her alt dizini ayri bir uygulama olarak baslangic
listesinde gosterir.

Ayrica /storage/emulated/0/kivy dizini telefonda "Internal storage"
denen dizine tekabul ediyor. Not: burada kivy dizini yoksa, bunu elle
olusturmak lazim.

Dugme Tiklama

Bu ornekte dugme olsun, ona basilinca /sdcard/Download (bu /sdcard da ayni sekilde Internal Storage demek), altinda bir dosyaya bir seyler yazilsin.

```python
from kivy.app import App
from kivy.uix.button import Button

def callback(instance):
    print('The button <%s> is being pressed' % instance.text)
    fout = open("/sdcard/Download/kivy-out.txt","aw")
    fout.write("filan falan fisman\n")
    fout.close()


class TestApp(App):
    def build(self):
        btn1 = Button(text='Hello World')
        btn1 = Button(text='Hello world 1')
        btn1.bind(on_press=callback)
        return btn1
        
TestApp().run()
```

Dizustunde Kivy GUI Isletmek

Acaba Kivy uygulamasinin nasil isleyecegini gelistirme ortaminda,
telefonda isletmeden once gorsek iyi olmaz mi? Bunun icin Python 3
icin bir virtualenv sanal ortami kurup,

```
pip install kivy
pip install pygame
```

yeterli. Simdi python main.py ile Kivy uygulamaniz baslayacaktir!
(Not: tabii telefona yapilan referanslar ayni olmayabilir, mesela
dizustu ortaminda /sdcard dizini yok).

Kendi APK'mizi kendimiz Olusturmak

Python kodlarindan direk APK'ye gitmek te mumkun. Burada Kivy'nin alt
projeleri olan buildozer ve python-for-android gibi cozumler var. Bu
projelerden python-for-android ile, anladigimiz kadariyla (daha
kullanmadik) numpy, PIL, opencv gibi yerli kodlari APK icine dahil
etmek mumun. Tek problem python-for-android sadece Linux uzerinde
isliyor.  Fakat Kivy, VirtualBox uzerinden isletilebilecek ve icinde
gerekli tum araclar onceden kurulmus bir sanal makina imajini
sitesinden paylasiyor. Ya bu, ya da kendimiz Ubuntu kurarak
python-for-android'i kullanabiliriz. 

