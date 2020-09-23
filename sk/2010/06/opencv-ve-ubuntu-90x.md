# OpenCV ve Ubuntu 9.x


OpenCV ve Ubuntu 9.x



Ubuntu 9.04'ten 9.10'a gecis yaptik; Her iki ortamda da OpenCV 2.0 daha sonra 2.1 bazi problemler cikardi. Ilk once standart kurulumu takip ettikten sonra cv.so dosyasi gorulmuyor, bu sebeple import patliyordu. cv.so dosyasini dogru yere koyduk. Soyle:sudo mv Downloads/OpenCV-2.1.0/lib/cv.so /usr/local/lib/python2.6/dist-packages/cv.soBundan sonra OPENCV/samples/python altindaki programlar problem verdi. Hata:libdc1394 error: Failed to initialize libdc1394Bunu tamir etmek icin surayi takip ettik, problem duzeldi.Simdi, OpenCv highgui paketini kullanan orneklere gelelim. OpenCv'yi kullanmanin bir suru degisik yontemi var, habire yapilan degisiklikler aslinda isleri bozuyor. Simdi cv paketine gecildi, orneklerde de buraya dogru bir gidis var. Eger swig uzerinden highgui kullanimina devam etmek istiyorsaniz, "sudo apt-get install python-opencv" ile gerekli kodlari alabilirsiniz, fakat bu halde bile artik tum ornekler isletmiyor, lkdemo.py bunlardan biri. Bizim tavsiyemiz tum kullanimin mumkun oldugu kadar cv paketi uzerinden yapilmasi.




