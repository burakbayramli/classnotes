# Kod: Kısıtlı Boltzman Makinaları (Restriced Boltzman Machines -RBM-)




Kod: Kısıtlı Boltzman Makinaları (Restriced Boltzman Machines -RBM-) 




Simdiye kadar buldugumuz en temiz RBM Python kodu surada 

git://github.com/echen/restricted-boltzmann-machines.git

Sadece tek bir main programi var, python rbm.py deyince hemen o isliyor. Kod tek bir sinif ve dosya icinde. 

Bir baska Python  RBM ornek kodu suradan indirilebilir.

git://github.com/lmjohns3/py-rbm.git

Bunu kurmak biraz daha cetrefilli, setup.py ile kurulur. Isletmeden once

sudo pip install glumpy 

sudo apt-get install python-opengl

gerekli. Ayrica MNIST imaj veri tabani lazim

http://yann.lecun.com/exdb/mnist/

Buradan train-images-* diye baslayan ilk iki dosyayi indirip acin,

Simdi py-rbm dizini altinda mnist.py adli kodu bulun ve bu dizinde

python mnist.py -i [DIR]/train-images.idx3-ubyte -l [DIR]/train-labels.idx1-ubyte

isletin. Ekrana bir GUI cikacak bu GUI uzerinde yapay ogrenim algoritmasinin imajlardan "baz goruntuler" cikartirken bunlarin sag kenarda basildigini goreceksiniz.











![](Screenshotfrom2013-01-01172650.png)
