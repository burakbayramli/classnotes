# Matlab Yerine Octave

Not: 2010 itibariyle tavsiyemiz matematiksel bilimsel hesaplar,
grafikleme isleri icin Python dili uzerinden ve Numpy, Scipy
paketlerinin kullanilmasidir.

Matlab programı, matematik formüllerini otomatik olarak çözmek için
şahane bir program. Artık çoğu üniversitede matematik dersleri için
öğrencilere Matlab veriliyor. İstatistik, olasılık, sembolik formül
çözümü, grafikleme, doğrusal cebir gibi işlemleriniz için Matlab'i
kullanabilirsiniz. Matlab'in en güçlü tarafı doğrusal cebir
hesaplamalarıdır. Bunda tabii ki Matlab'in tarihi önem kazanıyor,
programın ilk kullanıldığı alan doğrusal cebir problemleri idi.

Octave'ın yaratıcıları, programı Matlab ile birebir uyumlu yapmak için
yola çıkmışlar. Hakikaten de, çok yaklaşmışlar. Matlab script
programları, .m dosyaları üzerinde durur. Bu dosyalardan çoğunu
işlettiğimizde Octave ile aynen işlediğini gördük.Grafik için, Octave
gnuplot adlı programa dısardan çağrı yapıyor, fakat bu da hiç problem
değil, çünkü gnuplot programı da bedava.  

Octave'i Ubuntu uzerinde kurmak cok kolay: 

```
sudo apt-get install octave3.2 octave-optim octave-image
```

En son iki paket imaj isleyebilmek ve optimizasyon yapabilmek
icin. kaynak kodlarından derlenerek kurulabilir. 

Eger Ubuntu 16 uzerinde 4.2.1 kurmak istersek,

```
sudo add-apt-repository ppa:octave/stable
sudo apt update
sudo apt install octave
```

ODE Pkg kurmak icin

```
[fname, success] = urlwrite ("https://bitbucket.org/odepkg/odepkg/get/default.tar.gz", [P_tmpdir "/odepkg.tar.gz"]);
pkg ("install", fname)
```

Matlab ve Python Arasında Dosya Okumak

Eğer Matlab / Octave'den bir matris dosyaya yazıp onu Python'dan
okutmak istiyorsak, Octave'da

```
save ("A.mat", "A", "-v7")
```

diyebiliriz. Python tarafında `scipy.io` ile bu matris okunabilir,

```
f = 'A.mat'
import scipy.io as sio
mat = sio.loadmat(f)
A = mat['A']
print (A.shape)
```

Eger Python'dan yazip Octave'dan okumak istiyorsak, mesela `A` matrisi,

```
sio.savemat("A.mat", {"A": A})
```

Octave tarafinda

```
load A.mat
```

