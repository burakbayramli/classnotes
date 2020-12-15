# Jetson Nano

GPU geliştirmesi için Jetson Nano ürünü var, Türkiye'de 100 dolar
civarı fiyatla satılıyor. Altta görülen Nano 2GB Wifi ürünü. Güç
kaynağı bu pakete dahil değil, 5 Volt, 3 Amperlik bir adaptor almak
lazım. Raspberry Pi uyumlu bu tür adaptörler var.

Paketten iki tane ek kablo çıkacak, bunlardan birinin ucunda bir ufak
"dongle" var, üzerinde CE yazıyor, bu Wifi özelliği sağlıyor (dongle
kabloya bağlı geliyor, o çıkartıldıktan sonra kabloya gerek yok),
alttaki resimde devrenin sol altında. Diğer kablonun bir ucu
dizüstüne, diğeri Nano'ya takılacak, nihai resim

<img width="240" src="nano.jpg"/>

İşletim sistemi mikro SD kart üzerinden, en az 64 GB iyi olur,
işlemler aynen [Raspberry Pi](../07/raspberrypi.md) durumunda olduğu
gibi. Orada olduğu gibi yine işletim sistemi indirilecek, ve SD'ye
"yakılacak".

https://developer.nvidia.com/jetson-nano-2gb-sd-card-image

Şöyle bir komutla indirebilir,

```
wget --continue https://developer.download.nvidia.com/assets/embedded/downloads/jetson-nano-2gb-jp441-sd-card-image/jetson-nano-2gb-jp441-sd-card-image.zip
```

Büyük bir dosya dikkat. İndirildikten sonra yakmak için Etcher adlı
bir program kullanılır,

Etcher

https://www.balena.io/etcher/

https://phoenixnap.com/kb/etcher-ubuntu

Zip indirilir, açılır, içinde bir işler program var, tıklanır, görsel
programla yakma yapılır (Nano imajı seçilir, hedef seçilir, yak
denilir).

Yakma işlemi bitince kart Nano'ya sokulur, bağlantılar yapılır, ve
sistem başlayınca HDMI üzerinden monitöre bağlanılır, ayrıca fare,
klavye Nano'ya bağlanılıp oradan kuruluma devam edilir. Burada isim,
kullanıcı ismi, şifre, coğrafi yer vs sorulacak, bu bilgiler
girilir. Wifi bağlantısı orada görülür, Wifi şifresi girilip bağlantı
kurulur.

Aradaki o mavi kablo üzerinden seri bağlantıya dikkat, dizüstündeki
Ubuntu üzerinde

```
$ dmesg | grep --color 'tty'
[106921.687362] cdc_acm 1-2:1.2: ttyACM0: USB ACM device
```

deyince üsttekini görebilmek lazım.  Ya da `ls /dev/tty*` ile bakarsak
orada bir `/dev/ttyACM0` olmalı. Bu seri bağlantının sürekli olmalı
gerekli, yoksa Wifi üzerinden bile `ssh` çalışmaz.

Şimdi 


```
sudo screen /dev/ttyACM0 115200
```

seri bağlantı üzerinden ile sisteme girebiliriz, dediğimiz gibi bu
bağlantı hep lazım. Oradan `ifconfig -a` ile sistemin IP adresini
alırız, ben tüm ek işlemleri `ssh kullanıcı@IP` ile Nano'ya `ssh`
üzerinden girip ayrı bir ekranda yapıyorum.

Yeni sistemimizi kontrol edelim. `ssh` ile girip,

```
sudo nvpmodel -q

NVPM WARN: fan mode is not set!
NV Power Mode: MAXN
0
```

görülebilir, bu GPU durumunu raporladı. CUDA ekleri olan C++
derleyicisi için `nvcc` erişimi lazım,


```
vim ~/.bashrc
```

Ve dosya sonuna

```
export PATH=${PATH}:/usr/local/cuda/bin
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/usr/local/cuda/lib64
```

ekleriz, kaydederiz, ve komut satırına dönüp,

```
source ~/.bashrc
```

işletiriz, şimdi 

```
nvcc --version
```

```
nvcc: NVIDIA (R) Cuda compiler driver
Copyright (c) 2005-2019 NVIDIA Corporation
Built on Wed_Oct_23_21:14:42_PDT_2019
Cuda compilation tools, release 10.2, V10.2.89
```

göstermeli.

Not

Kurulum işlemini tamamen seri bağlantı üzerinden text bazlı da yapmak
mümkün, alttaki video'da anlatılıyor, macera isteyenler bunu
seçebilir, o zaman monitör vs bağlantısına gerek kalmaz.

PyCuda

Alttaki komutları yine Nano komut satırında işletiyoruz,


```
sudo apt install python3-pip

pip3 install cython

pip3 install pycuda
```

Acaba dizüstünde geliştirip Nano'ya işletmek için sürekli gönderme
yapabilir miyiz? Evet. [Şuradaki](../../2005/10/bir-makinaya-ssh-ile-sifresiz-giris.md)
`ssh` şifresiz giriş numarasını yaptıktan sonra,
[PyCuda](gpu-cuda-pycuda.md) yazısındaki herhangi bir örneği alırız,
mesela GPU tipini gösteren örnek, onu `tst.py` diye dizüstünde
kaydedelim, sonra mesela tüm kodların Nano üzerinde
`/home/user/Documents` dizinine gönderilecek şekilde ayarlarsak,

```
scp $1  user@192.168.43.34:/home/user/Documents/
ssh user@192.168.vs.vs "/usr/bin/python3 /home/user/Documents/$1"
```

ve script'i `run.sh` olarak kaydetsek, `sh run.sh tst.py` ile cağrınca
bu script `tst.py` dosyasını Nano üzerinde `/home/user/Documents/`
dizinine gönderir ve üzerinde uzaktan `ssh` ile `python3`
işletir. İşletince bizde

```
Device 0: NVIDIA Tegra X1
         Compute Capability: 5.3
         Total Memory: 1979 megabytes
```

sonucu geldi. PyCuda işliyor demektir, üstelik kodu rahat
çalışabildiğimiz kendi makinamiz üzerinden işlettik. Bir kere arada
`ssh` bağlantısı kurulunca daha ilginç şeyler de yapılabilir, `ssh`
üzerinden görsel X tünellemesi mesela, Nano üzerinde görsel program
işletip sonucu kendi makinamızda görebiliriz, ya da Jupyter servisi
işletip dizüstü tarayıcısı ile ona bağlanabiliriz, vs. Ben kendi
açımdan Nano üzerindeki yükü az tutmaya uğraşıyorum.

Kaynaklar

[1] https://imadelhanafi.com/posts/jetson_nano_setup/

[2] https://developer.nvidia.com/embedded/learn/get-started-jetson-nano-2gb-devkit

[3] https://developer.nvidia.com/embedded/learn/get-started-jetson-nano-devkit#setup

[4] https://developer.nvidia.com/embedded/learn/get-started-jetson-nano-2gb-devkit#write

[5] https://youtu.be/Ch1NKfER0oM



