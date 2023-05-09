# Vagrant

Linux ya da MacOS uzerinde bir sanal OS isletmek icin Vagrant. 

vagrantup.com

https://www.virtualbox.org/wiki/Linux_Downloads

sudo dpkg -i ile once virtualbox sonra vagrant deb dosyalari
kurulur. OSX icin dmg dosyalari ile kurulus yapilir. Ek olarak, eger X
uygulamalari isletmek gerekirse (yani sanal OS uzerinde X uygulamasi
isletip sonucu esas OS icinde gormek) OSX ortaminda XQuartz kurmak
lazim,

http://xquartz.macosforge.org/landing

Simdi

```
vagrant init ubuntu/trusty64

vagrant up
```

Bu islem, ilk seferde, uzun zaman alir, tum Ubuntu 14.04'u temsil eden
kurulus dosyasi Internet'ten indiriliyor olacak. Diger OS cesitleri
Vagrant sitesinden bulunabilir.

Bittikten sonra ustteki komutlar ardindan dizin icinde bir Vagrantfile
olur. X icin Vagrantfile icine (end ibaresi oncesi)

```
config.ssh.forward_x11=true
```

eklenmeli.

Simdi

```
vagrant ssh
```

ile sanal "makinamiza" baglanabiliriz (sonraki isletimlerde yine
vagrant ssh oncesi vagrant up yapmak lazim fakat ikinci sefer up
cagrisi hemen geri gelir). Giris yaptiktan sonra bu makina icinde
apt-get vs gibi her turlu Ubuntu komutunun mevcut oldugu
gorulebilir. Sanal makinanin Internet baglantisi "disarida" olan
makinanin Internet baglantisidir.

X uygulamalari icin, mesela xclock icin,

```
ssh -X -p 2222 vagrant@localhost xclock
```

Sifre (ilk basta) vagrant. Not: Eger xclock uygulamasi yok ise sanal
Ubuntu uzerinde apt-get install x11-utils isletilir.

Dis (host) makinanin dosyalarina erismek icin sanal makina icinde
/vagrant/ dizinini kullanabilirsiniz. Bu dizin bizi icinde Vagrantfile
olan dizine goturecektir. 





