# Docker

Docker bir sanal makina teknolojisi. MacOs üzerinde Ubuntu, Ubuntu
üzerinde Windows, envai türden sanal makina işletebilmek için
kullanılabilir.

Docker ile paylaşmak istediğimiz herhangi bir programı artık ona
gereken tüm işletim sistemi destek programları ile beraber paketleyip
sunabiliriz. Bir servis için filanca python paketleri gerekiyor 
bunları python paket listesiyle sunabilirdik. Ya peki apt-get ile
kurulması gereken yan programlar, hatta yan dosyalar, diğer büyük
programlar da varsa? Docker tüm bunları paketleyebilir.

Bizim şirkette sürekli Docker sözü duyuluyor, "abi _vs_ programı
işletmem gerekiyor onun Docker imajı neredeydi?", ya da "Docker'i
aldım işlettim, hangi makina üzerinde işleteyim". vs. Yani program
kurmaktan bahsetme kalmadı, kurulmuş, hazır programların Docker imajı
olarak paylaşılması konuşuluyor.

Çoğunlukla bir sistem Dockerfile dosyası üzerinden kuruluyor, bu
Dockerfile için gereken imajlar (Ubuntu gibi) bilinen referans
noktalardan indiriliyor. Fakat kendimiz bir docker imajı kaydedip onu
paylaşabilirdik.

https://www.digitalocean.com/community/tutorials/how-to-install-and-use-docker-on-ubuntu-18-04

```
sudo apt install apt-transport-https ca-certificates curl software-properties-common

curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -

sudo add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu bionic stable"

sudo apt update

apt-cache policy docker-ce

sudo apt install docker-ce

docker --version
```

isliyor olmali (basa her zaman sudo koymak gerekebilir). Basit kontrol

```
docker run hello-world
```

Altta bir web servisi isletmenin yolu

```
docker run -d -p 80:80 --name webserver nginx
```

Tarayıcı ile localhost ziyaret edince orada bir servis işlediğini
göreceksiniz! Alttaki komut mevcut sanal makinaları / kapları
(container) gösterir,

```
docker container ls
```

Sanal makinayi durdurmak icin

```
docker container stop webserver
```

Silmek

```
docker container rm webserver
docker image rm nginx
```

Simdi Mac uzerinde Ubuntu kuralim. Docker sitesinde onceden
hazirlanmis bir Ubuntu kurulumu var,

```
docker run -it --name ubuntu ubuntu:xenial bash

docker exec -it ubuntu bash
```

Bu komut bizi işleyen bir Ubuntu içine taşır! Bu izole bir sanal
makina, orada yapılan hiçbir şeyin "dış" sisteme etkisi yok.

Sistem icinde

```
apt-get update
```

Sonra mesela C derleyicisi kurabiliriz,

Mesela apt-get install gcc

Python kuralim,

```
apt-get install python python-pip python3 python3-pip
```

Ubuntu versiyon

```
cat /etc/*release
```

16.04 dedi.

Kopyalama (Dışarıdan içeri)

MacOS'ten sanal makina Ubuntu'ya dosya kopyalamak icin, docker
container ls ile listelenen container id (kimlik) alinir, ve

```
docker cp [YEREL DOSYA] [CONTAINER KIMLIK]:/
```

Mevcut imajlari listelemek,

```
docker image ls
```

Silmek icin

```
docker image rm [IMAGE ID]
```

Eğer hata gelirse (container [ID] hala silinmemiş diye) docker
container rm ile container silinir, sonra image silinir.

Şimdi üstteki örnekte iskelet bir kap yarattık ve o sanal makinaya
girip orada komutlar işlettik. Acaba tek bir komutla üstteki her
programı kurduramaz mıyız? Dockerfile yaklaşımı ile evet.

Bir Dockerfile icinde alttakileri yazalim,

````
FROM ubuntu:16.04
RUN apt-get update
RUN apt-get install -y python python-pip
RUN apt-get install -y python3 python3-pip
RUN pip install virtualenv
RUN virtualenv -p /usr/bin/python2 pyenv2
RUN /bin/bash -c "source /pyenv2/bin/activate && pip install ipython pandas && deactivate"
```

apt-get komutuna -y vererek y/n sorusuna otomatik y cevap vermis
oluyoruz, bunu yapmazsak apt-get soru sorup cevap bekliyor, biz
otomatik kurulum yaptigimiz icin bu takilmayi istemiyoruz.

Simdi imaji yaratalim,

```
docker build -t ubuntu_image .
```

Bu kadar. Listeleyelim,

```
docker image ls
```

```
REPOSITORY          TAG                 IMAGE ID            CREATED             SIZE
ubuntu_image        latest              cf7c904f9a65        47 seconds ago      675MB
ubuntu              16.04               b9e15a5d1e1a        7 days ago          115MB
```

Simdi sanal makinaya girelim,

```
docker run -it --name ubuntu2 ubuntu_image bash
```

Kurduğumuz tüm programların hazır bizi beklediğini göreceğiz.

Zihin egzersizi: Docker ile izole bir makina kurulumu yaratmış
olduk. Ama üstteki örnekte bu sanal makinada bir de sanal Python
ortamı yarattık, aslinda tum Python paketlerini global Python icin
kurabilirdik. Sanal ortama gerek var mıydı? Gerek olmayabilir. Fakat
virtualenv ile çalışmaya alıştıysak, ve sanal makina bile olsa onun
üzerinde de farklı Python derleyicileri kullanma olasılığı olduğu
için, yine de virtualenv kullanmakta sakınca yok.

Not: Usttte MacOS üzerinde Ubuntu örneği gösterdik. Docker sanal
makina işleticisi bir mikroişlemciyi de mi (mesela Intel) sanal olarak
işletiyor? Buna gerek yok, altta kullandığımız makina bir MacBook Pro
3,1 GHz Intel Core i7, yani Intel işlemcisi var, yani mikroislemci
komutlarini cevirmeye gerek yok. Makina / ikisel kod olduğu gibi
MacOS'in mikroişlemcisinde işletebilir.

https://stackoverflow.com/questions/44480740/how-to-save-a-docker-container-state

https://docs.docker.com/develop/develop-images/dockerfile_best-practices/

https://stackoverflow.com/questions/23935141/how-to-copy-docker-images-from-one-host-to-another-without-via-repository

https://www.howtoforge.com/tutorial/how-to-create-docker-images-with-dockerfile/

https://stackoverflow.com/questions/48561981/activate-python-virtualenv-in-dockerfil



