# Julia

Bilimsel hesaplama alaninda yeni bir dil: Julia. C, Fortran gibi
diller statik tipleme kullanir, kodlamasi zor ve hizlidir, Python, R,
Matlab gibi diller kullanmasi kolay, dinamik ama yavastir. Julia her
iki tarafin guclu taraflarini birlestirmeye ugrasiyor. Sahsen
Python'da gordugumuz bazi eksikleri cevaplandirmasi acisindan bizim
ilgimizi cekti. Julia LLVM bazli bir JIT kullanir, LLVM'i bir suredir
takip ediyoruz, saglam bir teknoloji (ve Java VM bazli cozumlere
alternatif saglamasi acisindan faydali oldu, Scala gibi).

Julia yazilan herhangi turden bir kodu "C performansina yakin" hizda
isletilmesini amacliyor. Python'da ayni performansi elde etmek icin
cogunlukla Numba ya da C cagrisi kullanmak lazim, bu cetrefil kodlama
demek, Julia bu isi paketten ciktigi haliyle halleder. Ayrica
paralelizasyon, hem ayni makinadaki, hem diger makinalardaki birden
fazla islemciyi (ssh uzerinden) basagrisi yaratmadan hesap surecinin
parcasi haline getirebiliyor. Python'da programcilar rahatsiz eden bir
konuyu adreslemisler yani, Python derleyicisinde GIL adli bir kilit
vardir ve bu sebeple gercek thread kullanimi mumkun olmaz,
multiprocessing gibi paketlerle arka planda surec yaratan cozumler
kullanilabilir, fakat bu cozumler dilin dogal bir uzantisi gibi
durmazlar. Julia bu isi temelden cozuyor.

Bir baska iyi haber: IPython not defterinin bir varyasyonu Julia icin 
kodlanmakta: IJulia.

Ubuntu'da kurmanin en kolay yolu

```
sudo add-apt-repository ppa:staticfloat/juliareleases
sudo add-apt-repository ppa:staticfloat/julia-deps
sudo apt-get updatesudo apt-get install julia
```





