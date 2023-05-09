# PDF Icinde Animasyon, Ubuntu Uzerinde Acrobat Reader - acroread

Animasyonlu PDF ornegi

http://mlss.tuebingen.mpg.de/Hennig_2013_Animating_Samples_from_Gaussian_Distributions.pdf 

Her nedense acroread Ubuntu 64 ortaminda yok, 32 bitlik olani 64 bit
uzerinde kurmak lazim.

```
wget http://ardownload.adobe.com/pub/adobe/reader/unix/9.x/9.5.1/enu/AdbeRdr9.5.1-1_i386linux_enu.deb

sudo dpkg -i --force-architecture AdbeRdr9.5.1-1_i386linux_enu.deb 

sudo apt-get install -f 

sudo apt-get install libxml2:i386 lib32stdc++6
```

Simdi acroread calisacaktir.






