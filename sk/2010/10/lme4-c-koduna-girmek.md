# Lme4 C Koduna Girmek

Lme4 (ya da herhangi bir R kutuphanesinin) C kodunu degistirip, bu
yeni kodlari R'den islettirmek istersek, sunlari yapabiliriz:C kodunu
degistirecegimiz R kutuphanesini herhalde install.packages ile
kurmusuzdur, ona tekabul eden kaynak kodlari buluruz. Versiyon no'suna
bakalim; home yani ~ altinda R dizini altindaki pakete gidelim. Bizim
icin bu ~/R/i486-pc-linux-gnu-library/2.11/lme4. Bu dizin altinda
DESCRIPTION dosyasinda hangi versiyon oldugu yaziyordur.Bu versiyona
ait kaynak kodu Internet'ten bulup indiririz. Bu kaynakta istedigimiz
degisikligi yapariz. Derlemek icin make.sh icinde sunlari
yazdik:#!/bin/shgcc -std=gnu99 -fPIC -g -O2 -c lmer.c -o lmer.o
-I/home/burak/R/i486-pc-linux-gnu-library/2.11/Matrix/include/ -I./src
-I/usr/share/R/include/gcc -std=gnu99 -fPIC -g -O2 -c init.c -o init.o
-I/home/burak/R/i486-pc-linux-gnu-library/2.11/Matrix/include/ -I./src
-I/usr/share/R/include/gcc -std=gnu99 -fPIC -g -O2 -c local_stubs.c -o
local_stubs.o
-I/home/burak/R/i486-pc-linux-gnu-library/2.11/Matrix/include/ -I./src
-I/usr/share/R/include/gcc -std=gnu99 -shared -o lme4.so init.o lmer.o
local_stubs.o -llapack -lblas -lgfortran -lm -L/usr/lib/R/lib
-lRKutuphane lme4.so yaratilinca bu dosyayi R altindaki so uzerine
kopyalarsak, eski kod yerine bizimki islemeye baslar.cp lme4.so
~/R/i486-pc-linux-gnu-library/2.11/lme4/libs/Biz R kodlarini "R -f
test.R" seklinde isletiyoruz, yani her cagrida yeni bir process
baslatiliyor, degisen so kutuphanesi hemen bir sonraki cagrida isleme
konacaktir.





