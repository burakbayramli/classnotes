# Git Commit ve Unite Halinde Degisimler

Diyelim ki bir Git deposundaki kodlarinizda calisiyorsunuz, degisiklik
yaptiniz. Bu kodlari nasil depoya geri koymak gerekir?Once dizin
icinde git status yapin. 

```
# On branch master# Changes not staged for commit:
#
   (use "git add/rm ..." to update what will be committed)
#
   (use "git checkout -- ..." to discard changes in working directory)
##
    modified:
   README#
    modified:
   build.py
## Untracked files:#
   (use "git add ..." to include in what will be committed)
##
    mining/dosya.py
#
no changes added to commit (use "git add" and/or "git commit -a")
```

Takip edilmeyen dosyalar (untracked files) altinda. Bu dosyalar Git'e
verilmemis, o yuzden git commit ile iceri alinmiyorlar. Eger bazi
dosyalarin hakikaten Git disinda kalmasini ister, ve ustteki mesajla
bile bunun bildirilmesini istemezsek, .gitignore icinde bunu
yapabiliriz. Takip edilmeyen dosyalari git add ile ekleyebiliriz.

Degistirilmis (modified:) ile gosterilen dosyalar Git'in izledigi ve
uzerinde degisim yapilmis dosyalardir. Bunlarin icinden
istediklerimizi git add ile secerek commit icin
hazirlayabiliriz. Mesela README dosyasinda bunu yapmis olsak, tekrar
git status deyince

```
# Changes to be committed:#
   (use "git reset HEAD ..." to unstage)##
    modified:
   README
#
    ...
```

gozukecek.

Onemli bir nokta: git commit oncesi git add ile eklenen her dosya
belli bir "toplu degisim" icin ve bu degisimi temsil eder sekilde
yapiliyor olmali. Ornek: yeni bir ozellik ekledik, ve bu ozellik icin
A,B,C dosyalarina dokunduk (sonra baska sebeple D,E,F'ye dokunduk). Bu
ozellik icin commit oncesi sadece A,B,C'yi git add ile eklemeliyiz
(ekleme derken, ekleme icin 'isaretlemeliyiz' daha dogrusu). Niye?
Cunku git commit derli toplu bir grup degisimi iceri almali, ve daha
da onemlisi bu degisimi depoya git commit -m 'mesaj' ile verdigimiz
mesaj da sadece o tek (unite) degisimi tarif ediyor olmali. Aslinda
sadece ve sadece bu mesaj sebebiyle birbiriyle alakali degisimlerin
tek unite halinde yapilmasina bir nevi mecburuz denebilir (kod
okunabilirligi, takip edilebilirligi, bakilabilirligi acisindan).

Eger bir commit oncesi yapilan her degisikliklerin tamami tek bir
uniteye aitse, o zaman basit bir sekilde git add -u ile tum bu
degisiklikleri otomatik olarak ekleyebiliriz.

