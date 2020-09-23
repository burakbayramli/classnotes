# Projelerde Git

Projelerde kullanilan bazi Git numaralari ve tavsiyeleri:

Eğer Wifi üzerinden git push yapmaya çalışıp `Connection refused` hatası
alırsanız, tamir için `.ssh/config` dosyasına girip

```
Host github.com
  Hostname ssh.github.com
  Port 443
```

yazmak yeterli.

N commit öncesi (mesela 10) ile en son commit arasında hangi dosyalar
değişti:

```
git diff --name-only HEAD~10
```

Git Pull

Artık Github çoğu projede kullanılan bir servis, onu baz alan bazı
tavsiyeleri paylaşmak iyi olur. Tipik bir tür kullanım şöyle olabilir,
kullanıcı

```
git add *
git commit -m 'vs'
git push origin master 
```

ile son kodu göndermektedir, ve gerektiğinde son kodları almak için
`git pull` yapmaktadır. Eğer çakışma var ise, bu durumda Git birleştirme
(merge) yapmaya uğraşır ve çakışmaları programcının çözmesini
ister. Çözme sonrası `git push` işler, fakat `git log`, yani nelerin
yapıldığını gösteren kayıt pek hoş gözükmez.

Alternatif yöntem, ortak depodan en son kodları alırken `git pull`
yerine `git fetch` kullanmaktır. Bu kullanım en son dalları (branch)
alır, fakat yerel dosyalar üzerinde değişim yapmaz. Sonra `git rebase origin/master`
yaparız, bu komut bizim kodumuzu biraz önce alınan son
kod üzerine "istifler" / geri çalar (replay). Git bunu öyle yapar ki
sanki bizim kodumuz mevcut olanın üzerine yazılmıştır, sanki o kodu o
kod biter bitmez hemen almışız, ve onun üzerinde bizimkini yazmışız
gibi... Bu durumda da çakışmalar görülebilir muhakkak, onlar varsa
çözülür; Sonra `git push` daha önce yapıldığı gibi yapılır.

Mevcut Olmayan Aşırı Büyük Dosya

Belki geliştirme dizini içinde geçici pkl dosyaları üretiliyor, ve
bunlardan bazılarını yanlışlıkla git add ve commit ile kendi deponuza
koydunuz. Sonra git push sırasında hatayı farkettiniz, `git rm` ile
dosyayı sildiniz. Fakat push hala "bu dosya çok büyük" diye
yakınıyor. Problem Git tüm değişiklikleri teker teker yollamaya
uğraşıyor - dosya eklemesi, sonra onu silmesi. Tabii eklerken yine
yakınıyor. Çözüm, şurada.

```
git filter-branch --force --index-filter 'git rm --cached \
   --ignore-unmatch [BUYUK DOSYA, * VS ISARETLERI KULLANILABILIR]' \
   --prune-empty --tag-name-filter cat -- --all
```

Ya da en basit kullanim

```
git filter-branch --tree-filter 'rm -rf [dizin]' HEAD
```

Bu islemler uzun zaman alabilir bu arada. Değişikliği göndermek için

```
git push origin --force --all 
```

Dal Yapısı (Branch Structure)

Bizim kullandığımız bir yaklaşım şöyle: master yani ana dal her zaman
"işleyen kodu" temsil eder. Yani master'da ne var ise, sonuç
(production) makinasında o var demektir. Bu bağlantının sIkİ tutulması
iyi olur, yani master'a gönderilen her şey, anında işleme konmalıdır.

Geliştirme için master'dan dallanarak mesela bir dev (geliştirme) dalı
kullanabiliriz, bu daldan her programcı kendisi için ayrı bir dal da
yaratabilir, bu onlara kalmış. Fakat kodcular arası entegrasyon için
dev dalı kullanılır. Çakışmalar vs var ise bu dalda çözülür, master a
göndermeden önce entegrasyon testleri bu dal  üzerinden
işletilir. Eğer sonuç iyi bulunursa buradan master'a birleştirme
(merge) yapılır. Önemli nokta şudur: artık kimse master dalı üzerinde
geliştirme yapmaz. Ana master dalı artık commit edilen bir şey değil,
merge edilen bir şeydir yani.

Gelistirme dali icin onu yaratiriz, ve kullanmaya baslariz, 

```
git checkout dev
```

ile gecis yapariz. Kodlama vs bitince commit edilir. Sonra git push
ile Github'a gonderilir. Birlestirme icin

```
git checkout master
git merge dev
git push origin master
```

yapilir.

Daha Sofistike Dal Yapilar

http://nvie.com/posts/a-successful-git-branching-model/

Üstteki yazıda tarif edilen şudur: geliştirme (dev) ve sürüm (master)
iki, sürekli orada olan dal vardır. Geliştiriciler belli özellikleri
geliştirmek için dev'den dal ayırırlar. Master her zaman en son, işler
kodu temsil etmektedir, sonuç ortamında işleyen budur. Sürüm yapmak
için dev'den isminde versiyon no'su taşıyan yepyeni bir dal yaratılır,
ve bu dalda artık yeni özellik eklenmez, sadece ufak tefek entegrasyon
pürüzleri çözülür, hatalar onarılır.

Github Projesini Yerel Dosyalardan Sıfırlamayıp Yaratmak

Bazen GH'daki projemizi ismen aynı tutsak bile içeriğini yerel
diskimizden en son dosyalardan yaratıp, Git tarihini sıfırlamak
isteyebiliriz. Sebepler, belki Git kayıtlarının olduğu .git içindeki
dosyalar bozulmuştur (corrupted), ya da Git tarihine aşırı büyük
dosyalar commit edilmiştir, her ne kadar onların git rm ile silmişsek
bile, hala tarihte duruyor olurlar, bu da .git dosyalarını
şişirir. Onları tarihten silebiliriz tabii, ama bazılarını kaçırmiş
olabiliriz. Her halükarda sıfırdan başlamak için pek çok sebep
olabilir. Bunu yapmak için

```
cd [PROJE DIZINI]
.git dizinini sil
git init
git add .
git commit -m 'Initial commit'
git remote add origin  - burada  git@github.com:kullanici/proje.git .. diye giden projenize ait olan url
git push --force --set-upstream origin master
```

Dikkat: üstteki adımları takip edince eğer projenizdein GH üzerinde
dallanma (branch) yapmış olan insanlarla bağlantı tamamen
kesilir. Çünkü bu noktada tarihi sildiniz, ve neredeyse yeni baştan
bir proje yaratmış oldunüz.

Üstteki adımların iyi bir tarafı projenizin GH seviyesindeki bilgileri
hala aynı kalır; alınan yıldızlar, takipçiler, vs. değişmez.

![](Screen-shot-2009-12-24-at-11.32.03.png)
![](https://1.bp.blogspot.com/-xg2XAf271oo/U4YPzRKa14I/AAAAAAAABZ0/Zm2o_8Vokhc/s1600/Screen-shot-2009-12-24-at-11.32.03.png)

Başka Kullanıcı İsmi ile Github'da Çalışmak

Bazen çoğunlukla kullandığımızdan farklı bir kullanıcı üzerinden iş
yapmamız gerekebilir. Belki ve ve iş kullanıcıları farklıdır
mesela. Bu durumda, eğer ikinci kullanıcı için şifre girmek rahatsız
etmiyorsa, en basit yol `git clone` komutunu `https` üzerinden
yapmak. Alınan repo'ya bakın, `.git` içinde bir `config` dosyası olacak. O dosya

```
[core]
	repositoryformatversion = 0
	filemode = true
	bare = false
	logallrefupdates = true
[remote "origin"]
	url = https://github.com/[ikincikullanici]/[repo].git
	fetch = +refs/heads/*:refs/remotes/origin/*
[branch "master"]
	remote = origin
	merge = refs/heads/master
```

gibi durmalı. Artık `git commit` yapınca ve `git push origin master`
ile kodu yollamak istediğinizde ikinci kullanıcı ve şifre
sorulacak. Girince ikinci repoya kod gönderilecek. Dikkat ürl isminde
`https` olması önemli, o yüzden kullanıcı / şifre soruluyor zaten. SSH
üzerinden şifresiz commit için ayarlanan repo'larda bu ürl
`git@github.com:...` diye gider.









