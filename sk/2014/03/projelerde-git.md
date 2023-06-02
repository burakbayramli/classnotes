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

### Git Pull

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

### Dosyayı Tamamen Silmek

Belki geliştirme dizini içinde geçici dosyalar üretiliyor, ya da şifre
dosyaları yanlışlıkla commit edilmiş, ya da aşırı büyük dosyalar
depoya bir şekilde girdi.. Sonra `git push` sırasında hatayı
farkettik, `git rm` ile dosyayı sildik. Fakat `push` hala çok zaman
alıyor. Problem şurada, Git tüm değişiklikleri teker teker yollamaya
uğraşıyor - dosya eklemesi, sonra onu silme. Şifre durumunda zaten
silsek bile bir şifre dosyasının tarihte kalması durumu var, birisi
`checkout` ile eski hale giderek şifreleri görebilir. Çözüm tamamen
silmek.

Metot 1

```
git filter-branch --force --index-filter 'git rm --cached \
   --ignore-unmatch [BUYUK DOSYA, * VS ISARETLERI KULLANILABILIR]' \
   --prune-empty --tag-name-filter cat -- --all
```

Ya da en basit kullanım

```
git filter-branch --tree-filter 'rm -rf [dizin]' HEAD
```

Bu islemler uzun zaman alabilir bu arada. Değişikliği göndermek için

```
git push origin --force --all 
```

Metot 2

Artik Git tarafindan tavsiye edilen yeni yaklasim `git-filter-repo`
kullanimi. Bu ayri bir yardimci kod, indirmek icin

https://github.com/newren/git-filter-repo

Yuksek Git versiyonuna ihtiyaci var, yoksa guncelleme yapalim,

```
sudo add-apt-repository ppa:git-core/ppa -y
sudo apt-get update
sudo apt-get install git -y
git --version
```

Şimdi silme işlemini yapacağımız depo en üst dizinine `cd` ile gidelim,
ve `git-filter-repo` [DİZİN] altında kurulduğunu varsayarak,

```
[DIZIN]/git-filter-repo/git-filter-repo--invert-paths --path dir1/dir2/dosya3.txt
```

ile `dir1/dir2/dosya3.txt` dosyasını tarihten silelim. Tabii önceden
`git-filter-repo` üzerinde `chmod u+x` yapmak gerekebilir.

Üstteki komut işlem yapmak için illa sıfırdan `git clone` edilmiş depo
istiyorum diye yakınabilir, eğer bu mümkünse yapın, değilse üstteki
komuta `--force` eklenebilir.

Komut işledikten sonra uzaktaki depoya ittirmek gerekli, herhangi bir
sebeple uzak (remote) deponun ne olduğu silinmiş ise, `git remote -v`
o listeye bakılır, eklemek için

```
git remote add origin git@github.com:[user]/[repo.git]
```

Artık

```
git push origin --force --all
```

ile kod gönderilebilir.

Dal Yapısı (Branch Structure)

Bizim kullandığımız bir yaklaşım şöyle: master yani ana dal her zaman
"işleyen kodu" temsil eder. Yani master'da ne var ise, sonuç
(production) makinasında o var demektir. Bu bağlantının sIkİ tutulması
iyi olur, yani master'a gönderilen her şey, anında işleme konmalıdır.

Geliştirme için master'dan dallanarak mesela bir dev (geliştirme) dalı
kullanabiliriz, bu daldan her programcı kendisi için ayrı bir dal da
yaratabilir, bu onlara kalmış. Fakat kodcular arası entegrasyon için
dev dalı kullanılır. Çakışmalar vs var ise bu dalda çözülür, master a
göndermeden önce entegrasyon testleri bu dal  üzerinden
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

### Daha Sofistike Dal Yapılar

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
olabilir. Bunu yapmak için kullanici `burakbayramli` icin ve
`classnotes` projesi icin,

```
cd [PROJE DIZINI]
.git dizinini sil
git init
git add .
git commit -m 'Initial commit'
git remote add origin git@github.com:burakbayramli/classnotes.git 
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

### Başka Kullanıcı İsmi ile Github'da Çalışmak

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
sorulmayacak. Dikkat url isminde `https` olmaması önemli, o yüzden
kullanıcı / şifre soruluyor zaten. SSH üzerinden şifresiz commit için
ayarlanan repo'larda bu url `git@github.com:...` diye gider.

### Belli Bir Tarihe Dönüş

Depo'nun eski tarihteki haline donmek istiyorsak,

```
git checkout "`git rev-list master  -n 1 --first-parent --before=2022-06-02`"
```

### Tüm Tarihte bir Dosyayı Aramak

Sadece en son versiyonda değil gelmiş geçmiş commit edilmiş belki silinmiş
ama hala tarihte olan dosyaları aramak istiyorsak,

```
git log --all --full-history -- "**/*myscript*.py"
```


