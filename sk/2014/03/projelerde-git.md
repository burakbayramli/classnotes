# Projelerde Git




Projelerde Git




Projelerde kullanilan bazi Git numaralari ve tavsiyeleri:

Eger Wifi uzerinden git push yapmaya calisip Connection refused hatasi alirsaniz, tamir icin .ssh/config dosyasina girip

Host github.com
  Hostname ssh.github.com
  Port 443

yazmak yeterli.

N commit oncesi (mesela 10) ile en son commit arasinda hangi dosyalar degisti:

git diff --name-only HEAD~10

Git Pull

Artik Github cogu projede kullanilan bir servis, onu baz alan bazi tavsiyeleri paylasmak iyi olur. Tipik bir tur kullanim soyle olabilir, kullanici

git add *
git commit -m 'vs'
 git push origin master 

ile son kodu gondermektedir, ve gerektiginde son kodlari almak icin git pull yapmaktadir. Eger cakisma var ise, bu durumda Git birlestirme (merge) yapmaya ugrasir ve cakismalari programcinin cozmesini ister. Cozme sonrasi git push isler, fakat git log, yani nelerin yapildigini gosteren kayit pek hos gozukmez.

Alternatif yontem, ortak depodan en son kodlari alirken git pull yerine git fetch kullanmaktir. Bu kullanim en son dallari (branch) alir, fakat yerel dosyalar uzerinde degisim yapmaz. Sonra git rebase origin/master yapariz, bu komut bizim kodumuzu biraz once alinan son kod uzerine "istifler" / geri calar (replay). Git bunu oyle yapar ki sanki bizim kodumuz mevcut olanin uzerine yazilmistir, sanki o kodu o kod biter bitmez hemen almisiz, ve onun uzerinde bizimkini yazmisiz gibi... Bu durumda da  cakismalar gorulebilir muhakkak, onlar varsa cozulur; Sonra git push daha once yapildigi gibi yapilir. 

Mevcut Olmayan Asiri Buyuk Dosya

Belki gelistirme dizini icinde gecici pkl dosyalari uretiliyor, ve bunlardan bazilarini yanlislikla git add ve commit ile kendi deponuza koydunuz. Sonra git push sirasinda hatayi farkettiniz, git rm ile dosyayi sildiniz. Fakat push hala "bu dosya cok buyuk" diye yakiniyor. Problem Git tum degisiklikleri teker teker yollamaya ugrasiyor - dosya eklemesi, sonra onu silmesi. Tabii eklerken yine yakiniyor. Cozum, surada.

git filter-branch --force --index-filter 'git rm --cached --ignore-unmatch [BUYUK DOSYA, * VS ISARETLERI KULLANILABILIR]' --prune-empty --tag-name-filter cat -- --all

git push origin --force --all 

Dal Yapisi (Branch Structure)

Bizim kullandigimiz bir yaklasim soyle: master yani ana dal her zaman "isleyen kodu" temsil eder. Yani master'da ne var ise, sonuc (production) makinasinda o var demektir. Bu baglantinin sIkI tutulmasi iyi olur, yani master'a gonderilen her sey, aninda isleme konmalidir. 

Gelistirme icin master'dan dallanarak mesela bir dev (gelistirme) dali kullanabiliriz, bu daldan her programci kendisi icin ayri bir dal da yaratabilir, bu onlara kalmis. Fakat kodcular arasi entegrasyon icin dev dali kullanilir. Cakismalar vs var ise bu dalda cozulur, master a gondermeden once entegrasyon testleri bu dal  uzerinden isletilir. Eger sonuc iyi bulunursa buradan master'a birlestirme (merge) yapilir. Onemli nokta sudur: artik kimse master dali uzerinde gelistirme yapmaz. Ana master dali artik commit edilen bir sey degil, merge edilen bir seydir yani.

Gelistirme dali icin onu yaratiriz, ve kullanmaya baslariz, 

git checkout dev

ile gecis yapariz. Kodlama vs bitince commit edilir. Sonra git push ile Github'a gonderilir. Birlestirme icin

git checkout master
git merge dev
git push origin master

yapilir.

Daha Sofistike Dal Yapilar

http://nvie.com/posts/a-successful-git-branching-model/

Ustteki yazida tarif edilen sudur: gelistirme (dev) ve surum (master) iki, surekli orada olan dal vardir. Gelistiriciler belli ozellikleri gelistirmek icin dev'den dal ayirirlar. Master her zaman en son, isler kodu temsil etmektedir, sonuc ortaminda isleyen budur. Surum yapmak icin dev'den isminde versiyon no'su tasiyan yepyeni bir dal yaratilir, ve bu dalda artik yeni ozellik eklenmez, sadece ufak tefek entegrasyon puruzleri cozulur, hatalar onarilir.








Github Projesini Yerel Dosyalardan Sifirlamayip Yaratmak

Bazen GH'daki projemizi ismen ayni tutsak bile icerigini yerel diskimizden en son dosyalardan yaratip, Git tarihini sifirlamak isteyebiliriz. Sebepler, belki Git kayitlarinin oldugu .git icindeki dosyalar bozulmustur (corrupted), ya da Git tarihine asiri buyuk dosyalar commit edilmistir, her ne kadar onlarin git rm ile silmissek bile, hala tarihte duruyor olurlar, bu da .git dosyalarini sisirir. Onlari tarihten silebiliriz tabii, ama bazilarini kacirmis olabiliriz. Her halukarda sifirdan baslamak icin pek cok sebep olabilir. Bunu yapmak icin

cd [PROJE DIZINI]
.git dizinini sil
git init
git add .
git commit -m 'Initial commit'
git remote add origin  - burada  git@github.com:kullanici/proje.git .. diye giden projenize ait olan url
git push --force --set-upstream origin master

Dikkat: ustteki adimlari takip edince eger projenizdein GH uzerinde dallanma (branch) yapmis olan insanlarla baglanti tamamen kesilir. Cunku bu noktada tarihi sildiniz, ve neredeyse yeni bastan bir proje yaratmis oldunuz.

Ustteki adimlarin iyi bir tarafi projenizin GH seviyesindeki bilgileri hala ayni kalir; alinan yildizlar, takipciler, vs. degismez.

Bu yaziya ekler olacak. 





![](Screen-shot-2009-12-24-at-11.32.03.png)
