# Git Tavsiyeleri

Kaynak Kod Idare Sistemi Git ile calisirken takip edilmesi tavsiye
edilen bir is akisi (workflow) surada. Tavsiyeye gore dis dunyaya acik
(public) bir dal (branch) olmali, bu dalin uzerindeki kod degisim
notlari (commit history) olabildigince lineer, temiz olmali. Programci
bir ozellik uzerinde calisiyorsa bunu kendine ozel, disa kapali
(private) bir dal uzerinde yapar, o dala bol bol, istedigi kadar
commit yapar, vs. Fakat -ve burasi onemli nokta- master dalina kod
birlestirme (merge) yapmadan once bu kapali dalin degisim tarihini
tekrar yazabilir. Gelistirme dallari bir nevi taslak olarak
gorulmelidir, ve "disa yayin yapilmadan once" yani birlestirme
operasyonundan once temizlenmelidirler. Pek cok kez yapilan birkac
commit birlestirilip tek hale getirilebilir mesela. Bu temizlik isi
icin kullanilabilecek bir rebase numarasi da gosterilmis.




