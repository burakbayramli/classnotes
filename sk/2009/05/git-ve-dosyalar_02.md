# Git ve Dosyalar

Git sistemi içinde dosyalar nasıl idare ediliyor? Git'in önemli
avantajlarından bir tanesi dosyaları kontrol edişinde; öncelikle her
commit edişinizde o commit noktası tekil bir hash kimliği ediniyor; bu
oldukca uzun bir kimlik ve git log komutundan alınabiliyor. Sonra
herhangi bir anda git checkout HASH ile o commit noktasına
gidebiliyorsunuz (eğer son eklerinize geri dönmek istiyorsanız onları
da commit etmeyi unutmayın, ki sonra git checkout master ile geri
dönebilesiniz).Bir commit noktasına gidince ne oluyor?Git, idare
ettiği dizin altındaki dosyaları tamamen değiştirerek o commit
anındaki hallerine dönüştürüyor. Bu, mesela, ClearÇase sisteminden
farklı. O sistemde, aslında içi normalde boş olan bir dizinde "set
view" gibi bir komutu işletince "network üzerinden" dizinin içi
birdenbire doluyordu, daha doğrusu network üzerinden başka bir
makinadaki bir içeriğe "ısaret etmeye" başlıyordunuz. Git günlük
işleriniz için network'ten bağımsız olmayı amaçladığı için bu yolu
takip etmiyor.

Checkout komutu sonrası gerçek, fiziki dosyalarınızı
değiştiriyor.Nokta git (.git) dizini içinde bunu yapması için gerekli
tüm bilgiye sahip. O dizin bir nevi sIkİştirilmiş bir database
görevini görmekte. İsterseniz bütün dosyalarınızı silin; nokta git
dizini içindeki veriler ile içeriğinizi sıfırdan tekrar
yaratabilirsiniz.Git bu bağlamda CVS, Sübversion sistemlerinden de
farklıdır; CVS'te bir branch yaratıp onun içeriğine bakmak ayrı bir
dizin gerektirir. Git, branch'ler arasında gidip gelmeyi hep aynı
dizin içinde halleder. Dosya içeriğinizi pat diye değiştirir. Peki
commit noktaları, branch arasında gidip gelmek performansı etkileyen
bir faktör olmaz mı?Çoğunlukla olmaz; çünkü commit noktaları arasında
değişik olan genellikle birkaç dosyadır, ve Git sadece bu dosyaları
değiştirmekle uğraştığı için fazla performans bedeli ödemez. Aslında
bir branch için koca bir yeni dizin yapısı yaratmak daha
pahalıdır. Clearçase örneğinde ise network üzerinden zaten bir bedel
ödenmekteydi, ve her an network'e bağlı olma zorunluluğu ortaya
çıktığı için işin akşama olasılığı vardı.





