# Git ve Dosyalar


Git ve Dosyalar



Git sistemi icinde dosyalar nasil idare ediliyor? Git'in onemli avantajlarindan bir tanesi dosyalari kontrol edisinde; oncelikle her commit edisinizde o commit noktasi tekil bir hash kimligi ediniyor; bu oldukca uzun bir kimlik ve git log komutundan alinabiliyor. Sonra herhangi bir anda git checkout HASH ile o commit noktasina gidebiliyorsunuz (eger son eklerinize geri donmek istiyorsaniz onlari da commit etmeyi unutmayin, ki sonra git checkout master ile geri donebilesiniz).Bir commit noktasina gidince ne oluyor?Git, idare ettigi dizin altindaki dosyalari tamamen degistirerek o commit anindaki hallerine donusturuyor. Bu, mesela, ClearCase sisteminden farkli. O sistemde, aslinda ici normalde bos olan bir dizinde "set view" gibi bir komutu isletince "network uzerinden" dizinin ici birdenbire doluyordu, daha dogrusu network uzerinden baska bir makinadaki bir icerige "isaret etmeye" basliyordunuz. Git gunluk isleriniz icin network'ten bagimsiz olmayi amacladigi icin bu yolu takip etmiyor. Checkout komutu sonrasi gercek, fiziki dosyalarinizi degistiriyor.Nokta git (.git) dizini icinde bunu yapmasi icin gerekli tum bilgiye sahip. O dizin bir nevi sIkIstirilmis bir database gorevini gormekte. Isterseniz butun dosyalarinizi silin; nokta git dizini icindeki veriler ile iceriginizi sifirdan tekrar yaratabilirsiniz.Git bu baglamda CVS, Subversion sistemlerinden de farklidir; CVS'te bir branch yaratip onun icerigine bakmak ayri bir dizin gerektirir. Git, branch'ler arasinda gidip gelmeyi hep ayni dizin icinde halleder. Dosya iceriginizi pat diye degistirir. Peki commit noktalari, branch arasinda gidip gelmek performansi etkileyen bir faktor olmaz mi?Cogunlukla olmaz; cunku commit noktalari arasinda degisik olan genellikle birkac dosyadir, ve Git sadece bu dosyalari degistirmekle ugrastigi icin fazla performans bedeli odemez. Aslinda bir branch icin koca bir yeni dizin yapisi yaratmak daha pahalidir. Clearcase orneginde ise network uzerinden zaten bir bedel odenmekteydi, ve her an network'e bagli olma zorunlulugu ortaya ciktigi icin isin aksama olasiligi vardi.




