# Ajax ve Geri Don Dugmesi


Ajax ve Geri Don Dugmesi



Pur Ajax bazli bir uygulamayi dusunelim: bu uygulama tamamen Javascript bazli olarak servis bilgilerine gore sayfada dinamik bazi degisiklikler yapacaktir. Bir sayfadan digerine gecis yine pur HTML bazli olur, a href=.. uzerinden gidilecek dosya ismi verilir, o dosya yeri geldiginde kendi Javascript cagrilarini yapar, vs.Bir problem tarayicida "geri git" dugmesi kullanildiginda cikabilir. Ornek olarak, form bilgilerinin oldugu bir sayfada, bu form bilgilerin alinip bir tiklama/aksiyon sonrasi ayni sayfada dinamik icerik uretildigini dusunelim, sonra a href=.. ile tiklanarak baska bir sayfaya ziplanildigini farzedelim. Bu noktada "geri" dugmesi tiklandiginda o sayfanin ilk 'ic aksiyonu' hatirlanmayacak, ve o sayfa ilk yuklendigi andaki icerigi ile gelecektir (buyuk bir ihtimalle bos olarak). Bu kullanicilar tarafindan 'geri' dugmesinin isleyinden beklenen bir sey degildir.Sayfanin en son halini 'hatirlamak' icin pek cok teknik, yardimci kutuphane var. Bu kavram literaturde 'derin baglantilamak (deep linking)' olarak geciyor, Facebook bu konuda ozellikle basarili. Yardimci kutuphanelerden jQuery kullananlar icin jQuery Adress diye bir cozum var.Ek kutuphane olmadan kendimizin kodlayabilecegi bir cozum olarak hash tag ('#' isareti) kullanimi mumkun. Bu tag genelde, statik html icinde belli bir bolgeye ziplamak icin kullanilir, Javascript ile beraber a href='#' seklinde bos kullanimi da gelistiricilere tanidik gelebilir.Ajax cagrisi yaptigimiz noktada cagri basariyla geri donerse hemen o noktada sayfayi dinamik olarak tekrar uretmeye yetecek kadar veriyi kodlayip window.location.hash degiskeninde depolayabiliriz. Buraya depoladigimiz her deger www.vs.com/sayfa.html#veri1veri2.. olarak URL icinde gozukecek (tabii parametreleri bir ayirac -delimeter- ile ayirmak iyi olabilir). Sonra, o  sayfaya 'geri donuldugunde' ve/yani sayfa ilk yuklendigi zaman ilk yaptigimiz is bu hash tag degerini bir kontrol etmek olur. Eger orada bir deger varsa, degeri alip icerik uretmek icin gerekli diger cagrilari yapariz. Kod suna benzer:    if (window.location.hash.indexOf(":") > -1) {       str = window.location.hash;       ...    }Bu kod sayfa 'body' seviyesinde, sayfa her yuklendiginde isleyecek sekilde konumlandirilmali.Boylece geri dugmesi icerigi hatirlayabilmis olur.




