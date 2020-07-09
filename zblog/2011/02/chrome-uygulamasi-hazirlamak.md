# Chrome Uygulamasi Hazirlamak


Chrome Uygulamasi Hazirlamak



Bir HTML uygulamasini  Chrome uygulumasi haline getirmenin en kolay yolu,  "URL paketlemek", yani Chrome magazasindan indirilecek uygulamamizin zaten mevcut URL'imize baglanmasini saglamak. Boylece HTML sayfalarini paketlemeye gerek kalmiyor (onun gerekli oldugu durumlar da olabilir tabii, online olmadan en azindan kismen calismasi gereken uygulamalar mesela). Magazahttps://chrome.google.com/webstore/adresinde. Uyguluma yayinlamak icin Google'a $5 bayilmak lazim. Bundan sonra istendigi kadar uygulama yayini yapilabilir. Parali uygulama yayinlamak icin ABD banka hesabi gerekiyor (simdilik).URL paketlemek icin bir manifest.json yaratalim. Suna benzer:{"name": "Cepteborsa","version": "7","icons": { "128": "icon_128.png" },"app": { "urls": [   "http://cepteborsa.appspot.com/mweb" ], "launch": {   "web_url": "http://cepteborsa.appspot.com/mweb/index.html" }},"permissions": ["unlimitedStorage"]}Ayni dizinde icinde 128x128 boyutunda bir imaj olan bir  icon_128.png dosyasi lazim, o dosyada uygulamamizin  resmi olacak. "launch" web_url adresi uygulamanin baslangic baglantisi.Manifest icinde iki URL var. Bu adreslerin ikisinin de "bizim" oldugunu Chrome'a dukkanina ispatlamamiz gerekiyor; o isi Webmaster Tools uzerinden yapmamiz gerekli (Chrome dukkani isi WT'den kontrol ediyor)http://www.google.com/webmasters/toolsadresinde herhangi bir siteyi, sitenin alt dizinini kayit ettirebiliyoruz. Google'in site tanimasi icin belli teknikleri var, ismi verilen sayfa icine bir kod eklememizin soylenmesi gibi.. Biz bu secenegi kullandik ve  http://cepteborsa.appspot.com/mweb/index.html ve http://cepteborsa.appspot.com/mweb/index.html sayfalari icine Google'in istedigi bazi kodlari ekledik. Sayfalar tanindi.En son islem paketlemek; Paketlemek icin bir zip komutu yeterlizip /tmp/cepteborsa.zip manifest.json icon_128.pngSonra bu zip dosyasini Chrome dukkanina yukluyoruz. Yukleme yapilan formda gereken bilgileri giriyoruz, en az bir tane uygulama goruntusu (resim, video) lazim, onu da veriyoruz ve is bitiyor.Versiyon numaralari kavrami Android'dekine benzemekte, her degisimde versiyon numarasi artmis olmali, bu kontrol ediliyor. Yeni zip yaratirken biz bu numarayi bir arttiriyoruz.




