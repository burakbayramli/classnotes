# Genel Hata Ekrani


Genel Hata Ekrani



Servis tarafinda bir hata ortaya cikinca, yani Java tarafindan Exception atilinca, ya da kullanici tarayiciya anlamsiz (sitemizde olmayan) bir sayfa ismi girdiginde sitenin gorselligini bozmayan uygun bir hata ekrani gostermek gerekir. Olagan haliyle bu ekranlar gorsel olarak uygunsuz ekranlardir. Uygun bir sayfa cogunlukla tepe logosu, renkleri uygulamanizin olan sadece ici bos bir ekranda tek bir hata mesaji iceren bir sayfadir.Bunu elde etmek icin iki yerde degisiklik lazim. Birincisi olmayan sayfa isimlerine, yani pur 404 hatalarina ekran gostermek icin Apache tarafinda./etc/apache2/sites-available/default dosyasina girelim ve <VirtualHost> etiketi kapanmadan onceErrorDocument 404 /error.seamibaresini ekleyelim. Sayfa error.xhtml, ustte bahsettigimiz gibi logosu yerinde, guzel ve bos bir ekrani tanimlayacak. Bu sayfa tabii ki /var/www altinda mevcut olmali. Tamam. Artik Apache tekrar baslatilinca 404 hatalari icin bu sayfa gosterilecek.Seam tarafinda ise, eger hic kimsenin caresine bakmadigi, tutmadigi (handle) Exception'lar JVM'de en ust seviyesine cikmis ise, bu hatayi guzel ekrana yonlendirmek icin WEB-INF/pages.xml icinde  <exception>  <end-conversation/>  <redirect view-id="/error.xhtml">    <message>Unexpected failure</message>  </redirect></exception>Bu kadar. pages.xml icinde aslinda daha detayli tip bazinda Exception yakalayip kullaniciyi daha ozel sayfalara yonlendirmek mumkun; ust gosterdigimiz eger o diger tum yakalayicilar bir sey yapamamissa devreye girecek olan "genel yakaliyici (catch-all)" tanimidir. Hic planda olmayan hatalar icin gereklidir.




