# Seam ve Hata Mesajlari


Seam ve Hata Mesajlari



Seam ile validasyon (veri dogruluk kontrolu), hata bulunca hata mesajlari yazma / gosterme mekanizmasi soyle kurulabilir.Hata kontrolu yapan her aksiyon,   @Out private transient List errors = new ArrayList();ile bir hata listesi tanimlar. Validasiyon sirasinda tum hatalar errors.add("mesaj") ibaresi ile bu listeye yazilir. Bu hatalar aksiyon sonrasi ustteki @Out ile de-enjekte edilecektir ve uygulamanin geri kalani artik bu listeye erisebilir. Bu noktada eger hatalarin olup olmadigina bagli olarak bir navigasyon yapmak istiyorsak, rule-if ile bunu yapmak artik cok basittir.  <page view-id="/page.xhtml" conversation-required="false"       login-required="true">   <navigation from-action="#{userHandler.someAction}">     <rule if="#{errors.size() == 0}">       <redirect view-id="/home.xhtml"/>     </rule>   </navigation> </page>Dikkat edelim: Hata olmadigi durumda hangi sayfaya gidilecegini tanimlamadik, cunku bunu tanimlamadiysak, Seam otomatik olarak bizi ayni sayfaya yonlendirir. Bu bizim icin uygun, zaten hata varsa o sayfayi terk etmek istemiyorduk. Simdi, bu ayni sayfada hatalari basmak icin bir JSF kodu soyle yazilabilir.  <font color="#FF0000">   <s:div rendered="#{errors.size() > 0}">     #{errors}   </s:div> </font>Bu sayfayi bir errors.xhtml diye tanimladik ve gereken her sayfadan include ile dahil ediyoruz.Tabii bu kodu surekli kullanabilmek icin hata verebilecek her aksiyonumuzun errors adli ayni degisken ismini kullanmasi gerekiyor.




