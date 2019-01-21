# Sayfa Oncesi Aksiyon


Sayfa Oncesi Aksiyon



Seam'in kullandigi sayfa -> aksiyon -> sayfa modelinden bahsettik. pages.xml adli bir dosyada tum navigasyon kurallari tanimlanmaktadir, ve X sayfasinda iken 'a' aksiyonu alinca Y sayfasina yonlendir gibi kurallar burada tanimlanabilir.Yanliz bazi durumlarda sayfa yuklenmeden once bir aksiyon isletmek gerekebilir. Bu durum icin view-id tanimladigimiz ayni satirda bir aksiyon tanimlamamiz mumkundur.<page view-id="/main.xhtml" conversation-required="false"     login-required="false" action="#{userHandler.someAction}">   <rule if="#{user == null}">     <redirect view-id="/home.xhtml"/>   </rule>   ...</page>Bu ornekte main.xhtml sayfasi icin (aslinda boyle bir sayfanin mevcut olmasi bile gerekli degil, tarayiciya main.seam girince ustteki navigasyon kurallari isler) userHandler.someAction komutu isletilsin istiyorsak, tanim yukaridaki gibidir... Ayrica, diger her tur navigasyon kurali icin de, bu isletimden sonraki yonlendirmeyi "eger kuralina" baglayabiliriz. Mesela, bir aksiyondan @Out ile de-enjekte edilmis bir user objesinin olup olmadigina bagli bir karar almak istiyorsak, rule-if ile bunu yapariz. Ustte, eger user null ise, "home.xhtml adli bir sayfaya yonlendir" diye bir tanim yapmisiz.




