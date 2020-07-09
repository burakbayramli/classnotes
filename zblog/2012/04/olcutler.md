# Olcutler (Measures)




Olcutler (Measures)




Pentaho ambar tasariminda bazi ek olcutler dusunelim. Onceki yazida yarattigimiz olcut totalamount uzerinde yaratildi ve toparlayici (aggregator) fonksiyonu olarak toplam (sum) tanimladi. Yani gruplama hangi boyut uzerinden yapiliyorsa, o gruplar icinde belirlenen kolon uzerinde toplam yapilacak.

Baska toparlayicilar da olabilir, mesela siparis eden kac tane tekil musteri oldugunu (unique customers) bir olcut olarak gormek istiyorsak, baska bir olcut customerid uzerinde distinct-count kullanabilirdi.




Bu kupu yayinlayalim ve konsola gidelim. Gidince olcutun konsolda gozukmedigini farkedebilirsiniz. Olcutun gozukmesi icin sol ust kosedeki ikona tiklayin (resim altta)




ve Measures uzerine tiklayin, ve unique customers yanindaki isarete tiklayin,



 ve OK deyin, ve sonraki (daha dogrusu onceki) ekran gelince tekrar OK yapin. Iki olcut yanyana gosteriliyor olacak. 



Bir baska olcut orderid uzerinde count olabilir, zaten her orders satirinda tekil bir orderid olduguna gore bu olcut tum satirlari sayacaktir, yani kac tane siparis oldugunu gosterecektir.

Olcutler bundan daha fazlasini da yapabilir. Diyelim ki "musterilerin siparislerinin oran olarak ne kadarinin mevcut (eski) musterilerden geldigini" hesaplatmak istiyoruz. Bunun icin elimizde bilgi var, reg_date ve orderdate. Demek ki olcut tek bir kolona degil, iki kolona bakacak, ve bir toplam degil, bir oran hesaplayacak.

Bunun icin xml dosyasina editor ile girin, ve sunu ekleyin,

    <Measure name="Existing Customer Rate" aggregator="avg">      <MeasureExpression>        <SQL dialect="generic">          (case when orderdate > reg_date then 1 else 0  end)        </SQL>      </MeasureExpression>    </Measure>            

Bu ornekte olcut icine SQL kodu bile gomebildigimizi goruyoruz. Bu kod olcute biraz daha akil kazandiriyor, bir sart ifadesi var, eger orderdate>regdate ise o zaman bu eski bir musteridir, geriye 1 dondur, degilse 0. Bu irdeleme satir bazli yapilacak. Bir de bu degerler uzerine toparlayici olarak ortalama (avg) fonksiyonu tanimlarsak, o zaman olcut bize her grup icin 1'den kucuk bir sayi verir, yani bir oran hesaplamis oluruz.

Bu kuvvetli bir olcut. Ustelik hangi boyut kullanirsa kullanilsin isleyecektir! Isin o kismini kullaniciya birakiyoruz, biz ambar tasariminda gerekli hazirligi yapmis oluyoruz. Yayin sonrasi bakiyoruz, posta koduna tiklayinca her kod altinda ne kadar musteri sadakati var aninda listeleniyor. 




Eger olcute Schema Workbench'ten bakarsak suna benziyor,



Eger oran bir yuzde olarak gosterilsin istiyorsak, sadece formatString tanimini degistirmemiz yeterli,


    <Measure name="Existing Customer Rate" formatString="0.0%" 

        aggregator="avg">    ..     </MeasureExpression>


Yayin sonrasi goruntunun soyle oldugunu goreceksiniz








![](Screenshot+at+2012-04-26+13:30:28.png)
![](Screenshot+at+2012-04-26+13:32:34.png)
![](Screenshot+at+2012-04-26+13:33:50.png)
![](Screenshot+at+2012-04-26+13:35:05.png)
![](Screenshot+at+2012-04-26+13:46:30.png)
![](Screenshot+at+2012-04-26+13:53:03.png)
![](Screenshot+at+2012-04-27+09:06:42.png)
