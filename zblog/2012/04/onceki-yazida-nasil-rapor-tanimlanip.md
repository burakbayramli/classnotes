# Parametreli Pentaho Raporlari




Parametreli Pentaho Raporlari



Onceki yazida nasil rapor tanimlanip yayinlandigini gorduk. Simdi bir rapor icin nasil parametre hazirlanabilecegini gorelim. Parametreler raporlarda bir nevi "bos birakilmis" alanlardir; kullanici raporu isletirken bu bilgileri o anda girer. Mesela bir parametre, tabandaki siparislerin raporlarken hangi tarih araligi uzerinden isletilecegini tanimlayabilir.Onceki yazidaki raporumuza donelim. Raporu actiktan sonra Data tab'i uzerinden Sample Query uzerine sag tiklama yapip Edit Query diyelim. Yani eski sorguya bazi ekler yapacagiz. Diyelim ki eski sorguyu belli tarih degerleri arasinda gormek istiyoruz. Mesela 2004-03-01 ve 2004-08-01.SELECTEXTRACT(MONTH FROM orderdate) AS month,EXTRACT(YEAR FROM orderdate) AS year,SUM(totalamount) AS totalFROM orderswhere orderdate between '2004-03-01' and '2004-08-01'GROUP BY year, monthORDER BY year, monthOnizleme ile bu sorgunun isledigini gorebilirsiniz.Fakat bu araligi kullaniciya sorsak daha iyi olmaz mi? O zamanSELECTEXTRACT(MONTH FROM orderdate) AS month,EXTRACT(YEAR FROM orderdate) AS year,SUM(totalamount) AS totalFROM orderswhere orderdate between ${fromDate} and ${toDate}GROUP BY year, monthORDER BY year, month${..} ile parametre kullanmis olduk. Simdi Data tab'i icinde en alttaki Parameters kismina gidip sag tiklama yapin, ve Add Parameter secin.Parametre ekleme icin soyle bir pencere gelecek, icini alttaki gibi doldurabilirsiniz.Goruldugu gibi fromDate kodunu verdik, Value Type yani alan tipi bir Date. Mandatory sectik cunku kullanicinin bu bilgiyi girmesini sart kosmak istiyoruz. Display Type ise bilginin hangi gorsel oge ile alinacagi. Date Picker bir tarih GUI objesi gosteriyor, hakikaten cok kullanisli bir sey. Ayni sekilde toDate de ekleyin, ve OK ile ikisini de kaydedin. Onizleme ile kontrol edin ve yayinlayin. Simdi Pentaho konsolundan raporu yukleyince, alttaki goruntuyu gorecegiz.Guzel bir tarih secici obje cikti goruldugu gibi. Buradan tarihleri seciyoruz, ve alttaki gibi filtrelenmis ciktinin basildigini goruyoruz.Parametrelerin cok daha kuvvetli ozellikleri de var. Mesela kullaniciya belli bir liste icinden bir secim yaptirmak istiyorsaniz, ve bu listenin kendisinin de bir sorgu ile tabandan cikmasini istiyorsaniz (mesela tabaninizdaki mevcut sehirlerin bir DISTINCT listesi olabilir) o zaman parametre tanimini bir sorguya baglamak mumkun.Ornek olarak su sorguyu alalimselect products.* from products, categorieswhere products.category = categories.categoryand categories.categoryname = 'Action'Bu sorgu tum aksiyon filmlerini listeler. Biz kategorinin kullaniciya sorulmasini istiyoruz. Simdi yeni bir rapor yaratip alttaki sorguyu verelim.select products.* from products, categorieswhere products.category = categories.categoryand categories.categoryname = ${category}Ama simdi ikinci bir sorgu daha lazim. Bu sorguyu eklemek icin DB baglantisi uzerine sag tik ve Edit Datasource. Available Queries yanindaki +,Onizleme yapin isledigini kontrol edin. OK ile ekleyin, eger Data tab'de sorgu listeniyor ama kolonlari listede gozukmuyorsa, sag tik ve Select Query ile bunu zorlayabilirsiniz. Ayni sey ana sorgu icin de gecerli. Simdi parametreyi ekleyelim.UI objesini Drop Down sectik ve buraya dikkat, Query icin onceden tanimladigimiz allCategories sorgusunu sectik. OK ile ekleyin, ve yayinlayin. Konsolda soyle cikacak.Kaynaklarhttp://www.prashantraju.com/2010/01/creating-parameters-with-pentaho-report-designer/




![](Screenshot%2Bat%2B2012-04-06%2B15%253A30%253A23.png)
![](Screenshot%2Bat%2B2012-04-06%2B15%253A36%253A18.png)
![](Screenshot%2Bat%2B2012-04-06%2B15%253A41%253A37.png)
![](Screenshot%2Bat%2B2012-04-06%2B15%253A43%253A55.png)
![](Screenshot%2Bat%2B2012-04-06%2B16%253A03%253A17.png)
![](Screenshot%2Bat%2B2012-04-06%2B16%253A07%253A24.png)
![](Screenshot%2Bat%2B2012-04-06%2B16%253A13%253A29.png)
