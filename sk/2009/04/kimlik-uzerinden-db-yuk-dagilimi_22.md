# Kimlik Uzerinden Db Yuk Dagilimi


Kimlik Uzerinden Db Yuk Dagilimi



Anahtar-deger tabanlarinin, ozellikle Amazon Dynamo modelini takip edenlerin, kimlik (id) degerine bakarak yuk dagilimini yaptigini anlatmistik. Bunu nasil yapiyorlar? Nasil yapmak mantikli olurdu? Oyle bir yonlendirici fonksiyon bulalim ki, bu fonsiyon rasgele ama ayni deger icin "hep ayni sekilde rasgele" bir nod'a yonlendirsin. Yonlendirme icin db kumesine sormamiz gerekmesin, cunku db'ye ne kadar az gidersek, performansta o kadar kazanc saglariz.O zaman matematikteki "mod" fonksiyonunu kullanabiliriz. Elimizdeki herhangi bir ID degerine elimizdeki db makina sayisiyla mod bolumu yapariz, sonuc yonlendirilecegimiz DB nod kimligi olur. Diyelim elimizde 4352345234 degeri var, ve 5 tane db makinasi var. 4352345234 mod 5 = 4.Demek ki 4 no'lu makinaya gidecegiz.Dikkat edilirse, mod bolumu ayni sayilar icin hep ayni degeri verir. 4. makinaya yazdigimiz degeri baska makinada aramis olmayiz yani. Ayrica mod hep ayni seviyede, dengeli (uniform) bir dagilim  yaparak yukun hep ayni makinalara gitmesine de engel olur. Ve en onemlisi mod bolumunun nod sayisindan fazla bir sayi dondurmesi mumkun degildir.Not 1: Yari-rasgele (pseudorandom) sayi ureteclerinin hep mod fonksiyonunu kullandigi ilginc bir ek ayrintidir.Not 2: Biz sistemimizde db'ye gidis/gelisi azaltmak icin yeni objeler icin ID uretimini bile DB'ye yaptirmayacagiz; kimligi Java UUID() objesini kullanarak urettirecegiz.




