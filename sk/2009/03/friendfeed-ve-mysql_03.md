# FriendFeed ve MySQL


FriendFeed ve MySQL



Friendfeed.com programcilarindan Bret Taylor blogunda MySQL ile kumeli ortamlarda nasil calistiklarini anlatti. Canli, calismakta olan db'lerde indeks eklemenin, silmenin cok zaman aldigini ve bunun canli tablolari kitledigini, sonuc olarak sitenin optimal isleyisini engellediginden bahsetti.Cozum olarak DB yapilarini tamamen degistirmisler. Klasik iliskisel yapi yerine veri iceren tek tablo Entities diye bir tablo, erisim icin UUID bir id tanimlanmis. Indeksler bu tek tablo tipine erismek icin, ihtiyac duyuldugu sekilde yaratilip silinebilen "ek tablo semalari" haline gelmisler. Yeni tablo yaratmak canli siteye bir engel teskil etmiyor, o problem boyle cozulmus. Silinmek istenen indeks tablolarina koddan erismeyi birakiyorsun, o kadar. O da halloldu.Indekslerin nasil kullanilacagini evde pisirdikleri in-house DB katman kodu biliyor.Post'un yorumlarina bakilirsa, MySQL'in diger tur db'lerden bazi eksikleri var. "Indeks guncellemenin db kitlemesi" problem Oracle da gorulmezdi mesela. Fakat MySQL'in bedava olmak gibi bir avantaj var, ayrica replikasyon, veri temelli bolme (sharding) hala isleyen, ve ici disi iyice bilinen ozellikler.Bu yaklasim hosuma gitti; Ustunde calistigim site servis tarafi kodlama asamasina gelince, aynisini Java'ya port edip kullanmaya baslayabilirim. Bu mekanizmayi, eger yazarsam, acik olarak paylasacagim.




