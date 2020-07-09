# Ilkel Bir 'Client Pull'


Ilkel Bir 'Client Pull'



POINTCAST'i hepiniz bilirsiniz. Kullanici bu servisin sagladigi arabirimde bir takim seçenekleri isaretler ve bu seçenekler dogrultusunda masaüstüne bilgiler gelir. <meta> tag'inin yaptigi is de bundan pek farkli degildir. Kullaniciya ulasan web sayfasi bu tag sayesinde kendini belirli araliklarla tekrar yükler veya kullaniciyi baska bir web sayfasina isinlar. ' Sayfayi isteme' isi kullanici tarafindan sunucuya yollandigindan bu komut ' client pull' islemini basariyor diyebiliriz. Bu tag ile birlikte asagidaki ekler kullanilmaktadir.HTTP-EQUIV=' REFRESH' : Saniyelerle belirlenen araliklarla dokümanin kullaniciya tekrar ulasmasini saglar.CONTENT=' n; URL=url' : Web tarayiciya sayfanin ne araliklarla tekrar yüklenecegini belirler. CONTENT eki içinde ayrica URL ifadesi kullanilmissa belirtilen zaman araligindan sonra hangi sayfanin yüklenecegi de belirtilebilir.Asagida, mutlaka <HEAD> - </HEAD> araliginda kullanilmasi gereken <META> tag'inin iki örnegini veriyoruz. 1. örnek, bulunulan sayfayi her iki saniyede bir tekrar yükleyecek, 2. örnek ise açildiktan 5 saniye sonra draskin.150m. com adresli DrA Online'in web sitesine baglanacaktir.ÖRNEK - 1<HTML><HEAD><META HTTP-EQUIV=' REFRESH' CONTENT=2><TITLE>Tekrar YÃ¼kleme</TITLE></HEAD><P>Bu sayfa 2 saniyede bir tekrar yÃ¼klenecektir.</HTML>Ã�RNEK - 2<HTML><HEAD><META HTTP-EQUIV=' REFRESH' CONTENT=' 5; URL=http://draskin.150m..com' ><TITLE>Ikinci DokÃ¼mani YÃ¼kle...</TITLE></HEAD><BODY><P>Bu sayfa 5 saniye sonra DrA Online'in web sitesine baglanacaktir.</BODY></HTML>Yazarin izni ile http://draskin.150m.com sitesinden alinmistir




