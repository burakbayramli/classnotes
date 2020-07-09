# IE ve Firefox Farklari


IE ve Firefox Farklari



Web sitemizi kodlarken sayfalarimizin hem Firefox hem Internet Explorer'da duzgun ciktigini projenin basindan itibaren kontrol etmemiz gerekiyor. Bu iki tarayici arasinda farkliliklar var, ve birinde duzgun cikan CSS digerinde duzgun cikmayabiliyor.Peki farkliliklari nasil onaracagiz? Burada guzel bir numara kullanilabilir; Bu numara dahilinde CSS/HTML sayfalar bir tarayici icin yazilir, ve farklar bir "yama" gibi ikinci bir css dosyasi icinde onarilir. Bu nasil mumkun oluyor? Soyle: 1) Eger gorsel elementleri "stilleyen" ayni id ve class ismi ikinci bir CSS (hatta ayni CSS dosyasi icinde ikinci kez) geciyorsa, her zaman daha "sonra" gelen ustunluk kazanir - onun degerleri bir oncekini ezer. 2) Cogunlukla bir Web sitesinin sayfalarinin IE ve Firefox farklarini onarmak icin CSS'te "bastan asagi" bir onarim gerekmez. Sadece bir blogun birkac elementinin ezilmesi yeterli olur. O zaman alttaki gibi bir kullanim mumkun olabilir. Dikkat edelim: Once Firefox icin kodlamisiz, sonra IE icin duzeltilmesi gerekenleri ikinci bir dosya ile ezmisiz. Burada sorulabilir: Niye once Firefox? Cunku FFstandartlari tam takip ediyor, o zaman onu merkez alarak kodlamak daha mantikli olur.<html><head><title>...</title><link href="style.css" rel="stylesheet" type="text/css" /><!--[if IE]>   <link rel="stylesheet" type="text/css" href="ie.css" />   <![endif]--></head>Ustteki kodda, ie.css adli sayfa [if IE] blogu icine alinmis, tum tarayicilar bu komutun anlamini biliyorlar - o blok sadece IE tarayicilari icin isletiliyor. Bu dosya icinde yama icin gereken tum ekler koyuluyor. Ornek olarak style.css sunlari iceriyor olsun;#top {position: absolute;background: url(./img/headers/header_bg2.gif) repeat-x fixed top left;left: 0;top:0;height: 150px;width: 90%;}Ve ie.css sunlari iceriyor olsun;#top {width: 100%;}Bu durumda IE icin #top id'si icindeki tum degerler ayni olacak, sadece width degiskeni ezilerek yeni 100% degerine set edilecektir.




