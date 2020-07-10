# Rastgele Islemler

WEB Okulu sayfalarimizda zaman zaman faydali Java Script kodlari
verecegiz. Iste bunlardan bir tanesi; kodlarimiz bulunulan saniyeyi
okuyarak her saniyede degisik bir Murphy kuralini ekrana getiriyor. '
AR' degiskeniyle kaç adet kural oldugu tanimlandiktan sonra, kural
Getmessge fonksiyonuna ataniyor ve fonksiyon <BODY...> tag'inin
içerisinden çagriliyor. Kendi vecizelerinizi bu script'e eklemek için
öncelikle ' Array(3)' degerini daha sonra ' ar[sec % 4])' degerini
yükselmeli ve vecizeyi yeni bir ' ar[x] =' satirina eklemelisiniz.
<HTML><HEAD><TITLE>Rastgele Vecizeler</TITLE><SCRIPT LANGUAGE='
JavaScript' ><!--function getMessage() {var ar = new Array(3)ar[0] = '
HiÃ§ bir is gÃ¶rÃ¼ndÃ¼gÃ¼ kadar kolay degildir.'ar[1] = ' Her is
tahmin ettiginizden Ã§ok vakit alir.'ar[2] = ' Yanlis gitme olasiligi
olan her is yanlis gider.'ar[3] = ' Bir takim islerin yanlis gitme
olasiligi varsa size en Ã§ok zarar verecek olani yanlis gider.'var now
= new Date()var sec = now.getSeconds()alert(' Murphy Kanunu:\r' +
ar[sec % 4])}//--></SCRIPT></HEAD><BODY onLoad=' getMessage()'
></BODY></HTML> Yazarin izni ile http://draskin.150m.com sitesinden
alinmistir




