# Hangi tarayici?


Hangi tarayici?



 Kullanicinizin sitenize hangi web tarayicisiyla baglandigini ve hangi siteden geldigini göstermek istiyorsaniz asagidaki kodlari kullanabilirsiniz.               Internet üzerindeki herhangi bir siteden sayfaniza gelen kullanici bu kodlar sayesinde geldigi adresi, Web tarayicisinin tipini ve sürümünü ögrenecektir. Kullanicinin geldigi adresi ögrenmesi için muhakkak bir web sunucusundan baglanmasi gerekmektedir.               Sayfaniza lokal bir web sayfasindan tiklayarak gelen kullanicilar 1. Satiri bos göreceklerdir. Verdigimiz örnek kodlari degistirerek sayfaniza baglanan kullanicinin geldigi siteye geri dönmesi için bir tus koyabilir, web tarayicisinin cinsine göre degisik içeriklere ulasmasini saglayabilirsiniz.               <HTML><HEAD><TITLE>Hangi Tarayici?</TITLE></HEAD><FONT SIZE=+1 FACE=ARIAL><SCRIPT LANGUAGE="JavaScript">var where = document.referrervar name = navigator.appNamevar vers = navigator.appVersiondocument.write("Sizi buraya yollayan adres: "+where+" <P>Kullandiginiz tarayici: "+name+" <BR>"+vers+" ")</SCRIPT></CENTER></HTML>             Yazarin izni ile http://draskin.150m.com sitesinden alinmistir




