# Yaygın İnternet Teknolojileri

İnternet servis tarafı için hangi teknolojiyi seçmeliyiz?  Doğru
çözümü bulmak için birkaç seviyede düşünmek lazım:

* Hangi işletim sistemi en güçlüsü?

* Hangi işletim sistemi en rahat kontrol edilebilir?

* Hangi programlama dili İnternet servis
paketleri içinde en yaygındır?

* Hangi programlama dili en rahat kullanılır?

* Hangi programlama dili için programcı daha rahat bulunur?

İşletim sistemi gücünden bahsederken önemli olan, hangisinin daha uzun
süre ayakta kalacağıdır. Microsoft NT ve 2000 hala dayanıklı bir
sistem değildir. Amerika'da büyük müşterilere ne zaman gitsek, eğer
Microsoft ile site kurmuşlar ise, ondan başka bir sey arıyorlar idi
(örnek Best Buy, Martha Stewart ). Genelde Microsoft İnternet
tarihinin başlangıcında, reklam yapabilmek için, büyük müşterilere
"size bedava site yapacağım" demiştir. Böylece etrafta hava
atabiliyordu, mesela müşteri XYZ'nin sitesinde Microsoft
programlarının olduğunu söyleyebiliyordu. Fakat bilinmeyen, bu
sitelerin sırf reklam için Microsoft tarafından bedava kurulmuş
olduğudur.

Fakat müşterinin site trafiği artmaya başlayınca site çökmeye
başladı. Sebepleri: Microsoft işletim sistemleri dayanıklı sistemler
değildir. O yüzden büyük siteler için (hatta küçükler için bile)
Microsoft'tan vazgeçin. Microsoft çözüm ortağı bir şirkette
danışmanlık yapmış biri olarak bunu üstüne basarak söylüyorum. Unix
sistemleri, özellikle "çekirdek" işletim sistemi yaşlı olanlar daha
iyidir. Solaris, HPUX bu tür sistemlerdir. Linux yaşlı olmasa bile o
kadar cok test yapan kullanıcısı vardır ki, hataları daha hızlı
yamanmıştır. FreeBSD içinde iyi seyler duyuyoruz. Fakat serbest
Unix'ler arasında pazar ivmesi şu anda Linux arkasındadır.  Kontrol:
Hangi sistem daha rahat kontrol edilir? Yani, sitenizin bakımını yapan
insan, hangi sistem ile daha rahat eder? Bu arada hemen "rahat"
kavramını tanımlayalım. Rahat ne demek? Eğer rahat, güzel
pencerelerden ikon'lu programların üzerine tıklayarak sistem idaresi
demekse, o zaman MS'in üstüne yoktur(!).  Yok eğer kontrol, hangi
sistem daha iyi "otomotik kontrole alınabiliyor" ise, o zaman Unix
çeşitlerini tavsiyeden baska çaremiz yok. Unix sistemleri genelde
betik/script edilebilir sistemlerdir. Yani, her türlü işi, kayıtlı
ufak programlar ile yapabilirsiniz. Görsel tıklamalar kayıt
edilemezler.

Fakat komut satırından yazdığınız kelime komutları kayıt
edilebilir. Sistem idarecileri icin bu vazgeçilmez bir
şeydir. İdarecilerin genelde aynı işi tekrar tekrar yapmaları
gerektiği için, kayıtlı komutları geri sisteme işletebiliyorlar ise,
bu çok iyidir.  Windows ile de nispeten kayıtlı komutlar işletmek
mümkün ama, bu komutlar işletim sisteminin her yerine erişiyor. Çoğu
önemli idare "muslukları" görsel şekilde yapılır Microsoft
ile. Unix'de bütün güç yazılı komuttadır. Yazı kaydedilebilir. O
yüzden sistem idarecileri Unix'i çok sever. Sevmeyenler Unix'i
bilmez. Microsoft ile bir şey öğrenmek basittir, "Unix ile çok zor"
diyorsa birisi, hemen dönün başka birini bulun.  İşletim sistemini
seçtiniz.

Peki hangi programlama dilini kullanacaksınız? Şu anda Java en iyisi.
C++, kullanması ve öğrenmesi zor bir dildir, ve çok kolay hata
yapmanıza izin verir. İşletim sistemi yazmak için uygundur, çünkü
makina seviyesine inmek rahattır. Fakat şirketler için yazılan
programlar bu seviyede calışmazlar, şirket programlarında çabuk ve
yanlışsız programlamak daha önemlidir. Özetle, C++ dili ile çok
müşterimize program yazdık, güçlü bir dildir ama vefasızdır. :)
Basitliği ve okunulabilirliği sayesinde Java cok kullanıcı
toplamıştır. Şu anda dünya çapında Java programcıları Visual Basic
programcılarından daha fazladır. Bunu unutmayın. Yaygın olduğu için,
ve ayrıca Sun Microsystems şirketi arkasında olduğu için, servis
paketleri tarafından çok tutulmuştur bu dil. ATG Dynamo, WebLogic, IMB
WebSphere gibi paketler Java programlarını doğrudan çalıştırabilirler.
Programcı bulma rahatlığını da galiba aynı anda cevapladık.





