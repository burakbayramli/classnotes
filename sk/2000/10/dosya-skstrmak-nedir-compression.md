# Dosya Sıkıştırmak Nedir? (Compression)

Hepiniz büyük bir ihtimalle İnternet'ten dosya
indiriyorsunuz. İndirirken mesela ZIP formatında dosyalar gördünüz,
indirdikten sonra, ZIP dosyası üzerine tıkladınız ve Winzip (yada
başka türlü) bir anti-sıkıştırma programı ZIP dosyasındaki bilgileri
açtı, ve birden bire bir sürü ve daha büyük ölçüde dosyalar karşınıza
çıkar.  Nasıl oldu bu iş? Bilgisayar bir dosyayı nasıl
sıkıştırabiliyor? Sıkıştırılan dosya bilgi kaybetmiyor mu? 100
Megabayt olan şeyi nasıl 20 Megabayt olarak gösterebilirsiniz?  Dosya
sıkıştırmanın sırrı, önce tekrar eden kelimeleri bulmaktır. Özellikle,
içinde okunabilir türden çok yazı olan dosyalar, bu şekilde çok tekrar
içerir. Sıkıştırıcı program tekrar eden bir kelime bulursa, o kelimeyi
bir daha söylemesine gerek kalmaz, onu simgeleyen, daha küçük bir
işaret bırakır. Böylece hem anlam kaybolmaz, hem de ikinci dosya daha
küçük yer tutar.

Örnek olarak bir cümle ile başlayalım.  BIR BERBER BIR BERBERE GEL
BERABER BIR BERBER DUKKANI ACALIM DEMIS Bu sıkıştırılmamış dosyamızın
içindeki cümle olsun. Hemen dikkat ettiyseniz, bazı kelimeler
tekrarlanıyor.  * BIR 3 kere gecmis * BERBER 2 kere geçmiş * ötekiler
sadece birer kere geçmişler Şimdi bütün kelimelerden bir sözlük
oluşturalım. Böylece kelimelere numaralar ile erişebileceğiz.  *
1. BIR * 2. BERBER * 3. BERBERE * 4. GEL * 5. BERABER * 6. DUKKANI *
7. ACALIM * 8. DEMIS Dikkat edin: Hala ikinci (sıkıştırılmış) dosyaya
bir şey yazmadık. Şimdiye kadar bütün yaptıklarımız, küçülmüş
bilgileri yaratmak için ön-hazırlık.  Eveet, şimdi ikinci dosyayı
yaratmaya başlıyoruz: Tekrar eden kelimeleri onların kodu ile
değiştireceğiz.

BIR|BERBER*1-2-1-BERBERE-GEL-BERABER-1-2-DUKKANI-ACALIM-DEMIS
Kodlamayı nasıl yaptığımızı açıklayalım: * işareti sol tarafına düşen
kelimeler tekrar eden kelimeler. Sıralarına göre numara verdik
onlara. BIR kelimesi BERBER kelimesinden önce geldiği icin numarası
'1', BERBER numarası '2'.  Kodlarken ne zaman BIR görsek, yerine 1
koyduk, ne zaman BERBER görsek, yerine '2' koyduk. '1', dosya içinde
BIR 'kelimesinden' daha az yer kapladığı için, yerden tasarruf
yaptık. BERBER içinde aynı sey geçerli.  Anladığınız gibi kodlama
metodumuzun, hem kodlayan hem de ters-kodlayan program tarafından
bilinmesi gerekiyor. Aynı metodu kullanırlar ise, iki program da
bilgileri küçültüp büyültebilirler.
