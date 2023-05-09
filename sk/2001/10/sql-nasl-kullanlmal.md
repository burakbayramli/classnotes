# SQL Nasıl Kullanılmalı

Küme kavramı (ilkokuldan hatırladınız mı?)  Veri tabanı programları,
büyük miktarda veriyi işlemek için yazılmıştır. Yaptıkları bütün
işlem, kaybedilmez türden veriyi saklamak, hızlı şekilde geri getirmek
ve geriği olduğu zaman filterden geçirip sadece bir bölümünü
kullanıcıya sunmaktır.  Mesela aşağıdaki komut,

INSERT INTO MUSTERI VALUES ('Murat', 'Bilisim', 'Adres', 111);

veri tabanınıza VALUES kısmından sonra gelen bölümü, MÜŞTERİ kayıtına
yazmanızı sağlar. Elektrik kesilse de, ekranınız patlasa da, bu veri
bir yere gitmeyecektir. (Hard dışkınız bozulmadığı sürece tabii). Eğer
bu veriyi geri istese idiniz,

SELECT * from MUSTERI WHERE ISIM = 'Murat';

komutunu uygulardınız.  Şimdi size ilginç bir şey açıklayacağım. SQL
Veri tabanları ilkokulda öğrendiğiniz küme kavramı ile
çalışırlar. Hani kesisen kümeler, ek kümeler, vs.. hatırlarsınız.
Sonuçta veri bulmanın en rahat yolu, veriyi bir küme gibi görmek, ve
istediğiniz elemanları istenen küme içinde, ötekileri dışında görmek
uygun olur.  Ve en can alıcı yere geliyoruz. Veri tabanları, küme
teorisi ile en hızlı halde çalışırlar. Bundan başka yol varmı ki?
diyebilirsiniz. Elbette var. SQL diline sonradan eklenen bu uzatmalar,
bazen gerekli olsa bile, genelde kaçınilmasi gereken kullanım
biçimleri. Bu alternatif metoda 'tekci metod' diyeceğiz, ötekine
küme metodu.

Tekci Metot

Elinizde 1 milyon müşteri kayıdının olduğunu düşünün. Diyelim ki, bir
program yazmanız istendi, bu programa göre, her müşterinin size
masrafı, getirdiği parayı birbirinden çıkarıp, müşterinin size net
maliyetini bulup, aynı müşteri kayıdı üzerinde 'net maliyet' adlı yere
geri yazmanız istendi. Bu programı iki şekilde yazabilirsiniz.  ---
Tarif kod yazıyoruz

BASLA;HER KAYIT ICIN EGER MUSTERI SEHIR = 'ANKARA'
ISE KAYIDI AC; NET_MALIYET = MASRAF - MUSTERI_KAR; MUSTERI.NET_MASRAF
= NET_MALIYET.  EGER SON;SONRAKI KAYIDA GEC;

Yukarıdaki koda göre, her kayıdı teker teker açıp, işleyip, tekrar
geri yazmanız gerekecek. Yani her işlemi, teker teker yapacaksınız,
teker teker nasıl işleyeceğini düşüneceksiniz demektir. Yani veri
tabanına, işini 'nasıl' yapacağını öğretmeye uğraşacaksınız demektir.
Fakat veri tabanları, büyük veri kümeleri üzerinde çalışmaya alışık
değiller mi? Niye nasıl yapacağımızı tarif edelim? Ne istediğimizi
istesek, gerisini veri tabanı yapsa olmaz mı?

Iste alternatif yontem:

Kume Metodu

UPDATE MUSTERI SET NET_MASRAF = MASRAF - MUSTERI_KAR WHERE
SEHIR = 'ANKARA';

Vay canına, bu daha kısa oldu. Hem de sonuçta daha hızlı işleyecektir
göreceksiniz. Bunun sebebi nedir? Çünkü küme teorisini kullandık. Veri
tabanları kümeler ile çalışmayı severler. Biz yukarıda, bir küme
tanımladık, 'ne' istediğimizi söyledik, ama nasıl yapılacağını
söylemedik. Kümemiz, Ankara şehrinde çalışan müşteriler idi. Bunu
yaptıktan sonra, veri tabanı arkasına dönüp veriye baktı, en hızlı
erişim ve değiştirme algoritmasini kendi hesapladı, ve uygun olan en
hızlı metodu kullanıma koydu. Biz veri tabanının önüne engel
koymadığımız için, bu metod en hızlısı oldu.  Her teknolojinin
kullanılmayı sevdiği bir şekil vardır. Bu şekli bulun, böylece
teknolojinin cevapladığı sorun tiplerini de bulacaksınız. Böylece
çekiç gereken yerde testere kullanmazsınız, daha tecrübeli, gereken
yerde gereken malzemeyi kullanan usta mühendis haline gelirsiniz.





