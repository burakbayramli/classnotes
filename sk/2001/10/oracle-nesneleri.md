# Oracle Nesneleri

Oracle birimlerini tanımaya devam ediyoruz. Bahsettiğimiz tekniklerin
başka veri tabanlarında karşılıkları olsa da, Oracle açısından bu
tekniklere bir göz atmak yararlı olacak.  Görüntü Görüntüler, önceden
depolanmış ve sorgulanabilen SQL kodlarıdır. Bir görüntü, güvenlik
ötürü bazı çizgeleri göstermeden, bu çizgelerin verisinin bir kısmını
göstermek için kullanılabilir. Mesela, halka ilişkiler bölümu için,
isim, soyad ve adres bir görüntü içinden gösterilebilir, öteki
bilgiler saklanabilir. Ya da, dağınık veri tabanlarından toparlanacak
bilgiler, bir görüntü ile önceden kodlanır ise, artık birden fazla
veri tabanına bağlanmak yerine, tek bir görüntü üzerinden veri
alınabilir. Özet olarak, normalde işleyen her SQL kodu, görüntü
yaramak için kullanılabilir.  -- örnek

```
create or replace view goruntu_1 as  select isim, soyad from MUSTERI;
```

Dizi (Sequence)

Tekil sayı yaratmak için kullanılan Oracle nesnesine Dizi adı
verilir. Dizi yaratmak için başlangıç değeri, artış miktarı ve bitiş
sayısı vermek yeterlidir. Yani, 1..n arası her seferinde 1 kadar
artacak bir dizi yaratmak mümkün. Bu sayılar genelde kimlik no gibi
özgün olması gereken değerler için kullanılır. Her seferinde kod
içinde, 'önceki değer + 1' demek yerine, SQL kullanarak her diziden
yeni sayı isteyişimide, güncel dizi artış değeri kadar otomatik olarak
arttırılır ve alınan sayı SQL sonucu olarak verilir.

Bir dizi yarattığınız zaman, NEXTVAL ve CURRVAL sanal kolonlarını
kullanarız. Mesela UYE_DIZISI adlı bir dizi yarattı isek,

```
SELECT UYE_DIZI.NEXTVAL FROM DUAL
```

diyerek o anki değeri alabiliriz. Bu komut sonucunda ayreten dizi
değeri 1 arttırılır.

```
SELECT UYE_DIZI.CURRVAL FROM DUAL
```

kullanımı o anki değeri verecektir. Fakat diziyi arttırmaz.  --
örnek

```
CREATE SEQUENCE DIZI_ISMI INCREMENT BY 1 -- artış
START WITH 100;
```

-- başlangıç Tetik Tetikler, depolanmış PL/SQL kodlarıdır.

Önceden öngörülen belli bazı çizelgelere, gene önceden öngörülen
şekilde bir erişim olduğunda tetikler ateşlenirler, ve kodlanan işlemi
yerine getirirler. Tetikler, satır ekleme, silme, güncelleme ya da
bütün bu temel işlemlerin değişik guruplamaları için
kodlanabilirler. Tetiklerin en çok kullanıldığı alan, veri amlığı için
kısıtlamalar koymaktır. Bu tür kısıtlamalar Oracle terimleri ile, Veri
Tamlık Kontrolleri diye bilinir, ve satırlar arasında bazı kontroller
getirebilir. Fakat bu tür satır bazlı kontrollerin yetmediği hallerde,
tetik kodları işe yarayabilir. Çünkü tetik kodları içine PL/SQL ile
kodlayabildiğiniz her türlü kod koyabilirsiniz.  -- ornek-- dikkat, bu
komutları komut satırından giriyoruz

```
SQL> create to replace trigger musteri_tetik_1 INSTEAD OF1 insert on
musteri for each row2 begin3 insert into maas values4 ('1', '2');
```

buraya daha değişken kod koyabilirsiniz.

Eşanlam (Synonym)

Eşanlamlar, bir veri tabanındaki çizelgelerden diğer çizelgelere
kestirme işaret olarak görülebilir. Bir eşanlam yaratmak için eşanlam
ismi, ve o eşanlamın yerini tuttuğu Oracle nesnesinin ismi
gereklidir. SQLPlus kullanarak bu eşanlama eriştiğimizde, Oracle arka
planda eşanlamın yerini tuttuğu nesneyi bulur, ve işlemi o öteki nesne
üzerinde yapar.  İki türlu eşanlam vardır. Umumi (public) ve özel
(private). Özel eşanlamlar içinde yaratıldıkları şematiğe özel
olurlar, o şematiğe ait kalırlar ve başka kullanıcılar tarafından
erişilemez, hatta görülmezler bile. Eğer eşanlamlar umumi ise, bütün
şematikler tarafından kullanılabilirler.  Oracle'ın bir nesneye
erişmek için hangi tür bir algoritma izlediğine gelelim: Eğer şöyle
bir kod işletildi ise

```
SELECT * FROM FROM MAAŞ

Oracle MAAS nesnesini bulmak için Oracle şunları yapar.

* MAAS adlı bir çizelge ya da görüntü var mı?

* Bu ikisi yoksa, Oracle MAAS adında özel bir eşanlam arar.

* Var ise, özel eşanlamın gösterdiği nesne kullanılır.

* Yok ise, MAAS adlı umumi eşanlam aranır.

* Hiçbiri yok ise, Oracle ORA-00942 hata mesajını verecektir.  --

ornek

```
create synonym MUSTERI for BIZIM_UZUN_MUSTERI_ISMI;
```

Veri Tabanı Bağlantısı

Bağlantılar nesne bazında değil, veri tabanı bazında bağlantı
kurar. Yani, uzakta olan bir veri tabanını, sürekli olarak uzak
adresini kullanarak erişmek istemiyorsanız, bir bağlantı, yani
kestirme yaratarak o veri tabanına sanki yerel bir tabanmış gibi
erişebilirsiniz.  -- örnek

```
CREATE PUBLIC DATABASE LINK baglanti_1

CONNECT TO kullanici IDENTIFIED BY sifre USING 'UZAK_VERI_TABAN_SID_DEGERI';
```




