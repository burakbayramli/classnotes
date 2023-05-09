# Oracle ve Autoincrement Kolonlar

Bazı veri şemalarında ID kolonu atanan bir değer değil, üretilmesi
gereken bir sayıdır.  Bu sayı, en basit hâliyle 1'den başlayarak her
yeni veri satırı için birer farkla artması gereken bir sayıdır.  O
zaman her satırı yazarken bu ID'nin üretilmesi gerekmektedir. Bu
üretimi, ya uygulama içinde yapacağız, ya da veri tabanın otomatik
olarak yapmasını sağlayacağız.  Otomatik attırım bazı veri
tabanlarında bir kolon tipi olarak bile karşınıza çıkabilir. Oracle'da
bu işi yapmak için bir sequence ve bir trigger kullanmamız
gerekiyor. Oracle, bu tekil sayının üretimini ve tabloya atanması
işini birbirinden ayırmıştır. Bu da sanıyorum isabet olmuştur,
auto-increment kolon tipi hakikaten çok basitleyici bir çözümdür.
Oracle'da otomatik ID üretimi için iki şey gerekir; Bir sequence, bir
de trigger. Meselâ, şöyle bir tablomuz olduğunu düşünelim.  create
table test ( id number, veri varchar2(20), ...); Bu tablo için bir
sequence ve bir trigger yaratalım.  create sequence test_seq start
with 1 increment by 1 nomaxvalue ;create trigger test_trigger before
insert on test for each row begin select test_seq.nextval into :new.id
from dual; end; Artık, INSERT into test (veri) values ('blablabla');
.. gibi bir INSERT kullandığımızda, hiç ID'ye dokunmamıza gerek
kalmadan, bir sonraki ID sayısı hesaplanacak ve kolona koyulacaktır.





