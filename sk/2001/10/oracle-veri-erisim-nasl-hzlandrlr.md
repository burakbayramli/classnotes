# Oracle Veri Erişim nasıl hızlandırılır

Eğer çok büyük veri tabanı üzerinde, veri işlemi yapıyorsanız, bu
yazımız işinize yarayabilir. Genelde CRM sistemleri için veriler
ambara alınmadan önce, bir ön-temizlikten geçirilir. Mesela eğer
"bütün isim" hanesi, ilk isim, soyisim diye ayrılmamışsa, bir ufak
program bu işi ambardan önce yapabilir. Fakat bu işlemi 4 milyon kayıt
üzerinde yapıyorsanız, uzun zaman bekleyeceksiniz! Buradaki teknikler
size yardımcı olabilir.  Oracle veri erişim, daha önce dediğimiz gibi,
SQL kodu ile yapılır. Oracle bu dil üzerinde bazı uzatmalar yapmıştır;
Eğer gerekirse, SQL veri erişim komutları 'aynı anda birden-fazla'
(paralel) şekilde işletilebilir. Bu özellik için tabii ki programcı bu
özel SQL komutlarını bir şekilde Oracle veri tabanına iletmeli.
Genelde en çok kullanılan metod, bu paralel değişikliğini 'çizelge
bazında' belirtmektir. Mesela MÜŞTERİ çizelgesini paralel seviye = 4
diye tanımladıysanız, bu kayıda erisen her kod, 4 kopya halinde
işletilir, ve bu kopyalar aynı anda işletilirler.

```
CREATE TABLE MUSTER PARALLEL DEGREE 4(HANE1 VARCHAR2(10),HANE2
VARCHAR2(30))SELECT * from MUSTERI; --
```

işte bu erişim, 4 kopya ile, daha hızlı geri döner.  Bunun işletim
sistemi seviyesindeki etkilerini görmek için Ünix seviyesinde ps -eaf
komutunu işletin. Sonuçları mesela şuradaki gibi olsun.

```
oraclesi 18578 1 0 Feb 06 ?  0:01 ora_p001_ESIPROD
oraclepi 18667 1 0 Feb 06 ?  158:59 ora_dbw0_ESIPROD
oraclesi 18578 1 0 Feb 06 ?  0:01 ora_p002_ESIPROD
oraclesi 18578 1 0 Feb 06 ?  0:01 ora_p003_ESIPROD
oraclesi 18578 1 0 Feb 06 ?  0:01 ora_p004_ESIPROD
oraclepi 18683 1 0 Feb 06 ?  9:47 ora_snp0_EPIPROD
```

Yukarıdaki listede gördüğünüz gibi, parelel komutunun işletim sistemi
seviyesinde etkisi oldu. İşletim sistemi 4 tane 'işlem' (process)
gösteriyor.  Şimdi bütün çizelgelerinizin paralel seviyesini ikiye
katlamadan önce iyi düşünün. Paralel işlemin en büyük yararları günlük
programlar içindir. Servis programlarının arkasında olan veri tabanını
değiştirmeden önce tartmak lazım; 30, 40 bin kayıt işleyen kod için,
bu değişiklik bir yarar sağlarmı?





