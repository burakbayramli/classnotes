# Oracle Veri Erişim nasıl hızlandırılır


Oracle Veri Erişim nasıl hızlandırılır



 Eger cok buyuk veri tabani uzerinde, veri islemi yapiyorsaniz,  bu yazimiz isinize yarayabilir. Genelde CRM sistemleri icin veriler ambara alinmadan once, bir on-temizlikten gecirilir. Mesela eger "butun isim" hanesi, ilk isim, soyisim diye ayrilmamissa, bir ufak program bu isi ambardan once yapabilir. Fakat bu islemi 4 milyon kayit uzerinde yapiyorsaniz, uzun zaman bekleyeceksiniz! Buradaki teknikler size yardimci olabilir.              Oracle veri erisim, daha once dedigimiz gibi, SQL kodu ile yapilir. Oracle bu dil uzerinde bazi uzatmalar yapmistir; Eger gerekirse, SQL veri erisim komutlari 'ayni anda birden-fazla' (paralel) sekilde isletilebilir. Bu ozellik icin tabii ki programci bu ozel SQL komutlarini bir sekilde Oracle veri tabanina iletmeli.               Genelde en cok kullanilan metod, bu paralel degisikligini 'cizelge bazinda' belirtmektir. Mesela MUSTERI cizelgesini paralel seviye = 4 diye tanimladiysaniz, bu kayida erisen her kod, 4 kopya halinde isletilir, ve bu kopyalar ayni anda isletilirler.   CREATE TABLE MUSTER PARALLEL DEGREE 4(HANE1    VARCHAR2(10),HANE2    VARCHAR2(30))SELECT * from MUSTERI; -- iste bu erisim, 4 kopya ile, daha hizli geri doner.              Bunun isletim sistemi seviyesindeki etkilerini gormek icin Unix seviyesinde  ps -eaf   komutunu isletin. Sonuclari mesela suradaki gibi olsun.  oraclesi 18578     1  0   Feb 06 ?        0:01 ora_p001_ESIPRODoraclepi 18667     1  0   Feb 06 ?       158:59 ora_dbw0_ESIPRODoraclesi 18578     1  0   Feb 06 ?        0:01 ora_p002_ESIPRODoraclesi 18578     1  0   Feb 06 ?        0:01 ora_p003_ESIPRODoraclesi 18578     1  0   Feb 06 ?        0:01 ora_p004_ESIPRODoraclepi 18683     1  0   Feb 06 ?        9:47 ora_snp0_EPIPROD              Yukaridaki listede gordugunuz gibi, parelel komutunun isletim sistemi seviyesinde etkisi oldu. Isletim sistemi 4 tane 'islem' (process) gosteriyor.               Simdi butun cizelgelerinizin paralel seviyesini ikiye katlamadan once iyi dusunun. Paralel islemin en buyuk yararlari gunluk programlar icindir. Servis programlarinin arkasinda olan veri tabanini degistirmeden once tartmak lazim; 30, 40 bin kayit isleyen kod icin, bu degisiklik bir yarar saglarmi?




