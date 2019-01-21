# GPU




GPU



Bilgisayarimizda hesaplari yapan islemci var, bu islemci son zamanlarda cok cekirdekli hale de gelmeye basladi. Fakat bilgisayarimizda islem yapan cok kuvvetli bir parca daha var: grafik islemci (GPU).GPU paralel islem acisindan neredeyse pur mikroislemci kadar kuvvetlidir, hatta bazi acilardan daha hizlidir, cunku tarihsel sebeplerle parallellige daha fazla yatkin olmasi gerekmistir. GPU bir goruntunun hizla cizilmesi (rendering) icin piksel bazinda paralellige gitmek zorundaydi ve NVIDIA sirketinin urunleri icin artik bu normal bir operasyondur.Arastirmacilar bu paralellikten istifade etmeye karar vermisler, ve grafiksel olmayan hesap islemlerini sanki oyleymis gibi GPU'ya sunuyorlar, ve cevabi geri tercume ediyorlar, boylece GPU'nun hizli paralel islemci ozelliginden faydalaniyorlar. Pek cok matematiksel hesabi bu sekilde yaptiran olmus, mesela matris carpimi, PDE cozumu, simulasyon.NVidia sirketi grafik kartlarinin GPU'suna erisim icin CUDA diye bir kutuphane sagliyor. Onun ustune PyCUDA ile Python bazli erisim de var. Cin universiteleri CUDA egitimini universitelerinin mufredatina dahil etmisler.Dikkat: NVidia karti piyasadaki grafik kartlarindan bir tanesidir, her laptop uzerinde NVidia olmayabilir (fakat NVidia piyasadaki en unlulerden birisi, bunu da ekleyelim). Simdilik GPU kodlamasi icin NVidia kartina sahip bir bilgisayar lazim, ve oldukca yeni modeller gerekiyor.Altta konu hakkinda bir video:




