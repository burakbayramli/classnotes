# Dizine Eklemek, Bolmek


Dizine Eklemek, Bolmek



Iki dizini ust uste dizmek (stacking) icin Python vstack (dikey) ve hstack (yatay) fonksiyonlari var. Bu fonksiyonlar ilginc sekillerde kullanilabiliyor: Mesela [100,100] 2 boyutlu baslangic noktasindan x-kord. ikiser ikiser, y-kord. ucer ucer buyuyecek sekilde 5 tane veri noktasi uretmek istesek:x0 = array([100, 100])xs = vstack((arange(5)*3, arange(5)*2)).T + x0yeterli.  arange(N) O..N-1 arasinda sayilari uretir. Bu sayilarin hepsini 3 ile carpiyoruz. Sonra aynisini yapip 2 ile carpiyoruz. Bu iki dizini ust uste "yigiyoruz", ve .T cagrisi ile devrigini (transpose) aliyoruz, boylece [5,2] boyutlu veri noktalarini elde ediyoruz. Tum bunlara x0 baslangic degerini ekleyince istedigimiz sonuc geliyor.[[100 100][103 102][106 104][109 106][112 108]]Bir dizini "kesmek" icin slice() fonksiyonu var.a = [1,2,3,4,5,6,7,8,9]sl = slice(2,8,2)Ustteki slice tanimi 2. oge ile 8. oge (haric olmak uzere) arasindaki tum elemanlari geri getirir. Eger son 3. parametre "2" verilirsek, bu  "ikiser ikiser git" anlamina geliyor, yani bir oge surekli atlanir. print a[sl] cagrisi bize [3, 5, 7] sonucunu dondurecek.Ayni cagri print a[2:8:2] seklinde de gerceklestirilebilir. Bazen degiskenler kullanilarak slice() objeleri yaratmak gerekebiliyor, bu durumlarda slice() cagrisi tercih edilmekte.




