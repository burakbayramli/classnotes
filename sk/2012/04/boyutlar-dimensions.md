# Boyutlar (Dimensions)

Dellstore ambar tasarimindan devam ediyoruz (bkz onceki yazi). Sadece
posta kodu tanimlamistik, simdi ETL ile zaten aldigimiz kredi kart
tipi (creditcardtype) ve cinsiyet (gender) uzerinde ek boyutlar
tanimlayalim. Bunlar da dejenere boyut olabilir. Direk xml dosyasina
girelim, ve

```
    <Dimension type="StandardDimension" highCardinality="false" name="creditcardtype">
      <Hierarchy name="creditcardtype" hasAll="true">
        <Level name="creditcardtype" column="creditcardtype" type="String" uniqueMembers="false"
                 levelType="Regular" hideMemberIf="Never">
        </Level>
      </Hierarchy>
    </Dimension>
    <Dimension type="StandardDimension" highCardinality="false" name="gender">
      <Hierarchy name="gender" hasAll="true">
        <Level name="gender" column="gender" type="String" uniqueMembers="false"
 
          levelType="Regular" hideMemberIf="Never">
        </Level>
      </Hierarchy>
    </Dimension>
```

Yayin sonrasi konsola gidiyoruz, ve mesela posta kodu 29435'e
bakiyoruz. Bu kod icin + tiklaninca, kredi kart tiplerine gore dagilim
gosteriliyor, cunku siralama olarak once kod, sonra kart var. Sonra
cinsiyet uzerindeki + tiklaninca M cikiyor, yani erkek (male)
musteriler. F cikmadi, demek ki bu kod, ve kart 3 ile alisveris yapan
hic kadin musteri olmamis. Kart kodu bu arada kredi kart numarasinin
ilk hanesi ve bilindigi gibi bu hane, 3,4 gibi sayilar Mastercard,
Visa, vs. gibi kredi kart sirketlerine tekabul ediyorlar.

Peki ya detaya inme sirasini degisik sekilde yapmak isteseydik? O
zaman sol ust kosedeki ikona yine tikliyoruz,

ve boyutlarin solundaki yukari, asagi ok isaretlerini kullanarak
siralamayi istedigimiz gibi degistiriyoruz. Mesela her seyi basasagi
dondurelim, ve konsola donunce farkli siralamanin raporlamayi
degistirdigini gorelim,

![](Screenshot+at+2012-04-26+14:28:41.png)

![](Screenshot+at+2012-04-26+14:29:29.png)

![](Screenshot+at+2012-04-26+14:32:42.png)

