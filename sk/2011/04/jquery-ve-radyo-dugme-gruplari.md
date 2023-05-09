# jQuery ve Radyo Dugme Gruplari

Radyo dugmeleri icinden sadece biri isaretlenebilecek secenekler icin
kullanilir. Fakat ayni sayfa uzerinde birden fazla grup istersek,
mesela secmeli bir sinav sayfasi icin diyelim, o zaman radyo dugme
gruplari kullaniriz. HTML ile:

```
<div>..<p>
<input type="radio" name="group0" value="1"/>
<input type="radio" name="group0" value="2"/>
<input type="radio" name="group0" value="3"/>
</p></div>

<div>...
<p>
<input type="radio" name="group1" value="1"/>
<input type="radio" name="group1" value="2"/>
<input type="radio" name="group1" value="3"/>
</p>
</div>
```

Gruplari birbirinden ayirmak icin "name" icin her grupta degisik
olacak bir isim vermek yeterli.Peki jQuery ile bu secenekleri nasil
okuruz? Soyle:

```
for (var i=0;i<[GRUP SAYISI];i++){
res = $("input[name='group"+i+"']:checked").val();...
}
```

Dongu icinde tum gruplar gezilecek, ve her grubun secilmis dugmesinin
degeri "res" icine atanacak.Deger atamasi nasil
yapilir?

```
$('input[name=group1]:eq(2)').attr('checked', 'checked');
```

Bu kodla 'group1' dugme grubunde '2' no'lu dugmeyi seciyoruz.

