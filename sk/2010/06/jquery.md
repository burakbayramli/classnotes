# jQuery

### jQuery ve Radyo Dugme Gruplari

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

### jQuery ile Girdi Tamamlamak (Auto Complete)

Form uzerinde metin bazli alanlarda girilecek birkac harf sonrasi
seceneklerin bize bir listede gosterilmesini istiyorsak (suggest,
autocomplete), suradaki jQuery plug-in ise yarayacaktir. Indirip
jquery.autocomplete.js ve jquery.autocomplete.css dosyalarini dizin
yapiniza ekleyin ve HTML dosyalari icinden "link href" ve "script src"
ile dahil edin. Kod kullanimi oldukca basit, form uzerindeki tipi text
olan input alaninin kimligini (id) alip, autocomplete koduna
geciyoruz. Eger id "kimlik1" olsaydi, jQuery ile

```html
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>jQuery UI Autocomplete - Default functionality</title>
  <link rel="stylesheet" href="//code.jquery.com/ui/1.13.2/themes/base/jquery-ui.css">
  <script src="https://code.jquery.com/jquery-3.6.0.js"></script>
  <script src="https://code.jquery.com/ui/1.13.2/jquery-ui.js"></script>
  <script>
  $( function() {
    var availableTags = [
      "ActionScript",
      "AppleScript",
      "Asp",
      "BASIC"
    ];
    $( "#tags" ).autocomplete({
      source: availableTags
    });
  } );
  </script>
</head>
<body>
 
<div class="ui-widget">
  <label for="tags">Tags: </label>
  <input id="tags">
</div>
  
</body>
</html>
```

kullanabilirdik. Degisken "liste" bir Javascript dizinidir; JSON ile
servis tarafindan alinmis olabilir, ya da yerel olarak gomulu
(hardcoded) bir liste olabilir, vs.

