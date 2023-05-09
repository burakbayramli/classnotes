# jQuery ile Girdi Tamamlamak (Auto Complete)

Form uzerinde metin bazli alanlarda girilecek birkac harf sonrasi
seceneklerin bize bir listede gosterilmesini istiyorsak (suggest,
autocomplete), suradaki jQuery plug-in ise yarayacaktir. Indirip
jquery.autocomplete.js ve jquery.autocomplete.css dosyalarini dizin
yapiniza ekleyin ve HTML dosyalari icinden "link href" ve "script src"
ile dahil edin. Kod kullanimi oldukca basit, form uzerindeki tipi text
olan input alaninin kimligini (id) alip, autocomplete koduna
geciyoruz. Eger id "kimlik1" olsaydi, jQuery ile

```
$().ready(function() {
   var liste = ...;
   $("#kimlik1").setOptions({ max: 20 });
   $("#kimlik1").focus().autocomplete(liste);
});
```

kullanabilirdik. Degisken "liste" bir Javascript dizinidir; JSON ile
servis tarafindan alinmis olabilir, ya da yerel olarak gomulu
(hardcoded) bir liste olabilir, vs.





