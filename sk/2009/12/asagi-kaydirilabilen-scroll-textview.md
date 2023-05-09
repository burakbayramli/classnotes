# Asagi Kaydirilabilen (Scroll) TextView

Cep telefonu uygulamizda bir TextView duz metin basabilmemizi
saglar. Ekrana sigmayacak kadar buyuk metinleri gosterebilmek icin
asagi dogru gitme destegi lazim. Bu destek soyle eklenir:

```
public void onCreate(Bundle savedInstanceState) {
  super.onCreate(savedInstanceState); ScrollView sv = new
  ScrollView(this); TextView tv = new TextView(this); sv.addView(tv);
  ...
 }
```




