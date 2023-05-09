# Aktiviteler Arasi Gecis

Android kodlamasinda bir ekran bir Activity objesine tekabul eder. Bir
Activity'den otekine gecis su sekilde oluyor;Intent myIntent = new
Intent();myIntent.setClassName("[paket]", "[tum paketle beraber gecis
yapilan class]");startActivity(myIntent); Peki gecis yaparken bir
yandan bilgi aktarmak istersek ne yapariz? En basit isleyecek cozum,
ya kaynak Activity'ye ya da hedef Activity'ye bir "public static" oge
eklemek ve gecis yapmadan once gereken veriyi bu ogeye statik olarak
set etmek. Gecis sonrasi gidilen Activity ayni statik erisimi yaparak
gereken bilgiyi alacaktir.Static erisimin kullanilmasi cok kullanicili
bir ortam olan Web kodlamasi icin uygun olmayabilirdi. Fakat
unutmayalim, cep telefonu kodlamasinda tek kullanicili bir ortamdayiz,
ve uygulamada donen her turlu islem, veri atamasi tek kullanici icin
yapiliyor. Bu yuzden static kullanimin tehlikesi yok.





