# WebView ve CSS

WebView class'i ile Android telefonlarda, Internet'e cikmaya gerek
kalmadan apk dosyasi icine gomulebilecek statik html
gosterebildigimizi isledik. WebView aslinda tam tekmilli bir
tarayici. Bu yuzden goruntuye CSS uzerinden stil uygulayabilme
yetenegine de sahip. CSS kullanimi icin css stil dosyamizi daha once
bahsettigimiz HTML dosyalarinin gittigi assets/ altina birakiriz, ve
HTML dosyalarimizin basinda hepimizin bildigi "link rel" komutunu
uygulayarak css dosyasina referans veririz. CSS dosyasinin dizin
olarak yeri neresidir? assets/ altina birakilan dosya,
file:///android_asset/ diye baslamali, o zaman style.css kullanimi
icin ... link rel= .. href="file:///android_asset/style.css .. diye
giden bir kullanimdan bahsediyoruz. Bu kadar.
