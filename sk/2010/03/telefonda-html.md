# Telefonda HTML

Mobil ortamda HTML apk paketi icinde yeralan HTML icerigi gostermek
icin Android API icindeki WebView class'i kullanilir. Duz metni
suslemek icin Android icinde API kullanmak yerine, tur islerin
tamamini HTML'de yapabiliriz; boylece yeni bir paket ogrenmeye gerek
kalmaz. Tanidik bildik etiketler metin suslemek icin
kullanilabilir.

APK icinde html dahil etmek icin bu dosyalari en ust seviyede assets/
adli bir dizine koymak yeterli. Bundan sonra bu dosyalara
android_asset adindaki bir referanstan kod icinde
erisebiliyorsunuz. Ornek.html adindaki bir dosya soyle
gosterilebilir:

```
public class DetailView extends Activity {
   @Override public void onCreate(Bundle savedInstanceState) {j
        super.onCreate(savedInstanceState); ...
	WebView webview = new WebView(this);
	setContentView(webview);
	webview.loadUrl("file:///android_asset/ornek.html");
   }
}
```
