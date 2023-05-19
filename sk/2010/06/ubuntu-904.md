# Ubuntu 9.x

Caps tuşunu Ctrl yapma çözümü: System | Preferences | Keyboard'dan
Layouts tab, oradan USA klavyesi (bizim kullandığımız) ve o listeden
de Ctrl key position seçeneğinde "make Caps lock an additional
Ctrl".

Wifi adaptoru için bizim Toshiba Satellite için ve 9.04 kernel header
dosyaları ile uyumlu olan `compat-wireless` paketi 2.6.33. Ubuntu 9.10
üzerinde compat-wireless paketine gerek yok.Python versiyonu değişmiş,
2.5 yerine 2.6; o sebeple Scipy/Numpy, ve diğer bilimum paketleri
tekrar kurmak lazım. Dert değil. Eğer 8 -> 9.04 -> 9.10 geçişini hep
güncellemeler üzerinden yaptıysanız, makina kendini
kaybedebilir. Aslında en iyisi önemli dosyaları yedekleyip 9.10
versionunu diskten sıfırdan kurmak.Ubuntu 9.x versiyonlarında pil
üzerinde çalışıyorsanız, sistem "Battery Discharging" gibi bir mesajı
rahatsız edici bir popup, dialog, vs. içinde sürekli ekrana
basıyor. Bundan kurtulmak için gconf-editör komut satırından
başlatılacak. Sonra gconf-editor | apps | gnome-power-manager |
notify'a gidip dischargıng kutusundaki işareti iptal etmek gerekiyor.

Sonra "threshold" altındaki `percentage_critical` ve `percentage low`
değerlerini `0` yapın, `general` altındaki `use_time_for_policy` ve
`use_profile_time` kutularını iptal edin.Ubuntu başlayınca çalan davul
sesini kapatmak için Preferences -> Startup Applications'a gidin ve
Gnome Login Sound seçeneğini bulun. Bu öğenin sol tarafındaki seçeneği
iptal edin. Eğer bu ise yaramazsa, davul sesinin ses dosyası

`/usr/share/sounds/ubuntu/stereo/dialog-question.ogg`.

Bu dosyayı silip, sonra `sudo touch dialog-question.ogg` komutu ile
boş bir dosya yaratabilirsiniz, bu dosyada hiçbir ses olmadığı için
login esnasında hiç ses çıkmamış olur.Daha detay çözdükçe buraya
ekleyeceğiz.

Ubuntu'da Internet'e bağlanma, güç seviyesi, vs. ile alakalı mesajlar
masaüstünde balon mesajlar olarak çıkıyor, ve kullanıcı dikkatini
çekmeye uğraşarak bazen rahatsız edici olabiliyorlar. Balon
mesajlarını kaldırmak için şu yeterli:

```
sudo mv /usr/share/dbus-1/services/org.freedesktop.Notifications.service /usr/share/dbus-1/services/org.freedesktop.Notifications.service.disabled
```

Bir sonraki açılmada sonra mesajların bir daha gelmediğini göreceğiz.

