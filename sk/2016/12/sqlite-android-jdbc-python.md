# SQLite, Android, JDBC, Python

Android cep telefonlarında SQL bazlı erişilebilen bir taban gerekirse
SQLite programı bu ihtiyacı karşılıyor. Bu taban çok hafif, hızlı
çalışır, taban olarak tek gerekli olan bir .db dosyasından
ibarettir. Dizüstü bilgisayarında da SQLite kullanılabiliyor, pek çok
dilden erişim var, Python, Ruby, Java, vs.. Diğer bazı özellikler
tablo indeksleyebilmek, birleşim (join), alt sorgular (subjoin) -
kuvvetli bir program.

Bizim için en çok gereken özellik dışarıdan yaratılan (mobil için)
büyükçe bir tabanı, 20 MB civarı, telefona kopyalayarak bu veriye
anahtar bazlı hızlı erişim sağlamak. SQLite bunu rahatça sağladı. Niye
koca bir JSON'u Android'den okuyup, mesela HashMap üzerinden, o
şekilde direk erişim yapmadık? Çünkü o şekilde tek bir obje için 20
MB'in tamamını hafızaya yüklemek gerekir, SQLite, her diğer saygıdeğer
ilişkisel tabanın yapacağı gibi, düzgün indekslendiği durumda disk'te
sadece gerekli yere gider ve gereken satırı yükler, tabanının tamamını
hafızaya yüklemeden.

Geliştirme ortamında veriyi Ubuntu'da Python ile yaratıyoruz. Dizüstü
Ubuntu ortamında kurmak

```
sudo apt-get install sqlite3 libsqlite3-dev
```

Basit kod

```
import sqlite3
conn = sqlite3.connect('taban.db')

c.execute('''CREATE TABLE vs (key text primary key not null, ... )''')

c.execute('INSERT INTO vs values (...)')
...

conn.commit()

conn.close()

```

Eger taban.db yoksa yaratilir. 

Android 

Yaratılan tabanı SD kartına kopyalarsak Android SQLite'i ona rahatça erişiyor. 

```
import android.database.sqlite.*;
import android.database.*;

..
f = "/storage/emulated/0/Downloads/taban.db"

SQLiteDatabase db = SQLiteDatabase.openDatabase(f, null, SQLiteDatabase.NO_LOCALIZED_COLLATORS);

Cursor c = db.rawQuery("SELECT key FROM vs where ..", null);

c.moveToFirst();

Log.d("cam", "key"+c.getString(0));

..

c.close();

db.close();
```

Bu kod işler, burada problem yok.

Fakat üstteki API JDBC değil, yani standart bir arayüz değil. Eğer
geliştirme ortamında birim testi yazmak istersek bu kod baş ağrısı
yaratacaktır. Gerçi üstteki kullanımın taklitleme (mocking) üzerinden
testi mümkün, ama bizce en iyisi Android üzerinde JDBC kullanmak. 

Şu arkadaşlar Android için bir JDBC kodu yazmışlar, 

https://github.com/SQLDroid/SQLDroid

Jar'i indirmek icin 

http://search.maven.org/#search%7Cga%7C1%7Csqldroid

Biraz onceki kodu

```
import java.sql.*;

..
try {
    Class clazz = Class.forName("org.sqldroid.SQLDroidDriver");
    DriverManager.registerDriver((Driver)clazz.newInstance());
} catch (Exception e) {
    e.printStackTrace();
}


String jdbcUrl = "jdbc:sqldroid:" + "/storage/emulated/0/Downloads/taban.db"

try {

    Connection conn = DriverManager.getConnection(jdbcUrl);

    Statement stmt = conn.createStatement();

    String sql = "SELECT key FROM vs where ..";

    ResultSet rs = stmt.executeQuery(sql);

    rs.next();

    Log.d("cam", "key"+rs.getString(1));

    rs.close();

    conn.close();         

} catch (SQLException e) {

    throw new RuntimeException(e);

}       
```

olarak değiştirebiliriz. Tabii SQLDroid jar'ını projemizin lib dizine
koymayı unutmuyoruz.

Dikkat: Android DB API'si kolonlara 0 indis bazlı erişiyor, JDBC 1
indis bazlı erişiyor.

Birim testler şimdi daha kolaylaştı; Android kodundaki veri erişimini
belli metotlara koyarız, bu metotlar dışarıdan bir taban bağlantısı
Connection alırlar; Android ortamından bu objenin gerçekleştirimi /
gerçek hali SQLDroıdConnection olur, geliştirme ortamında
org.sqlite.JDBC olur.. ama test edilen metot bunlardan habersiz bir
şekilde ona verilen Connection ile güzelce çalışır.

Dizustu icin JDBC

```
try {
    Class clazz = Class.forName("org.sqlite.JDBC");
    DriverManager.registerDriver((Driver)clazz.newInstance());
    String jdbcUrl = "jdbc:sqlite:" + "/vs/vs/taban.db";
    Connection connection = DriverManager.getConnection(jdbcUrl);
} catch (Exception e) {
    e.printStackTrace();
}
```

Not: Akla şu soru gelebilir, "Android'in kapalı API'sinin hangi jar'ı
kullandığını bulsam, o jar'ı dizüstü'nde kullansam, testi bu şekilde
işletebilir miyim?". Bu olmuyor, Android içindeki SQLite'i telefon
dışında işletmek mümkün değil. Android benzetici (emulator) ile tabii
tüm telefon fonksiyonları dizüstü'nde işletilebilir, ama bu da uzun
iş. Zaten o sebeple insanlar taklitleme işine girmişler, ama o da bize
göre külfetli.

Dizustu ortami icin Java JDBC SQLite jar'i

https://repo1.maven.org/maven2/org/xerial/sqlite-jdbc/3.8.9.1/

