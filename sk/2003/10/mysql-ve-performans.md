# MySql ve Performans

Aşağıda MySql üzerinde çalışan Java uygulamaları için bazı performans
tavsiyeleri.  JVM'den ölçüm değerleri almak için aşağıdaki seçeneği
java komutunuza ekleyebilirsiniz.

-verbose:gc -XX:+PrintGCTimeStamps

JVM yığıt belleğin olağan değeri 64 MB'tır, ve bu değer çoğu web
uygulaması için küçük kalmaktadır.

-Xms128m -Xmx256m

DB bağlantı havuzu (connection pool) için 15-20 bağlantı çoğu uygulama
için yeterli olmaktadır, elinizdeki Thread'lerin sayısından daha fazla
bağlantı açmamaya özen gösterin.  ConnectionJ 3.0, ConnectionJ
2.0.14'ten 40%-400% daha hızlı! En son sürücü kodlarını kullanmaya
özen gösterin.  JDBC URL üzerinden tanımlanıp fayda sağlayacak
parametreler şunlar.

* Yavaş işleyen sorguları kayıtlara geçirmek için logSlowQueries=true
ve slowQueryThresholdMillis=n (2000 olağan değer)

* Performans ölçümlerini toplamak: gatherPerfMetrics=true ve
reportMetricsIntervalMillis=n (30 saniyede bir olağan değer)

* Kullanım danışmanı (usage advisor), kullanılmayan nesneler, SELECT
sorgularında kullanılmayan kolonlar, yarıda kesilmiş Resultset
döngülerini raporlamak için: useUsageAdvisor=true Bir Theserverside
yazısından alınmıştır.





