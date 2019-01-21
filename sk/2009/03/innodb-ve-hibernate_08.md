# InnoDB ve Hibernate


InnoDB ve Hibernate



MySQL her tablonun depolanma seklini secmenize izin verir. Depolanma sekli (storage engine) secenekleri MyISAM, InnoDB gibi secenekler. Bu seceneklerden InnoDB islemsel (transactional) depolama yapabilmenize izin verir, MyISAM FULLTEXT kavramini kullanmaniza izin verir, vs. Her iki metotun kendine has guclu ve zayif taraflari vardir. Mimari secim olarak bizim tavsiyemiz islemsel olan InnoDB kullanilmasi, arama icin Lucene paketinin kullanilmasi. MyISAM kullanimini tavsiye etmiyoruz.Her tabloyu yaratirken depolama seklini CREATE TABLE komutuna ekleyerek secebiliyoruz. Fakat eger tablolari Hibernate'e otomatik olarak yarattiriyorsak, yani DDL'i Hibernate uretiyorsa, o zaman istedigimiz depolama seklini bir sekilde Hibernate'e bildirmemiz lazim.Yeni bir dialect yaratarak bunu cozebiliriz:package vs.vs;import org.hibernate.dialect.MySQL5InnoDBDialect;public class CustomMysqlDialect  extends MySQL5InnoDBDialect { public String getTableTypeString() {     return " ENGINE=InnoDB DEFAULT CHARSET=utf8"; }} persistence.xml icinde <persistence ...     ....     <property name="hibernate.dialect" value="vs.vs.CustomMysqlDialect">     ....</persistence> Kaynak




