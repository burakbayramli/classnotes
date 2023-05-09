# Kurumsal Java Hatalar (1. Basım)

148 Alttan ikinci paragrafta "Throable" kelimesi Throwable

398 Enscript kurulumu Enscript kurmak için verilen adres artık geçerli
değil. Bu sebeple enscript kurulum kodlarını Kurumsal Java tools
dizini altına ekledik. Kurumsal Java ana sayfasından bu programı
edinebilirsiniz. Kurmak için yapılması gereken, sadece zip dosyasının
açılıp, c:/ seviyesine kopyalanmasıdır. Bundan sonra c:/enscript
PATH'e eklenmelidir.

192 OsCache Bölgeleri OsCache'de artık bölge ismi tanımlamak
mümkün. Kitapta gösterilen cache.capacity=100000cache.timeout=-1
yerine cache.Simple.capacity=10000cache.Simple.timeout=-1
kullanılırsa, Simple adlı bir bölge kullanılmış olacaktır.

190 Önbellek ve POJO'lar Bir Java class'ını herhangi bir önbellekte
(OsCache, EhCache) tutabilmek için, o class'ın equals() çağrısı
tanımlanmış olmalıdır. Java equals(), iki Java nesnesini
karşılaştırmak için kullanılır. Tipik bir equals fonksiyonu aşağıda
gösteriliyor.  public class Test {private int id;private String
name;public int getId() { return id;}public void setId( int newId) {
this.id = newId;}public String getName() { return name;}public void
setName(String newName) { this.name = newName;}public final boolean
equals(final Object obj) { if (obj == this) return true; if (obj ==
null || getClass() != obj.getClass()) return false; Test o = (Test)
obj; return (id == o.id) && (name == null ? o.name == null :
name.equals(o.name));}}

204 JBoss Kümesindeki Birimler Birbirini Görmüyor (Linux) JBoss kümesi
oluşturmak için IP'lerini DHCP ile almış makinalarda bir ek ayar daha
gerekebiliyor. Ayrı makinalarda tek kümeye dahil olmak üzere
başlatılmış JBoss'lar birbirini görmezse, sorun Linux'taki birimin
127.0.0.1 IP adresini kullanıyor olması olabilir. Bunun için Linux
makinasındaki JBoss'unuzun bin dizini altındaki run.conf dosyasında
JAVA_OPTS için -Dbind.address=192.168.x.x (192.168.x.x yerine kendi
makinanızın IP adresini koyun) değerini set etmeniz gerekecektir.  204
Küme Client Programı Tek Birime Bağlanıyor Suse Linux'ta ortaya
çıkabilecek bir durum, /etc/hosts içinde default olarak 127.0.0.2
adresinin linux ismine bağlanmış olması durumunda çıkmaktadır. Çözüm
olarak 127.0.0.2 yerine makinanın gerçek adresi kullanılırsa sorun
çözülecektir.





