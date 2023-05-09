# GraphHopper: Pür Java ile Haritada Yol Bulmak

Yol bulmak için OSRM projesinden bahsettik. OSRM çok hızlı çalışır,
sonuçları kullanışlı, fakat C++ ile yazılmış, eğer pür Java bazlı bir
çözüm istiyorsak (ki böylece kodları mesela Android'e rahatça
koyabilelim) o zaman Graphhopper projesi var. GH ismi bir kelime
oyunu, grasshopper bilinebileceği gibi çekirge demek, graph (yani
çizit) hopper "bir çizitte sağ sola zıplayan" bir görüntü zihinde
canlandırıyor, ki bu görüntü pek gerçekten uzak değil, haritada yol
bulma algoritmaları hakikaten bilgisayar bilimdeki çizit yapısını
temel alıyorlar. Kod şurada,

https://github.com/graphhopper/graphhopper

Kodun derleme sistemi gradle / Maven bazlı, fakat o kadar çetrefil
derleme işlemlerine gerek yok, önce GH içinden gerekli java kodlarını
çıkartalım, sonra çok baz bir derleme sistemi ile ve ek birkaç jar ile
derlemeyi kendimiz dışarıda halledelim. Alternatif bir proje / dizin
yaratılır, altında lib, src, resources dizinleri olur. GH indirilir,
şimdi alternatif dizin altında

```
cp -r [GH]/core/src/main/java/* src/
cp -r [GH]/core/src/main/resources/* resources
cp -r [GH]/reader-osm/src/main/java/* src/
```

ile sadece gerekli Java kodlarını alırız. Şimdi gereken jarları alalım, 

https://mvnrepository.com/artifact/com.carrotsearch/hppc/0.7.2

https://mvnrepository.com/artifact/com.fasterxml.jackson.core/jackson-annotations/2.8.8

https://mvnrepository.com/artifact/com.fasterxml.jackson.core/jackson-databind/2.8.8

https://mvnrepository.com/artifact/com.vividsolutions/jts/1.13

https://mvnrepository.com/artifact/org.openstreetmap.osmosis/osmosis-osm-binary/0.45

https://mvnrepository.com/artifact/com.google.protobuf/protobuf-java/3.3.1

https://mvnrepository.com/artifact/org.slf4j/slf4j-api/1.7.21

https://mvnrepository.com/artifact/javax.xml.stream/stax-api/1.0-2

https://mvnrepository.com/artifact/org.apache.xmlgraphics/xmlgraphics-commons/1.4

Bu jarları nasıl bulduk? Ya derlerken olmayan jar için gelen hata
mesajlara bakıp bu class ismini "vs.vs.Class maven jar" ile Google'da
ararız, ya da [GH]/pom.xml ya da hangi pom.xml var ise versiyon
sayısını oradan buluruz ve yine Google'da ararız. Mesela
jackson-databind lazım, "jackson-databind maven jar" bize gerekli
bağlantıyı verir. Zaten görüldüğü gibi bağlantılarda belli bir kalıp
da var, class ismi ve versiyon ile bağlantı tahmin de edilebilir.

Jar'lar lib altına gidiyor tabii. Derleme için Ant build.xml

```
<project name="gh" default="dist" basedir=".">
  <property name="src" location="src"/>
  <property name="build" location="build"/>
  <property name="dist" location="bin"/>
  <target name="init">
    <tstamp/>
    <mkdir dir="${build}"/>
  </target>
  <path id="build.classpath">
    <fileset dir="lib">
      <include name="**/*.jar"/>
    </fileset>
  </path>
  <path id="compile.classpath">
    <pathelement location ="${build}"/>
    <fileset dir="lib">
      <include name="**/*.jar"/>
    </fileset>
  </path>
  <target name="compile" depends="init"
        description="compile the source">
    <javac destdir="${build}">
      <src path="${src}"/>
    <classpath refid="build.classpath"/>
  </javac>
  </target>
  <target name="resources">
    <copy todir="${build}" includeEmptyDirs="no">
      <fileset dir="resources">
        <patternset>
          <include name="**/*.*"/>
        </patternset>
      </fileset>
    </copy>
  </target>
  <target name="dist" depends="compile" description="generate the distribution">
    <mkdir dir="${dist}"/>
    <jar jarfile="${dist}/ghopper.jar" basedir="${build}"/>
  </target>
    <target name="test" depends="compile">
      <java fork="yes" classname="Test" failonerror="true">
        <classpath refid="compile.classpath"/>
      </java>
  </target>
  <target name="clean"
        description="clean up">
    <delete dir="${build}"/>
    <delete dir="${dist}"/>
  </target>
</project>
```

Bu kadar, ant ile derleriz. Kodların işleyişi için OSM harita
dosyaları gerekli, bu dosyalar daha önce de bahsedilen 

http://download.geofabrik.de

sitesinden alınabiliyor. Mesela Türkiye için 

http://download.geofabrik.de/europe/turkey-latest.osm.bz2

Bu dosyayı bunzip2 ile açıp gzip ile tekrar zipleyelim, çünkü GH .gz
dosyası istiyor. Ya da .osm dosyası olduğu gibi bırakılır, GH onu da
işler.

GH sürekli direk .gz dosyasını da kullanmaz bu arada, onu işleyip bazı
ara dosyalar üretmesi lazım (daha önce bahsedilen çizit dosyaları
bunlar, GH OSM formatını alıp daha hızlı işleyiş için çizit yapısına
geçiyor). Ara dosyaların üretilmesi ve yol bulma testi çin src altında
Test.java yaratalım, 

```
import com.graphhopper.*;

import com.graphhopper.routing.util.EncodingManager;

import com.graphhopper.reader.osm.GraphHopperOSM;



public class Test {



    static String tmpGraphFile = "/tmp/gh";



    public static void createGraph() {

        String fin = "[DB DIZINI]/turkey-latest.osm.gz";

        GraphHopper gh = new GraphHopperOSM().setStoreOnFlush(true).

            setEncodingManager(new EncodingManager("foot")).setCHEnabled(false).

            setGraphHopperLocation(tmpGraphFile).

            setDataReaderFile(fin);

        gh.importOrLoad();

    }



    public static void testPath() {

        GraphHopper gh = new GraphHopperOSM().

            setEncodingManager(new EncodingManager("foot")).setCHEnabled(false).

            setGraphHopperLocation(tmpGraphFile);

        gh.importOrLoad();

        GHResponse res = gh.route(new GHRequest(40.987659, 29.036428, 
                                                40.992186, 29.039228).
                                 setVehicle("foot"));

        System.out.println(""+res );

    }



    public static void main(String[] args) throws Exception{

        createGraph();

        testPath();

        testPath();

    }

}
```

Isletmek icin ant test. Araba tarifleri için setEncodingManager ve
setVehicle çağrılarında car kullanılır. Kodda createGraph'ın sadece
bir kez çağırılması yeterli, bu çağrı /tmp/gh altında gerekli çizit
dosyalarını yaratır (çağrı birkaç dakika sürebilir), diğer tüm yol
bulma çağrıları bundan sonra çizit dosyalarını kullanıp hızlı şekilde
cevabı üretir. Artık elimizde pür Java bazlı, çok hızlı işleyen,
mobilde İnternet bağlantısız çalışabilecek harita tarif kodları
var. Üstteki kodların kendisi fazla yer tutmuyor zaten (biraz da bunun
için daha büyük GH içinden sadece gerekli java ve jar dosyalarını
çekip çıkardık, az sayıda java ve 10 tane jar dosyasi ile elimize yol
tarifi yapabilen çekirdek bir kod geçti), çizit dosyaları ise bir
şehrin büyüklüğüne göre 10-20 MB arasında olur.

Kodun teorik, algoritmik yapısını merak edenler için: kısa yol
algoritmaları çoğunlukla Dijkstra algoritmasını kullanırlar
(Dijkstra'yı bilgisayar bilim notlarında isledik), OSRM ve Graphhopper
da böyle yapıyor, yanlız Dijkstra yaklaşımına bazı ekler yapıyorlar,
yaklaşımın adı "kısalan hiyerarşiler (contracting hierarchies)"; yol
ağ yapısına bazı noktalarda haritaya dışarıdan kısa yol / zıplamalar
ekliyorlar, böylece Dijkstra daha hızlı, verimli işliyor.

Dil, derleme hakkında bazı bilgiler:  






