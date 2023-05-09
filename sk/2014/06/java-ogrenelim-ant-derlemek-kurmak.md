# Java Öğrenelim, Ant, Derlemek, Kurmak

Blog'da Java'yı dil seviyesinde işlemiyoruz, bunun için çok iyi
kaynaklar var, mesela Altuğ Altıntaş'ın kitabı,

http://www.kitapyurdu.com/kitap/java-programlama-dili-ve-yazilim-tasarimi/53209.html

İki tane bedava PDF temelli kitap, ki kuruluşu, temel dili Windows seviyesinde gösteriyorlar, 

http://web.cs.hacettepe.edu.tr/~bbm102/misc/java_notes_by_oa.pdf

http://turhancoban.com/kitap/JAVA%20B%C4%B0LG%C4%B0SAYAR%20D%C4%B0L%C4%B0YLE%20PROGRAMLAMA.pdf

Bizim tavsiyemiz her türlü geliştirmenin Ubuntu Linux üzerinde yapılması. Java kurmak için 

sudo apt-get install openjdk-8-jre

En basit derleme şekli komut satırında, bir script içinden javac kullanmak. Aynı dizinde olan bir Test.java için

```
export CP=/bir/dizin/lib1.jar:.

javac -classpath $CP Test.java 

java -classpath $CP Test
```

CP içinde nokta kullanımına dikkat, nokta Unix'te mevcut dizine işaret
eder. /bir/dizin/lib1.jar herhangi bir dizindeki jar dosyasına
referans için, daha fazla varsa onları iki nokta üst üste ardından
listeye ekleriz. Test.java içinde

```
import java.util.ArrayList;

public class Test 

{

    public static void main(String[] args) throws Exception{

        System.out.println("Hello World");

        ArrayList<String> l = new ArrayList<String>();

        l.add("1");

        l.add("2");

        System.out.println("Bir liste "+l );

    }

}
```

yeterli. İki üstteki üç satırı ayrı ayrı komut satırında ya da bir
script içinde işletince derleme ve işletim sonuçlarını görürüz. Aynı
dizinde bir .class dosyası da yaratılmış olmalı.

Eğer paket sistemi kullanıyor olsaydık, mesela üstteki kod
sample/src/com/vs/Test.java içinde olsaydı ve ilk satırında package
com.vs; ibaresi olsaydı, o zaman kod bir com.vs paketinde demektir,
derlemek ve işletmek için 

```
export CP=/bir/dizin/lib1.jar:./sample/src

javac -classpath $CP ./sample/src/com/vs/Test.java 

java -classpath $CP com.vs.Test
```

gerekirdi. Üsttekiler çok basit derlemeler için (ama bilmek faydalı),
daha çetrefil derlemeler için Ant var. 

```
sudo apt-get install ant
```

ile kurulur. Proje dizini sample altında yine src olsun ve bir de lib
dizini var. Üstteki proje yapısını derlemek için bir build.xml
projenin en üst seviyesine konur ve icinde, 

```
<project name="sample" default="dist" basedir=".">

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

  <target name="dist" depends="compile" description="generate the distribution">

    <mkdir dir="${dist}"/>

    <jar jarfile="${dist}/sample.jar" basedir="${build}"/>

  </target>

    <target name="test" depends="compile">

      <java fork="yes" classname="com.vs.Test" failonerror="true">

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

olabilir. Script ant komutu ile derlenir, testi işletmek için ant
test. İşlem ardından mesela bin altında projemizin class kodları
jar'lanmış halde olacak (böylece dışarıdan başka projeler bizim
jar'imizi kendi projelerine dahil edebilirler). Ayrıca pek çok
CLASSPATH ayarı Ant tarafında otomatik olarak yapıldı.

Ant, derleme, vs. ile ilgili bazı ek bilgiler bizim kitabın ek
bölümünde bulunabilir. 

