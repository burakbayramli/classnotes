# Ant

Ant, bizi Makefile kavramından kurtardığı için sevdiğimiz
teknolojilerden biridir. Makefile'lar ile uğraşan programcılar bilir:
Eğer Makefile içinde bir TAB karakteri unuttuysanız, birden hiçbir şey
çalışmaz. O TAB'i bulana kadar da saatler geçer. Geliştirme ortamını
make ile kurmak zorunda olan her teknik liderin bu başına gelmiştir.
Java dünyası temiz bir sayfa ile başlayarak Ant sistemini kurdu. Tabii
Ant'inde eksikleri vardı. Uzun yıllar bu eksikleri antmerge ile
kapatmaya çalıştık. Ve nihayet artık Ant sürüm 1.6 ile beklediğimiz
özelliklere kavuştuk.  Bu özelliklerden biri bir build.xml'in bir
diğerini referans edebilmesidir, bir nevi #include, ya da import
denilebilir. Bu yeni etiket olarak biliniyor. Böylece her projenin
ortak hedefleri (target) tek bir yerde toplanabiliyor. Meselâ,
build_ortak.xml adlı aşağıdaki Ant programı olabilir.

```
<project basedir="." name="build_ortak">

<!-- ========== Isletilebilir Hedefler ======================================== -->

<target description="Ortami sifirla" name="init" depends="project_init">

<!-- build.properties'den degiskenleri oku (eger varsa) -->

<property name="build.home" value="build"/>
<property name="build.compiler" value="javac1.4" />
<property name="test.build.home" value="build/test"/>
<property name="dist.home" value="dist"/>
<property name="source.home" value="src/main"/>
<property name="source.test" value="src/test"/>
<property name="compile.debug" value="true"/>
<property name="compile.deprecation" value="true"/>
<property name="compile.optimize" value="true"/>

<property file="${basedir}/build.properties"/>

<path id="compile.classpath">
<pathelement location="${build.home}/classes"/>
<fileset dir="./lib">
<include name="*.jar"/>
</fileset>

</path>

<path id="javadoc.path">
<pathelement path="${source.home}"/>
</path>
</target>

<target description="Proje seviyesinde hazirlanma evresi" name="project_prepare" />

<target depends="init,project_prepare" description="Prepare build directory" name="prepare">
<mkdir dir="${build.home}"/>
<mkdir dir="${build.home}/classes"/>
<mkdir dir="${test.build.home}/classes"/>
</target>

<target depends="prepare" description="Compile source" name="compile">
<javac debug="${compile.debug}" deprecation="${compile.deprecation}"
destdir="${build.home}/classes" target="1.4" source="1.4"
optimize="${compile.optimize}" srcdir="${source.home}">
<classpath refid="compile.classpath"/>
</javac>
</target>

<target description="proje seviyesindeki hazirlanma evresi" name="project_clean" />

<target depends="init,project_clean" description="Wipeout all generated files"
name="clean">
<delete dir="${build.home}"/>
<delete dir="${dist.home}"/>
</target>

<target depends="clean,compile" description="Bilesenleri temizle ve tekrar derle"
name="all"/>

<target depends="compile" description="Javadoclari uret"
name="javadoc">
<mkdir dir="docs/api"/>
<javadoc author="true"
bottom="Pyrasun Java Libraries - Pyrasun Logging"
destdir="docs"
source="1.4"
doctitle="${component.title}"
packagenames="pyrasun.*"
access="public"
sourcepathref="javadoc.path" version="true"
windowtitle="${component.title} (Version ${component.version})"/>
</target>

<target depends="compile" name="build-test">
<javac debug="${compile.debug}" deprecation="${compile.deprecation}"
destdir="${test.build.home}/classes" target="1.4" source="1.4"
optimize="${compile.optimize}" srcdir="${source.test}">
<classpath refid="compile.classpath"/>
</javac>
</target>

<target depends="compile" description="Islerkod uret" name="jar">
<mkdir dir="${dist.home}"/>
<mkdir dir="${build.home}/src"/>

<copy file="LICENSE" todir="${build.home}/classes"/>

<jar basedir="${build.home}/classes"
jarfile="${dist.home}/${component.name}-${component.version}.jar" >
<include name="**/*"/>
</jar>
</target>

</project>
```

Bu build dosyası oldukça basit. Birkaç olağan değer tanımlıyor, ondan
sonra da prepare, init, clean, compile, jar, ve javadoc hedeflerini
tanımlıyor. Nükleer fizik değil yâni. Bu ortak build.xml'in yararı
şurada ortaya çıkıyor. Aşağıdaki projeye özel bir build.xml:

```
<project basedir="." default="compile" name="EmberIO">

<!-- ========== Executable Targets ======================================== -->

<target description="Ortami sifirla" name="project_init">
<property name="component.name" value="XYZKutuphanesi"/>
<property name="component.package" value="xyzlib"/>
<property name="component.title" value="XYZ Kutuphanesi"/>
<property name="component.version" value="0.1_Beta"/>
</target>
<import file="../build_ortak.xml" />
</project>
```

Üstteki, proje bazlı bir build.xml dosyası. Ne kadar boş olduğunu görüyoruz. Çünkü çoğu ortak hedefleri build_ortak.xml'den alıyor!
Dikkat edilmesi gereken bazı noktalar şunlar:

* Build_ortak.xml içinde project_init, project_prepare gibi proejeye
  özel hedefler boş olarak tanımlanmış. Aynı xml içindeki prepare,
  project_prepare'a bağımlı (depends ilişkisi). Init hedefi,
  project_init'e bağımlı. Böylece proje bazındaki project_prepare ve
  project_init bu hedefleri doldurduğu anda, zâten tanımlı prepare ve
  init tarafından otomatik olarak çağırılacaklardır.

* Build_ortak.xml, build.properties dosyasını mevcut ise okur. Bu ince
  bir noktaya parmak basıyor. etiketi, tüm referansları import edene
  göre tekrar anlamlandırıyor, yani build.properties'in yüklemek için
  diskte aranması build_ortak.xml'in dizininde değil, build.xml'in
  (import eden) dizinine göre yapılacaktır.


