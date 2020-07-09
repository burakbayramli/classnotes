# Icerik Guncelleme


Icerik Guncelleme



JBoss ile gelistirme yaparken sadece xhtml sayfalari, css dosyalari degisti ise, sadece bu degisen icerigi JBoss'a gondermek yeterli; EAR derlemesi yapip, JBoss'un tekrar baslatilmasina gerek yok. Hot-deploy devrede olsa bile bu baslatma uzun zaman alabiliyor. Icerik guncellemesi icin bir jboss_tmp_dir.py adinda bir Python script ve bazi build.xml degisiklikleri yeterli.import shutilimport osimport globbasedir = "[JBOSS DIZIN YERI]"dir = basedir + "/server/default/tmp/deploy"os.chdir(dir)list = glob.glob('*[SIZIN EAR ISMINIZ]*contents*')list.sort()if len(list) < 1: raise RuntimeErrorto = dir + "/" + list[len(list)-1] + "/" + "pocketbudda-exp.war"print toBu script'i build.xml icinden soyle cagirabiliriz:  <target name="x">  <exec outputproperty="jboss.tmp.dir" executable="python">    <arg line="src/script/jboss_tmp_dir.py"/>  </exec>  <copy overwrite="yes" toDir="${jboss.tmp.dir}">    <fileset dir="src/view"/>  </copy></target>Python script icinde JBoss'un sayfalari EAR'den cikartip koydugu tmp altindaki dizinin nerede oldugunu buluyoruz. Bu dizinin yeri EAR her yuklendiginde degistigi icin eger buraya parasutle dosya atacaksak her seferinde nerede oldugunu tekrar bulmamiz gerekiyor.Ayrica yukarida Ant ve Python arasindaki baglantinin nasil kurulacagini da goruyoruz. outputproperty bu amacla kullaniliyor, Python icinde "print" ile ekrana basilan her sey, Ant'e outputproperty icine set edilecektir.




