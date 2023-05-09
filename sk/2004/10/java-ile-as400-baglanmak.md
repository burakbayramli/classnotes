# Java ile AS/400'e Bağlanmak

IBM, ilk bilgi işlem ortamını oluşturan anabilgisayar (mainframe)
makinaları sayesinde uzun süre gündemde kalmış tanınan bir
şirkettir. Yavaş yavaş mikrobilgisayarların güçlenmesi, ve kümelenerek
bir öbek halinde birarada işlem yapabilmeleri de mümkün hâle
gelmesiyle, anabilgisayarlara istek, eskisine göre daha azalmaya
başladı.
  
Fakat bu talep yokolmadı. Özellikle İnternet ortamı gibi dağıtıklık
kavramının borusu öttüğü bir zamanda bile, tek, merkezi, koskoca bir
sistem olan anabilgisayar hâlen isteyen var.

Bunun örneklerinden biri, büyük şirketler olabilmektedir. Eğer
şirketler çok sayıda makina ile ilgilenmek istemiyorlarsa,
anabilgisayar yatırım yapabilmektedirler.
  
Ya da, fi tarihinde IBM ile olan ilişkileri sayesinde, eski sisteme
bakım, daha sonra onu güncelleştirme, onu da güncelleştirme derken,
bazı şirketler anabilgisayar dünyasından hiç dışarı çıkmamış
olabilmektedirler. IBM, bu müşterilerini sürekli güncel hâlde tutarak,
onlara yeterli olacak anabilgisayar sürekli onlara temin etmiştir.
  
Bu hikayenin biz Java'cıları ilgilendiren kısmı şudur: XYZ projemiz
altında, bazen müşterimizin erişilmesi gereken bütün verileri bir
anabilgisayar üzerinde saklı olabilir.

Bu yüzden, Java programlarımızın AS/400 gibi bir bilgisayara erişmesi
gerekecektir.

Gerekli Dosyalar 
    
AS/400'e Java ile bâglanabilme hakkında IBM'in sitesine
gidebilirsiniz. Bu siteden gerekli JAR dosyalarının hepsini
indirebilirsiniz.
  
Örnek kodlar altta. Bu kodlar, AS400 anabilgisayarı, DB2 veri tabanı
üzerinde olan XXISCDTA20.ISCH01 adlı tablodan bütün verileri seçip
göstermektedir.
  
make.bat adlı script, Test.java adlı dosyayı derleyip, otomatik olarak
çalıştıracaktır.

make.bat

```
set CLASSPATH=.\lib\composer.jar;.\lib\jt400.jar;.\lib\jt400Micro.jar;.\lib\jt400Native.jar;.\lib\jt400Proxy.jar;.\lib\jt400Servlet.jar;.\lib\jui400.jar;.\lib\outputwriters.jar;.\lib\reportwriter.jar;.\lib\tes.jar;.\lib\uitools.jar;.\lib\util400.jar;.\lib\pg72jdbc2.jar;. 
rem C:\Progra~1\j2sdk_nb\j2sdk1.4.2\bin\javac -classpath %CLASSPATH% TransferStok.java
rem C:\Progra~1\j2sdk_nb\j2sdk1.4.2\bin\java -classpath %CLASSPATH% TransferStok
C:\Progra~1\j2sdk_nb\j2sdk1.4.2\bin\javac -classpath %CLASSPATH% Test.java
C:\Progra~1\j2sdk_nb\j2sdk1.4.2\bin\java -classpath %CLASSPATH% Test
```


baglanti_en_US.properties


```
AS400.SISTEM=11.1.1.1
AS400.SIFRE=[SIFRE]
AS400.KULLANICINO=[KULLANICI]
```

Test.java

```
import java.io.*;
import java.sql.*;
import java.util.*;
import java.net.*;
import com.ibm.as400.access.*;

public class Test
{

    /**

     * @ejb.interface-method view-type = "remote'

     */

    private static boolean debug = true;

    protected static String sysName;

    protected static String kullaniciNO;

    protected static String pw;

    protected static AS400 sys;

    protected static Connection baglanti = null;

    protected static ResultSet rs;

    protected static ResultSetMetaData rsmd;

    protected static int kolonSayisi = 0;

    

    /**

     * @ejb.interface-method view-type = "remote'

     */

    public static void main(String arg[]) throws Exception

    {

 boolean rc;



 try {

     // JDBC surucusunu kayit ettir..

     try {

 DriverManager.registerDriver

     (new com.ibm.as400.access.AS400JDBCDriver());

     }

     catch(Exception e) {

 System.out.println("JDBC Surucusu bulunamadi");

     }

 }

 catch(Exception e) {

     e.printStackTrace();

 }



 rc = kur();



 if (!rc) {

     System.out.println("Hata, properties dosyasini okurken hata cikti.");

 }



 try {

      

     // AS/400'e baglan

     sys = new AS400(sysName, kullaniciNO, pw);

     sys.setGuiAvailable(false);

      

 }

 catch(Exception e) {

     e.printStackTrace();

 }



 try {

     baglanti = DriverManager.getConnection( "jdbc:as400://" +

     sysName + "/" +

     "MYPOP", // herhangi bir isim

     kullaniciNO,

     pw );

 }

 catch(Exception e) {

     System.out.println("JDBC baglantisini yaparken hata cikti");

 }



 tabloyuOku();

 //satirSay();

 //durumGoster();



    }



    /**

     * @ejb.interface-method view-type = "remote'

     */

    private static void tabloyuOku() throws Exception{



 Statement oku = baglanti.createStatement();



 String sql =

     "SELECT * FROM TESTDATA.LTRAN WHERE TRYCMP >= 200301 " +

     "AND TRYCMP < 200401";

    

 System.out.println(sql);

 rs = oku.executeQuery(sql);



 rsmd = rs.getMetaData();

 kolonSayisi = rsmd.getColumnCount();



 System.out.println("\n\n" + kolonSayisi + " kolon var");

    

 while (rs.next()) {

     satiriGoster();

 }



 baglanti.close();      

    }



    /**

     * @ejb.interface-method view-type = "remote'

     */

    private static void satirSay() throws Exception{



 Statement oku = baglanti.createStatement();

 

 rs = oku.executeQuery("SELECT COUNT(*) from TESTDATA.LTRAN");



 while (rs.next()) {

     System.out.println(rs.getString(1));

 }



 baglanti.close();      

    }



    /**

     * @ejb.interface-method view-type = "remote'

     */

    private static void durumGoster() throws Exception{



 Statement oku = baglanti.createStatement();

 

 rs = oku.executeQuery("SELECT FAVAIL from TESTDATA.LFGF");



 while (rs.next()) {

     System.out.println(rs.getString(1));

 }



 baglanti.close();      

    }



    /**

     * @ejb.interface-method view-type = "remote'

     */

    private static void satiriGoster() throws IOException

    {

 System.out.println("");



 for (int i = 1; i <= kolonSayisi; i++) {

     String kolonVerisi = null;



     try {

 kolonVerisi = rs.getString(i);

     }

     catch (Exception e) { System.out.println("metin alirken hata cikti"); }



     if (kolonVerisi == null)

 kolonVerisi = " ";



     System.out.print("<<");

     System.out.print(kolonVerisi);

     System.out.print(">> ");

 }



 System.out.println("\n----------------------------------------------");

    }

    

    /**

     * @ejb.interface-method view-type = "remote'

     */

    private static boolean kur()

    {

 try  {

     ResourceBundle rb = ResourceBundle.getBundle("baglanti");





     String sistem = rb.getString("AS400.SISTEM");



     if (sistem != null) {

 sysName = sistem;

     }

     else {

 System.out.println

     ("Hata, sistem ismini properties dosyasindan alamadik");

 return false;

     }



     String sifre = rb.getString("AS400.SIFRE");



     if (sifre != null) {

 pw = sifre;

     }

     else {

 System.out.println

     ("Hata, sifreyi properties dosyasindan alamadik");

 return false;

     }



     kullaniciNO = rb.getString("AS400.KULLANICINO");



     return true;



 }

 catch (Exception e)

     {

 System.out.println("Hata, properties dosyasini yukleyemedik");

 e.printStackTrace();

 return false;

     }

    }



}
```




