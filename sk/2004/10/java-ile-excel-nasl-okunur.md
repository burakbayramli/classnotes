# Java İle Excel Nasıl Okunur?

Bir Excel dosyasını herhangi bir veri tabanına yüklemeniz
gerekebilir. Müşteriniz, belki de bazı verileri Excel üzerinden
girmekte, ya da Excel dosyaları başka bir veri ortamından sizin
ortamınıza aktarma yapmak için bir aracı format olarak
kullanılmaktadır. Her iki durum için de, Java dili ile Excel .xls
dosyasında istediğiniz hücreye erişmeniz gerekecektir.

Excel çalışma kitapları (workbook) birçok çalışma sayfasından
(worksheet) oluşmaktadır. Her sayfa iki boyutlu bir tabloyu içerir. Bu
tablolar, hücrelerden oluşmaktadır.

Java Excel API 
  
Yukarıda anlatılan türden bir erişimi sağlamak için Java Excel API
biçilmiş kafandır.
  
Isletmek icin jxl.jar lazim.
  
Örnek Kullanım 
    
İlk önce, tek satır kod yazmadan, bir .xls dosyasının içeriğini
ekranda göstermeyi örnekleyelim.

```
java -jar jxl.jar -csv benimexceltablom.xls
```

Bu kadar! Tabii isteğimize uygun değişik bir program yazmak biraz daha
uğraş gerektiriyor. Sitemizden indirebileceğiniz dosyada,
ExcelTablo.java adlı bir dosya bulacaksınız. Bu dosya, basit bir
kullanım kalıbını örneklemektedir. Mesela, çalışma kitabını açmak için
gereken kod aşağıdadır.

```
String kodlama = "ISO-8859-9";
WorkbookSettings settings = new WorkbookSettings();
settings.setEncoding(kodlama);
Workbook workbook = Workbook.getWorkbook(new File("Book8.xls"));
```
  
Daha sonra, bu çalışma kitabından, birinci çalışma sayfasını
çıkartalım, ve 1. satır, 1. kolondaki hücreye erişelim.

```  
Sheet sheet = workbook.getSheet(0);
Cell a1 = sheet.getCell(1,1);
String stringa1 = a1.getContents();
byte tampon[] = stringa1.getBytes(kodlama);
```
  
Kodlama (encoding)
    
Özellikle setEncoding komutu ile gösterdiğimiz kodlamanın ne işe
yaradığını anlatmamız gerekiyor.
  
Java Excel API'ın kendi örneklerinde kodlama (encoding) hakkında bir
tavsiye bulamayacaksınız. Biz, projemiz dahilinde Türkçe karakterler
olan bir Excel tablosunu okumamız gerekince, bu ekleri yapmamız
gerektiğini farkettik.

Bulgulara göre, doğru karakterleri görebilmek için bir hücreden gelen
String nesnesini getBytes(kodlama) ile kullanacağımızı, ya da daha
önceden, çalışma kitabı seviyesinde kullandığımız kodlamayı (encoding)
JVM'e (Java'ya) bildirmemiz gerekmektedir.

```
import java.io.*;
import java.util.Date;
import jxl.*;

public class ExcelTablo
{
    public String hucre(int i, int j) throws Exception{
 String encoding = "ISO-8859-9";

 WorkbookSettings settings = new WorkbookSettings();
 settings.setEncoding(encoding);
 
 Workbook workbook = Workbook.getWorkbook(new File("Book8.xls"));
 Sheet sheet = workbook.getSheet(0);
 Cell a1 = sheet.getCell(i,j);
 String stringa1 = a1.getContents();
 
 byte tampon[] = stringa1.getBytes(encoding);

 return new String(tampon);
    }
    
    public static void main (String args[] ) throws Exception {
 String encoding = "ISO-8859-9";
 
 WorkbookSettings settings = new WorkbookSettings();
 settings.setEncoding(encoding);
 
 Workbook workbook = Workbook.getWorkbook(new File("Book8.xls"));
 Sheet sheet = workbook.getSheet(0);
 Cell a1 = sheet.getCell(0,0);
 String stringa1 = a1.getContents();
 
 File f = new File ("out.txt");
 FileOutputStream out = new FileOutputStream(f);

 byte tampon[] = stringa1.getBytes(encoding);
 
 out.write(tampon);
 out.close(); 
  }
}
```











