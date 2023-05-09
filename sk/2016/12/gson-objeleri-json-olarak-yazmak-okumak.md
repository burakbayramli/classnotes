# Gson, Java Objelerini JSON Olarak Yazmak, Okumak

Java nesnelerini diske nasıl yazıp okuruz, eski ismiyle (!)
serileştiririz (serialization) konusuna bakarken şu kodu gördük,

https://github.com/google/gson

Gson projesi Google tarafından geliştirilmiş, pek çok kişinin alışık
olduğu JSON formatına objelerin yazılıp okunmasını sağlıyor.

Bizim için en çok gereken "objeler arası göstergeç takibi olup
olmadığı", yani A nesnesi B nesnesine referans ediyor, acaba A'yı
diske yaz dediğimizde B referansı takip edilip o da A ile beraber
yazılacak mı? Ufak bir test bunun olduğunu gösteriyor.

Kurmak için derlemeye gerek yok, Maven tüm dünyayı indirebilir, tek bir jar yeterli,

http://repo1.maven.org/maven2/com/google/code/gson/gson/2.8.0/gson-2.8.0.jar

Simdi

```
public class Exam {
    public String SUBJECT;
    public double GRADE;
    @Override
    public String toString() {
        return SUBJECT + " - " + GRADE;
    }    
}
```

```
public class Person {
    public String NAME;
    public String LOCATION;
    public Exam EXAM;
    @Override
    public String toString() {
        return NAME + " - " + LOCATION;
    }    
}
```

```
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;

public class Test {
    
    public static void main(String[] args) {
        Gson gson = new GsonBuilder().create();
        Exam ee = new Exam();
        ee.SUBJECT = "adsasf";
        Person pp = new Person();
        pp.NAME = "Husnu";
        pp.EXAM = ee;   
        try {
            Writer writer = new OutputStreamWriter(new FileOutputStream("Output.json") , "UTF-8");
            gson.toJson(pp, writer);
            writer.close();
        } catch (Exception e) {System.err.println("yazim hatasi"); } 
    }
}
```

Bu kodlar kendi java dosyalarinda olacak tabii ki.. Derleyelim,

```
javac -classpath gson-2.8.0.jar:. Test.java

java  -classpath gson-2.8.0.jar:. Test
```

Kod isledi, sonuc olarak Output.json yazildi, icinde 

```
{"NAME":"Husnu","EXAM":{"SUBJECT":"adsasf","GRADE":0.0}}
```

goruluyor.

Okumak icin iki turlu yol gosterelim, ilki direk diskten okumak,
ikincisi bir String icindeki objeyi okumak.

```
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import java.io.*;

public class Test2 {
    
    public static void main(String[] args) {
        Gson gson = new GsonBuilder().create();
        try {
            // disk
            InputStream in = Person.class.getResourceAsStream("Output.json");       
            Reader reader = new InputStreamReader(in, "UTF-8");
            Person p1 = gson.fromJson(reader, Person.class);
            System.out.println(p1);

            // string
            String s = "{'NAME':'Husnu','EXAM':{'SUBJECT':'adsasf','GRADE':0.0}}";
            Person p2 = gson.fromJson(s, Person.class);
            System.out.println(p2);
            
        } catch (Exception e) { System.err.println("okuma hatasi"); }
        
    }
}
```

Şimdi ilginç bir kullanım görelim. Acaba Java bağlamında sözlük içinde
sözlük içeren bir veri yapısını nasıl okurum / yazarım, daha doğrusu
Java objesi olarak yazılıp / okunan şey neye benzer? Belki bu tür
veriyi Python'dan üretiyorum, ve onu olduğu gibi Java'da okuyabilmek
istiyorum. Bir deneme yaptık, tahmin olarak herhalde (GeoMap.java
icinde)

```
import java.util.*;
public class GeoMap extends HashMap> { }
```

gibi bir kod olacakti. Yazmak icin 

```
import com.google.gson.Gson;

import com.google.gson.GsonBuilder;

import java.io.FileOutputStream;

import java.io.IOException;

import java.io.OutputStreamWriter;

import java.io.Writer;

import java.util.*;

public class Test3 {

    

    public static void main(String[] args) {

        Gson gson = new GsonBuilder().create();

        

        GeoMap m = new GeoMap();

        m.put("11.111 22.2222", new HashMap());

        m.put("22.111 22.2222", new HashMap());

        m.get("11.111 22.2222").put("11.111 22.4444", "10.0");

        m.get("22.111 22.2222").put("33.111 22.2222", "11.0");

        

        try {

            Writer writer = new OutputStreamWriter(new FileOutputStream("Output.json") , "UTF-8");

            gson.toJson(m, writer);

            writer.close();

        } catch (Exception e) {System.err.println("yazim hatasi"); }    

    }

}
```

Bu kod hakikaten bekleneni yapti, veri suna benzedi, 

```
{"22.111 22.2222":{"33.111 22.2222":"11.0"},"11.111 22.2222":{"11.111 22.4444":"10.0"}}
```

Okumak icin alttaki kod; isi biraz ilginclestirelim dedik, bir zip
dosyasi icinden json okuyoruz (test icin ustteki dosya elle
ziplenebilir)

```
import com.google.gson.Gson;

import com.google.gson.GsonBuilder;

import java.util.zip.*;

import java.io.*;


public class Test4 {

    

    public static void main(String[] args) {

        Gson gson = new GsonBuilder().create();

        try {

            ZipFile  zipFile =  new  ZipFile ("Output.json.zip");

            ZipEntry e = zipFile.getEntry("Output.json");

            InputStream in = zipFile.getInputStream(e);     

            Reader reader = new InputStreamReader(in, "UTF-8");

            GeoMap p = gson.fromJson(reader, GeoMap.class);

            System.out.println(p.size());

            System.out.println(p.get("22.111 22.2222"));

            

        } catch (Exception e) {

            e.printStackTrace();

            System.err.println("okuma hatasi");

        }

        

    }

}
```
