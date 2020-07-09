# Java İle Nasıl Dosya Kopyalanır?


Java İle Nasıl Dosya Kopyalanır?



Java 1.4 Öncesi          Şu anki mevcut "engin" Java arayüzleri ve kütüphaneleriyle bile, bâzen basit bir istek bizi hazırlıksız yakalayabiliyor. "Bir dosyayı Java'da nasıl kopyalayabiliriz?". java.io.File sınıfında gerekli işlemin olduğuna inanırken, Javadoc'larda bir inceleme durumun böyle olmadığını gösterdi. Eh, o zaman bu kopyalama sınıfını sıfırdan yazmak gerekti. Aşağıdaki kod 1.4 öncesi ve sonrası Java derleyicileri için verilmiştir.                 import java.io.*;public class Kopyalayici{public static void main(String args[]){ try {   Kopyalayici j = new Kopyalayici();   j.dosyaKopyala(new File(args[0]),new File(args[1]));   } catch (Exception e) {   e.printStackTrace();   } }public void dosyaKopyala(File giris, File cikis) throws Exception { FileInputStream fis  = new FileInputStream(giris); FileOutputStream fos = new FileOutputStream(cikis); byte[] buf = new byte[1024]; int i = 0; while((i=fis.read(buf))!=-1) {      fos.write(buf, 0, i); } fis.close(); fos.close(); }}          Java 1.4           import java.nio.channels.*;import java.io.*;public class Kopyalayici2 {public static void main(String args[]){ try {   Kopyalayici2 j = new Kopyalayici2();   j.kopyala(new File(args[0]),new File(args[1])); } catch (Exception e) {   e.printStackTrace(); }}public void kopyala(File giris, File cikis) throws Exception {  FileChannel kaynakKanali = new       FileInputStream(giris).getChannel();  FileChannel hedefKanali = new       FileOutputStream(cikis).getChannel();  kaynakKanali.transferTo(0, kaynakKanali.size(), hedefKanali);  kaynakKanali.close();  hedefKanali.close();  }}




