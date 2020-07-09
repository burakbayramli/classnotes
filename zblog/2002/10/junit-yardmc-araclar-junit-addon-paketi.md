# JUnit Yardımcı Araçları - Junit-Addon Paketi


JUnit Yardımcı Araçları - Junit-Addon Paketi



 JUnit testlerimizin bazen metin (text) bazlı çıktı dosyalarını karşılaştırması gerekiyor. Bu gibi "ek" assert yardımcı metotları için, JUnit-addons paketi kullanabilirsiniz. FileAssert adlı bir class, java.io.File olarak verilen iki dosyayı karşılaştırabiliyor. Örnek kullanım:               import junitx.framework.*;// ...// burada bir takım işlemler çağrılmış ve sonuç olarak// bir sonuc.xml dosyası üretilmiş olsun)// ..File esas = new File("./sonuc.xml");File beklenen = new File("./test/aslir_beklenen_cevap.xml");FileAssert.assertEquals(beklenen, esas);          Kaynaklar          * JUnit-addons SourceForge Sitesi   * JUnit-addons Javadoc'ları      




