# Seam ve Sayfa Navigasyonu


Seam ve Sayfa Navigasyonu



Seam sayesinde EJB ve xhtml sayfa kavramlari birbirine iyice yaklasti. Sayfa icinden EJB ismini kullanarak button, link tiklamalari direk metot cagrilarina eslemek mumkun. Peki bu tiklama sonrasi hangi sayfaya gidecegimizi nasil soyleyecegiz?Aksiyon metotlarindan String tipi dondurmek ve bu parametreye "falanca.xhtml" gibi bir deger koymak bir cozumdur. Seam burada gordugu degeri sayfa ismi olarak kabul eder ve oraya yonlendirme yapar.Daha iyi bir cozum, aksiyon metotlarinin hicbir sey dondurmemesi (yani void) ve navigasyonun tamamen disarida bir ayar dosyasinda yapilmasidir. Bu dosya pages.xml dosyasidir.Navigasyonu bu sekilde yapinca, ayni aksiyonun (metotun) degisik sayfalarda degisik yerlere yonlendirilmesi mumkun olur. Yani bu teknik daha gucludur ve daha temizdir. Bu dosya, WEB-INF altinda web.xml ile ayni seviyede olmali.<pages xmlns="http://jboss.com/products/seam/pages" xsi="http://www.w3.org/2001/XMLSchema-instance" schemalocation="http://jboss.com/products/seam/pages http://jboss.com/products/seam/pages-2.1.xsd" id="/main.xhtml"> <page id="/burada.xhtml" required="false">   <navigation action="#{obj1.doIt}">     <redirect id="/suraya.xhtml">     </redirect>   </navigation> </navigation></page></pages>




