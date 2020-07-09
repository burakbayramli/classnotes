# JDBC ile Tablo Karşılaştırmak


JDBC ile Tablo Karşılaştırmak



 Bazen müşterimiz, bilgi işlem sisteminde çok önemli olarak görülen bazı bilgilerin 'değiştirilmesi' halinde bu olaydan haberdar olmak isteyebilir.               Genelde bu tip erişim problemleri için klasik çözüm, neyin değiştiğini raporlamak yerine, kullanıcı erişim haklarını kısıtlayan bir sistem getirmektir.                Fakat bazen sistem kullanıcıları özellikle erişim kısıtlamayı değil, neyin değiştiğini bilmek isteyebilirler. Müşteriniz, "bazı bilgilere erişmeye normal olarak hakkı olan kişilerin bile yaptığı değişikliklerden haberdar olmak istiyorum" diyebilir. Bu pek normal olmayan bir istek olsa da, mümkündür.              Bu teknik probleme, şu şekilde bir çözüm getirebilirsiniz.              Ya, değişimi görsel bazlı olarak görsel kod üzerinde yakalarsınız, ya da, değişimin nihai adresi olan veri tabanına inerek çıkartabilirsiniz.              Veri tabanına inerek yapabileceğiniz kontrollerin bir iyi tarafı şudur: JDBC genel veri bilgileri (meta data) denilen, bilgiyi tasvir eden bilgi sayesinde, bir veri tabanı içindeki tabloları ve onların kolon isimlerini genelci bir şekilde şekilde (metin bazlı) olarak toplamamız mümkün oluyor. Bu bilgileri kullanarak, veri tabloları içindeki veriye bile aynı genelci şekilde erişebilirsiniz. Yani, veri erişmek için yazdığınız kod, bir tablodan ötekine değiştirilmeden işleyebilir!               Özellikle değişim yakalamak gibi nerede bulunacağı bilinmeyen bir veri değişim arayışı, genelci bir yaklasımla çok daha yararlı olacaktır.              Ekte verilen TabloKarşılaştırıcı  kodunda gördüğünüz gibi, herhangi bir tablonun 'öncesi' ve 'sonrası' karşılaştırmasının yapılması, bu tür bir yaklaşım ile çok kolay oluyor.               TabloKarşılaştırıcı




