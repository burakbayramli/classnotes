# Derin Ogrenim, Farkli Yaklasimlar

Derin Öğrenim (deep learning) patlama yaptı, fakat ilgilenenler için
bir zorluk var, literatürdeki DO anlatımları çakışabiliyor. Mesela bir
yaklaşım Geoffrey Hinton'ın (2006-2009 arası) kullandığı, üst üste
konmuş Kısıtlı Boltzmann Makinaları (RBM) kullanarak. Kevin Murphy'nin
kitabında da bu yaklaşım var, diğer yanda yine Hinton'un kendisi (2012
sonrası) ve Yann LeCun'un (şimdi Facebook ta) kullandığı. O
Convolutional Net (kısaca convnet) yaklaşımı kullanıyor ki bu metotun
ilk kullanımı 80'lere uzanıyor, burada girdi evrişim (convolutional)
operasyondan geçiriliyor. LeCun açık şekilde "ben RBM kullanmıyorum"
dedi, DO hakkında bir yazıda da RBM kimse kullanmıyor gibi bir yorum
yapıldı, ama geçende Hadoop üzerinden RBM seviyelerini derin şekilde
öğrenen bir yazı bile geçende gördük. Özellikle etiketli veri (labeled
data) az ise, bir öneğitim evresi (pretrain) RBM ve RBM tabakaları
kullanımı hala gerekli gibi gözüküyor çünkü RBM bir olasılık
dağılımıdır ve gizli katmanı üzerinden özellik azaltması (feature
reduction) yapmaktadır. Takip edilen eğitim yapan Convnet'lerin
etiketli veriye ihtiyacı var.

Her iki tarafta da bol inovasyon var, Facebook'ta (LeCun idaresinde)
çalışan grup Torch diye bir kütüphane yayınladılar, ayrıca
convnet'leri GPU üzerinde işletmenin bir sürü yolu var. Yaklaşım
açısından convnet'ler klasik yapay sınır ağları (NN) yaklaşımına daha
yakın denebilir.

