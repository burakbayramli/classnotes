# Derin Ogrenim, Farkli Yaklasimlar

Derin Ogrenim (deep learning) patlama yapti, fakat ilgilenenler icin
bir zorluk var, literaturdeki DO anlatimlari cakisabiliyor. Mesela bir
yaklasim Geoffrey Hinton'in (2006-2009 arasi) kullandigi, ust uste
konmus Kisitli Boltzmann Makinalari (RBM) kullanarak. Kevin Murphy'nin
kitabinda da bu yaklasim var, diger yanda yine Hinton'un kendisi (2012
sonrasi) ve Yann LeCun'un (simdi Facebook ta) kullandigi. O
Convolutional Net (kisaca convnet) yaklasimi kullaniyor ki bu metotun
ilk kullanimi 80'lere uzaniyor, burada girdi evrisim (convolutional)
operasyondan geciriliyor. LeCun acik sekilde "ben RBM kullanmiyorum"
dedi, DO hakkinda bir yazida da RBM kimse kullanmiyor gibi bir yorum
yapildi, ama gecende Hadoop uzerinden RBM seviyelerini derin sekilde
ogrenen bir yazi bile gecende gorduk. Ozellikle etiketli veri (labeled
data) az ise, bir onegitim evresi (pretrain) RBM ve RBM tabakalari
kullanimi hala gerekli gibi gozukuyor cunku RBM bir olasilik
dagilimidir ve gizli katmani uzerinden ozellik azaltmasi (feature
reduction) yapmaktadir. Takip edilen egitim yapan Convnet'lerin
etiketli veriye ihtiyaci var.

Her iki tarafta da bol inovasyon var, Facebook'ta (LeCun idaresinde)
calisan grup Torch diye bir kutuphane yayinladilar, ayrica
convnet'leri GPU uzerinde isletmenin bir suru yolu var. Yaklasim
acisindan convnet'ler klasik yapay sinir aglari (NN) yaklasimina daha
yakin denebilir.






