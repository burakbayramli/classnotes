# Araba Kullanan Derin YSA

Keras / Python ile bir model arabayı kullanan ilginç bir proje. YSA
eğitilirken girdi olarak arka arkaya alınmış video resimleri
kullanılmış, test yol ortamında sağ ve solda beyaz şeritler var, hedef
değişkeni direksiyonun kaç derece sağa ya da sola döndüreceğini
belirten -90,+90 arasında bir sayı. YSA evrişimsel derin bir ağ,
içinde RNN / LSTM kullanılmamış, yine de başarı fena değil. Eğitim
için arkadaş video kaydedilirken arabayı kendisi kullanmış, test
ortamında araba bu modeli kullanarak şeritler arasında kalarak
ilerliyor.

Tek kamera ve yapay öğrenim üzerinden araba kullanmak hala bakir bir
alan. Şu anda revaçta olan otomatik araba kullanan teknolojiler radar,
lidar, sonar gibi envai türden algılayıcı ile çevre hakkında bilgi
topluyorlar, ve filtreleme (parçacık filtreleri) teknikleri ile etraf
hakkında konum tahmini yapıp ilerliyorlar. Deep Drive gibi sistemler
yeni denenen pür görüntü bazlı yeni yaklaşımlar arasında. Maceracı
araştırmacılar buraya atlayabilir. 






