# RNN, CNN

Zamansal, sıralı verileri öğrenmek için bilinen yaklaşım RNN (Kendini
Tekrarlayan Yapay Sinir Ağları (Recurrent Neural Network)
kullanmak. Fakat yeni pek çok uygulamada gorulmeye baslandi ki RNN
mekanizmasına hiç gerek yok, direk evrişim (convolution) kullanan
YSA'lar (CNN) aynı işi yapabilirler. Evrişimsel YSA bilindiği gibi
yapay sinir ağlarının "geriye dönüşünü" sağlayan ağlardı. Görüntü
verisinde ilerleme sağladılar ve YSA kavramı geri gelmiş oldu,
SVM'lerin pabucu dama atıldı. Fakat herkes sadece CNN görüntü için
uygundur zannediyordu, farklı alanlarda kullanım bulması sürpriz oldu.

RNN ve CNN karşılaştırmasından bahseden yazılar

https://towardsdatascience.com/the-fall-of-rnn-lstm-2d1594c74ce0

https://arxiv.org/abs/1803.01271

https://arxiv.org/abs/1808.03867

https://arxiv.org/abs/1705.03122

RNN problemlerinden biri yokolan gradyan problemi. Ayrıca üstteki
yazının bahsettiği gibi donanım açısından pahalı bir yaklaşım.

Bu tür trendleri takip etmek faydalı, enerji hangi yaklaşımın
arkasında böylece biliriz, o alanda daha fazla çözüm görmek mümkündür,
daha fazla araç, vs mevcut olacaktır. Bu sebeple mesela ben ses
verisinden özellik çıkartımı yapan bir ağ ararken RNN, LSTM bazlı
ağlara hiç bakmadım. Direk CNN bazlı ağ aradık ve yazıda bahsedilen
örneği bulduk.

Böyle kim batıyor, kim çıkıyor hakkında bir sürü tartışma, konuşma var
tabii. Birisi espri olsun diye LSTM (RNN) geri geldi türünde bir
uyduruk makale başlığı hazırlamış. Schmidhuber ünlü RNN
araştırmacılarından, ismini makalede iki kere kullanmışlar, bunlar
camiada dönen geyikler.

