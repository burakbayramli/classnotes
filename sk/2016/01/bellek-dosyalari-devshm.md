# Bellek Dosyalari - /dev/shm

Tüm Unix sistemlerinin takip ettiği POSİX standartına göre `/dev/shm`
dizini altına yazılan tüm dosyalar hafızada işlem görür, diske
yazılmaz. Bu özelliğin bazı faydaları var; dosyadan okuyacak, dosyaya
yazacak şekilde kodlanmış pek çok yazılım var etrafta, bu yazılımlara
mesela diğer bir dil üzerinden erişmek için veri transferi özel
kodlaması gerekebilir, bu da külfetli olabilir. Mesela Python wave,
scipy.iö.wavfile.read çağrıları, bunlar pür dosya IO'su  için
kodlanmış, ama sürekli olarak hızlı bir şekilde bu ikisi arasında
alışveriş istiyorsak, transferi direk `/dev/shm` üzerinden yapmak
problemi çözer.


