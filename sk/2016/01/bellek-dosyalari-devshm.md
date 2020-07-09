# Bellek Dosyalari - /dev/shm


Bellek Dosyalari - /dev/shm




Tum Unix sistemlerinin takip ettigi POSIX standartina gore /dev/shm dizini altina yazilan tum dosyalar hafizada islem gorur, diske yazilmaz. Bu ozelligin bazi faydalari var; dosyadan okuyacak, dosyaya yazacak sekilde kodlanmis pek cok yazilim var etrafta, bu yazilimlara mesela diger bir dil uzerinden erismek icin veri transferi ozel kodlamasi gerekebilir, bu da kulfetli olabilir. Mesela Python wave, scipy.io.wavfile.read cagrilari, bunlar pur dosya IO'su  icin kodlanmis, ama surekli olarak hizli bir sekilde bu ikisi arasinda alisveris istiyorsak, transferi direk /dev/shm uzerinden yapmak problemi cozer.


Kaynak






