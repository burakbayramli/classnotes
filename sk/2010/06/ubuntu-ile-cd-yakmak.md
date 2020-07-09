# Ubuntu ile CD Yakmak


Ubuntu ile CD Yakmak



ISO dosyalarini CD uzerine yakmak icin kullanisli bir arac: cdrecord. Once hangi CDR birimlerinin bagli olduguna bakilir:cdrecord -scanbusSuna benzer bir cevap gelir;scsibus3: 3,0,0    300) 'MATSHITA' 'DVD-RAM UJ-852S ' '1.80' Removable CD-ROM 3,1,0    301) * 3,2,0    302) * 3,3,0    303) *sudo cdrecord -v dev=3,0,0 dosya.isoya dawodim dev=/dev/cdrw -v -data dosya.isoEger elimizde onceden hazir bir iso imaji yoksa, mesela kendi duz dosyalarimizi CD'ye yakmak istiyorsak, once ISO imajini kendimiz yaratiriz:genisoimage -joliet-long -input-charset utf-8 -r -J -o imaj.iso /falan/filan/dizinKaynak




