# Python ile Dinamik Excel Dosyasi Yaratmak


Python ile Dinamik Excel Dosyasi Yaratmak




Gerekli paketler surada listeli:

http://www.python-excel.org/

Biz Excel uretimi icin xlwt kullandik. En basit kod

from tempfile import TemporaryFile
from xlwt import Workbook
book = Workbook()
sheet1 = book.add_sheet('Sheet 1')
sheet1.write(0,0,'A1')
book.save('simple.xls')
book.save(TemporaryFile())

0,0 hucresin bir seyler yazilip kaydedildi. Stil uygulamasi icin easyxf tavsiye edilir, mesela write(0,0,'vs',easyxf('alignment: horizontal right')) gibi.

Paketin bazi eksikleri sadece yazmaya yonelik olmasi, bir hucrenin mevcut durumunu okuyup bir seyler eklemek imkansiz. Okuma icin diger paket xlrd kullaniliyor, o zaman da ta  en bastan bir xls dosyasi acmis oluyorsunuz, ve baska bir ortamda / mod icinde oluyorsunuz.Excel ureten kodlarda icerik dinamik oldugu zaman kor bir sekilde veriyi uygun yere atmakla ugrasiriz, birkac kolonu satiri kapsayacak stil uygulamasi (mesela renk, agirlik -bold-, font buyuklugu) sonradan belli alanlara uygulamak tercihimiz. xlwt ile bunlari yapmak zor, write ile tekrar ayni hucreye yazdiginizda o hucrenin eski degeri eziliyor.

Cozum write() metotunu kendi write metotumuz sarmalayip (wrap), bu bizim metot icinde global bir stil listesini her yazim icin kontrol etmek, yani stili surekli / her x,y degeri icin bu listeden  almak. Bu stil listesi basit bir Python listesi olabilir, yazim baslamadan once hazir olmalidir, (x1,x2,y1,y2,stil) tuple listesi seklinde, stiller belli bloklar icin tanimlandigi icin bir dikdortgen icindeler, x1,x2 kullanimi bunun icin. Sarmalanan write surekli kendisine verilen x,y'nin hangi stil dikdortgeninin icine dustugunu kontrol edecek yani.













