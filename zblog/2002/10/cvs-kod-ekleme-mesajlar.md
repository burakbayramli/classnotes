# CVS Kod Ekleme Mesajları


CVS Kod Ekleme Mesajları



 Bir projenin yazılım safhasında, herkesin bir diğerinin yaptığını bilmesi çok önemlidir. Özellikle hızlı tempoda seyreden yazılım projelerinde, iletişim hat safhada önem taşır. İletişim, hem sözel, hem de yazılı (e-posta mesajları) formatlarında proje elemanları arasında gidip gelmelidir.              Bu gidip gelmesi gereken mesajlardan önemli bir tanesi, CVS Kaynak Kod Deposuna her "cvs commit" ile her kod eklediğinizde diğer programcıları haberdar etmek için göndereceğiniz e-posta mesajıdır.               Bu mesaj, basit bir şekilde "Konu" kısmında "CVS Ekleme: Şunlar şunlar oldu" gibi bir başlık taşıyabilir. E-posta içeriği de, o gün eklenen dosyaların bir listesini içermelidir. (Ekleme postalarının başlığı hep CVS Ekleme: gibi aynı öneği içeriyor, çünkü bu sayede ekleme e-posta mesajlarını başından hemen ayırdetmek mümkün olmaktadır).               Şimdi gelelim bu yazının esas konusuna: cvs commit komutundan sonra otomatik olarak verilen "değişmiş/eklenmiş dosya listesi yeterli olmayacaktır, çünkü bu liste, sadece o en son cvs commit komutundan sonra eklenen/değiştirilen dosya isimlerini gösterecektir. Eğer arka arkaya birkaç oturumda eklenen dosyaların listesi, hatta o gün eklenen "tüm" dosyaların listesi isteniyorsa, bunun için bir cvs betiği yazmamız gerekecek.               Bu Perl betiğini aşağıda veriyoruz.                $BASLA = "c:/kaynak_kod/proje/src";  ## bu yeri projenize göre değiştirin$ben = `whoami`;$bugun = `date +%Y%m%d`;print "$bugun";print "$ben";print "$BASLA";print "\n";chomp $ben;    # \n işaretini siliyoruzchomp $bugun;  # \n işaretini siliyoruzchdir($BASLA);$cikti = `cvs log -NS -d ">$bugun" -w$ben`;print $cikti;              Betik, whoami ve date gibi Unix komutlarını kullanmaktadır, bu yüzden betiği çalıştırabilmek için Windows üzerinde Cygwin Unix paketini kurmanız gerekecek. Ayrıca CVSROOT çevre değişkeninin de CVS havuzunun adresini göstermesi gerekmektedir. Bu önemsiz bir ayrıntı aslında çünkü CVSROOT olmadan hiçbir cvs komutu zaten çalışmazdı.               Yukarıdaki betiğin işi bitirdikten sonra çıkarttığı listeyi, artık kopyalama ve yapıştırma işlemi ile e-posta mesajınızın içine kolayca atabilirsiniz.




