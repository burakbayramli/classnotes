# Kitap Scan Dosyalari ve DJVU

Kitaplarin scan edilmis imaj dosyalarini sıkıstırabilen bir format
DJVU. Unix uzerinde cjb2 adli komut bir dosyayi TIFF formatindan alip
DJVU formatina cevirebiliyor. Sıkıstırma orani 1/8! Yani bir kitap
scan dosyasini sekizde birine indirmek mumkun. Kurmak
icindjvu.sourceforge.netadresinden kaynak kodlari indirebiliriz. Ek
bazi detaylar, cjb2 komutunun siyah / beyaz bir imaj bekledigidir. O
zaman "convert -monochrome .." komutu ile bu hazirligi
yapabiliriz. Eger tum sayfalar TIFF ve monochrome halinde hazirsa, tek
sayfayi kitaba soyle ekleriz:cjb2 -clean /tmp/mono.tiff
/tmp/book.djvuYani mono.tiff dosyasini book.djvu dosyasina
ekliyoruz.Bu isi otomize etmek icin; bir dizin altinda 1,2,..9 diye
giden bolum (chapter) alt dizinlerinde dosyalarimiz varsa, bu alt
dizinlerde scan edilmis sayfalari birlestirmek icinimport os, re,
glob, sys, string;def run_command(command): result = [] f =
os.popen(command, "r") sys.stdout.flush() for l in f.xreadlines():
result.append(l) return resultrun_command("convert -monochrome
1/01.tiff /tmp/mono.tiff")run_command("cjb2 -clean /tmp/mono.tiff
/tmp/book.djvu")for a in range(9):os.chdir(str(a+1))for file in
sorted(glob.glob("*.tiff")): print file run_command("convert
-monochrome " + str(file) + " /tmp/mono.tiff") run_command("cjb2
-clean /tmp/mono.tiff /tmp/page.djvu") run_command("djvm -i
/tmp/book.djvu /tmp/page.djvu")os.chdir("..")Bu script tmp altinda bir
book.djvu dosyasini yaratmis oluyor.





