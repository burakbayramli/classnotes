# Fakir Adamin HTML Include Teknigi


Fakir Adamin HTML Include Teknigi



Prototip sayfalar kodlarken, yani duz HTML uzerinden, Web servisi olmadan calisirken, bir sekilde bir HTML'den otekini dahil etmek gerekebiliyor. Ne yazik ki servis isliyor olmadan bunu yapmanin direk bir yolu yok; Biz de bir Perl script ile dahil etme isini kendimiz yapmaya karar verdik. Bizim sectigimiz komut, #include "dosya.html" komutu. Bu komutu direk HTML icine koyuyorsunuz, sonra Perl scripti isletiyorsunuz. Script dizinde gordugu tum HTML dosyalarini isliyor, include ibaresi yerine o include'un belirttigi dosyalarin icerigini koyuyor. Sonuclar output adinda ikinci bir dizinin altinda yaziliyor. Script soyle;foreach $file (<*.html>) {  print "$file\n";  open IN, $file;  open OUT, ">./output/$file";  undef $/;  $content = <IN>;  while ($content =~ m/#include \"(.*?)\"/sg) {   open INN, "$1";   undef $/;   $other = <INN>;   $content =~ s/#include \"$1\"/$other/g;  }  print OUT $content;    close IN;  close OUT;}




