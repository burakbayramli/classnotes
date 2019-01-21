# imap


imap



Python itertools paketinden bir fonksiyon daha: imap. Bu yardimci cagri adindan anlasilabilecegi uzere "eslestirme" yapar. Iki veya daha fazla parametre alir, birinci parametre her zaman cagirilacak bir fonksiyon F olacaktir (evet fonksiyona parametre olarak fonksiyon geciyoruz), geri kalan parametreler icinde F'e verilecek olan parametrelerin listesi oluyor. Mesela pow cagrisini dusunelim, bu cagri iki parametre alir, pow(2,2) mesela 2 uzeri 2 hesabini yapar, 2^2 = 4. Diyelim ki bunun gibi pek cok sayi var elimizde, ustu alinacak sayilar bir listede, ust degerleri baska bir listede duruyor. imap ile bu isi soyle hallederiz:from itertools import imapaa = [2,3,4]bb = [2,4,6]for x in imap(pow, aa, bb): print xBu cagri 2^2, 3^4, 4^6 hesaplarini sirasiyla yapar ve sonuclarin "gezilebilir" hale getirilmesini saglar (imap kodlamasinda yield kullanimi var yani).




