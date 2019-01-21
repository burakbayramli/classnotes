# Ikili PDF Sayfalari Tek Sayfali Yapmak (Ortadan Bolmek)


Ikili PDF Sayfalari Tek Sayfali Yapmak (Ortadan Bolmek)



2 sayfa yanyana scan edilmis PDF dokumanlarini normal hale getirmek icin alttaki yontem kullanilabilir. Once pdfjam adli program lazim, apt-get bunu kurar. Ardindanpdfjam -o cift.pdf --trim '14cm 0cm 0cm 0cm' --clip true --scale 1.0 dokuman.pdfpdfjam -o tek.pdf --trim '0cm 0cm 14cm 0cm' --clip true --scale 1.0 dokuman.pdfpdftk A=cift.pdf B=tek.pdf shuffle BE AE output final_dokuman.pdfBu komutlarin yaptigi su; biri sagdan bir soldan olmak uzere 14 cm'lik bolumu kesip atiyoruz (trim ve clip true), ve bu budanmis dokumanlardan birini tek sayili sayfalar, digerini cift sayili sayfalar olarak kaydediyoruz. Sonra bir pdftk taklasi atmak gerekiyor, bu komuta iki dokumani birlestirmesini soyluyoruz, ama tek sayfalari bir yerden, cift sayfalari baska bir yerden almasini soyluyoruz (shuffle B A secenegi, E ekini biz yaptik, E harfi dogu yonunde -east- cevirim, boylece BE AE oldu cunku bizim sayfalari bir de 90 derece saga cevirmek gerekti).




