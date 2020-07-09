# JBoss'a Ne Kadar Bellek Ayirmak Lazim?


JBoss'a Ne Kadar Bellek Ayirmak Lazim?



EC2 makinamizi baslattik, ve JBoss app server'i kosturmak istiyoruz. JBoss'a ne kadar hafiza ayirmak lazim?Komut satirina gidelim ve tum servis programlarini durduralim. Ne varsa. MySql, Apache, JBoss; makina ilk acildigi, kuruldugu anki gibi olmali. Bu durumdaki makinada "free -m" komutunu isletelim. Geriye gelen "used" yani makina hicbir sey yapmazken kullanilan bellek miktarini ikiyle carpalim. Sonra bu sayiyi "free" sayisindan cikartalim. Geri kalan JBoss'a ayirilabilecek hafiza miktaridir.Bu hesabin mantigi nerede? Oncelikle makina hicbir sey yapmazken kullanilan hafizaya bakiyoruz, bu hafiza inaktif haldeyken bile gereken miktar, demek ki "en az" bu kadar gerekiyor. Sonra, ikiyle carparak makina kullanim halindeyken isletim sisteminin biraz daha fazla hafizaya ihtiyac duyabilecegini varsayiyoruz. Eh bundan arta kalan artik JBoss'a verilebilir.Nasil verilecek? JBOSS/bin/run.conf icinde JAVA_OPTS icin -Xms[EN AZ]m ve -Xmx[EN FAZLA]m tanimlarini yaparak. Burada bir tavsiye; ustteki aritmetikten elimize gecen sayiyi hem en az, hem en fazla icine set edelim. JBoss baslar baslamaz eldeki tum hafizaya alsin yani, boylece hafizayi "arttirmaya" ugrasmakla vakit kaybetmesin.Kaynak




