# Simbad




Simbad



Robot simulatorlerinden Simbad Java bazli ve kullanimi ve kod yapisi temiz bir yazilim. Kurmak icin alttaki adimlari takip etmek lazim.Java 3D icin https://java3d.dev.java.net/binary-builds.html dizininden j3d-1_5_2-linux-i586.bin indirin. Sonracd $JAVA_HOME/jresudo sh /dizin/yeri/j3d-1_5_2-linux-i586.binftp://ftp.freedesktop.org/pub/mesa/current/ adresinden 2.4 uzeri olmasi lazim, alttan kaynaklari indirin, sonra configure, sudo make install.http://packages.ubuntu.com/source/jaunty/libdrm adresinden ayni hareket.http://xorg.freedesktop.org/releases/individual/proto/  1.99 sonrasi versiyon lazim.Ardindan komut satirindan sunlari isletin:sudo apt-get build-dep libdrm mesasudo apt-get install linux-headers-`uname -r`sudo apt-get install libxi-dev libxmu-dev x11proto-xf86vidmode-devsudo apt-get install git-core autoconf automake libtoolSimdi eger robot kodlamamizi Python uzerinden yapmak istiyorsak, Java JVM icinde Python isletme yetenegine sahip olan Jython kodlarini indirmemiz lazim. http://www.jython.org/Project/download.html adresinden installer programi indirelim, java -jar [installer] ile kuralim. Guzel. Artik Simbad kodlarini indirebiliriz http://simbad.sourceforge.net. Kodu actiktan sonra o dizine cd komut ile girelim ve en ust seviyeden alttaki komutu uygulayalim:java -cp lib/simbad.jar:[JYTHON DIZINI]/jython.jar org.python.util.jython python/simple_plan.pySonuc olarak alttaki gibi bir ekranda robotumuzu goruyor olmamiz lazim. "Run" dugmesine tiklayarak robotun otonom olarak hareket ettigini gorebiliriz. Robotun uzerindeki olcum aletleri ayni anda degerlerini sol ust kosedeki ekranda gostermeye baslayacaktir.Not: Jython pur Java ortaminda calistigi icin bazi Python kutuphanelerini isletemiyor. Numpy, Scipy bu kutuphanelerinden bazilari. Biz bu kutuphaneleri bir client -> server mimarisi uzerinden isletecegiz; boylece server tarafi "pur Python" olabilecek ve her tur temel Python kodu isletebilecek, client tarafi ise Simbad iliskilerini halledip agir hesaplamalar icin server'a baglanacak. Niye pur Python illa lazim? Cunku Numpy, Scipy aleminden faydali onemli kodlar var, ve bilimsel hesaplama baglaminda bu kutuphanelerde bir standardizasyona gidiyoruz, bu sebeple o kodlar kullanimda olmali. Bu konuyu ileriki yazilarda isleyebiliriz.




![](simbad.jpg)
