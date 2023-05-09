# OpenCV 2.0

Open CV 1.1 versiyonunda bazi problemler var; en son versiyona
yukseltilmesi tavsiye edilir. Kurmak icin gerekli baz paketler:sudo
apt-get install libavformat-dev libgtk2.0-dev pkg-config cmake
libswscale-dev bzip2Sonra Open CV paketi indirilir:wget
http://downloads.sourceforge.net/project/opencvlibrary/opencv-unix/2.0/OpenCV-2.0.0.tar.bz2Paketi
acip dizine girilir ve su dizinler yaratilir:mkdir release; cd
releaseVecmake -D CMAKE_BUILD_TYPE=RELEASE -D
CMAKE_INSTALL_PREFIX=/usr/local -D BUILD_PYTHON_SUPPORT=ON ..sudo make
install.bashrc ya da cevre degiskenlerinin set edildigi yerde su
ibareler eklenmeli:export
LD_LIBRARY_PATH=/usr/local/lib/:$LD_LIBRARY_PATHKaynak





