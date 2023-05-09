# OpenCV 3.0

OpenCV en son sürüm kurmak için

```
sudo apt-get install build-essential
sudo apt-get install cmake git libgtk2.0-dev pkg-config libavcodec-dev 
sudo apt-get libavformat-dev libswscale-dev python-dev python-numpy libtbb2 
sudo apt-get install libtbb-dev libjpeg-dev libpng-dev libtiff-dev 
sudo apt-get install libjasper-dev libdc1394-22-dev
```

Sonra

https://github.com/opencv/opencv

adresinden kod indirilir, dizine gidip 

```
mkdir release

cd release/

cmake -D CMAKE_BUILD_TYPE=RELEASE -D CMAKE_INSTALL_PREFIX=/usr/local ..

make -j2

sudo make install
```

işletilir.

OpenCV ile C++ kodu derlemek icin tipik ornek,

```
g++ -c -O3 -Wall `pkg-config --cflags opencv` -D LINUX -fPIC videostab.cpp

g++ -o videostab.exe videostab.o `pkg-config --libs opencv`
```








