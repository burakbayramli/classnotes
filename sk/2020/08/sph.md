# Pürüzleştirilmiş Parcaçık Hidrodinamiği (Smoothed Partıcle Hydrodynamics -SPH-)

Şu akışını modellemek fizikteki en zor problemlerden biri, hatta bu
akışı temsil eden Navier-Stokes denklemlerinin her durumda analitik
bir çözümü olacağının garantisinin ispatı yok, kaos ortaya çıkması çok
rahat ki bu türbülans denen fenomendir.

Analitik çözüm zorluğu durumunda sayısal çözüme başvurulmuştur, burada
Euler yöntemi ve Hamilton yöntemi diye yaklaşımlar ikiye
ayrılır. Euler yöntemi bilinen, bir sayısal izgara / göz (mesh)
yaratmak ve bu izgara köşe noktalarında ayrıksallaştırmaya gitmek. Bir
diğer yaklaşım, ki bu SPH yaklaşımı, sistemdeki her parçacığı takip
eder, onun değişkenlerini hatırlar. Ama bunu bir ekle yapar, her
parçacık etrafındaki diğer parçacıklardan etkilenir o zaman bu
parçacık üzerine bir çekirdek (kernel) koyulabilir, ve bu çekirdek
üzerinden takip yapılır.

Bu yaklaşım stabil bir sistem ortaya çıkartıyor.

Alttaki iki kod, C++ üzerinden 2 boyutta bu tür sistemlerin
modellemesi. Derleyebilmek için

```
sudo apt-get install mesa-common-dev libgl1-mesa-dev libglu1-mesa-dev freeglut3-dev
```

Ardindan

```
g++ dosya.cpp -lX11 -lGL -lGLU -lglut -g -Wall -O2 -o islet.exe
```

gibi bir derleme yapilabilir. 

[SPH Kod 1](gl1.cpp)

[SPH Kod 2](gl2.cpp)



[1] https://imdoingitwrong.wordpress.com/2010/12/14/why-my-fluids-dont-flow/

[2] https://github.com/cerrno/mueller-sph

