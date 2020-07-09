# add_subplot(222)?


add_subplot(222)?




Matplotlib (ve Matlab) ana grafik ekranini bolerek ufak grafikleri bu bolumlere koyma yetenegine sahip. Bu orneklere bakarken, belki de 332, 222 gibi sayilar kullanildigini gormussunuzdur. Mesela

fig = plt.figure()
ax1 = fig.add_subplot(321,title="baslik")ax1.plot(data1)

gibi. Burada 321'in anlami sudur: Ana ekran 3 satir ve 2 kolon olmak uzere, yani 3x2 olarak bolunecek, ve ustteki grafik 1'inci bolume konacak.

1., 2. tabii ki sol ustten baslanip saga dogru gidince artan sayilar, yani hucrelerin indisi.

O zaman 321'in 32 kismi diger tum alt grafikler icin tekrarlanacak. Yeni grafikler 322, 323, 324, vs gibi gidecekler.





