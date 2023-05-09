# Obje Yansitmak

Uc boyutlu kordinat sistemindeki bir objeyi, noktayi alip onu iki
boyuta yansitma isi kamera matrisi uzerinden yapiliyor. Bu matris
kamera kalibre edilirken uretilen 3x3 boyutunda bir matristir. Test
icin alttaki OpenCV Python kodunu paylasalim; bu kod 20x20
boyutlarindaki bir hayali "kup" icin noktalar uretiyoruz ve bu
noktalari teker teker kamera matrisi K ile carparak onlari 2 boyutlu
piksel konumlarina yansitiyoruz. Carpim sonucu ele gecen sayilar 3x1
boyutunda olacak, fakat bu sayilar homojen kordinatlar, sonuncu
(ucuncu) kordinat 1 ya da katlari olmali / olabilir, o zaman 1
sayisina erismek icin 1. ve 2. kordinat degerine sonuncuya boluyoruz,
piksel degerlerini aliyoruz.Test icin xl, yl, zl degerleri ile
oynanabilir, ve yesil renkle boyanan kup seklinin resim uzerinde
degisik yerlere kondugu gorulebilir. K sayisini herkesi kendi
kamerasina gore OpenCV kalibrasyonundan hesaplatmasi
gerekiyor. Alttaki K degeri bizim kamera icin.import cvfrom pylab
import *K = array([[653.52398682, 0., 326.47888184], [0.,
653.76440430, 259.63595581], [0., 0., 1.]])color = cv.RGB(0, 255,
0)cv.NamedWindow("win")im = cv.LoadImage("[HERHANGI BIR JPG RESIM]",
cv.CV_LOAD_IMAGE_COLOR)image_size = cv.GetSize(im)print
image_size[0]print image_size[1]xl = -90yl = -50zl = 200for x in
arange(xl, xl+20): for y in arange(yl, yl+20): for z in arange(zl,
zl+20): X = array([x, y, z]) q = dot(K, X) real_q = [q[0]/q[2],
q[1]/q[2]] i_real_q = [int(real_q[0]), int(real_q[1])] if i_real_q[0]
< image_size[1] and i_real_q[1] < image_size[0]: cv.Set2D(im,
i_real_q[0], i_real_q[1], color) cv.ShowImage("win", im);k =
cv.WaitKey()





