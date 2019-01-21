# Imaj Uzerine Text


Imaj Uzerine Text



OpenCV ile imaj islerken ayni imaj uzerine yazi / metin (text) koymak istiyorsak, su cagriyi kullanabiliriz.pt1 = cv.cvPoint (100, 120)font = cv.cvInitFont (CV_FONT_HERSHEY_SIMPLEX, 1.0, 1.0, 0, 1, CV_AA)cvPutText (image, "Testing text rendering!", pt1, font, CV_RGB(255,0,0))Font parametresinden sonra gelen her iki 1.0 yazinin buyuklugunu, en sondaki 1 ise kalinligini temsil ediyor. CV_RGB parametresi yazinin rengi icin kullaniliyor (255,0,0 kirmizi rengi).




