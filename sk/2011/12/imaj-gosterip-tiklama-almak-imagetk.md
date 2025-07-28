# İmaj Gösterip Tıklama Almak, ImageTk

Tkİnter diye bir paket gerekli `sudo apt-get install python3-tk`.

Kordinat Toplamak

Diyelim ki elimizde JPG üzerinde bir harita var, üzerinde bir sınır
gösterilmiş, ve oradan sınır verilerini toplamak istiyoruz. Alttaki
basit GUI kodu şunu yapar 1) Parametre olarak geçilen resmi gösterir,
2) O resim üzerinde yapılacak fare tıklamalarının x,y piksel
kordinatlarını ekrana basar. Bu sayede çıplak gözle sınıra bakarız,
üzerine tıklarız, ve kordinatlarını "geriye mühendislik" ile toplamış
oluruz.

```python
window = tkinter.Tk(className="bla")
image = Image.open(sys.argv[1])
print ('size',image.size[0],image.size[1])

image = image.resize((int(1400), int(900)), Image.LANCZOS)
canvas = tkinter.Canvas(window, width=image.size[0], height=image.size[1])
canvas.pack()
image_tk = ImageTk.PhotoImage(image)

canvas.create_image(image.size[0]//2, image.size[1]//2, image=image_tk)

def callback(event):
    print ("[%d,%d]," % (event.x, event.y))

canvas.bind("<Button-1>", callback)
tkinter.mainloop()
```

Gerekli paketi kurmak için `sudo apt install python3-tk` yeterli.

Tabii nihai kordinatlar için ek bir adım daha gerekli, piksel
değerlerini alıp enlem, boylama çevirmek; bu hesap için enlem
boylamını bildiğimiz iki piksel değeri yeterli. Alttaki gibi bir kodla
bu transformasyon yapılabilir. Referans iki kordinat değeri `refc`,
ona tekabül eden iki piksel x,y değeri `refp` içinde, ve toplanan tüm
piksel değerleri bir liste olarak `clicks` içinde.

```python
def click_geo(refc, refp, clicks):
    if len(refc) != 2 or len(refp) != 2:
        raise ValueError("`refc` and `refp` must each contain exactly two reference points.")
    if any(len(p) != 2 for p in refc) or any(len(p) != 2 for p in refp):
        raise ValueError("All reference points must be 2-element lists [x, y] or [lat, lon].")
    if not isinstance(clicks, list) or not all(isinstance(c, list) and len(c) == 2 for c in clicks):
        raise ValueError("`clicks` must be a list of 2-element lists [x, y].")

    x1_pix, y1_pix = refc[0]
    x2_pix, y2_pix = refc[1]

    lat1_geo, lon1_geo = refp[0]
    lat2_geo, lon2_geo = refp[1]

    delta_x_pix = x2_pix - x1_pix
    delta_y_pix = y2_pix - y1_pix

    delta_lat_geo = lat2_geo - lat1_geo
    delta_lon_geo = lon2_geo - lon1_geo

    lat_scale_per_pixel = delta_lat_geo / delta_y_pix
    lat_offset = lat1_geo - (lat_scale_per_pixel * y1_pix)

    lon_scale_per_pixel = delta_lon_geo / delta_x_pix
    lon_offset = lon1_geo - (lon_scale_per_pixel * x1_pix)

    converted_coords = []
    for click_x, click_y in clicks:
        new_lat = (lat_scale_per_pixel * click_y) + lat_offset
        new_lon = (lon_scale_per_pixel * click_x) + lon_offset
        converted_coords.append([new_lat, new_lon])

    return converted_coords
```

Mesela alttaki degerler Yunanistan'dan baslayip Mumbai'de biten bir cizgiyi
ceviriyor,

```python
refc = [217,313],[987,643],
refp = [[37.94218358, 23.645406289],[19.057240727, 73.32631926]]
pts = [[219,317], [401,386], [441,422], [570,535],[611,556], [660,566], [722,530], [987,643]]
click_geo(refc,refp,pts)
```

Eski Yazı

Tıklama almak için örnek kod alttta. [DİZİN] başlangıç dizininden
seçilen imaj seçilir, ekranda gösterilir, sonra bu imaj üzerindeki
tıklamalar konsola yazılır.

```python
from Tkinter import *from tkFile
Dialog import askopenfilename
import Image, ImageTkif
__name__ == "__main__":

root = Tk()
#setting up a tkinter canvas with scrollbars
frame = Frame(root, bd=2, relief=SUNKEN)
frame.grid_rowconfigure(0, weight=1)
frame.grid_column
configure(0, weight=1)
xscroll = Scrollbar(frame, orient=HORIZONTAL)
xscroll.grid(row=1, column=0,sticky=E+W)
yscroll = Scrollbar(frame)
yscroll.grid(row=0, column=1,sticky=N+S)
canvas = Canvas(frame, bd=0, xscrollcommand=xscroll.set,yscrollcommand=yscroll.set)
canvas.grid(row=0, column=0, sticky=N+S+E+W)
xscroll.config(command=canvas.xview)
yscroll.config(command=canvas.yview)
frame.pack(fill=BOTH,expand=1)
#adding the
imageFile = askopenfilename(parent=root, initialdir="/[DIZIN]",title='Choose an image.')

img = ImageTk.PhotoImage(Image.open(File))
canvas.create_image(0,0,image=img,anchor="nw")
canvas.config(scrollregion=canvas.bbox(ALL))
#function to be called when mouse is clicked

def printcoords(event):
    #outputting x and y coords to console
    print (event.x,event.y)
#mouseclick eventcanvas.bind("<Button 1>",printcoords)
root.mainloop()
```

Daha oz kisa bir ornek

```python
import Tkinter
from PIL import ImageDraw, Image, ImageTk
import sys

window = Tkinter.Tk(className="bla")
image = Image.open(sys.argv[1])
image = image.resize((1000, 800),Image.ANTIALIAS)
canvas = Tkinter.Canvas(window, width=image.size[0], height=image.size[1])
canvas.pack()
image_tk = ImageTk.PhotoImage(image)
canvas.create_image(image.size[0]//2,image.size[1]//2, image=image_tk)

def callback(event):
  print "clicked at: ", event.x, event.ycanvas.bind("", callback)

Tkinter.mainloop()
```




