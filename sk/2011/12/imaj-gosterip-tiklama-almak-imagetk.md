# Imaj Gosterip Tiklama Almak, ImageTk

TkInter diye bir paket gerekli `sudo apt-get install python python-tk
idle python-pmw python-imaging python-imaging-tk`. Tiklama almak icin
ornek kod alttta. [DIZIN] baslangic dizininden secilen imaj secilir,
ekranda gosterilir, sonra bu imaj uzerindeki tiklamalar konsola
yazilir.

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




