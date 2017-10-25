
```python
import numpy as np
import string
from PIL import Image, ImageFont, ImageDraw
 
def MakeImg(t, f, fn, s = (300, 50), o = (20, 20)):
    '''
    Generate an image of text
    t:      The text to display in the image
    f:      The font to use
    fn:     The file name
    s:      The image size
    o:      The offest of the text in the image
    '''
    img = Image.new('RGB', s, "Gray")
    draw = ImageDraw.Draw(img)
    draw.text(o, t, (0, 0, 0), font = f)
    img.save(fn)

S = "Hello alkjsfl klajsdlkj"
font = ImageFont.truetype("LiberationMono-Regular.ttf", 16)
MakeImg(S, font,'out.png')
```

```text
(0, 0)
```






















