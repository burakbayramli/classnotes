
```python
import util
print util.randomstring()
```

```text
('CZ13909878', 'sender_dic')
```

```python
from PIL import Image
import util

(s,t) = util.randomstring()
print s, t
res = util.paint_text(s,520,64,rotate=True,ud=True,multi_fonts=True)
res = res.reshape(64,520)
print res.shape
#im = Image.fromarray(res)
#plt.imshow(res)
plt.imshow(res,cmap='gray',interpolation="none")
plt.savefig('out1.png')
```

```text
0,00 amount_rounding
(64, 520)
```













