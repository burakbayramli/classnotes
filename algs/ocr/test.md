
```python
import fakestrings
print fakestrings.randomstring()
```

```text
('+420-207962763', 'phone_num')
```

```python
from PIL import Image
import ockre
(s,t) = fakestrings.randomstring()
print s, t
res = ockre.paint_text(s,520,64,rotate=True,ud=True,multi_fonts=True)
res = res.reshape(64,520)
print res.shape
#im = Image.fromarray(res)
#plt.imshow(res)
plt.imshow(res,cmap='gray',interpolation="none")
plt.savefig('out1.png')
```

```text
CZ916500000033206770205 iban
(64, 520)
```












