
```python
import util
print util.randomstring()
```

```text
('333848355', 'invoice_id')
```

```python
import util, ockre
from PIL import Image

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
EQBKCZPP bic
(64, 520)
```













