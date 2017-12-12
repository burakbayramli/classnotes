```python
zip = '/media/burak/New Volume/archive/data/google_voice/test.zip'
#zip = '/home/burak/Downloads/goog_voice_train.zip'
import zipfile, pandas as pd, random
with zipfile.ZipFile(zip, 'r') as z:
     res = z.namelist()
```


```python
f = random.choice(res)
print f	 
```

```text
test/audio/clip_5c76812f0.wav
```
