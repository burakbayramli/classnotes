
```python
a = [3,4,3,4,5]

it = iter(a)
print it.next()
print it.next()
print it.next()
print it.next()
print it.end()
```

```text
3
4
3
4
5
```


```python
import zipfile

zip = '/home/burak/Downloads/goog_voice_test.zip'

with zipfile.ZipFile(zip, 'r') as z:
     filesit = iter(z.namelist())

```

```python
print filesit.next()
print filesit.next()
print filesit.next()
print filesit.next()
print filesit.next()
print filesit.
```

```text
test/audio/
test/audio/clip_7afff8114.wav
test/audio/clip_d7129a6e9.wav
test/audio/clip_2fba10c36.wav
test/audio/clip_977e1b901.wav
```


```python
import re
s = 'test/audio/clip_977e1b901.wav'
print re.findall(".*/.*?/(.*?.wav)",s)[0]
```

```text
clip_977e1b901.wav
```






