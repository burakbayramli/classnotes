

```python
import pickle
file = open("recs.pkl",'r')
recs = pickle.load(file)
print len(recs)
file.close()
```

```text
3731
```

```python
idx = 1000
print len(recs[idx])
#print recs[idx]
# tte,censor,age,gender
```

```text
24
```

































































