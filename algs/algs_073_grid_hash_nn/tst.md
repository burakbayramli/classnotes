
```python
bin_width = 10.
bin_max = 200.
bins = np.linspace(0, bin_max, int((bin_max-bin_min)/bin_width) )
print (bins)

n = 193
def bin(x):
   res = int(x / bin_width)+1
   if res>=len(bins): return len(bins)
   return res
print (bin(n))
print (np.digitize(n, bins))
```

```text
[  0.  10.  20.  30.  40.  50.  60.  70.  80.  90. 100. 110. 120. 130.
 140. 150. 160. 170. 180. 190. 200.]
20
20
```

















