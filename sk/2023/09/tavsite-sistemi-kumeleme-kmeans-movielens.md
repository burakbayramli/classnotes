# Kumeleme (Clustering) ile Tavsiye Sistemi


ml-latest-small.zip

ml-25m.zip


```python
import json, csv
d = "."
fin = d + "/ratings.csv"
fout = d + "/ratings-json.csv"
curruser = 0
row_dict = {}
fout = open(fout, "w")
with open(fin) as csvfile:   
    rd = csv.reader(csvfile,delimiter=',')
    headers = {k: v for v, k in enumerate(next(rd))}
    for row in rd:
        if row[headers['userId']] != curruser:
            fout.write(str(curruser) + "|")
            fout.write(json.dumps(row_dict))
            fout.write("\n")
            fout.flush()
            curruser = row[headers['userId']]
            row_dict = {}       
        row_dict[int(row[headers['movieId']])] = float(row[headers['rating']])
fout.close()
```



```python
s = np.zeros((3,3,))
N = np.zeros((3,3,))

s[0,2] = 4; s[1,1] = 4; s[2,1] = 7

N[0,2] = 2; N[1,1] = 4; N[2,1] = 7

print (s)
print (N)

print (s/N)
print (np.divide(s, N, out=np.zeros_like(N), where=N>0))
```

```text
[[nan nan  2.]
 [nan  1. nan]
 [nan  1. nan]]
[[0. 0. 2.]
 [0. 1. 0.]
 [0. 1. 0.]]
```





```
movie,rating
Swordfish (2001),5
"Rock, The (1996)",5
Dunkirk (2017),2
```


Kaynaklar

[1] https://grouplens.org/datasets/movielens/

