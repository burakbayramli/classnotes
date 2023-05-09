# Numpy Temelli 2 Kolon Icin Basit Gruplama (Group By)

SQL'deki group by komutunun Numpy karsiligi. 2 kolon bazinda
calisiyor, tum islemler hafizada.

```
import numpy as npimport itertools
def group2(arr):
    rows,cols = arr.shape
    res = []
    c = 0
    for unique_0,unique_1 in itertools.product(np.unique(arr[:,0]), np.unique(arr[:,1])):
        tmp = arr[arr[:,0] == unique_0]
        tmp = tmp[tmp[:,1] == unique_1]
        res.append([unique_0, unique_1, len(tmp)])
    return np.array(res)

if __name__ == "__main__":
    a = np.array([[1,2,5,4,5,5],
                  [4,5,3,4,4,3]])
    print group2(a.T)
    print group2(a.T).shape
```
