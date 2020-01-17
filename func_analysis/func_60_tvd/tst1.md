
https://math.stackexchange.com/questions/176810/sum-of-all-elements-in-a-matrix

```python
e = np.ones((4,1))
A = np.array([[1,2,3,4],
              [1,2,3,4],
              [1,2,3,4],
	      [1,2,3,4]])

print (np.dot(np.dot(e.T,A),e))
print ((1 + 2**2 + 3**2 + 4**2)*4)
E = np.dot(A.T,A)
print (E)
print (np.dot(np.dot(e.T,E),e))

print (np.trace(np.dot(A.T,A)))
```

```text
[[40.]]
120
[[ 4  8 12 16]
 [ 8 16 24 32]
 [12 24 36 48]
 [16 32 48 64]]
[[400.]]
120
```









