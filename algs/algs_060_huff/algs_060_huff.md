# Huffman Kodlamasi ile Veri Sıkıştırma (Compression)

[1, sf. 159]

```python
from heapq import heapify, heappush, heappop
from itertools import count
def huffman(seq, frq):
    num = count()
    trees = list(zip(frq, num, seq))
    heapify(trees)
    while len(trees) > 1:
       fa, _, a = heappop(trees)
       fb, _, b = heappop(trees)
       n = next(num)
       heappush(trees, (fa+fb, n, [a, b]))
    return trees[0][-1]

seq = "abcdefghi"
frq = [4, 5, 6, 9, 11, 12, 15, 16, 20]
tree = huffman(seq, frq)
print tree
```

```
[['i', [['a', 'b'], 'e']], [['f', 'g'], [['c', 'd'], 'h']]]
```

```python
def codes(tree, prefix=""):
    if len(tree) == 1:
        yield (tree, prefix)                    # A leaf with its code
        return
    for bit, child in zip("01", tree):          # Left (0) and right (1)
        for pair in codes(child, prefix + bit): # Get codes recursively
            yield pair
	    
print list(codes(tree))
```

```
[('i', '00'), ('a', '0100'), ('b', '0101'), ('e', '011'), ('f', '100'), ('g', '101'), ('c', '1100'), ('d', '1101'), ('h', '111')]
```

Kaynaklar

[1] Heatland, *Python Algorithms*


