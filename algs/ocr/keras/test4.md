

```python
import util
img_gen = util.TextImageGenerator(minibatch_size=1,
                                  img_w=256,
                                  img_h=64,
                                  downsample_factor=4,
				  absolute_max_string_len=12)

```

```python
for x in img_gen.next_train():
    print x[0]['source_str'],
    img = x[0]['the_input'].reshape(256,64).T
    plt.imshow(img,cmap='gray',interpolation="none")
    plt.savefig('out1.png')    
    break
```

```text
[u'p,b,']
```





