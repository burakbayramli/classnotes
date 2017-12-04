
```python
import ker_ocr5
mfile = 'ocr4.h5'

pool_size = 3
img_w = 256
img_h = 64
minibatch_size = 1
model, test_func = ker_ocr5.get_model(img_w,img_h,minibatch_size,pool_size)
model.load_weights(mfile)
```

```text
____________________________________________________________________________________________________
Layer (type)                     Output Shape          Param #     Connected to                     
====================================================================================================
the_input (InputLayer)           (None, 256, 64, 1)    0                                            
____________________________________________________________________________________________________
conv1 (Conv2D)                   (None, 256, 64, 20)   100         the_input[0][0]                  
____________________________________________________________________________________________________
max1 (MaxPooling2D)              (None, 85, 21, 20)    0           conv1[0][0]                      
____________________________________________________________________________________________________
conv2 (Conv2D)                   (None, 85, 21, 20)    1620        max1[0][0]                       
____________________________________________________________________________________________________
max2 (MaxPooling2D)              (None, 28, 7, 20)     0           conv2[0][0]                      
____________________________________________________________________________________________________
reshape (Reshape)                (None, 28, 140)       0           max2[0][0]                       
____________________________________________________________________________________________________
dense1 (Dense)                   (None, 28, 32)        4512        reshape[0][0]                    
____________________________________________________________________________________________________
gru1 (GRU)                       (None, 28, 256)       221952      dense1[0][0]                     
____________________________________________________________________________________________________
gru1_b (GRU)                     (None, 28, 256)       221952      dense1[0][0]                     
____________________________________________________________________________________________________
add_3 (Add)                      (None, 28, 256)       0           gru1[0][0]                       
                                                                   gru1_b[0][0]                     
____________________________________________________________________________________________________
gru2 (GRU)                       (None, 28, 256)       393984      add_3[0][0]                      
____________________________________________________________________________________________________
gru2_b (GRU)                     (None, 28, 256)       393984      add_3[0][0]                      
____________________________________________________________________________________________________
concatenate_3 (Concatenate)      (None, 28, 512)       0           gru2[0][0]                       
                                                                   gru2_b[0][0]                     
____________________________________________________________________________________________________
dense2 (Dense)                   (None, 28, 40)        20520       concatenate_3[0][0]              
____________________________________________________________________________________________________
softmax (Activation)             (None, 28, 40)        0           dense2[0][0]                     
====================================================================================================
Total params: 1,258,624
Trainable params: 1,258,624
Non-trainable params: 0
____________________________________________________________________________________________________
```

```python
import util5
img_gen = util5.TextImageGenerator(minibatch_size=1,
                                  img_w=img_w,
                                  img_h=img_h,
                                  downsample_factor=4,
                                  absolute_max_string_len=12)

for x in img_gen.next_train():
    print x[0].keys()
    print x[0]['source_str'],
    img = x[0]['the_input'].reshape(img_w,img_h).T
    plt.imshow(img,cmap='gray',interpolation="none")
    plt.savefig('out1.png')    
    break
```

```text
['the_input', 'the_labels', 'label_length', 'source_str', 'input_length']
[u'b3ds4uehz']
```
b3qs4uehz

```python
#preds = model.predict(x_test)
#preds = model.evaluate(x_test, y_test)
#o = test_func(x)
```

```python
import itertools

def labels_to_text(labels):
    ret = []
    for c in labels:
        if c == len(util5.alphabet):  # CTC Blank
            ret.append("")
        else:
            ret.append(util5.alphabet[c])
    return "".join(ret)


def decode_batch(test_func, word_batch):
    out = test_func([word_batch])[0]
    ret = []
    for j in range(out.shape[0]):
        out_best = list(np.argmax(out[j, 2:], 1))
        out_best = [k for k, g in itertools.groupby(out_best)]
        outstr = labels_to_text(out_best)
        ret.append(outstr)
    return ret

#test_func([ x[0]['the_input'] ])
pred_result = decode_batch(test_func, x[0]['the_input'])[0]
print pred_result
```

```text
b3qs4uehz
```

































