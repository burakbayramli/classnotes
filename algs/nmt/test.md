
```python
import utils
loaded_infer_model, global_step = utils.create_or_load_model(\
    infer_model.model, model_dir, infer_sess, "infer")

```


















```python
import utils, pickle
print 'loading...'
[en_token_ids, en_seq_lens, en_vocab_dict, en_rev_vocab_dict] = pickle.load(open('/tmp/vocab_en.pkl','rb'))
[sp_token_ids, sp_seq_lens, sp_vocab_dict, sp_rev_vocab_dict] = pickle.load(open('/tmp/vocab_sp.pkl','rb'))
```

```text
loading...
```


```python
print 'train / valid split...'
train_encoder_inputs, \
    train_decoder_inputs, \
    train_targets, \
    train_en_seq_lens, \
    train_sp_seq_len, \
    valid_encoder_inputs, \
    valid_decoder_inputs, \
    valid_targets, \
    valid_en_seq_lens, \
    valid_sp_seq_len = \
    utils.split_data(en_token_ids, sp_token_ids, en_seq_lens, sp_seq_lens, train_ratio=0.8)
```

```text
train / valid split...
288000 training samples and 72000 validations samples
```



```python
res = utils.get_minibatch(train_encoder_inputs,\
                          train_decoder_inputs, train_targets,\
                          train_en_seq_lens, train_sp_seq_len, 10)

(batch_encoder_inputs, batch_decoder_inputs,
 batch_targets, batch_en_seq_lens,
 batch_sp_seq_lens) = res

print batch_encoder_inputs.shape
print batch_decoder_inputs.shape
print batch_targets
print batch_en_seq_lens
```

```text
(10, 20)
(10, 20)
[[   26   216 18602     4     2     0     0     0     0     0     0     0
      0     0     0     0     0     0     0     0]
 [   93    51    11     3     4     2     0     0     0     0     0     0
      0     0     0     0     0     0     0     0]
 [    3     9    17    22     6     2     0     0     0     0     0     0
      0     0     0     0     0     0     0     0]
 [    5    29  8473   364     4     2     0     0     0     0     0     0
      0     0     0     0     0     0     0     0]
 [    5    38   216   204   461     4     2     0     0     0     0     0
      0     0     0     0     0     0     0     0]
 [  186  2531    16    14     6     2     0     0     0     0     0     0
      0     0     0     0     0     0     0     0]
 [11193   395     4     2     0     0     0     0     0     0     0     0
      0     0     0     0     0     0     0     0]
 [   13   137  2201  1060     6     2     0     0     0     0     0     0
      0     0     0     0     0     0     0     0]
 [    5     9  1416   776   602     4     2     0     0     0     0     0
      0     0     0     0     0     0     0     0]
 [ 1474 11189  1810     4     2     0     0     0     0     0     0     0
      0     0     0     0     0     0     0     0]]
[ 5.  7.  6.  6.  7.  5.  5.  7.  8.  9.]
```

```python
a = np.zeros(10)
print a
```

```text
[ 0.  0.  0.  0.  0.  0.  0.  0.  0.  0.]
```








