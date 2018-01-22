
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
(20,)
(20,)
[   5   11   20    4    3    5 6527   20 1297    4    2    0    0    0    0
    0    0    0    0    0]
5
```

```python
a = np.zeros(10)
print a
```

```text
[ 0.  0.  0.  0.  0.  0.  0.  0.  0.  0.]
```








