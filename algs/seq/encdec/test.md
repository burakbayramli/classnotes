
```python
import data_utils

en_token_ids, en_seq_lens, en_vocab_dict, en_rev_vocab_dict = data_utils.process_data('data/en.p', max_vocab_size=10000, target_lang=False)
sp_token_ids, sp_seq_lens, sp_vocab_dict, sp_rev_vocab_dict = data_utils.process_data('data/sp.p', max_vocab_size=10000, target_lang=True)

train_encoder_inputs,\
train_decoder_inputs,\
train_targets, \
train_en_seq_lens,\
train_sp_seq_len, \
valid_encoder_inputs,\
valid_decoder_inputs,\
valid_targets, \
valid_en_seq_lens,\
valid_sp_seq_len = \
    data_utils.split_data(en_token_ids, sp_token_ids, en_seq_lens, sp_seq_lens,
        train_ratio=0.8)
```

```text
16 training samples and 4 validations samples
```

```python
print train_en_seq_lens
print valid_targets
```

```text
[17 16 11 12 31 11 18 18 31 11 14 12 29 11 14 16]
[[ 16  39 217 177  31 115  36 180   5  91   9  70   5  13 220  49   4   2
    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0]
 [195 198 193 101  50  11 192  10 114  86 154  34  24  73  20 206  89   4
    2   0   0   0   0   0   0   0   0   0   0   0   0   0   0]
 [141  38  21 194  12 202 144   6  18 122  80  28 211   8  23  15 133  96
   19   7 143  31   4   2   0   0   0   0   0   0   0   0   0]
 [ 46  11  83 119  26 213   6  18 162  10 127  62 138  74   8   7 209  52
    4   2   0   0   0   0   0   0   0   0   0   0   0   0   0]]
```











