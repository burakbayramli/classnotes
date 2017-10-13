
```python
import data_utils
import translate

en_token_ids, en_seq_lens, en_vocab_dict, en_rev_vocab_dict = data_utils.process_data('data/my_en.txt', max_vocab_size=5000, target_lang=False)
sp_token_ids, sp_seq_lens, sp_vocab_dict, sp_rev_vocab_dict = data_utils.process_data('data/my_sp.txt', max_vocab_size=5000, target_lang=True)
FLAGS = translate.parameters()

FLAGS.batch_size = 1
FLAGS.en_vocab_size = len(en_vocab_dict)
FLAGS.sp_vocab_size = len(sp_vocab_dict)
FLAGS.sp_max_len = max(sp_seq_lens) + 1 # GO token
```

```python
inference_sentence = ["the economy is growing."]
# Split into tokens
tokenized = []
for i in xrange(len(inference_sentence)):
    tokenized.append(data_utils.basic_tokenizer(inference_sentence[i]))
# Convert data to token ids
data_as_tokens, sample_en_seq_lens = data_utils.data_to_token_ids(
    tokenized, en_vocab_dict, target_lang=False, normalize_digits=True)

# make dummy_sp_inputs
dummy_sp_inputs = np.array([[data_utils.GO_ID]*FLAGS.sp_max_len])
sample_sp_seq_lens = np.array([len(dummy_sp_inputs)])

print data_as_tokens
print sample_en_seq_lens
print dummy_sp_inputs
print sample_sp_seq_lens
```

```text
[[ 4 63 15 59  6  0]]
[5]
[[1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
  1 1 1 1 1 1]]
[1]
```

```python
import tensorflow as tf
tf.reset_default_graph()
with tf.Session() as sess:
     model = translate.restore_model(sess, FLAGS)

     batch_encoder_inputs=data_as_tokens
     batch_decoder_inputs=dummy_sp_inputs
     batch_targets=None
     batch_en_seq_lens=sample_en_seq_lens
     batch_sp_seq_lens=sample_sp_seq_lens
     dropout=0.0
     forward_only=True
     sampling=True

     input_feed = {
         model.encoder_inputs: batch_encoder_inputs,
         model.decoder_inputs: batch_decoder_inputs,
         model.en_seq_lens: batch_en_seq_lens,
	 model.sp_seq_lens: batch_sp_seq_lens,
         model.dropout: dropout}


     logits = tf.reshape(model.logits_flat, [FLAGS.batch_size,FLAGS.sp_max_len, FLAGS.sp_vocab_size])
     y_pred = tf.argmax(logits, 2)
     output_feed = [y_pred]
     outputs = sess.run(output_feed, input_feed)
     res = outputs[0]

```

```text
[322  44  44  44  44  44  44  44  44  44  44  44  44  44  44  44  44  44
  44  44  44  44  44  44  44  44  44  44  44  44  44  44  44  44  44  44
  44  44  44  44  44  44  44]
```

```python
fout = open("out.md","w")
sp_sentence = []
for idx in list(res[0]):
    sp_sentence.append(sp_rev_vocab_dict[idx])
for word in sp_sentence:
    fout.write(word)
    fout.write(" ")
fout.close()
```




























