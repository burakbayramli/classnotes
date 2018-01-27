
```python
import utils
hparams = utils.load_hparams('/tmp/nmt_model')
```

```text
# Loading hparams from /tmp/nmt_model/hparams
```


```python
import model, model_helper
model_creator = model.Model
infer_model = model_helper.create_infer_model(model_creator, hparams)
```

```text
# creating infer graph ...
  num_layers = 2, num_residual_layers=0
  cell 0  LSTM, forget_bias=1  DeviceWrapper, device=/gpu:0
  cell 1  LSTM, forget_bias=1  DeviceWrapper, device=/gpu:0
  cell 0  LSTM, forget_bias=1  DeviceWrapper, device=/gpu:0
  cell 1  LSTM, forget_bias=1  DeviceWrapper, device=/gpu:0
# Trainable variables
  embeddings/encoder/embedding_encoder:0, (24646, 128), /device:GPU:0
  embeddings/decoder/embedding_decoder:0, (106604, 128), /device:CPU:0
  dynamic_seq2seq/encoder/rnn/multi_rnn_cell/cell_0/basic_lstm_cell/kernel:0, (256, 512), /device:GPU:0
  dynamic_seq2seq/encoder/rnn/multi_rnn_cell/cell_0/basic_lstm_cell/bias:0, (512,), /device:GPU:0
  dynamic_seq2seq/encoder/rnn/multi_rnn_cell/cell_1/basic_lstm_cell/kernel:0, (256, 512), /device:GPU:0
  dynamic_seq2seq/encoder/rnn/multi_rnn_cell/cell_1/basic_lstm_cell/bias:0, (512,), /device:GPU:0
  dynamic_seq2seq/decoder/multi_rnn_cell/cell_0/basic_lstm_cell/kernel:0, (256, 512), /device:GPU:0
  dynamic_seq2seq/decoder/multi_rnn_cell/cell_0/basic_lstm_cell/bias:0, (512,), /device:GPU:0
  dynamic_seq2seq/decoder/multi_rnn_cell/cell_1/basic_lstm_cell/kernel:0, (256, 512), /device:GPU:0
  dynamic_seq2seq/decoder/multi_rnn_cell/cell_1/basic_lstm_cell/bias:0, (512,), /device:GPU:0
  dynamic_seq2seq/decoder/output_projection/kernel:0, (128, 106604), 
```

```python
import tensorflow as tf
import inference, os, time

log_device_placement = hparams.log_device_placement
out_dir = hparams.out_dir
num_train_steps = hparams.num_train_steps
steps_per_stats = hparams.steps_per_stats
steps_per_external_eval = hparams.steps_per_external_eval
steps_per_eval = 10 * steps_per_stats
avg_ckpts = hparams.avg_ckpts
dev_src_file = "%s.%s" % (hparams.dev_prefix, hparams.src)
dev_tgt_file = "%s.%s" % (hparams.dev_prefix, hparams.tgt)
print (dev_src_file)
print (dev_tgt_file)
sample_src_data = inference.load_data(dev_src_file)
sample_tgt_data = inference.load_data(dev_tgt_file)
print 'sample_src_data', sample_src_data[:3]
print 'sample_tgt_data', sample_tgt_data[:3]
summary_name = "train_log"
model_dir = hparams.out_dir

# Log and output files
log_file = os.path.join(out_dir, "log_%d" % time.time())
log_f = tf.gfile.GFile(log_file, mode="a")
utils.print_out("# log_file=%s" % log_file, log_f)

config_proto = utils.get_config_proto(
    log_device_placement=log_device_placement,
    num_intra_threads=hparams.num_intra_threads,
    num_inter_threads=hparams.num_inter_threads)

infer_sess = tf.Session(target='', config=config_proto, graph=infer_model.graph)
```

```text
/home/burak/Downloads/tur-eng/tst2012.en
/home/burak/Downloads/tur-eng/tst2012.tr
sample_src_data [u'"Would you like to go with me ?" "You bet !"', u'A baseball came flying through the window .', u'A belt keeps your pants from falling down .']
sample_tgt_data [u'"Benimle gitmek ister misin ?" "Kesinlikle !"', u'Bir beyzbol topu pencereden u\xe7arak geldi .', u'Kemer pantolonunun d\xfc\u015fmesini \xf6nler .']
# log_file=/tmp/nmt_model/log_1517060367
```

```python
my_src = ['I was walking with my friend outside']
my_tgt = ['Disarida arkadasimla yuruyordum']
```



```python
import train, inference

model_dir = "/tmp/nmt_model/translate.ckpt-11000"
with infer_model.graph.as_default():
    loaded_infer_model, global_step = model_helper.create_or_load_model(
        infer_model.model, model_dir, infer_sess, "infer")

sample_src_data = inference.load_data(dev_src_file)
sample_tgt_data = inference.load_data(dev_tgt_file)

summary_writer = tf.summary.FileWriter("/tmp/out", infer_model.graph)

train._sample_decode(loaded_infer_model, global_step, infer_sess, hparams,
#                     infer_model.iterator, sample_src_data, sample_tgt_data,
                     infer_model.iterator, my_src, my_tgt,
                     infer_model.src_placeholder,
                     infer_model.batch_size_placeholder, summary_writer)	
```









