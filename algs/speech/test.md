
```python
s = "r aɪ t"
print len(s)
```

```text
7
```



```python
from audio_reader import AudioReader
from utils import convert_inputs_to_ctc_format
from constants import c
import random

num_examples = 1
batch_size = 1
num_batches_per_epoch = int(num_examples / batch_size)


audio = AudioReader(audio_dir=c.AUDIO.VCTK_CORPUS_PATH,
                    sample_rate=c.AUDIO.SAMPLE_RATE)

def next_training_batch():
    random_index = random.choice(list(audio.cache.keys())[0:5])
    training_element = audio.cache[random_index]
    target_text = training_element['target']
    out = convert_inputs_to_ctc_format(
        training_element['audio'],c.AUDIO.SAMPLE_RATE,target_text
    )
    train_inputs, train_targets, train_seq_len, original = out
    return train_inputs, train_targets, train_seq_len, original

def next_testing_batch():
    random_index = random.choice(list(audio.cache.keys())[0:5])
    training_element = audio.cache[random_index]
    target_text = training_element['target']
    random_shift = np.random.randint(low=1, high=1000)
    print 'random_shift', random_shift
    #print('random_shift =', random_shift)
    truncated_audio = training_element['audio'][random_shift:]
    out = convert_inputs_to_ctc_format(truncated_audio,
                                       c.AUDIO.SAMPLE_RATE,
                                       target_text)
    train_inputs, train_targets, train_seq_len, original = out
    return train_inputs, train_targets, train_seq_len, original, random_shift
```

```text
Initializing AudioReader()
audio_dir = /home/burak/Downloads/vctk-p225-small
sample_rate = 8000
speakers_sub_list = None
Using the generated files at /tmp/tensorflow-ctc-speech-recognition/. Using them to load the cache. Be sure to have enough memory.
Cache took 0.13 seconds to load. 231 keys.
```



```python
out = next_training_batch()
train_inputs, train_targets, train_seq_len, original = out
print original
print train_inputs
print train_targets
print train_seq_len
```

```text
convert_inputs_to_ctc_format target_text:It's great, because it takes a lot of pressure off the players.
original:its great because it takes a lot of pressure off the players
its great because it takes a lot of pressure off the players
[[[-1.01410335  0.09610348  0.05885676 ...,  0.45179511  0.26158304
    0.16483495]
  [-1.09645912  0.61540028  1.28911651 ...,  0.27691079  0.11544174
   -0.30690321]
  [-1.19217631  0.05314168  0.83497053 ...,  0.47216508 -0.5476366
   -0.41284144]
  ..., 
  [-1.06501802  0.33609422  1.16680957 ..., -0.22431247  0.66875988
    0.64475852]
  [-1.18232858 -0.06084756  1.03090284 ..., -0.15786219  0.35646062
    0.65764037]
  [-0.93931444 -0.53781341  0.02873257 ..., -0.09629398 -0.09122601
   -0.12842322]]]
(array([[ 0,  0],
       [ 0,  1],
       [ 0,  2],
       [ 0,  3],
       [ 0,  4],
       [ 0,  5],
       [ 0,  6],
       [ 0,  7],
       [ 0,  8],
       [ 0,  9],
       [ 0, 10],
       [ 0, 11],
       [ 0, 12],
       [ 0, 13],
       [ 0, 14],
       [ 0, 15],
       [ 0, 16],
       [ 0, 17],
       [ 0, 18],
       [ 0, 19],
       [ 0, 20],
       [ 0, 21],
       [ 0, 22],
       [ 0, 23],
       [ 0, 24],
       [ 0, 25],
       [ 0, 26],
       [ 0, 27],
       [ 0, 28],
       [ 0, 29],
       [ 0, 30],
       [ 0, 31],
       [ 0, 32],
       [ 0, 33],
       [ 0, 34],
       [ 0, 35],
       [ 0, 36],
       [ 0, 37],
       [ 0, 38],
       [ 0, 39],
       [ 0, 40],
       [ 0, 41],
       [ 0, 42],
       [ 0, 43],
       [ 0, 44],
       [ 0, 45],
       [ 0, 46],
       [ 0, 47],
       [ 0, 48],
       [ 0, 49],
       [ 0, 50],
       [ 0, 51],
       [ 0, 52],
       [ 0, 53],
       [ 0, 54],
       [ 0, 55],
       [ 0, 56],
       [ 0, 57],
       [ 0, 58],
       [ 0, 59]]), array([ 9, 20, 19,  0,  7, 18,  5,  1, 20,  0,  2,  5,  3,  1, 21, 19,  5,
        0,  9, 20,  0, 20,  1, 11,  5, 19,  0,  1,  0, 12, 15, 20,  0, 15,
        6,  0, 16, 18,  5, 19, 19, 21, 18,  5,  0, 15,  6,  6,  0, 20,  8,
        5,  0, 16, 12,  1, 25,  5, 18, 19], dtype=int32), array([ 1, 60]))
[545]
```


```python
import librosa

def read_audio_from_filename(filename, sample_rate):
    # import scipy.io.wavfile as wav
    # fs, audio = wav.read(filename)
    audio, _ = librosa.load(filename, sr=sample_rate, mono=True)
    audio = audio.reshape(-1, 1)
    return audio

filename = '/home/burak/Downloads/vctk-p225-small/wav48/p225/p225_001.wav'
audio = read_audio_from_filename(filename, 8000)
print audio.shape
txt = 'Please call Stella.'
out = convert_inputs_to_ctc_format(audio,8000,txt)
train_inputs, train_targets, train_seq_len, original = out
print original
print train_inputs
print train_targets
print train_seq_len```

```text
(16413, 1)
convert_inputs_to_ctc_format target_text:Please call Stella.
original:please call stella
please call stella
[[[-0.67487496  0.16195085  0.22103811 ...,  0.30688176  0.46616323
    0.30333008]
  [-0.78461139  0.45824027  1.10632913 ...,  1.15581928 -0.21745038
    0.09131419]
  [-0.78385004  0.07659654  1.35290426 ...,  0.6201917  -0.36027755
    0.91094108]
  ..., 
  [-0.74619432 -0.30510182  0.60356367 ...,  1.16440837  0.73415372
   -0.3586614 ]
  [-0.75728365 -0.28775713  0.47213882 ...,  0.34081892 -0.27396868
   -0.2989732 ]
  [-0.68014333 -0.30977439  0.2680564  ...,  0.42448062  0.04323652
    0.12273666]]]
(array([[ 0,  0],
       [ 0,  1],
       [ 0,  2],
       [ 0,  3],
       [ 0,  4],
       [ 0,  5],
       [ 0,  6],
       [ 0,  7],
       [ 0,  8],
       [ 0,  9],
       [ 0, 10],
       [ 0, 11],
       [ 0, 12],
       [ 0, 13],
       [ 0, 14],
       [ 0, 15],
       [ 0, 16],
       [ 0, 17]]), array([16, 12,  5,  1, 19,  5,  0,  3,  1, 12, 12,  0, 19, 20,  5, 12, 12,
        1], dtype=int32), array([ 1, 18]))
[204]
```












