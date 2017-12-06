
```python
from audio_reader import AudioReader
from utils import convert_inputs_to_ctc_format
from constants import c
import random
import librosa

def read_audio_from_filename(filename, sample_rate):
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
print train_seq_len
```

```text
{u'AUDIO': {u'SAMPLE_RATE': 8000,
            u'VCTK_CORPUS_PATH': u'/home/burak/Downloads/vctk-p225-small'}}
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


```python
import os
fdir = "/home/burak/Downloads/train/audio"

def find_files(directory, pattern='.wav'):
    """Recursively finds all files matching the pattern."""
    files = []
    for root, directories, filenames in os.walk(directory):
        for filename in filenames: 
            path = os.path.join(root,filename)
            if pattern in path: files.append(path)    
    res = sorted(files)
    return res

res = find_files(fdir)

print len(res)
```

```text
23682
```

```python
import re
print res[0]
print re.findall(".*/(.*?)/.*?.wav",res[0])[0]
```

```text
/home/burak/Downloads/train/audio/down/00176480_nohash_0.wav
down
```







