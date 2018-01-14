
```python
import os
train_dir = '/home/burak/Downloads/train/audio'
all_train_files = []
for d, r, f in os.walk(train_dir):
    for filename in f:
    	all_train_files.append(os.path.join(d,filename))
print all_train_files[:10]
noise_files = [x for x in all_train_files if "_background_noise_" in x and ".wav" in x]
train_files = [x for x in all_train_files if ".wav" in x and x not in noise_files]
```

```python
import random, re

labels = ['down','go','left','no','off','on','right','stop','up','yes']
all_labels = labels + ['unknown','silence']

def get_minibatch(batch_size):
    for i in range(batch_size):
        if random.choice(range(15)) == 0:
	   # silence
	   pass
        elif random.choice(range(10)) == 0:
	   # unknown
	   pass
	else:
           f = random.choice(train_files)
           label = re.findall(".*/(.*?)/.*?.wav",f)[0]
	   print f, label
	break

print get_minibatch(1)

```

```text
/home/burak/Downloads/train/audio/two/28ce0c58_nohash_0.wav two
None
```


























