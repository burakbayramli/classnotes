
```python
import os, random, re

train_dir = '/home/burak/Downloads/train/audio'
labels = ['down','go','left','no','off','on','right','stop','up','yes']
all_labels = labels + ['unknown','silence']

all_train_files = []
for d, r, f in os.walk(train_dir):
    for filename in f:
    	all_train_files.append(os.path.join(d,filename))
noise_files = [x for x in all_train_files if "_background_noise_" in x and ".wav" in x]
train_files = []
unknown_files = []
for x in all_train_files:
    if ".wav" in x: 
       label = re.findall(".*/(.*?)/.*?.wav",x)[0]
       if label not in labels and x not in noise_files: unknown_files.append(x)
       elif x not in noise_files: train_files.append(x)
```

```python
print all_train_files[:10]
print train_files[:10]
print unknown_files[:10]
```

```text
['/home/burak/Downloads/train/audio/_background_noise_/pink_noise.wav', '/home/burak/Downloads/train/audio/_background_noise_/README.md', '/home/burak/Downloads/train/audio/_background_noise_/white_noise.wav', '/home/burak/Downloads/train/audio/_background_noise_/exercise_bike.wav', '/home/burak/Downloads/train/audio/_background_noise_/running_tap.wav', '/home/burak/Downloads/train/audio/_background_noise_/dude_miaowing.wav', '/home/burak/Downloads/train/audio/_background_noise_/doing_the_dishes.wav', '/home/burak/Downloads/train/audio/on/789e4ee7_nohash_0.wav', '/home/burak/Downloads/train/audio/on/c103a2d5_nohash_0.wav', '/home/burak/Downloads/train/audio/on/36050ef3_nohash_3.wav']
['/home/burak/Downloads/train/audio/on/789e4ee7_nohash_0.wav', '/home/burak/Downloads/train/audio/on/c103a2d5_nohash_0.wav', '/home/burak/Downloads/train/audio/on/36050ef3_nohash_3.wav', '/home/burak/Downloads/train/audio/on/f953e1af_nohash_2.wav', '/home/burak/Downloads/train/audio/on/126a31d2_nohash_0.wav', '/home/burak/Downloads/train/audio/on/6414258b_nohash_0.wav', '/home/burak/Downloads/train/audio/on/2c7c33e8_nohash_0.wav', '/home/burak/Downloads/train/audio/on/39999a0f_nohash_0.wav', '/home/burak/Downloads/train/audio/on/763188c4_nohash_3.wav', '/home/burak/Downloads/train/audio/on/340c8b10_nohash_0.wav']
['/home/burak/Downloads/train/audio/happy/3efef882_nohash_0.wav', '/home/burak/Downloads/train/audio/happy/789e4ee7_nohash_0.wav', '/home/burak/Downloads/train/audio/happy/19b05529_nohash_0.wav', '/home/burak/Downloads/train/audio/happy/9080f6d3_nohash_0.wav', '/home/burak/Downloads/train/audio/happy/126a31d2_nohash_0.wav', '/home/burak/Downloads/train/audio/happy/c392e01d_nohash_0.wav', '/home/burak/Downloads/train/audio/happy/fffcabd1_nohash_0.wav', '/home/burak/Downloads/train/audio/happy/d750966e_nohash_0.wav', '/home/burak/Downloads/train/audio/happy/340c8b10_nohash_0.wav', '/home/burak/Downloads/train/audio/happy/fb24c826_nohash_0.wav']
```


```python
def get_minibatch(batch_size, silence_percent=0.10, unknown_percent=0.15):
    for i in range(batch_size):
        if random.choice(range(int(1/silence_percent))) == 0:
	   # silence
           f = random.choice(noise_files)
	   print f, 'noise'
        elif random.choice(range(int(1/unknown_percent))) == 0:
	   # unknown
           f = random.choice(unknown_files)
	   print f, 'unknown'  
	else:
           f = random.choice(train_files)
           label = re.findall(".*/(.*?)/.*?.wav",f)[0]
	   print f, label
	

print get_minibatch(20)
```

```text
/home/burak/Downloads/train/audio/right/743edf9d_nohash_0.wav right
/home/burak/Downloads/train/audio/down/f3cee168_nohash_1.wav down
/home/burak/Downloads/train/audio/three/18a8f03f_nohash_0.wav unknown
/home/burak/Downloads/train/audio/stop/0ff728b5_nohash_3.wav stop
/home/burak/Downloads/train/audio/house/626e323f_nohash_1.wav unknown
/home/burak/Downloads/train/audio/right/b3327675_nohash_0.wav right
/home/burak/Downloads/train/audio/yes/39a6b995_nohash_0.wav yes
/home/burak/Downloads/train/audio/left/6166ae21_nohash_1.wav left
/home/burak/Downloads/train/audio/four/6823565f_nohash_1.wav unknown
/home/burak/Downloads/train/audio/left/3777c08e_nohash_1.wav left
/home/burak/Downloads/train/audio/right/9aa21fa9_nohash_1.wav right
/home/burak/Downloads/train/audio/_background_noise_/pink_noise.wav noise
/home/burak/Downloads/train/audio/no/c120e80e_nohash_2.wav no
/home/burak/Downloads/train/audio/_background_noise_/dude_miaowing.wav noise
/home/burak/Downloads/train/audio/six/37e8db82_nohash_0.wav unknown
/home/burak/Downloads/train/audio/happy/62641b88_nohash_0.wav unknown
/home/burak/Downloads/train/audio/on/7eee5973_nohash_1.wav on
/home/burak/Downloads/train/audio/down/00b01445_nohash_1.wav down
/home/burak/Downloads/train/audio/up/c661be6e_nohash_0.wav up
/home/burak/Downloads/train/audio/off/ab76ac76_nohash_0.wav off
None
```


























