# Jetson Nano

microSD Card (UHS-1 32 GB minimum) 64 GB

https://developer.nvidia.com/embedded/learn/get-started-jetson-nano-2gb-devkit

https://developer.nvidia.com/embedded/learn/get-started-jetson-nano-devkit#setup

https://developer.nvidia.com/embedded/learn/get-started-jetson-nano-2gb-devkit#write

https://developer.nvidia.com/jetson-nano-2gb-sd-card-image

```
wget --continue https://developer.download.nvidia.com/assets/embedded/downloads/jetson-nano-2gb-jp441-sd-card-image/jetson-nano-2gb-jp441-sd-card-image.zip
```

Etcher

https://www.balena.io/etcher/

https://phoenixnap.com/kb/etcher-ubuntu

https://youtu.be/Ch1NKfER0oM

```
$ dmesg | grep --color 'tty'
[106921.687362] cdc_acm 1-2:1.2: ttyACM0: USB ACM device
```

Ya da `ls /dev/tty*` ile bakarsak orada bir `/dev/ttyACM0` gormemiz lazim.


https://imadelhanafi.com/posts/jetson_nano_setup/


```
sudo nvpmodel -q

NVPM WARN: fan mode is not set!
NV Power Mode: MAXN
0
```

```
vim ~/.bashrc
```


```
export PATH=${PATH}:/usr/local/cuda/bin
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/usr/local/cuda/lib64
```

```
source ~/.bashrc
```


```
nvcc --version
```


```
nvcc: NVIDIA (R) Cuda compiler driver
Copyright (c) 2005-2019 NVIDIA Corporation
Built on Wed_Oct_23_21:14:42_PDT_2019
Cuda compilation tools, release 10.2, V10.2.89
```









