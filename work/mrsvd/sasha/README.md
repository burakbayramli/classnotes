Sasha 
--------------------------------------------

[S]treaming [A]dd-on for [SHA]rded Computation aims to be a
distributed streaming framework using a map/reduce-like language, in
Python. For distribution it uses ZeroMQ queues to distribute work to
nodes, based on key. Nodes are found and delegated to by using a
mod(key, cluster_size) calculation.

**REQUIREMENTS**

python, numpy, pyzmq

If there are problems with queues, download latest libzmq,
./autogen.sh, ./configure, make and sudo make install.

**CONFIGURATION**

Cluster definitions reside in $HOME/.sasha.conf file. Single process,
single server definition can look like:
    
```
my_local:  
  cluster:
    - localhost:5000:/data/node1_dir
```
    
To start

```
python sasha.py my_local localhost 5000
```

Single node, two server setup

```
my_multi_local:  
  cluster:
    - localhost:5000:/data/node1_dir
    - localhost:5001:/data/node2_dir
```
    
In this case, sasha.py server would have to executed for both 5000 and
5001 in seperate terminals on the same machine (see the Scripts
section for a script that can automatically start each server in the
cluster) . Seperate hosts, seperate servers setup:

```
my_production:  
  cluster:
    - 192.168.1.1:5000:/data/sasha
    - 192.168.1.2:5000:/data/sasha
    - 192.168.1.3:5000:/data/sasha
```

You need to define sasha.dir as well, such as

```
sasha.dir:
  /home/user1/some/directory/sasha
```

Simple job

```
import re, sys
from sasha import job

WORD_RE = re.compile(r"[\w']+")

class WordFreqCount(job.SashaJob):

    def __init__(self):
        job.SashaJob.__init__(self)
        self.psum = 0
            
    def mapper(self, key, line):
        for word in WORD_RE.findall(line):
            yield (word.lower(), 1)
            
    def reducer(self, count):
        self.psum += int(count)
    
    def result(self):
        yield self.psum
        
if __name__ == "__main__":
    
    WordFreqCount.run()
```

In order to run, copy your data file [input file] under the data folder, and run

```
python wc.py my_local -i [input file] --output=out.dat
```

The results for the my_local example would be under
/data/node1_dir/out.dat. For my_multi_local, input data file would
have to be split and placed under each data folder, the output goes to
each data folder as well, such as /data/node1_dir/out.dat,
/data/node2_dir/out.dat, etc. The contents of the output will depend
on the keys that were processed by each server. For more details, see
[here](doc/INTRO.md).

**OPTIONS**

-f, --file send a local file to a server, it will be placed under data_folder

-i, --input file, this file must reside in the data folder of each
 server. Multiple files can be passed in comma seperated form.

-r, --response tcp://host:port, this address is sent to all servers,
 when they are done, they notify this address. Job runner (client)
 will wait for messages on this queue, when all servers signal they
 are done, job runner quits. This way client runner can block until
 all processing on all servers are completed.
 
**SCRIPTS**

servers.py script can be used to execute commands that take effect on
all servers in a cluster. For example

```
python servers.py start my_production
```

starts all servers defined in my_production. For this to work,
passwordlesss ssh for $USER needs to work on all nodes in that
cluster. Logs go under /tmp/sasha-[num].err and .out files. Other
options are

- stop [conf]: stops all servers in cluster using pkill -f

- split-copy [conf] [data file] [to file]: Splits [data file] into
  equal chunks (# equal to the size of the cluster), and copies each
  chunk to a different data_dir of a server.

- pull-combine [conf] [remote file]: copies the same file from each
  server's data_dir, and combines them under /tmp/.

**Example**

Sasha has been used succesfully in multi-node, multi-core
configurations. [This example][1] here demonstrates the implementation
of large-scale SVD (Singular Value Decomposition) on map/reduce
architecture.

[1]: https://github.com/burakbayramli/classnotes/tree/master/stat/stat_mr_rnd_svd/sasha
