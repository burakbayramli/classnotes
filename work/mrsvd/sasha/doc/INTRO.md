##Architecture

There are three threads, one main loop that processes outside machines
from other servers and the job runner (this send commands, defining
input, output files, job files, etc), one thread that reads input
files and feeds them into mapper and reducer, and another that
processes all reduce work.

Mappers generate keys. In order to distribute reduce work for each
key,value pair, we run a hash(key) % cluster_size computation (% sign
stands for the mathematical mod function, the remainder after a
division) on a key. This can never generate a number greater than the
cluster size, and is "different enough" on each value (dare I say
"random"?) so it can be used as a way to distribute work. For example

```
print hash("ID10014") % 5
```

reports 3, and

```
print hash("ID10010") % 5
```

reports 4. In this case, ID10014 would be sent to server # 3 whereas
ID10010 would be sent to server 4, to be 'reduced'. Now before we talk
about how this helps speeding up map/reduce work, we need to talk
about files.

**Files**

The input, output file name specified for a job is the same for each
server. If I specify A.dat as input, all servers will look for A.dat
file under their own data_dir. Same is true for output files. Then
this is a way of simple data distribution, I split file into chunks,
name each chunk the same, place it under each data_dir, it will be
picked up by the job on that server.

**Result**

A side benefit that comes out of this is this: for jobs with multiple
steps, after the first step, map/reduce would have distributed all
keys to where they need to be, according to the mod(hash)
function. The second, third steps that follow, if they are using the
same keys, would not generate any intra-server traffic, follow-up
computation would remain local. New kinds of keys can be generated in
other steps which would cause new traffic of course, but otherwise,
things can remain local.

**Hadoop**

Not surprisingly, a lot of the ideas here are borrowed from
Hadoop. What Hadoop does using temporary files, sorting however, we do
with messaging and dictionaries. Sasha also aimed to simplify
creation, definition, and deploying of clusters, all it takes create a
cluster is defining a new entry in .sasha.conf file, a new data_dir,
and you are done. Simply spawning a new process through simple command
adds that much more processing power to your cluster.




