import json, os

def process(file_name,chunk_i,N,hookobj,skip_lines=0):
    file_size = os.path.getsize(file_name)
    hookobj.infile = file_name # lineprocessor object
    hookobj.chunk = chunk_i
    with open(file_name, 'r') as f:
        for j in range(skip_lines): f.readline()
        beg = f.tell()
        f.close()
    chunks = []
    for i in range(N):
        with open(file_name, 'r') as f:
            s = int((file_size / N)*(i+1))
            f.seek(s)
            f.readline()
            end_chunk = f.tell()-1
            chunks.append([beg,end_chunk])
            f.close()
        beg = end_chunk+1
    c = chunks[chunk_i]
    with open(file_name, 'r') as f:
        f.seek(c[0])
        while True:
            line = f.readline()
            hookobj.exec(line) # process the line
            if f.tell() > c[1]: break
        f.close()
        hookobj.post()    

