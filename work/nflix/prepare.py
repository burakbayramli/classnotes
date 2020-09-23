import sys, os

'''
Add a tab after the key (first column) and pick non-null columns
*only* and indexing them with the column position as index:value
seperated by semicolon
'''
fin = open (sys.argv[1])
fout = open (sys.argv[2],"w")
for x in fin.readlines():
    tokens = x.split(';')
    res = [str(i)+":"+str(x) for i,x in enumerate(tokens[1:]) if x != '' ]
    fout.write( tokens[0] + '\t' + ";".join(map(str,res )))
fout.close()

