# Statik Sitelerde Dinamik Kelime Arama Sistemi

from collections import defaultdict
from unidecode import unidecode
import re, json, glob

WORD = re.compile(r'\w+')

def clean_text(text):
    text = text.replace("\n"," ").replace("\r"," ")
    punc_list = '!"#$%^()*+,-./:;<=>?@[\]^_{|}~' + '0123456789'
    t = str.maketrans(dict.fromkeys(punc_list, " "))
    text = text.translate(t)
    t = str.maketrans(dict.fromkeys("'`",""))
    text = text.translate(t)
    return text

def reg_tokenize(text):
    text = clean_text(text)
    words = WORD.findall(text)
    return words

invidx = defaultdict(lambda: defaultdict(int))
dir = "[DIR]"

files = glob.glob(dir + "/**/**/*.md")
files = sorted(files)
for file in enumerate(files):
    doc = file[1].replace(dir,"")    
    for word in reg_tokenize(open(file[1]).read()):
        word = unidecode(word).lower()
        invidx[word][doc] += 1
    print (doc)

fout = open("data/invidx.json","w")
fout.write(json.dumps(invidx))
fout.close()









invidx = json.loads(open("data/invidx.json").read())
invidx_dict = {}

for c in  string.ascii_lowercase:
    invidx_dict[c] = defaultdict(lambda: defaultdict(int))

for k,v in invidx.items():
    first_letter = k[0]
    if first_letter in string.ascii_lowercase:
        invidx_dict[first_letter][k] = v

for k,v in invidx_dict.items():
    fout = open("data/invidx-%s.json" % k,"w")
    fout.write(json.dumps(v))
    fout.close()    






#points = [0, 12, 9]
#res = sorted(range(len(points)), key=lambda x: points[x])
#print(res)
#exit()

#a = [1, 2, 3, 4]; b = [2, 3, 4, 5]; c = [3, 4, 5, 6]; d = [4, 1, 3, 9]
#u = set.intersection(set(a),set(b),set(c),set(d))
#print (u)

search = "green ammonia"

stok = search.split()

stok_hits = {}

results = []
for tok in stok:
    stok_hits[tok] = json.loads(open("data/invidx-%s.json" % tok[0]).read())[tok]
    results.append(set(stok_hits[tok]))
    
u = set.intersection(*results)

hits = []
for f in u:
    hits.append([f,sum([stok_hits[tok][f] for tok in stok])])
    
sorted_hits = sorted(range(len(hits)), key=lambda x: hits[x][1], reverse=True)

for i in range(20):
    print (hits[sorted_hits[i]])






<head>
    <script defer src="pyscript.js"></script>
</head>
<body>    
      <py-script>
        from pyodide.http import open_url
        from collections import defaultdict
        import json

        search = "green ammonia"

        stok = search.split()

        stok_hits = {}

        results = []
        
        for tok in stok:
           url = 'http://192.168.43.49:5000/static/invidx-%s.json' % tok[0]
           df = open_url(url)        
           stok_hits[tok] = json.loads(df.getvalue())[tok]
           results.append(set(stok_hits[tok]))


        u = set.intersection(*results)

        hits = []
        
        for f in u:
           hits.append([f,sum([stok_hits[tok][f] for tok in stok])])
    
        sorted_hits = sorted(range(len(hits)), key=lambda x: hits[x][1], reverse=True)

        for i in range(20):
           display (hits[sorted_hits[i]])
           
      </py-script>
</body>
</html>
