import re, json, glob, string, os, util
from collections import defaultdict
from unidecode import unidecode

WORD = re.compile(r'\w+')
#target_dir = "/home/burak/Documents/repos/burakbayramli.github.com"    
target_dir = "/tmp"    
topdirs = ['algs','calc_multi','chaos','compscieng','func_analysis','linear','ode', 'stat','tser','vision','phy']

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

def index_dir():

    invidx = defaultdict(lambda: defaultdict(int))
    basedir = os.getcwd()
    tex_html_map = {}
    files1 = glob.glob(basedir + "/**/**/**/*.md")
    files2 = []
    for dir in topdirs:
        for subdir in sorted(os.listdir(dir)):
            if not os.path.isdir(dir + "/" + subdir): continue
            if "cover" in subdir or "000" in subdir: continue
            # read tex file, get header
            ftex = subdir + ".tex"
            if ftex=='dict.tex': continue
            title = util.get_title_from_tex(dir + "/" + subdir + "/" + ftex)
            html =  "/dersblog/" + dir + "/" + subdir + "/" + util.filename_from_title(title) + ".html"
            tex = dir + "/" + subdir + "/" + ftex
            tex_html_map[tex] = html
            files2.append(tex)

    files = files1 + files2
    files = sorted(files)
    for file in enumerate(files):        
        doc = file[1].replace(basedir,"")    
        for word in reg_tokenize(open(file[1]).read()):
            word = unidecode(word).lower()
            if len(word) < 2: continue
            if ".tex" in doc: doc = tex_html_map[doc]
            if ".md" in doc: doc = '/dersblog' + doc.replace(".md",".html")
            invidx[word][doc] += 1
        print (doc)

    fout = open("/tmp/invidx.json","w")
    fout.write(json.dumps(invidx))
    fout.close()

    # split the index

    invidx = json.loads(open("/tmp/invidx.json").read())
    invidx_dict = {}

    for c in  string.ascii_lowercase:
        invidx_dict[c] = defaultdict(lambda: defaultdict(int))

    for k,v in invidx.items():
        first_letter = k[0]
        if first_letter in string.ascii_lowercase:
            invidx_dict[first_letter][k] = v

    for k,v in invidx_dict.items():
        fout = open(target_dir + "/invidx-%s.json" % k,"w")
        fout.write(json.dumps(v))
        fout.close()    
    
if __name__ == "__main__": 

    index_dir()

