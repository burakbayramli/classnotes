import os, sys, glob

if len(sys.argv) == 1:
    cmd = "pdftk ./cover/cover.pdf \
    ./mlintro/mlintro.pdf \
    ./complexity/complexity.pdf \
    ./basic/basic.pdf \
    ./sort/sort.pdf \
    ./dynp/dynp.pdf  \
    ./minspan/minspan.pdf \
    ./dijks/dijks.pdf \
    ./mstseg/mstseg.pdf \
    ./probsolve/probsolve.pdf \
    ./huff/huff.pdf \
    ./id3/id3.pdf \
    ./knn/knn.pdf \
    ./mlp/mlp.pdf \
    ./convnet/convnet.pdf \
    ./autodiff/autodiff.pdf \
    ./tensorflow/tensorflow.pdf \
    ./rnn/rnn.pdf \
    ./lstm/lstm.pdf \
    ./enc/enc.pdf \
    ./kmeans/kmeans.pdf \
    ./kmeans_mr/kmeans_mr.pdf \
    ./meanshift/meanshift.pdf \
    ./modeval/modeval.pdf \
    ./crf/crf.pdf \
    ./mesquita/mesquita.pdf \
    ./rsa/rsa.pdf \
    ./street/street.pdf \
    ./phd/phd.pdf \
    ./zapp/zapp.pdf \
    output ../../Dropbox/Public/skfiles/algs.pdf"
    os.system(cmd)
    print cmd
    exit()
elif sys.argv[1] == 'all':
    print os.listdir(".")
    for a in os.listdir("."):
        if os.path.isdir(a):
            os.chdir(a)
            os.system("pdflatex -shell-escape *.tex")    
            os.chdir("..")
               
elif sys.argv[1] == 'clean':
    os.system("find . -name '_region_*' | xargs rm  -rf")

elif sys.argv[1] == 'tex':
    file = glob.glob('*.tex')
    os.system("pdflatex -shell-escape %s" % file[0])
        
elif sys.argv[1] == 'dict':
    os.system("pdflatex -shell-escape dict1.tex")
    os.system("pdflatex -shell-escape dict2.tex")
    os.system("pdflatex -shell-escape dict3.tex")
    os.system("pdflatex -shell-escape dict4.tex")
