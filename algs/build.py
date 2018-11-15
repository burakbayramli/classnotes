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
    ./mcts/mcts.pdf \
    ./go/go.pdf \
    ./huff/huff.pdf \
    ./id3/id3.pdf \
    ./knn/knn.pdf \
    ./enc/enc.pdf \
    ./kmeans/kmeans.pdf \
    ./kmeans_mr/kmeans_mr.pdf \
    ./meanshift/meanshift.pdf \
    ./modeval/modeval.pdf \
    ./crf/crf.pdf \
    ./mesquita/mesquita.pdf \
    ./graph/graph.pdf \
    ./rsa/rsa.pdf \
    ./street/street.pdf \
    ./mlp/mlp.pdf \
    ./autodiff/autodiff.pdf \
    ./tensorflow/tensorflow.pdf \
    ./convnet/convnet.pdf \
    ./rnn/rnn.pdf \
    ./lstm/lstm.pdf \
    ./autoenc/autoenc.pdf \
    ./reinf/reinf.pdf \
    ./nlp/nlp.pdf \
    ./recom/recom.pdf \
    ./nmt/nmt.pdf \
    ./ocr/ocr.pdf \
    ./speech/speech.pdf \
    ./graphconv/graphconv.pdf \
    ./gan/gan.pdf \
    ./wavenet/wavenet.pdf \
    ./zapp/zapp.pdf \
    output ../../Dropbox/Public/skfiles/algs.pdf"
    os.system(cmd)
    print (cmd)
    exit()
elif sys.argv[1] == 'all':
    print (os.listdir("."))
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
        
