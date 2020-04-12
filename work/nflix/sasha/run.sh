date

# Comment out the necessary configuration section below

#CONF=cluster2
#RESPONSE=tcp://chost1:5040

CONF=cluster1
RESPONSE=tcp://host3:5040

#CONF=local2
#RESPONSE=tcp://localhost:5040

# cleanup
#rm /tmp/x-*
#find $HOME/Downloads/sasha/ -name '*' | xargs rm

#
# data file
#

# Comment out the necessary data section below

# big netflix
# python $HOME/Documents/sasha/servers.py split-copy $CONF /home/burak/Downloads/netflix/download/A_netflix.dat A.dat

# one server, small
# cp ../A.dat $HOME/Downloads/sasha/node1/A.dat

# two server small
#python $HOME/Documents/sasha/servers.py split-copy $CONF ../A.dat A.dat

# two server movielens
# python $HOME/Documents/sasha/servers.py split-copy $CONF $HOME/Downloads/movielens_withids.csv A.dat

# netflix small
#head -10000 /home/burak/Downloads/netflix/download/A_netflix.dat > /tmp/net_small.dat
#python $HOME/Documents/sasha/servers.py split-copy $CONF /tmp/net_small.dat A.dat

# 
# processing
#

python proj.py $CONF -i A.dat -o Y.dat  -r $RESPONSE
python ata.py $CONF -i Y.dat -o YtY.dat -r $RESPONSE
python $HOME/Documents/sasha/servers.py pull-combine $CONF YtY.dat
sort /tmp/YtY.dat > /tmp/YtY_sorted.dat # sort the results by key
python chol.py /tmp/YtY_sorted.dat /tmp/R.dat
python a_inv_r.py $CONF -i Y.dat -o Q.dat  -f /tmp/R.dat -r $RESPONSE
python join.py $CONF -i A.dat,Q.dat -o AQ.dat -r $RESPONSE
python atq.py $CONF -i AQ.dat -o BT.dat -r $RESPONSE
python ata.py $CONF -i BT.dat -o BTB.dat -r $RESPONSE
python $HOME/Documents/sasha/servers.py pull-combine $CONF BTB.dat
sort -h /tmp/BTB.dat > /tmp/BTB_sorted.dat
python chol.py /tmp/BTB_sorted.dat /tmp/R_BT.dat
python q_uhat.py $CONF -i Q.dat -o U_final.dat  -f /tmp/R_BT.dat -r $RESPONSE
python $HOME/Documents/sasha/servers.py pull-combine $CONF U_final.dat

date