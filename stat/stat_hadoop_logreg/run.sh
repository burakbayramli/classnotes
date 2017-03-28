ssh localhost -l hduser  python /home/burak/Documents/classnotes/stat/stat_hadoop_logreg/logreg.py hdfs:///user/testSet1.txt -r hadoop  --jobconf mapred.map.tasks=1 --jobconf mapred.reduce.tasks=1
