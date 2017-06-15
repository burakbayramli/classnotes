import zipfile, csv
import networkx as net
mg=net.Graph()

with zipfile.ZipFile('../../stat/stat_ratings/data.zip', 'r') as z:
    with z.open('ratings.dat') as csvfile:
    	 spamreader = csv.reader(csvfile)
    	 for row in spamreader:
	     tokens = row[0].split("::")
	     mg.add_edge(tokens[0],tokens[1],weight=tokens[2])
