#!/usr/bin/python
import os,sys
os.environ['MPLCONFIGDIR']='/tmp' 
import pandas as pd
data = pd.read_csv(sys.stdin,sep="\t",names=['country','count'])
grouped = data.groupby('country').mean()
grouped.to_csv(sys.stdout,sep="\t",header=False)
