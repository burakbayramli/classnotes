from mrjob.job import MRJob
from mrjob.protocol import PickleProtocol
from mrjob.protocol import RawValueProtocol
from mrjob.protocol import RawProtocol
import numpy as np, sys, itertools
from scipy import sparse
import random

'''
Change format from "movieId \tab user1:rating" format into
"user1 \tab movie1:rating, movie2:rating, .."

'''
class MRTr(MRJob):
    INTERNAL_PROTOCOL = PickleProtocol
    INPUT_PROTOCOL = RawProtocol
            
    def __init__(self, *args, **kwargs):
        super(MRTr, self).__init__(*args, **kwargs)

    def mapper(self, key, line):
        tokens = line.split(";")
        for tok in tokens:
            tmp = tok.split(":")
            yield int(tmp[0]), str(key)+":"+tmp[1]

    def reducer(self, key, values):
        yield key, ";".join(values)
        
if __name__ == '__main__':
    MRTr.run()
