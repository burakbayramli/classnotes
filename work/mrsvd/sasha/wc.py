# python wc.py local -l wc.py --output=wc_out.dat -r tcp://localhost:5040
# from the command line, does wordcount on itself
import re, sys
from sasha import job

WORD_RE = re.compile(r"[\w']+")

class WordFreqCount(job.SashaJob):

    def __init__(self):
        job.SashaJob.__init__(self)
        self.psum = 0
            
    def mapper(self, key, line):
        for word in WORD_RE.findall(line):
            yield (word.lower(), 1)
            
    def reducer(self, count):
        self.psum += int(count)
    
    def result(self):
        yield self.psum
        
if __name__ == "__main__":
    
    WordFreqCount.run()
