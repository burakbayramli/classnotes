# python wc_mapfinal.py local -l wc_mapfinal.py --output=wc_out.dat
# from the command line, does wordcount on itself
import re, job, sys

WORD_RE = re.compile(r"[\w']+")

class WordFreqCount(job.SashaJob):

    def __init__(self):
        job.SashaJob.__init__(self)
        self.psum = 0
            
    def mapper(self, key, line):
        for word in WORD_RE.findall(line):
            yield (word.lower(), 1)

    def mapper_final(self):
        yield ("finally", 10)
        yield ("end finally", 10)
        yield ("end end finally", 10)
            
    def reducer(self, count):
        self.psum += int(count)
    
    def result(self):
        yield self.psum
        
if __name__ == "__main__":
    
    WordFreqCount.run()
