# python test_nored.py local -l test.py --output=wc_out.dat
import re, job, sys

WORD_RE = re.compile(r"[\w']+")

class NoReducer(job.SashaJob):
    
    def mapper(self, key, line):
        for word in WORD_RE.findall(line):
            yield (word.lower(), 3)
                    
if __name__ == "__main__":
    
    NoReducer.run()
