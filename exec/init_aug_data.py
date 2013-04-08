# Script to initalize augmented word counts (by topic)
import string
import math
import numpy

def init_aug_ragarray(infilename_ragarray,ntopics):
  
  # Probabilities for multinomial
  multi_probs = [1/float(ntopics)]*int(ntopics)
  
  # Read in LDA file line by line
  outfile_aug_ragarray = open(outfilename_aug_ragarray, "r")
  infile_ragarray = open(infilename_ragarray, "r")
  for line in infile_lda:
    doc_id, word_id, count_str = string.split(line.strip(),"\t")
    wdraws = numpy.random.multinomial(n=ntopics, pvals=multi_probs)