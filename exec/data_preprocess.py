out_dir = "/home/jbischof/Project_output/word_rate_model/"
data_dir = "../data/"
src_dir = "../exec/"
mod_file = src_dir + "parse_lda_data.py"

import sys
sys.path.append(src_dir)

from parse_lda_data import *

infilename_lda = data_dir + "ap.dat"
outfilename_doclength = out_dir + "ap_doclengths.txt"
outfilename_ragarray = out_dir + "ap_ragarray.txt"
outfilename_margwc = out_dir + "ap_margwc.txt"

parse_lda_data(infilename_lda,outfilename_doclength, \
   outfilename_ragarray,outfilename_margwc)