out_dir =  "/n/airoldifs2/lab/jbischof/word_rate_output/"
data_dir = out_dir + "data/"
src_dir = "../exec/"
mod_file = src_dir + "parse_lda_data.py"

import sys
sys.path.append(src_dir)
from parse_lda_data import *

infilename_lda = data_dir + "ap_ldaformat.txt"
outfilename_doclength = data_dir + "ap_doclengths.txt"
outfilename_ragarray = data_dir + "ap_ragarray.txt"
outfilename_margwc = data_dir + "ap_margwc.txt"

parse_lda_data(infilename_lda,outfilename_doclength, \
   outfilename_ragarray,outfilename_margwc)