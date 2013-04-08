# Script to take raw data (in LDA format) and create data inputs for model
import string
import warnings
import math

def parse_lda_data(infilename_lda,outfilename_doclength, \
   outfilename_ragarray,outfilename_margwc):
   
   # Initialize dictionaries
   doc_length_dict = {}
   marginal_word_count_dict = {}
   
   # Initialize doc_ids
   doc_id = int(0)
   
   # Open outfiles for writing
   #outfile_doclength = open(outfilename_doclength, "w")
   outfile_ragarray = open(outfilename_ragarray, "w")
   
   # Read in LDA file line by line
   infile_lda = open(infilename_lda, "r")
   for line in infile_lda:
      line_list = string.split(line.strip()," ")
      doc_wc_list = line_list[1:]
      doc_id += 1
      
      # Parse word counts
      # Initialize doc length
      doc_length = int(0)
      doc_wc_dict = {}
      for item in doc_wc_list:
	 word_id, count_str = string.split(item,":")
	 
	 # Write key,value pair to ragged array
	 outstring = "%s\t%s\t%s\n" % (doc_id,word_id,count_str)
	 outfile_ragarray.write(outstring)
	 
	 # Use count to populate other dictionaries
	 count = int(float(count_str))
	 doc_wc_dict[word_id] = count
	 doc_length += count
	 if marginal_word_count_dict.has_key(word_id):
	    marginal_word_count_dict[word_id] += count
	 else:
	    marginal_word_count_dict[word_id] = count
      
      # Add entries to each dictionary
      doc_length_dict[doc_id] = doc_length
      
   # Close ragarray file
   outfile_ragarray.close()
   
   # Write doc length info to file
   outfile_doclength = open(outfilename_doclength,"w")
   for doc_id in doc_length_dict:
      doc_length = doc_length_dict[doc_id]
      outstring = "%s\t%s\n" % (doc_id,doc_length)
      outfile_doclength.write(outstring)
   outfile_doclength.close()
   
   # Write marginal word counts to file
   outfile_margwc = open(outfilename_margwc,"w")
   for word_id in marginal_word_count_dict:
      count = marginal_word_count_dict[word_id]
      outstring = "%s\t%s\n" % (word_id,count)
      outfile_margwc.write(outstring)
   outfile_margwc.close()
