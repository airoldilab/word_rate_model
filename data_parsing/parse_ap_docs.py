# Script to POS tag the AP documents and record tokens of interest

import string
import nltk

# Minimum count for word to be used
min_count = 6

out_dir =  "/n/airoldifs2/lab/jbischof/word_rate_output/"
data_dir = out_dir + "/data/"
ap_filename = data_dir + "ap_docs.txt"
stop_filename = data_dir + "english.stop"
outfilename_vocab = data_dir + "ap_vocab.txt"
outfilename_lda = data_dir + "ap_ldaformat.txt"

# Create list of stopwords
stop_list = ["n't"]
infile_stop = open(stop_filename,"r")
for line in infile_stop:
	stop_list.append(line.strip())

# File with AP documents
infile = open(ap_filename,"r")
#line = infile.readlines()[0]

# Create dictionary of unique vocab and marginal word counts
vocab_dict = {}

# Create holder list for doc content
doc_wc_list = []
bad_list = ["NNP","CD"]

# Get list of document word count lists
for line in infile:
	# Read in text and replace punctuation with whitespace
	text_str = string.strip(line).translate(string.maketrans(string.punctuation, ' '*len(string.punctuation)))
	text_list = nltk.word_tokenize(text_str)
	text_list_pos = nltk.pos_tag(text_list)
	text_list_noNNP = [item[0] for item in text_list_pos if (item[1] not in bad_list)]
	ngrams = [w.lower() for w in text_list_noNNP]
	ngrams_nostop = [item for item in ngrams if item not in stop_list]
	unique_ngrams = set(ngrams_nostop)
	count_list = [(u,ngrams_nostop.count(u)) for u in unique_ngrams]
	doc_wc_list.append(count_list)
	for item in count_list:
		word, count = item
		if vocab_dict.has_key(item):
			vocab_dict[word] +=count
		else: vocab_dict[word] = count

infile.close()

# Create vocab file and assign ids
outfile_vocab = open(outfilename_vocab,"w")
vocab_id_dict = {}
id_counter = 0
for w in sorted(vocab_dict, key=vocab_dict.get, reverse=True):
	count = vocab_dict[w]
	if count >= min_count:
		vocab_id_dict[w] = id_counter
		id_counter += 1
		outstring = "%s\n" % (w)
		outfile_vocab.write(outstring)

outfile_vocab.close()

# Write LDA output file using assigned word ids
outfile_lda = open(outfilename_lda,"w")
for count_list in doc_wc_list:
	doc_lda_list = []
	ntokens = len(count_list)
	for item in count_list:
		word, count = item
		if word in vocab_id_dict:
			word_id = vocab_id_dict[word]
			word_str = "%s:%d" % (word_id,count)
			doc_lda_list.append(word_str)
	doc_lda_str = string.join(doc_lda_list," ")
	outstring = str(ntokens) + " " + doc_lda_str + "\n"
	outfile_lda.write(outstring)

outfile_lda.close()