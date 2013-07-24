# Script to generate aturk coherence output
from aturk_word_intrusion import *
import sys

main_dir =  "/n/airoldifs2/lab/jbischof/word_rate_output/"
data_dir = main_dir + "topic_sum/"
out_dir = main_dir + "aturk_input/"

ntopics = int(sys.argv[1])
njobs = int(sys.argv[2])
nreplicates=int(sys.argv[3])

outfilename = out_dir + "aturk_word_intrusion_t" + str(ntopics) + "_j" + str(njobs) + "_r" + str(nreplicates) + ".csv"
models = ["dtr_" + str(ntopics) + "_frex","lda_" + str(ntopics) + "_freq","lda_" + str(ntopics) + "_frex"]
topic_sum_filenames_list = [data_dir + model + "_ap_sum.txt" for model in models]
topic_sum_filenames_dict = {}
for j in range(len(models)):
	topic_sum_filenames_dict[models[j]] = topic_sum_filenames_list[j]

# Run script
gen_wi_csv(topic_sum_filenames_dict,outfilename,nwords_sum=5,nwords_sum_full=10, \
n_replicates=nreplicates,n_tasks=njobs)