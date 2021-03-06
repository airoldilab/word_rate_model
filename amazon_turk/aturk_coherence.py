# Script to create csv input files for Amazon Turk coherence task
# Input: 
# Game plan:
# First create list of topic summaries for each model
# Put these summaries in a dictionary
# 

# Import needed libraries
import string
import random
from get_topic_sum import *

# List of filenames where name before suffix is descriptive of model (e.g., model_name.txt)
def gen_coherence_csv(topic_sum_filenames_dict,outfilename,nwords_sum=5, \
n_replicates=1,n_tasks=10):
	nmodels = len(topic_sum_filenames_dict)
	outfile = open(outfilename,'w')
	
	# Write header of output file
	header = ""
	ntopics_tot = nmodels*n_replicates
	for j in range(ntopics_tot):
		h_add = "topic%0.2d,topic%0.2did" % (j,j)
		if j < (ntopics_tot - 1): header += h_add + ","
		else: header += h_add + "\n"
	outfile.write(header)
	
	# Read in list of topic summaries and add it to dictionary
	model_sum_dict, ntopics_dict = get_topic_sum(topic_sum_filenames_dict,nwords_sum)
	#print ntopics_dict
	#print model_sum_dict
  
	# Generate tasks
	model_name_list = topic_sum_filenames_dict.keys()
	for j in range(n_tasks):
		task_output = []
		for rep in range(n_replicates):
			# Shuffle order of models in output
			random.shuffle(model_name_list)
			for model_name in model_name_list:
				# Select random topic from each model
				topic_choose = random.randrange(ntopics_dict[model_name])
				model_topic = model_sum_dict[model_name][topic_choose]
				task_output += [model_topic, model_name]
		outstring = string.join(task_output,",") + "\n"
		outfile.write(outstring)
	
	outfile.close()
  
