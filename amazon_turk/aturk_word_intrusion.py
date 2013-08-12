# Script to create csv input files for Amazon Turk word intrusion task
import random
import string
from get_topic_sum import *

def gen_wi_csv(topic_sum_filenames_dict,outfilename,outfilename_ans,nwords_sum=5,nwords_sum_full=10, \
n_replicates=1,n_tasks=10):

	nmodels = len(topic_sum_filenames_dict)
	outfile = open(outfilename,'w')
	outfile_ans = open(outfilename_ans,'w')
	
	# Write header of output file
	header = ""
	ntopics_tot = nmodels*n_replicates
	for j in range(ntopics_tot):
		h_id = "q%did," % (j + 1)
		h_words_list = ["q%dw%d" % (j+1,k) for k in range(nwords_sum + 2)[1:]]
		h_words = string.join(h_words_list,",")
		h_add = h_id + h_words
		if j < (ntopics_tot - 1): header += h_add + ","
		else: header += h_add + "\n"
	outfile.write(header)
	
	# Read in list of topic summaries and add it to dictionary
	model_sum_dict, ntopics_dict = get_topic_sum(topic_sum_filenames_dict,nwords_sum)
	full_model_sum_dict, junk = get_topic_sum(topic_sum_filenames_dict,nwords_sum_full)
	#print model_sum_dict
	#print full_model_sum_dict
	
	# Generate tasks
	question_counter = 0
	model_name_list = topic_sum_filenames_dict.keys()
	for j in range(n_tasks):
		task_output = []
		for rep in range(n_replicates):
			# Shuffle order of models in output
			random.shuffle(model_name_list)
			for model_name in model_name_list:
				# Select random topic from each model
				topic_choose = random.randrange(ntopics_dict[model_name])
				# Select an intruder topic that is not the same
				topic_intrude = random.randrange(ntopics_dict[model_name])
				while topic_intrude is topic_choose:
					topic_intrude = random.randrange(ntopics_dict[model_name])
				model_topic_list = string.split(model_sum_dict[model_name][topic_choose],"   ")
				model_intrude_list = string.split(full_model_sum_dict[model_name][topic_intrude],"   ")
				# Choose intruder word and add it to topic summary
				intrude_word =  model_intrude_list[random.randrange(nwords_sum_full)]
				model_topic_list.append(intrude_word)
				# Shuffle order of summary
				random.shuffle(model_topic_list)
				# Get intruder position
				intrude_pos = model_topic_list.index(intrude_word) + 1
				# Create question id
				# First need to create question number
				question_num = question_counter
				question_counter += 1
				question_id = model_name + "_q" + str(question_num)
				task_output += [question_id] + model_topic_list
				# Write intruder position to answer file
				outstring_ans = "%s,%d\n" % (question_id,intrude_pos)
				outfile_ans.write(outstring_ans)
		#print task_output
		outstring = string.join(task_output,",") + "\n"
		outfile.write(outstring)
		
	outfile.close()