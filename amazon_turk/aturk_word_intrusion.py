# Script to create csv input files for Amazon Turk word intrusion task
import random
import string

def gen_wi_csv(topic_sum_filenames_dict,outfilename,nwords_sum=5,nwords_sum_full=10 \
n_replicates=1,n_tasks=10):

	nmodels = len(topic_sum_filenames_dict)
	outfile = open(outfilename,'w')
	
	# Write header of output file
	header = ""
	ntopics_tot = nmodels*n_replicates
	for j in range(ntopics_tot):
		h_id = "q%did," % j
		h_words_list = ["q%dw%d" % (j,k) for k in range(nwords_sum + 1)[1:]]
		h_words = string.join(h_words_list,",")
		h_add = h_id + h_words
		if j < (ntopics_tot - 1): header += h_add + ","
		else: header += h_add + "\n"
		outfile.write(header)
	
	# Read in list of topic summaries and add it to dictionary
	model_sum_dict, ntopics_dict = get_topic_sum(topic_sum_filenames_dict,nwords_sum)
	full_model_sum_dict, junk = get_topic_sum(topic_sum_filenames_dict,nwords_sum_full)

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
				# Select an intruder topic that is not the same
				while topic_intrude is not topic_choose:
					topic_intrude = random.randrange(ntopics_dict[model_name])
				model_topic_list = string.split(model_sum_dict[model_name][topic_choose]," ")
				model_intrude_list = string.split(full_model_sum_dict[model_name][topic_intrude]," ")
				# Choose intruder word and add it to topic summary
				intrude_word =  model_intrude_list[random.randrange(nwords_sum_full)]
				model_topic_list.append(intrude_word)
				# Shuffle order of summary
				shuffle(model_topic_list)
				task_output += [model_name,model_topic_list]
		outstring = string.join(task_output,",") + "\n"
		outfile.write(outstring)
				
	outfile.close()