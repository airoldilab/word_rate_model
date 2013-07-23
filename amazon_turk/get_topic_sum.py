# Script to get topic sum dictionaries
import string

def get_topic_sum(topic_sum_filenames_dict,nwords_sum=5):
	# Figure out position where to stop summary list
	stop_pos = nwords_sum + 1
	model_sum_dict = {}
	ntopics_dict = {}
	for model_name in topic_sum_filenames_dict:
		filename = topic_sum_filenames_dict[model_name]
		topic_sum_file = open(filename,'r')
		topic_sum_list = []
		for line in topic_sum_file:
			line_list = string.split(line.strip()," ")[1:stop_pos]
			topic_sum = string.join(line_list,"   ")
			topic_sum_list.append(topic_sum)
			model_sum_dict[model_name] = topic_sum_list
			ntopics_dict[model_name] = len(topic_sum_list)
		topic_sum_file.close()
	return (model_sum_dict,ntopics_dict)