# Script to create csv input files for Amazon Turk coherence task
# Input: 
# Game plan:
# First create list of topic summaries for each model
# Put these summaries in a dictionary
# 
import string
import random

# List of filenames where name before suffix is descriptive of model (e.g., model_name.txt)
def gen_coherence_csv(topic_sum_filenames_list,outfilename,nwords_sum=5, n_tasks=10):
  # Figure out position where to stop summary list
  stop_pos = nwords_sum + 1
  model_sum_dict = {}
  ntopics_dict = []
  nmodels = len(topic_sum_filenames_list)
  outfile = open(outfilename,'w')
  
  # Write header of output file
  header = ""
  for j in range(nmodels):
    h_add = "topic%0.2d,topic%0.2did" % (j+1,j+1)
    if j < (nmodels - 1): header += h_add + ","
    else: header += h_add + "\n"
  outfile.write(header)
  
  # Read in list of topic summaries and add it to dictionary
  for filename in topic_sum_filenames_list:
    model_name = string.split(filename,".")[0]
    topic_sum_file = open(filename,'r')
    topic_sum_list = []
    for line in topic_sum_file:
      line_list = string.split(line.strip(),"\t")[1:stop_pos]
      topic_sum = string.join(line_list,"   ")
      topic_sum_list.append(topic_sum)
    model_sum_dict[model_name] = topic_sum_list
    ntopics_dict[model_name] = len(topic_sum_list)
    
  model_name_list = model_sum_dict.get_keys()
  
  # Generate tasks
  for j in range(n_tasks):
    task_output = []
    for model_name in model_name_list:
      # Select random topic from each model
      topic_choose = random.randrange(ntopics_dict[model_name])
      model_topic = model_sum_dict[model_name][topic_choose]
      task_output += [model_topic, model_name]
    outstring = string.join(task_output,",") + "\n"
    outfile.write(outstring)
  
  outfile.close()

   
    
  
  