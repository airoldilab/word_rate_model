#!/bin/bash

# QUEUE
#BSUB -q airoldi

# EDIT THE EMAIL-ADDRESS BELOW TO YOUR FAS EMAIL:
#BSUB -u jbischof@fas.harvard.edu

# THE JOB ARRAY:
#BSUB -J "ctm_analyze[1-4]"

# BASH CODE THAT YOU WANT TO RUN:
# Figure out the number of topics for the job index
declare -a ntopics_list=(10 25 50 100)
job_index=$(expr ${LSB_JOBINDEX} - 1)
ntopics=${ntopics_list[$job_index]}
output_dir=/n/airoldifs2/lab/jbischof/word_rate_output/ctm_output/run_k${ntopics}/

# Make the output directory if it already doesn't exist
if [ ! -d $output_dir ]
   then
      mkdir $output_dir
fi	

# Run LDA model
python ctm-topics.py ${output_dir}/final-log-beta.beta ../data/vocab.txt 20