#!/bin/bash

# Script to coordinate create of wi tasks
njobs=$1

python get_aturk_word_intrusion.py 10 $njobs 2
python get_aturk_word_intrusion.py 25 $njobs 2
python get_aturk_word_intrusion.py 50 $njobs 2
python get_aturk_word_intrusion.py 100 $njobs 2