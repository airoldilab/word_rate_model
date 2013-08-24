#!/bin/bash

# Script to coordinate create of wi tasks
njobs=$1

python get_aturk_coherence.py 10 $njobs 1
python get_aturk_coherence.py 25 $njobs 1
python get_aturk_coherence.py 50 $njobs 1
python get_aturk_coherence.py 100 $njobs 1