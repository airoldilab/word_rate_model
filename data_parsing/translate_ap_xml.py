# Script to read in original XML version of AP docs and turn into lines of outfile

import xml.etree.ElementTree as ET
import string

output_dir =  "/n/airoldifs2/lab/jbischof/word_rate_output/"
data_dir = out_dir + "/data/"
ap_file = data_dir + "ap.xml"
outfilename = data_dir + "ap_docs.txt"

outfile = open(outfilename, "w")

tree = ET.parse(ap_file)
root = tree.getroot()

# Iterate through documents in file
for doc in root:
	doc_str = doc.find("TEXT").text
	outfile.write(doc_str.strip() + "\n")

outfile.close()