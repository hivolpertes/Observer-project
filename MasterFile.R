## Master file

# 1. Compile data 
>> Data_reduction.R
# Pares down columns from E-Prime file
# Isolates experimental trials and writes to text file 
experimentalTrials.txt
# Separates WIT and AP trials and writes to separate text files 
WITdata.txt
APdata.txt

# 2. Look for bad subjects
>> exploreSubs.R
# looks at meanRT and numCor for WIT and AP tasks by subject
# Visualizes relationship between meanRT/numCor using scatterplots
# Creates descriptions for mean, max, min for meanRT and numCor
# Finds subjects with <80 correct trials (out of 192)

# 3. Calculates number of errors on each type of trial
>> calcRaceBias.R

