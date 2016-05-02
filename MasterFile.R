## Master file

# 1. Compile data 
>> Data_reduction.R
# Pares down columns from E-Prime file
# Adds MTCP scores (IMS/EMS)
# Isolates experimental trials and writes to text file 
experimentalTrials.txt #badsubs not taken out
# Separates WIT and AP trials and writes to separate text files 
WITdata.txt #badsubs not take out
APdata.txt #badsubs not taken out

# 2. Look for bad subjects
>> exploreSubs.R
# Reads in
experimentalTrials.txt #badsubs not taken out
## 1. Distribution of meanRT and numCor for WIT and AP tasks by subject
## 2. Creates output object with RT/accuracy for each task, participant race, & IMS/EMS scores for each subject
## 3. Plots meanRT/numCor for each task, compare accuracy across tasks
## 4. Looks at participant race/IMS/EMS in relation to accuracy
## 5. Creates descriptions for mean, max, min for meanRT and numCor
## 6. Finds subjects with <80 correct trials (out of 192)
## 7. Look at effort/attention questions
## 8. Plots of how effort/attention is related to RT/accuracy
## 9. Writes 2 badsubs.txt files (one for each task) with Sub numbers and reason they're being excluded
badsubsAP.txt
badsubsWIT.txt
########## On the fence about Sub 8, 59
## 10. Do the same things with bad subjects excluded
##### Histograms, compare accuracy across tasks, look at IMS/EMS and accuracy 
##### No relationships between IMS/EMS and accuracy
##### No main effect of observer condition on accuracy

# 3. Calculate number of errors on each type of trial
>> calcErrors.R
# reads in 
experimentalTrials.txt #badsubs not taken out
# Has number of errors for each subject in each type of trial (black-gun, black-tool etc.) for both tasks
# adds observer condition and attend/effort responses
# adds congruent condition
# adds TO and Com errors columns (# of errors, not # correct)
# Writes to
errCountLong.txt #no badsubs

# 4. Plots errors, with and without observer condition
>> plotObsInfo.R
# reads in
errCountLong.txt #no badsubs
# plots total errors, committed errors, and time out errors, separated by observer condition

# 5. Create data for HLM or ANOVA analysis ########### Need to rerun since additional bad subs
>> prepforAnalysis.R
# reads in 
experimentalTrials.txt #badsubs not taken out
# takes out badsubs data (only for relevant task)
# keeps data in long form (every trial on a different line)
# adds observer condition and attend/effort responses
# adds congruent condition
# adds TO and Com errors columns: 1 = error, 0 = no error (in contrast to Target.ACC where 1 = accurate, 0 = error)
# writes
ForAnalysis.txt #badsubs taken out, only on relevant task
#### In WIT: 92 Ss total (47 obs-absent, 45 obs-present)
#### In AP: 93 Ss total (47 obs-absent, 46 obs-present)

# 6. Do repeated measures ANOVA  ########### need to rerun prepforAnalysis.R first
>> ANOVAanalyses.R
# reads in
ForAnalysis.txt #badsubs taken out
# Look at Race x Valence interactions on accuracy
# - including simple effects
# Look at Observer x Race x Valence 
# - need to check- same # of subs for both observer conditions? type 1 vs type 3?
# Used total errors, timeout errors, and committed errors as DVs
# All results in PowerPoint
# look at observer effect on performance bias
# look at IMS/EMS * observer interaction on perf bias

# 7. Look at order effects on self-reported effort/attention
>> ANOVAanalyses2.R
# reads in 
ForAnalysis.txt #bad subs taken out
# Lok at IMS/EMS x observer on self=reported effort/attention

# 7. Calculate PDP estimates for AP and WIT
>> PDPcalcWIT.R
>> PDPcalcAP.R
# Calculates PDP estimates separately for different race primes, then collapsed across race prime
# Adds self-reported attention and effort scores
# Look at how C and A estimates are related to effort and attention
# Calculates PDP bias difference score (Black_A - White_A)
# outputs both wide and long form of estimates
PDPestimatesWITlong.txt   # One row for one estimate. Columns for Observer, Estimate type (Black_C etc), value for estimate
PDPestimatesWITwide.txt   # All data for one subject on one row. Column for Black_C, Black_A etc.
PDPestimatesAPlong.txt
PDPestimatesAPlong.txt

# 8. Look at PDP estimates, how they correlate and the effect of observer
>> PDPanalysis.R
# Look at means of estimates across tasks
# Look at correlations of estimates within and across tasks
# Look at observer effect on estimates

# 9. Relationship between IMS/EMS and PDP scores (and how that differs by observer)
>> PDPanalysis2.R
# Look at relationship between IMS/EMS scores and PDP estimates
# Look at how that differs between observer conditions



########################################################################################
############### Stream of files used for data write up #################################
########################################################################################

>> 1 Data_reduction.R
# Pares down columns from E-Prime file
# Adds MTCP scores (IMS/EMS)
# Isolates experimental trials and writes to text file 
experimentalTrials.txt #badsubs not taken out

>> 2 Determine bad subjects.R
# Reads in
experimentalTrials.txt #badsubs not taken out
# Does lots of things, not immediately relevant
# Writes 2 badsubs.txt files (one for each task) with Sub numbers and reason they're being excluded
badsubsAP.txt
badsubsWIT.txt

>> 3 PrepForAnalysis.R
# reads in 
experimentalTrials.txt #badsubs not taken out
# takes out badsubs data (only for relevant task) using 
badsubsAP.txt
badsubsWIT.txt
# keeps data in long form (every trial on a different line)
# adds observer condition and attend/effort responses
# adds congruent condition
# adds TO and Com errors columns: 1 = error, 0 = no error (in contrast to Target.ACC where 1 = accurate, 0 = error)
# writes
ForAnalysis.txt #badsubs taken out, only on relevant task
#### In WIT: 92 Ss total (47 obs-absent, 45 obs-present)
#### In AP: 93 Ss total (47 obs-absent, 46 obs-present)
# Has number of errors for each subject in each type of trial (black-gun, black-tool etc.) for both tasks
# adds observer condition and attend/effort responses
# adds congruent condition
# adds TO and Com errors columns (# of errors, not # correct)
# Writes to
errCountLong.txt #no badsubs

>> 4 PDPcalc.R
# Calculates PDP estimates separately for different race primes, then collapsed across race prime
# Adds self-reported attention and effort scores
# Look at how C and A estimates are related to effort and attention
# Calculates PDP bias difference score (Black_A - White_A)
# outputs both wide and long form of estimates
PDPestimatesWITlong.txt   # One row for one estimate. Columns for Observer, Estimate type (Black_C etc), value for estimate
PDPestimatesWITwide.txt   # All data for one subject on one row. Column for Black_C, Black_A etc.
PDPestimatesAPlong.txt
PDPestimatesAPwide.txt

>> 5 AnalysesForWriteup.R
# reads in 
ForAnalysis.txt #badsubs taken out
errCountLong.txt #badsubs taken out
PDPestimatesWITlong.txt
PDPestimatesWITwide.txt
PDPestimatesAPlong.txt
PDPestimatesAPwide.txt