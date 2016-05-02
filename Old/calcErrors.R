# Calculate race bias by looking at the number of errors on each trial type for each subject
require(dplyr)
require(magrittr)
require(tidyr)

dat = read.delim("experimentalTrials.txt", stringsAsFactors=F)



# 1. First look at WIT trials
WITdat = dat[dat$Procedure.Block. == "WITproc",]
# read in badSubs for WIT and discard accordingly
badSubsWIT = read.delim("badSubsWIT.txt")
WITdat = WITdat[!(WITdat$Subject %in% badSubsWIT$Subject),]       # returns T or F for every element in first array (F for NA)

# create column separating trials into black_tool, black_gun, etc.
for (i in 1:nrow(WITdat)) {
  WITdat$TrialType[i] = paste(WITdat$PrimeType[i],WITdat$TargetType[i], sep = "_")
}

# count errors for each type of trial (out of 48 trials per trial type)
WITdat$TargetWIT.ACC = as.numeric(WITdat$TargetWIT.ACC)

errCount = data.frame("Subject" = c(1:max(WITdat$Subject)))

for (i in 1:max(WITdat$Subject)) {
  tempDat = WITdat[WITdat$Subject == i,]
  errCount$WITnumCor_bw[errCount$Subject == i] = 
    sum(tempDat$TargetWIT.ACC[tempDat$TrialType == "black_weapon"])
  errCount$WITnumCor_ww[errCount$Subject == i] = 
    sum(tempDat$TargetWIT.ACC[tempDat$TrialType == "white_weapon"])
  errCount$WITnumCor_bt[errCount$Subject == i] = 
    sum(tempDat$TargetWIT.ACC[tempDat$TrialType == "black_tool"])
  errCount$WITnumCor_wt[errCount$Subject == i] = 
    sum(tempDat$TargetWIT.ACC[tempDat$TrialType == "white_tool"])
}

# 2. Do the same for AP trials
APdat = dat[dat$Procedure.Block. == "APproc",]
# read in badSubs for AP and discard accordingly
badSubsAP = read.delim("badSubsAP.txt")
APdat = APdat[!(APdat$Subject %in% badSubsAP$Subject),]       # returns T or F for every element in first array (F for NA)

# create column separating trials into black_tool, black_gun, etc.
for (i in 1:nrow(APdat)) {
  APdat$TrialType[i] = paste(APdat$PrimeType[i],APdat$TargetType[i], sep = "_")
}

# count errors for each type of trial (out of 48 trials per trial type)
APdat$TargetAP.ACC = as.numeric(APdat$TargetAP.ACC)

for (i in 1:max(APdat$Subject)) {
  tempDat = APdat[APdat$Subject == i,]
  errCount$APnumCor_bp[errCount$Subject == i] = 
    sum(tempDat$TargetAP.ACC[tempDat$TrialType == "black_positive"])
  errCount$APnumCor_wp[errCount$Subject == i] = 
    sum(tempDat$TargetAP.ACC[tempDat$TrialType == "white_positive"])
  errCount$APnumCor_bn[errCount$Subject == i] = 
    sum(tempDat$TargetAP.ACC[tempDat$TrialType == "black_negative"])
  errCount$APnumCor_wn[errCount$Subject == i] = 
    sum(tempDat$TargetAP.ACC[tempDat$TrialType == "white_negative"])
}
# Change 0 values (no data for subject) to NA
errCount[errCount == 0] = NA

# 3. Calculate number of error trials for each condition in both tasks

errCount$WITnumErr_bw = 48 - errCount$WITnumCor_bw
errCount$WITnumErr_ww = 48 - errCount$WITnumCor_ww
errCount$WITnumErr_bt = 48 - errCount$WITnumCor_bt
errCount$WITnumErr_wt = 48 - errCount$WITnumCor_wt

errCount$APnumErr_bp = 48 - errCount$APnumCor_bp
errCount$APnumErr_wp = 48 - errCount$APnumCor_wp
errCount$APnumErr_bn = 48 - errCount$APnumCor_bn
errCount$APnumErr_wn = 48 - errCount$APnumCor_wn

# 4. Condense to long version, add descriptive columns/variables

long = select(errCount, -contains("numCor")) %>%
  gather(TrialType, numErr,WITnumErr_bw:APnumErr_wn) 

long$GenType[long$TrialType == "WITnumErr_bw"] = "black_con"
long$GenType[long$TrialType == "APnumErr_bn"] = "black_con"
long$GenType[long$TrialType == "WITnumErr_bt"] = "black_incon"
long$GenType[long$TrialType == "APnumErr_bp"] = "black_incon"
long$GenType[long$TrialType == "WITnumErr_wt"] = "white_con"
long$GenType[long$TrialType == "APnumErr_wp"] = "white_con"
long$GenType[long$TrialType == "WITnumErr_ww"] = "white_incon"
long$GenType[long$TrialType == "APnumErr_wn"] = "white_incon"

# Add column determining task (WIT or AP), ConType (congruent or incongruent), PrimeType (black or white)
long$Task = NA
long$Task[grep("WIT",long$TrialType)] = "WIT" 
long$Task[grep("AP",long$TrialType)] = "AP" 

long$ConType = NA
long$ConType[grep("_con",long$GenType)] = "congruent"
long$ConType[grep("_incon",long$GenType)] = "incongruent"

long$PrimeType = NA
long$PrimeType[grep("black",long$GenType)] = "black"
long$PrimeType[grep("white",long$GenType)] = "white"

long$TargetType = NA
long$TargetType[long$Task == "WIT" & long$GenType == "black_incon"] = "tool"
long$TargetType[long$Task == "WIT" & long$GenType == "black_con"] = "gun"
long$TargetType[long$Task == "WIT" & long$GenType == "white_incon"] = "gun"
long$TargetType[long$Task == "WIT" & long$GenType == "white_con"] = "tool"
long$TargetType[long$Task == "AP" & long$GenType == "black_incon"] = "positive"
long$TargetType[long$Task == "AP" & long$GenType == "black_con"] = "negative"
long$TargetType[long$Task == "AP" & long$GenType == "white_incon"] = "negative"
long$TargetType[long$Task == "AP" & long$GenType == "white_con"] = "positive"




# 5. Add data for time out errors vs commitment errors for each trial type

# First for WIT task
for (i in unique(WITdat$Subject)) {
  tempDat = WITdat[WITdat$Subject == i,]
  # Time out errors (black weapon trials)
  long$numTOErr[long$Subject == i & long$TrialType == "WITnumErr_bw"] = 
    nrow(tempDat[tempDat$TrialType == "black_weapon" & tempDat$responseAccData == 3,])
  # Commitment errors (black weapon trials)
  long$numComErr[long$Subject == i & long$TrialType == "WITnumErr_bw"] = 
    nrow(tempDat[tempDat$TrialType == "black_weapon" & tempDat$responseAccData == 1,])

  # Time out errors (black tool trials)
  long$numTOErr[long$Subject == i & long$TrialType == "WITnumErr_bt"] = 
    nrow(tempDat[tempDat$TrialType == "black_tool" & tempDat$responseAccData == 3,])
  # Commitment errors (black tool trials)
  long$numComErr[long$Subject == i & long$TrialType == "WITnumErr_bt"] = 
    nrow(tempDat[tempDat$TrialType == "black_tool" & tempDat$responseAccData == 1,])
  
  # Time out errors (white weapon trials)
  long$numTOErr[long$Subject == i & long$TrialType == "WITnumErr_ww"] = 
    nrow(tempDat[tempDat$TrialType == "white_weapon" & tempDat$responseAccData == 3,])
  # Commitment errors (white weapon trials)
  long$numComErr[long$Subject == i & long$TrialType == "WITnumErr_ww"] = 
    nrow(tempDat[tempDat$TrialType == "white_weapon" & tempDat$responseAccData == 1,])

  # Time out errors (white tool trials)
  long$numTOErr[long$Subject == i & long$TrialType == "WITnumErr_wt"] = 
    nrow(tempDat[tempDat$TrialType == "white_tool" & tempDat$responseAccData == 3,])
  # Commitment errors (white tool trials)
  long$numComErr[long$Subject == i & long$TrialType == "WITnumErr_wt"] = 
    nrow(tempDat[tempDat$TrialType == "white_tool" & tempDat$responseAccData == 1,])
}

# Next for AP task
for (i in unique(APdat$Subject)) {
  tempDat = APdat[APdat$Subject == i,]
  # Time out errors (black negative trials)
  long$numTOErr[long$Subject == i & long$TrialType == "APnumErr_bn"] = 
    nrow(tempDat[tempDat$TrialType == "black_negative" & tempDat$responseAccData == 3,])
  # Commitment errors (black negative trials)
  long$numComErr[long$Subject == i & long$TrialType == "APnumErr_bn"] = 
    nrow(tempDat[tempDat$TrialType == "black_negative" & tempDat$responseAccData == 1,])
  
  # Time out errors (black positive trials)
  long$numTOErr[long$Subject == i & long$TrialType == "APnumErr_bp"] = 
    nrow(tempDat[tempDat$TrialType == "black_positive" & tempDat$responseAccData == 3,])
  # Commitment errors (black positive trials)
  long$numComErr[long$Subject == i & long$TrialType == "APnumErr_bp"] = 
    nrow(tempDat[tempDat$TrialType == "black_positive" & tempDat$responseAccData == 1,])
  
  # Time out errors (white negative trials)
  long$numTOErr[long$Subject == i & long$TrialType == "APnumErr_wn"] = 
    nrow(tempDat[tempDat$TrialType == "white_negative" & tempDat$responseAccData == 3,])
  # Commitment errors (white negative trials)
  long$numComErr[long$Subject == i & long$TrialType == "APnumErr_wn"] = 
    nrow(tempDat[tempDat$TrialType == "white_negative" & tempDat$responseAccData == 1,])
  
  # Time out errors (white positive trials)
  long$numTOErr[long$Subject == i & long$TrialType == "APnumErr_wp"] = 
    nrow(tempDat[tempDat$TrialType == "white_positive" & tempDat$responseAccData == 3,])
  # Commitment errors (white positive trials)
  long$numComErr[long$Subject == i & long$TrialType == "APnumErr_wp"] = 
    nrow(tempDat[tempDat$TrialType == "white_positive" & tempDat$responseAccData == 1,])
}


# 6. Add observer condition
obs = read.delim("ConditionList.txt", stringsAsFactors=F)

long$Observer = NULL
for (i in unique(long$Subject)) {
  long$Observer[long$Subject == i] = obs$ObsCond[i]
}
# In WIT: 92 Ss total (47 absent, 45 present)
# In AP: 93 Ss total (47 absent, 46 present)
length(unique(long$Subject[long$Task == "WIT" & long$Observer == "Absent" & !is.na(long$numErr)]))

# 7. Add effort and attend question responses
questDat = read.delim("questDat.txt", stringsAsFactors=F) %>%
  rename(Resp = errorQSlide.RESP)
questDat$Task[questDat$blockName == "PostWITquestions"] = "WIT"
questDat$Task[questDat$blockName == "PostAPquestions"] = "AP"

for (i in unique(questDat$Subject)) {
  long$Effort[long$Subject == i & long$Task == "WIT"] = questDat$Resp[questDat$Subject == i & 
                                                                          questDat$SubTrial == 5 &
                                                                          questDat$Task == "WIT"]
  long$Effort[long$Subject == i & long$Task == "AP"] = questDat$Resp[questDat$Subject == i & 
                                                                         questDat$SubTrial == 5 &
                                                                         questDat$Task == "AP"]
  long$Attend[long$Subject == i & long$Task == "WIT"] = questDat$Resp[questDat$Subject == i & 
                                                                          questDat$SubTrial == 4 &
                                                                          questDat$Task == "WIT"]
  long$Attend[long$Subject == i & long$Task == "AP"] = questDat$Resp[questDat$Subject == i & 
                                                                         questDat$SubTrial == 4 &
                                                                         questDat$Task == "AP"]
  long$Anx[long$Subject == i & long$Task == "WIT"] = questDat$Resp[questDat$Subject == i & 
                                                                       questDat$SubTrial == 2 &
                                                                       questDat$Task == "WIT"]
  long$Anx[long$Subject == i & long$Task == "AP"] = questDat$Resp[questDat$Subject == i & 
                                                                      questDat$SubTrial == 2 &
                                                                      questDat$Task == "AP"]
}
  



write.table(long, file = "errCountLong.txt", sep = "\t", row.names = F)

