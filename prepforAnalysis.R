# To create data files for HLM or ANOVA analysis 

require(dplyr)
require(magrittr)

dat = read.delim("experimentalTrials.txt", stringsAsFactors=F) %>%
  select(-errorQSlide.RESP)
# take out bad subs
badSubsWIT = read.delim("badSubsWIT.txt")
badSubsAP = read.delim("badSubsAP.txt")

dat = dat[!(dat$Subject %in% badSubsWIT$Subject & dat$blockName == "WIT"),]    
dat = dat[!(dat$Subject %in% badSubsAP$Subject & dat$blockName == "AP"),]


# need to add questionnaire data, observer status, MTCP scores

# 1. Add observer status
obs = read.delim("ConditionList.txt", stringsAsFactors=F)

dat$Observer = NULL
for (i in unique(dat$Subject)) {
  dat$Observer[dat$Subject == i] = obs$ObsCond[i]
}
# In WIT: 92 Ss total (47 absent, 45 present)
# In AP: 93 Ss total (47 absent, 46 present)


# 2. Add effort and attend questions
questDat = read.delim("questDat.txt", stringsAsFactors=F) %>%
  rename(Resp = errorQSlide.RESP)
questDat$Task[questDat$blockName == "PostWITquestions"] = "WIT"
questDat$Task[questDat$blockName == "PostAPquestions"] = "AP"

for (i in unique(dat$Subject)) {
  dat$Effort[dat$Subject == i & dat$blockName == "WIT"] = questDat$Resp[questDat$Subject == i & 
                                                          questDat$SubTrial == 5 &
                                                          questDat$Task == "WIT"]
  dat$Effort[dat$Subject == i & dat$blockName == "AP"] = questDat$Resp[questDat$Subject == i & 
                                                          questDat$SubTrial == 5 &
                                                          questDat$Task == "AP"]
  dat$Attend[dat$Subject == i & dat$blockName == "WIT"] = questDat$Resp[questDat$Subject == i & 
                                                          questDat$SubTrial == 4 &
                                                          questDat$Task == "WIT"]
  dat$Attend[dat$Subject == i & dat$blockName == "AP"] = questDat$Resp[questDat$Subject == i & 
                                                          questDat$SubTrial == 4 &
                                                          questDat$Task == "AP"]
  
  
}

# 3. Add column for congruence

dat$Congruence[dat$PrimeType == "black" & dat$TargetType == "weapon"] = "congruent"
dat$Congruence[dat$PrimeType == "black" & dat$TargetType == "tool"] = "incongruent"
dat$Congruence[dat$PrimeType == "white" & dat$TargetType == "weapon"] = "congruent"
dat$Congruence[dat$PrimeType == "white" & dat$TargetType == "tool"] = "incongruent"
   
dat$Congruence[dat$PrimeType == "black" & dat$TargetType == "negative"] = "congruent"
dat$Congruence[dat$PrimeType == "black" & dat$TargetType == "positive"] = "incongruent"
dat$Congruence[dat$PrimeType == "white" & dat$TargetType == "negative"] = "congruent"
dat$Congruence[dat$PrimeType == "white" & dat$TargetType == "positive"] = "incongruent"

# 4. Add columns for time out errors and committed errors
# AP trials
dat$TargetAP.ACC.TO = 0
dat$TargetAP.ACC.TO[dat$responseAccData == 3 & dat$blockName == "AP"] = 1 # 1 = TO error, 0 = no TO error

dat$TargetAP.ACC.Com = 0
dat$TargetAP.ACC.Com[dat$responseAccData == 1 & dat$blockName == "AP"] = 1 # 1 = Committed error, 0 = no committed error

# WIT trials
dat$TargetWIT.ACC.TO = 0
dat$TargetWIT.ACC.TO[dat$responseAccData == 3 & dat$blockName == "WIT"] = 1 # 1 = TO error, 0 = no TO error

dat$TargetWIT.ACC.Com = 0
dat$TargetWIT.ACC.Com[dat$responseAccData == 1 & dat$blockName == "WIT"] = 1 # 1 = Committed error, 0 = no committed error



write.table(dat, file = "ForAnalysis.txt", sep = "\t", row.names = F)

