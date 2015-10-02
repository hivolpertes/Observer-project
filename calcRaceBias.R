# Calculate race bias by looking at the number of errors on each trial type for each subject
require(dplyr)
require(magrittr)
require(tidyr)

dat = read.delim("experimentalTrials.txt", stringsAsFactors=F)

# 1. First look at WIT trials
WITdat = dat[dat$Procedure.Block. == "WITproc",]

for (i in 1:nrow(WITdat)) {
  WITdat$TrialType[i] = paste(WITdat$PrimeType[i],WITdat$TargetType[i], sep = "_")
}

# count errors for each type of trial (out of 48 trials per trial type)
WITdat$TargetWIT.ACC = as.numeric(WITdat$TargetWIT.ACC)

errCount = data.frame("Subject" = unique(WITdat$Subject))

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

long$Task = NA
long$Task[grep("WIT",long$TrialType)] = "WIT" 
long$Task[grep("AP",long$TrialType)] = "AP" 

long$ConType = NA
long$ConType[grep("_con",long$GenType)] = "congruent"
long$ConType[grep("_incon",long$GenType)] = "incongruent"

long$RaceType = NA
long$RaceType[grep("black",long$GenType)] = "black"
long$RaceType[grep("white",long$GenType)] = "white"



# 5. Plot average errors in each trial across conditions

ggplot(long, aes(RaceType, numErr, fill = ConType)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_wrap(~Task)
