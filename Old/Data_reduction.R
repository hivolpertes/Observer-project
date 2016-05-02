# Reduces data file to important columns
# Isolates experimental trials, writes to .txt file
# Adds IMS/EMS data; calculates alphas for both scales

require(dplyr)
require(magrittr)

# read in data
dat = read.delim("ForRMerge101.txt")

dat.select = select(dat, 
                    Subject, 
                    Session,
                    DemoAge.RESP,
                    DemoEthnicity.RESP,
                    DemoGender.RESP,
                    DemoRace.RESP,
                    responseMappingTool,
                    responseMappingVal,
                    Block, 
                    negResponse, 
                    posResponse,
                    Procedure.Block.,
                    toolResponse, 
                    weaponResponse,
                    Trial,
                    blockName,
                    Practice.Trial.,
                    SubTrial,
                    CorrectResponse,
                    facePictureFile,
                    ITIDuration,
                    PrimeType,
                    responseAccData,
                    rtFeedbackText,
                    TargetAP.ACC,
                    TargetAP.CRESP,
                    TargetAP.RESP,
                    TargetAP.RT,
                    targetPictureFile,
                    TargetType,
                    TargetWIT.ACC,
                    TargetWIT.CRESP,
                    TargetWIT.RESP,
                    TargetWIT.RT,
                    targetWord,
                    errorQSlide.RESP)

# select experimental trials (minus practice and questions)
expTrials = dat.select[dat.select$blockName == "WIT"|dat.select$blockName == "AP",]

# adds MTCP scores
MTCP = read.delim("MTCPscores.txt")
# includes 5 subjects added after semester was over: 37, 45, 70, 73, 85

# reverse score IMS_1
MTCP$IMS_1.rev = NULL

for (i in unique(MTCP$Subject)) {
  MTCP$IMS_1.rev[MTCP$Subject == i] = 10 - MTCP$IMS_1[MTCP$Subject == i]
}

MTCP = mutate(MTCP, IMS = (MTCP$IMS_1.rev + MTCP$IMS_2 + MTCP$IMS_3 + MTCP$IMS_4 + MTCP$IMS_5)/5)
MTCP = mutate(MTCP, EMS = (MTCP$EMS_1 + MTCP$EMS_2 + MTCP$EMS_3 + MTCP$EMS_4 + MTCP$EMS_5)/5)
# manually add IMS/EMS scores for subject 15 and 59 because of missing data on one item
MTCP$EMS[MTCP$Subject == 15] = 1.8
MTCP$IMS[MTCP$Subject == 59] = 2.3

# calculate alphas for IMS and EMS
require(psych)

IMS = select(MTCP, starts_with("IMS")) %>%
  select(-IMS) %>%
  select(-IMS_1)

EMS = select(MTCP, starts_with("EMS")) %>%
  select(-EMS)

alpha(IMS)
alpha(EMS)

for (i in unique(expTrials$Subject)) {
  expTrials$IMS[expTrials$Subject == i] = MTCP$IMS[MTCP$Subject == i]
  expTrials$EMS[expTrials$Subject == i] = MTCP$EMS[MTCP$Subject == i]
}

write.table(expTrials, file = "experimentalTrials.txt", sep = "\t", row.names = F)



# create separate data frames for each task and only include relevant columns
# WIT.dat = expTrials[expTrials$blockName == "WIT",] %>%
#   select(-responseMappingVal) %>%
#   select(-contains("TargetAP")) %>%
#   select(-contains("pos")) %>%
#   select(-contains("neg")) %>%
#   select(-targetWord)
# 
# AP.dat = expTrials[expTrials$blockName == "AP",] %>%
#   select(-responseMappingTool) %>%
#   select(-contains("TargetWIT")) %>%
#   select(-contains("weapon")) %>%
#   select(-contains("tool")) %>%
#   select(-targetPictureFile)
# 
# write.table(WIT.dat, file = "WITdata.txt", sep = "\t", row.names = F)
# write.table(AP.dat, file = "APdata.txt", sep = "\t", row.names = F)


# Select question data for each task

questDat = dat.select[dat.select$blockName == "PostWITquestions"|dat.select$blockName == "PostAPquestions",] %>%
  select(c(Subject, SubTrial, errorQSlide.RESP, blockName))

write.table(questDat, file = "questDat.txt", sep = "\t", row.names = F)
