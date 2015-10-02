# Reduces data file to important columns
# Isolates experimental trials, writes to .txt file
# Separates WIT and AP blocks, write to separate .txt files

require(dplyr)
require(magrittr)

# read in data
dat = read.delim("ForRMerge76.txt")

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
                    targetWord)

#select experimental trials (minus practice and questions)
expTrials = dat.select[dat.select$blockName == "WIT"|dat.select$blockName == "AP",]

write.table(expTrials, file = "experimentalTrials.txt", sep = "\t", row.names = F)



# create separate data frames for each task and only include relevant columns
WIT.dat = dat.select[dat.select$Procedure.Block. == "WITproc",] %>%
  select(-responseMappingVal) %>%
  select(-contains("TargetAP")) %>%
  select(-contains("pos")) %>%
  select(-contains("neg")) %>%
  select(-targetWord)

AP.dat = dat.select[dat.select$Procedure.Block. == "APproc",] %>%
  select(-responseMappingTool) %>%
  select(-contains("TargetWIT")) %>%
  select(-contains("weapon")) %>%
  select(-contains("tool")) %>%
  select(-targetPictureFile)

write.table(WIT.dat, file = "WITdata.txt", sep = "\t", row.names = F)
write.table(AP.dat, file = "APdata.txt", sep = "\t", row.names = F)
