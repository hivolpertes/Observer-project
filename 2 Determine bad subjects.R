# look at mean RT, num of errors for each subject
require(dplyr)
require(magrittr)

dat = read.delim("experimentalTrials.txt", stringsAsFactors=F)

# 1. First look at WIT trials
WITdat = dat[dat$Procedure.Block. == "WITproc",]

# Look at errors on WIT trials
WITdat$TargetWIT.ACC = as.numeric(WITdat$TargetWIT.ACC)
# separate by subject
numCor.WIT = tapply(WITdat$TargetWIT.ACC, INDEX=WITdat$Subject, FUN=sum)
barplot(numCor.WIT)
hist(numCor.WIT, breaks = 20)

meanCor.WIT = mean(numCor.WIT)
sdCor.WIT = sd(numCor.WIT)

numCor.WIT < (meanCor.WIT - 2*sdCor.WIT)
# 3 subjects with more than 2 sds below the mean: 4, 28, 59
# Apparently low accuracy for 28 is because they mixed up buttons
# 19%, 29%, and 30% accuracy respectively 


# 2. Next look at AP trials
APdat = dat[dat$Procedure.Block. == "APproc",]
# Look at errors on AP trials
APdat$TargetAP.ACC = as.numeric(APdat$TargetAP.ACC)

numCor.AP = tapply(APdat$TargetAP.ACC, INDEX=APdat$Subject, FUN=sum)
barplot(numCor.AP)
hist(numCor.AP, breaks = 20)

meanCor.AP = mean(numCor.AP)
sdCor.AP = sd(numCor.AP)

numCor.AP < (meanCor.AP - 2*sdCor.AP)
sum(numCor.AP < (meanCor.AP - 2*sdCor.AP))
# 2 subjects with more than 2 sds below the mean: 34, 64
# Low accuracy for 64 is fell asleep
# 14%, 11% accuracy, respectively


# 3. Write badsubs files

badSubsWIT = data.frame( "Subject" = integer(), "Reason" = character(), stringsAsFactors=FALSE) %>%
  rbind(data.frame("Subject" = 7,
                   "Reason" = "Not a native english speaker",
                   "Reason2" = "")) %>%
  rbind(data.frame("Subject" = 9,
                   "Reason" = "Not a native english speaker",
                   "Reason2" = "")) %>%
  rbind(data.frame("Subject" = 15,
                   "Reason" = "Not a native english speaker",
                   "Reason2" = "")) %>%
  rbind(data.frame("Subject" = 36,
                   "Reason" = "Not a native english speaker",
                   "Reason2" = "")) %>%
  rbind(data.frame("Subject" = 39,
                   "Reason" = "Not a native english speaker",
                   "Reason2" = "")) %>%
  rbind(data.frame("Subject" = 4,
                   "Reason" = ">2 sds below mean",
                   "Reason2" = "")) %>%
  rbind(data.frame("Subject" = 28,
                   "Reason" = ">2 sds below mean",
                   "Reason2" = "Mixed up buttons")) %>%
  rbind(data.frame("Subject" = 59,
                   "Reason" = ">2 sds below mean",
                   "Reason2" = "")) %>%
  rbind(data.frame("Subject" = 53,
                   "Reason" = "No data",
                   "Reason2" = ""))

badSubsAP = data.frame( "Subject" = integer(), "Reason" = character(), stringsAsFactors=FALSE) %>%
  rbind(data.frame("Subject" = 7,
                   "Reason" = "Not a native english speaker",
                   "Reason2" = "")) %>%
  rbind(data.frame("Subject" = 9,
                   "Reason" = "Not a native english speaker",
                   "Reason2" = "")) %>%
  rbind(data.frame("Subject" = 15,
                   "Reason" = "Not a native english speaker",
                   "Reason2" = "")) %>%
  rbind(data.frame("Subject" = 36,
                   "Reason" = "Not a native english speaker",
                   "Reason2" = "")) %>%
  rbind(data.frame("Subject" = 39,
                   "Reason" = "Not a native english speaker",
                   "Reason2" = "")) %>%
  rbind(data.frame("Subject" = 34,
                   "Reason" = ">2 sds below mean",
                   "Reason2" = "")) %>%
  rbind(data.frame("Subject" = 64,
                   "Reason" = ">2 sds below mean",
                   "Reason2" = "Fell asleep")) %>%
  rbind(data.frame("Subject" = 53,
                   "Reason" = "No data",
                   "Reason2" = "")) 

write.table(badSubsWIT, file = "badSubsWIT.txt", sep = "\t", row.names = F)
write.table(badSubsAP, file = "badSubsAP.txt", sep = "\t", row.names = F)


