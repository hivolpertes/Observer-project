# Explore data
# look at mean RT, num of errors for each subject
require(dplyr)
require(magrittr)

dat = read.delim("experimentalTrials.txt", stringsAsFactors=F)

# 1. First look at WIT trials
WITdat = dat[dat$Procedure.Block. == "WITproc",]

# Look at reaction times on WIT trials
WITdat$TargetWIT.RT = as.numeric(WITdat$TargetWIT.RT)

meanRT.WIT = tapply(WITdat$TargetWIT.RT[WITdat$responseAccData!=3], INDEX=WITdat$Subject[WITdat$responseAccData!=3], FUN=mean)
meanRT.WIT
barplot(meanRT.WIT)
hist(meanRT.WIT, breaks=20)

# Look at errors on WIT trials
WITdat$TargetWIT.ACC = as.numeric(WITdat$TargetWIT.ACC)

numCor.WIT = tapply(WITdat$TargetWIT.ACC, INDEX=WITdat$Subject, FUN=sum)
numCor.WIT
barplot(numCor.WIT)
hist(numCor.WIT, breaks = 20)



# 2. Next look at AP trials
APdat = dat[dat$Procedure.Block. == "APproc",]

# Look at reaction times on WIT trials
APdat$TargetAP.RT = as.numeric(APdat$TargetAP.RT)

meanRT.AP = tapply(APdat$TargetAP.RT[APdat$responseAccData!=3], INDEX=APdat$Subject[APdat$responseAccData!=3], FUN=mean)
meanRT.AP
barplot(meanRT.AP)
hist(meanRT.AP, breaks=20)

# Look at errors on AP trials
APdat$TargetAP.ACC = as.numeric(APdat$TargetAP.ACC)

numCor.AP = tapply(APdat$TargetAP.ACC, INDEX=APdat$Subject, FUN=sum)
numCor.AP
barplot(numCor.AP)
hist(numCor.AP, breaks = 20)

# Put into separate file
output = data.frame(meanRT.WIT) %>%
  bind_cols(data.frame(numCor.WIT)) %>%
  bind_cols(data.frame(meanRT.AP)) %>%
  bind_cols(data.frame(numCor.AP))
  
output$Subject = as.factor(row.names(output))
##### Where is Sub 53?? ######################################


# 3. Visualize relationship between RT and numCor
require(ggplot2)

#WIT: meanRT x numCor
ggplot(output, aes(meanRT.WIT, numCor.WIT)) +
  geom_point(color = output$Subject)

#AP: meanRT x numCor
ggplot(output, aes(meanRT.AP, numCor.AP)) +
  geom_point()

#numCor.WIT x numCor.AP
ggplot(output, aes(numCor.AP, numCor.WIT)) +
  geom_point()

#meanRT.WIT x meanRT.AP
ggplot(output, aes(meanRT.AP, meanRT.WIT)) +
  geom_point()

# 4. Descriptives for max, min, average across tasks
mean(output$numCor.WIT)
mean(output$numCor.AP)

max(output$numCor.WIT)
max(output$numCor.AP)

min(output$numCor.WIT)
min(output$numCor.AP)

# 5. Grab subjects with fewer than 80 correct trials
output[output$numCor.WIT < 80,]
output[output$numCor.AP < 80,]

# 6. Figure out which subs need to be thrown out and write badsubs file

badSubs = data.frame("Subject" = 4,
                     "reason" = "Only pressed one response on WIT")

# consider Sub 34 (<= 10 trials correct on any AP trial type)
# consider Sub 64 (< 10 trials correct on any AP trial type)