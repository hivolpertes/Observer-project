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

meanCor.WIT = mean(numCor.WIT)
sdCor.WIT = sd(numCor.WIT)

numCor.WIT < (meanCor.WIT - 2*sdCor.WIT)
# 3 subjects with more than 2 sds below the mean: 4, 28, 59
# Apparently low accuracy for 28 is because they mixed up buttons
# 19%, 29%, and 30% accuracy respectively 


# Next look at AP trials
APdat = dat[dat$Procedure.Block. == "APproc",]

# Look at reaction times on AP trials
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

meanCor.AP = mean(numCor.AP)
sdCor.AP = sd(numCor.AP)

numCor.AP < (meanCor.AP - 2*sdCor.AP)
sum(numCor.AP < (meanCor.AP - 2*sdCor.AP))
# 2 subjects with more than 2 sds below the mean: 34, 64
# Low accuracy for 64 is fell asleep
# 14%, 11% accuracy, respectively

# 2. Put into separate file
output = data.frame(meanRT.WIT) %>%
  bind_cols(data.frame(numCor.WIT)) %>%
  bind_cols(data.frame(meanRT.AP)) %>%
  bind_cols(data.frame(numCor.AP))
  
output$Subject = as.numeric(row.names(output))
output$Subject[output$Subject >52] = as.numeric(row.names(output[output$Subject >52,])) + 1
##### Where is Sub 53?? ######################################

# Add additional things to output so I can visualize
# - MTCP (IMS/EMS)
# - Participant race
for (i in unique(output$Subject)) {
  output$IMS[output$Subject == i] = dat$IMS[dat$Subject == i & dat$SubTrial == 1 & dat$blockName == "WIT"]
  output$EMS[output$Subject == i] = dat$EMS[dat$Subject == i & dat$SubTrial == 1 & dat$blockName == "WIT"]
  output$ParRace[output$Subject == i] = dat$DemoRace.RESP[dat$Subject == i & dat$SubTrial == 1 & dat$blockName == "WIT"]
}

# 3. Visualize relationship between RT and numCor
require(ggplot2)

#WIT: meanRT x numCor.WIT
ggplot(output, aes(meanRT.WIT, numCor.WIT)) +
  geom_point()

#AP: meanRT x numCor.AP
ggplot(output, aes(meanRT.AP, numCor.AP)) +
  geom_point()

#numCor.WIT x numCor.AP
ggplot(output, aes(numCor.AP, numCor.WIT)) +
  geom_point()

#meanRT.WIT x meanRT.AP
ggplot(output, aes(meanRT.AP, meanRT.WIT)) +
  geom_point()


# 4. Separate by participant race
#numCor.WIT x numCor.AP
ggplot(output, aes(numCor.AP, numCor.WIT)) +
  geom_point(aes(color = factor(output$ParRace))) 

# Look at relationship between IMS/EMS and accuracy (nothing is related)
# WIT/IMS
ggplot(output, aes(numCor.WIT, IMS)) +
  geom_point(aes(color = factor(output$ParRace)), na.rm = T) + 
  geom_smooth(method = "lm")
lm(IMS ~ numCor.WIT, data = output) %>% summary()

# WIT/EMS
ggplot(output, aes(numCor.WIT, EMS)) +
  geom_point(aes(color = factor(output$ParRace)), na.rm = T) + 
  geom_smooth(method = "lm")
lm(EMS ~ numCor.WIT, data = output) %>% summary()

# AP/IMS
ggplot(output, aes(numCor.AP, IMS)) +
  geom_point(aes(color = factor(output$ParRace)), na.rm = T) + 
  geom_smooth(method = "lm")
lm(IMS ~ numCor.AP, data = output) %>% summary()

# AP/EMS
ggplot(output, aes(numCor.AP, EMS)) +
  geom_point(aes(color = factor(output$ParRace)), na.rm = T) + 
  geom_smooth(method = "lm")
lm(EMS ~ numCor.AP, data = output) %>% summary()



# 5. Descriptives for max, min, average across tasks
mean(output$numCor.WIT)
mean(output$numCor.AP)

max(output$numCor.WIT)
max(output$numCor.AP)

min(output$numCor.WIT)
min(output$numCor.AP)

# 6. Grab subjects with fewer than 80 correct trials
output[output$numCor.WIT < 80,]
output[output$numCor.AP < 80,]
  

# 7. Import effort/attention questions
questDat = read.delim("questDat.txt", stringsAsFactors=F) %>%
  rename(Resp = errorQSlide.RESP)

questDat$Task[questDat$blockName == "PostWITquestions"] = "WIT"
questDat$Task[questDat$blockName == "PostAPquestions"] = "AP"

# Look for subjects with responses on how attentive they were (#4) and how hard they
# tried (#5) < 4 (midpoint of scale)
questDat[questDat$Resp <=2 & questDat$SubTrial >=4,]

# Sub 8: 2 on effort in WIT
# Sub 59: 2 on effort in WIT
# Sub 80: 2 on effort in AP


# 8. Look at how numCor is related to how hard they tried

for (i in unique(output$Subject)) {
  output$WITeffort[output$Subject == i] = questDat$Resp[questDat$Subject == i & 
                                                      questDat$SubTrial == 5 &
                                                      questDat$Task == "WIT"]
  output$APeffort[output$Subject == i] = questDat$Resp[questDat$Subject == i & 
                                                      questDat$SubTrial == 5 &
                                                      questDat$Task == "AP"]
}

#WIT: WITeffort x numCor.WIT
ggplot(output, aes(WITeffort, numCor.WIT)) +
  geom_point(aes(color = factor(output$ParRace)), na.rm = T) + 
  geom_smooth(method = "lm")
lm(numCor.WIT ~ WITeffort, data = output) %>% summary()

#AP: APeffort x numCor.AP
ggplot(output, aes(APeffort, numCor.AP)) +
  geom_point(aes(color = factor(output$ParRace)), na.rm = T) +
  geom_smooth(method = "lm")
lm(numCor.AP ~ APeffort, data = output) %>% summary()

# Same for how much they paid attention

for (i in unique(output$Subject)) {
  output$WITattend[output$Subject == i] = questDat$Resp[questDat$Subject == i & 
                                                          questDat$SubTrial == 4 &
                                                          questDat$Task == "WIT"]
  output$APattend[output$Subject == i] = questDat$Resp[questDat$Subject == i & 
                                                         questDat$SubTrial == 4 &
                                                         questDat$Task == "AP"]
}

#WIT: WITattend x numCor.WIT
ggplot(output, aes(WITattend, numCor.WIT)) +
  geom_point(aes(color = factor(output$ParRace)), na.rm = T) +
  geom_smooth(method = "lm")
lm(numCor.WIT ~ WITattend, data = output) %>% summary()


#AP: APattend x numCor.AP
ggplot(output, aes(APattend, numCor.AP)) +
  geom_point(aes(color = factor(output$ParRace)), na.rm = T) +
  geom_smooth(method = "lm")
lm(numCor.AP ~ APattend, data = output) %>% summary()


# No clear answers for whether anyone should be eliminated based on effort/attention answers
# Possibly Sub 34 for AP task


# 9. Write badsubs files

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

## possibly Sub 8 & 59 on WIT (for answering "2" on how hard they tried)
# possibly Sub 80 on AP (for answering "2" on how hard they tried)

write.table(badSubsWIT, file = "badSubsWIT.txt", sep = "\t", row.names = F)
write.table(badSubsAP, file = "badSubsAP.txt", sep = "\t", row.names = F)





############################################################################
##### Do same figures, but without bad subjects ############################
############################################################################

WITdat.nb = WITdat[!(WITdat$Subject %in% badSubsWIT$Subject),]
APdat.nb = APdat[!(APdat$Subject %in% badSubsAP$Subject),]

# 1. Histograms
# WIT RT
meanRT.WIT.nb = tapply(WITdat.nb$TargetWIT.RT[WITdat.nb$responseAccData!=3], INDEX=WITdat.nb$Subject[WITdat.nb$responseAccData!=3], FUN=mean)
barplot(meanRT.WIT.nb)

hist(meanRT.WIT.nb, breaks=30) # histogram of mean RTs (averaged for each subject)
hist(WITdat.nb$TargetWIT.RT[WITdat.nb$responseAccData!=3], breaks = 30) # histogram of RTs for each trial (skewed negatively)

# AP RT
meanRT.AP.nb = tapply(APdat.nb$TargetAP.RT[APdat.nb$responseAccData!=3], INDEX=APdat.nb$Subject[APdat.nb$responseAccData!=3], FUN=mean)
barplot(meanRT.AP.nb)

hist(meanRT.AP.nb, breaks=30) # histogram of mean RTs (averaged for each subject)
hist(APdat.nb$TargetAP.RT[APdat.nb$responseAccData!=3], breaks = 30) # histogram of RTs for each trial (skewed negatively)

# WIT accuracy
numCor.WIT.nb = tapply(WITdat.nb$TargetWIT.ACC, INDEX=WITdat.nb$Subject, FUN=sum)
barplot(numCor.WIT.nb)

hist(numCor.WIT.nb, breaks=30) # histogram of accuracy (averaged for each subject)

# AP accuracy
numCor.AP.nb = tapply(APdat.nb$TargetAP.ACC, INDEX=APdat.nb$Subject, FUN=sum)
barplot(numCor.AP.nb)

hist(numCor.AP.nb, breaks=30) # histogram of accuracy (averaged for each subject)



# 2. Relationship between WIT and AP
# Takes out both WIT and AP data for subjects that are bad, even if only bad in one
outputNoBad = output[!(output$Subject %in% badSubsWIT$Subject),]
outputNoBad = outputNoBad[!(outputNoBad$Subject %in% badSubsAP$Subject),]

#numCor.WIT x numCor.AP
ggplot(outputNoBad, aes(numCor.AP, numCor.WIT)) +
  geom_point() +
  geom_smooth(method = "lm") + 
#  ggtitle("Correlation between accuracy on WIT and accuracy on AP") +
  labs(x = "Overall accuracy on AP", y = "Overall accuracy on WIT") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        title = element_text(size=20)
        #axis.text.x  = element_text(angle=90, vjust=0.5, size=16)
        )


lm(numCor.AP ~ numCor.WIT, data = outputNoBad) %>% summary()

mean(outputNoBad$numCor.WIT)
mean(outputNoBad$numCor.AP)


# 3. Look at relationship between IMS/EMS and accuracy (no relationship)
# Takes out data for each bad subject, but only for one task that's bad
outputNB.WIT = output[!(output$Subject %in% badSubsWIT$Subject),] %>%
  select(-contains("AP"))

outputNB.AP = output[!(output$Subject %in% badSubsAP$Subject),] %>%
  select(-contains("WIT"))


# WIT/IMS
ggplot(outputNB.WIT, aes(numCor.WIT, IMS)) +
  geom_point(aes(color = factor(outputNB.WIT$ParRace)), na.rm = T) + 
  geom_smooth(method = "lm", na.rm = T)
lm(IMS ~ numCor.WIT, data = outputNB.WIT) %>% summary()

# WIT/EMS
ggplot(outputNB.WIT, aes(numCor.WIT, EMS)) +
  geom_point(aes(color = factor(outputNB.WIT$ParRace)), na.rm = T) + 
  geom_smooth(method = "lm", na.rm = T)
lm(EMS ~ numCor.WIT, data = outputNB.WIT) %>% summary()

# AP/IMS
ggplot(outputNB.AP, aes(numCor.AP, IMS)) +
  geom_point(aes(color = factor(outputNB.AP$ParRace)), na.rm = T) + 
  geom_smooth(method = "lm", na.rm = T)
lm(IMS ~ numCor.AP, data = outputNB.AP) %>% summary()

# AP/EMS
ggplot(outputNB.AP, aes(numCor.AP, EMS)) +
  geom_point(aes(color = factor(outputNB.AP$ParRace)), na.rm = T) + 
  geom_smooth(method = "lm", na.rm = T)
lm(EMS ~ numCor.AP, data = outputNB.AP) %>% summary()


# 4. Look at observer condition
obs = read.delim("ConditionList.txt", stringsAsFactors=F)

outputNoBad$Observer = NULL
for (i in unique(outputNoBad$Subject)) {
  outputNoBad$Observer[outputNoBad$Subject == i] = obs$ObsCond[i]
}

# for just WIT subjects
for (i in unique(outputNB.WIT$Subject)) {
  outputNB.WIT$Observer[outputNB.WIT$Subject == i] = obs$ObsCond[i]
}

# for just AP subjects
for (i in unique(outputNB.AP$Subject)) {
  outputNB.AP$Observer[outputNB.AP$Subject == i] = obs$ObsCond[i]
}

# Look at relationship between IMS/EMS and accuracy (no relationship), separated by observer condition
# WIT/IMS
ggplot(outputNoBad, aes(numCor.WIT, IMS)) +
  geom_point(aes(color = factor(outputNoBad$ParRace)), na.rm = T) + 
  geom_smooth(method = "lm", na.rm = T) +
  facet_wrap(~Observer)
lm(numCor.WIT ~ IMS*Observer, data = outputNoBad) %>% summary()

# WIT/EMS
ggplot(outputNoBad, aes(numCor.WIT, EMS)) +
  geom_point(aes(color = factor(outputNoBad$ParRace)), na.rm = T) + 
  geom_smooth(method = "lm", na.rm = T) +
  facet_wrap(~Observer)
#lm(EMS ~ numCor.WIT, data = outputNoBad) %>% summary()

# AP/IMS
ggplot(outputNoBad, aes(numCor.AP, IMS)) +
  geom_point(aes(color = factor(outputNoBad$ParRace)), na.rm = T) + 
  geom_smooth(method = "lm", na.rm = T) +
  facet_wrap(~Observer)
#lm(IMS ~ numCor.AP, data = outputNoBad) %>% summary()

# AP/EMS
ggplot(outputNoBad, aes(numCor.AP, EMS)) +
  geom_point(aes(color = factor(outputNoBad$ParRace)), na.rm = T) + 
  geom_smooth(method = "lm", na.rm = T) +
  facet_wrap(~Observer)
lm(numCor.AP ~ EMS*Observer, data = outputNoBad) %>% summary()
lm(numCor.AP ~ EMS, data = outputNoBad[outputNoBad$Observer == "Absent",]) %>% summary()


# 5. Look at main effect of observer on accuracy (no effect)
# AP
ggplot(outputNB.AP, aes(numCor.AP, Observer)) +
  geom_point(aes(color = factor(outputNB.AP$ParRace)), na.rm = T) 

mean(outputNB.AP$numCor.AP[outputNB.AP$Observer == "Present"])
mean(outputNB.AP$numCor.AP[outputNB.AP$Observer == "Absent"])

require(car)
outputNB.AP$Observer = as.factor(outputNB.AP$Observer)
Anova(lm(numCor.AP ~ Observer, data = outputNB.AP), type = 3)


# WIT
ggplot(outputNB.WIT, aes(numCor.WIT, Observer)) +
  geom_point(aes(color = factor(outputNB.WIT$ParRace)), na.rm = T) 

mean(outputNB.WIT$numCor.WIT[outputNB.WIT$Observer == "Present"])
mean(outputNB.WIT$numCor.WIT[outputNB.WIT$Observer == "Absent"])

outputNB.WIT$Observer = as.factor(outputNB.WIT$Observer)
Anova(lm(numCor.WIT ~ Observer, data = outputNB.WIT), type = 3)


# 6. Look at relationship between accuracy and effort/attention

# WIT/Effort
ggplot(outputNB.WIT, aes(numCor.WIT, WITeffort)) +
  geom_point(na.rm = T) + 
  geom_smooth(method = "lm", na.rm = T) +
  labs(x = "Overall accuracy on WIT", y = "Self-reported Effort") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        title = element_text(size=20)
        #axis.text.x  = element_text(angle=90, vjust=0.5, size=16)
  )
lm(WITeffort ~ numCor.WIT, data = outputNB.WIT) %>% summary()

# WIT/Attend
ggplot(outputNB.WIT, aes(numCor.WIT, WITattend)) +
  geom_point(na.rm = T) + 
  geom_smooth(method = "lm", na.rm = T) +
  labs(x = "Overall accuracy on WIT", y = "Self-reported Attention") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        title = element_text(size=20)
        #axis.text.x  = element_text(angle=90, vjust=0.5, size=16)
  )
lm(WITattend ~ numCor.WIT, data = outputNB.WIT) %>% summary()

# AP/Effort
ggplot(outputNB.AP, aes(numCor.AP, APeffort)) +
  geom_point( na.rm = T) + 
  geom_smooth(method = "lm", na.rm = T) +
  labs(x = "Overall accuracy on AP", y = "Self-reported Effort") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        title = element_text(size=20)
        #axis.text.x  = element_text(angle=90, vjust=0.5, size=16)
  )
lm(APeffort ~ numCor.AP, data = outputNB.AP) %>% summary()

# AP/Attend
ggplot(outputNB.AP, aes(numCor.AP, APattend)) +
  geom_point( na.rm = T) + 
  geom_smooth(method = "lm", na.rm = T) +
  labs(x = "Overall accuracy on AP", y = "Self-reported Attention") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        title = element_text(size=20)
        #axis.text.x  = element_text(angle=90, vjust=0.5, size=16)
  )
lm(APattend ~ numCor.AP, data = outputNB.AP) %>% summary()





 