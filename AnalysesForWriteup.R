# Analyses for results write up section, in order presented in paper. 

require(ggplot2)
require(magrittr)

# Read in file prepped for HLM (long form: each trial is a row, each column is a variable)
# Bad subs are already taken out (just for task that's bad- data for other task is still in)
dat.trial = read.delim("ForAnalysis.txt")

# change variables to factors
dat.trial$Subject = as.factor(dat.trial$Subject)
dat.trial$Block = as.factor(dat.trial$Block)
dat.trial$responseAccData = as.factor(dat.trial$responseAccData)
dat.trial$TargetAP.ACC = as.factor(dat.trial$TargetAP.ACC)
dat.trial$TargetWIT.ACC = as.factor(dat.trial$TargetWIT.ACC)
dat.trial$TargetAP.ACC.TO = as.factor(dat.trial$TargetAP.ACC.TO)
dat.trial$TargetWIT.ACC.TO = as.factor(dat.trial$TargetWIT.ACC.TO)
dat.trial$TargetAP.ACC.Com = as.factor(dat.trial$TargetAP.ACC.Com)
dat.trial$TargetWIT.ACC.Com = as.factor(dat.trial$TargetWIT.ACC.Com)

# For analyses with accuracy as DV, need trials to be grouped into conditions (ie number of errors in each condition)
dat.cond = read.delim("errCountLong.txt")
dat.cond$Subject = as.factor(dat.cond$Subject)
dat.cond$TargetType = as.character(dat.cond$TargetType) # need to get rid of irrelevant factor levels for each task

# separate by task
dat.cond.AP = dat.cond[dat.cond$Task == "AP",]
dat.cond.WIT = dat.cond[dat.cond$Task == "WIT",]

dat.cond.AP$TargetType = as.factor(dat.cond.AP$TargetType) # need to make TargetType a factor again
dat.cond.WIT$TargetType = as.factor(dat.cond.WIT$TargetType)

#######################################################################
######################## Performance Bias ##############################
#######################################################################

##### 1a. Looking at priming effect (specifying race, valence as within Ss variables) in AP task

# Race x Valence on accuracy (AP)
aov(numErr ~ PrimeType*TargetType + # IVs of interest. In nonorthoganal design, order matters 
      Error(Subject/(PrimeType*TargetType)), # need to add error term for within subjects variables (ie repeated measures)
    data = dat.cond.AP) %>% 
  summary() # displays Type 1 SS

##### 1b. Look at simple contrasts in AP
require(dplyr)
require(tidyr)

dat2.AP = summarise(group_by(dat.trial[dat.trial$blockName == "AP",], TargetType, PrimeType, Subject), Correct = sum(TargetAP.ACC))
dat2.AP = unite(dat2.AP, Condition, PrimeType, TargetType,  sep = "_")
flat.AP = spread(dat2.AP, Condition,  Correct)

# Order: Black neg, Black pos, White neg, White pos
# Testing Black pos vs neg- SIGNIFICANT (p = .010)
contr1 = c(1, -1, 0, 0)
xc1=cbind(flat.AP$black_negative, flat.AP$black_positive, flat.AP$white_negative, flat.AP$white_positive) %*% contr1 
aov1=aov(xc1~1) 
summary(aov1,intercept=T)

# check with ANOVA that limits to two conditions: same thing
aov(numErr ~ GenType + Error(Subject/(GenType)), 
    data = dat.cond.AP[dat.cond.AP$GenType == "black_con" | dat.cond.AP$GenType == "black_incon",]) %>%
  summary()

# Testing White pos vs neg- SIGNIFICANT (p < .0001)
contr2 = c(0, 0, 1, -1)
xc2=cbind(flat.AP$black_negative, flat.AP$black_positive, flat.AP$white_negative, flat.AP$white_positive) %*% contr2
aov2=aov(xc2~1) 
summary(aov2,intercept=T)

###### 2a. Looking at priming effect (specifying race, valence as within Ss variables) in WIT task

# Race x Valence on accuracy (WIT)
aov(numErr ~ PrimeType*TargetType + Error(Subject/(PrimeType*TargetType)), data = dat.cond.WIT) %>% 
  summary()

##### 2b. Look at simple contrasts in WIT

dat2.WIT = summarise(group_by(dat.trial[dat.trial$blockName == "WIT",], TargetType, PrimeType, Subject), Correct = sum(TargetWIT.ACC))
dat2.WIT = unite(dat2.WIT, Condition, PrimeType, TargetType,  sep = "_")
flat.WIT = spread(dat2.WIT, Condition,  Correct)

# Order: Black gun, Black tool, White gun, White tool
# Testing Black gun vs tool
contr1 = c(1, -1, 0, 0)
xc1=cbind(flat.WIT$black_weapon, flat.WIT$black_tool, flat.WIT$white_weapon, flat.WIT$white_tool) %*% contr1 
aov1=aov(xc1~1) 
summary(aov1,intercept=T)

# Testing White gun vs tool
contr2 = c(0, 0, 1, -1)
xc2=cbind(flat.WIT$black_weapon, flat.WIT$black_tool, flat.WIT$white_weapon, flat.WIT$white_tool) %*% contr2
aov2=aov(xc2~1) 
summary(aov2,intercept=T)


#######################################################################
######################## Comparing tasks ##############################
#######################################################################


dat.cond$TargetType = as.factor(dat.cond$TargetType)

# Look just at subjects that have data for both tasks, otherwise throws error ("Error() model is singular")
# Probably because some subjects don't have data across both levels of task
bsWIT = read.delim("badsubsWIT.txt")
bsAP = read.delim("badsubsAP.txt")
dat.cond.nobs = dat.cond[!(dat.cond$Subject %in% bsWIT$Subject) & !(dat.cond$Subject %in% bsAP$Subject),]

# See if pattern of racial bias differs across two tasks- TOTAL ERRORS
aov(numErr ~ (PrimeType*ConType*Task)+Error(Subject/(PrimeType*ConType*Task)), data = dat.cond.nobs) %>%
  summary()

# Total number of errors
ggplot(dat.cond.nobs, aes(PrimeType, numErr, fill = ConType)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_wrap(~Task) + 
  #  ggtitle("Total number of errors") +
  labs(y = "Number of errors", x = "Race of Prime") +
  scale_fill_manual(values=c("firebrick4","goldenrod1"))+
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        #title = element_text(size=20)
        axis.text.x  = element_text(vjust=0.5, size=16, color="black"),
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        strip.text.x = element_text(size = 16, face="bold")
  )

#############################################
# calculate performance bias scores (error|incongruent - errors|congruent)
perfBias = data.frame("Subject" = unique(dat.cond.nobs$Subject))

# add errors from all congruent trials together, add errors form all incongruent trials together
for (i in unique(perfBias$Subject)) {
  perfBias$WITconErrors[perfBias$Subject == i] = dat.cond.nobs$numErr[dat.cond.nobs$Subject == i &
                                                                        dat.cond.nobs$GenType == "black_con" &
                                                                        dat.cond.nobs$Task == "WIT"] + 
    dat.cond.nobs$numErr[dat.cond.nobs$Subject == i &
                           dat.cond.nobs$GenType == "white_con" &
                           dat.cond.nobs$Task == "WIT"]
  perfBias$WITinconErrors[perfBias$Subject == i] = dat.cond.nobs$numErr[dat.cond.nobs$Subject == i &
                                                                          dat.cond.nobs$GenType == "black_incon" &
                                                                          dat.cond.nobs$Task == "WIT"] + 
    dat.cond.nobs$numErr[dat.cond.nobs$Subject == i &
                           dat.cond.nobs$GenType == "white_incon" &
                           dat.cond.nobs$Task == "WIT"]
  
  
  perfBias$APconErrors[perfBias$Subject == i] = dat.cond.nobs$numErr[dat.cond.nobs$Subject == i &
                                                                       dat.cond.nobs$GenType == "black_con" &
                                                                       dat.cond.nobs$Task == "AP"] + 
    dat.cond.nobs$numErr[dat.cond.nobs$Subject == i &
                           dat.cond.nobs$GenType == "white_con" &
                           dat.cond.nobs$Task == "AP"]
  perfBias$APinconErrors[perfBias$Subject == i] = dat.cond.nobs$numErr[dat.cond.nobs$Subject == i &
                                                                         dat.cond.nobs$GenType == "black_incon" &
                                                                         dat.cond.nobs$Task == "AP"] + 
    dat.cond.nobs$numErr[dat.cond.nobs$Subject == i &
                           dat.cond.nobs$GenType == "white_incon" &
                           dat.cond.nobs$Task == "AP"]
}

# create difference score for performance bias estimate
# larger perf bias estimate means more bias (more errors on incongruent trials than congruent trials)
perfBias = mutate(perfBias, WITperfBias = WITinconErrors/96 - WITconErrors/96) %>%
  mutate(APperfBias = APinconErrors/96 - APconErrors/96)

# create standardized scores
perfBias$WITStand = scale(perfBias$WITperfBias)
perfBias$APStand = scale(perfBias$APperfBias)

# readjust subject factor levels, change standardized scores to numeric
perfBias$Subject = factor(perfBias$Subject)
perfBias$WITStand = as.numeric(perfBias$WITStand)
perfBias$APStand = as.numeric(perfBias$APStand)

# Look at correlation between tasks
ggplot(perfBias, aes(APStand, WITStand)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  #  ggtitle("Correlation between accuracy on WIT and accuracy on AP") +
  labs(x = "Stand. Performance bias on AP", y = "Stand. Performance bias on WIT") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        title = element_text(size=20)
        #axis.text.x  = element_text(angle=90, vjust=0.5, size=16)
  )

lm(APStand ~ WITStand, data = perfBias) %>%
  summary()


#######################################################################
######################## PDP analyses ##############################
#######################################################################

# read in PDP estimates for both tasks

longWIT = read.delim("PDPestimatesWITlong.txt")
longAP = read.delim("PDPestimatesAPlong.txt")

# make separate data sets for estimates that separate by race 
# WIT
longWITsep = longWIT[!(longWIT$Type == "PDPbiasDiff"),]
longWITsep = longWITsep[grep("White|Black", longWITsep$Type),]

# correct factor levels for Type
longWITsep$Type = factor(longWITsep$Type)

# AP
longAPsep = longAP[!(longAP$Type == "PDPbiasDiff"),]
longAPsep = longAPsep[grep("White|Black", longAPsep$Type),]

# correct factor levels for Type
longAPsep$Type = factor(longAPsep$Type)

# make Subject a factor in order to do anovas
longWIT$Subject = as.factor(longWIT$Subject)
longAP$Subject = as.factor(longAP$Subject)

longWITsep$Subject = as.factor(longWITsep$Subject)
longAPsep$Subject = as.factor(longAPsep$Subject)

# 1. Look at comparison of means of estimates across race of prime within each task

# pairwise comparison of Black_A and White_A in WIT
aov(value ~ PrimeType + Error(Subject/(PrimeType)), data = longWITsep[longWITsep$Estimate == "A",]) %>% 
  summary()
# pairwise comparison of Black_C and White_C in WIT
aov(value ~ PrimeType + Error(Subject/(PrimeType)), data = longWITsep[longWITsep$Estimate == "C",]) %>% 
  summary()

# pairwise comparison of Black_A and White_A in AP
aov(value ~ PrimeType + Error(Subject/(PrimeType)), data = longAPsep[longAPsep$Estimate == "A",]) %>% 
  summary()
# pairwise comparison of Black_C and White_C in AP
aov(value ~ PrimeType + Error(Subject/(PrimeType)), data = longAPsep[longAPsep$Estimate == "C",]) %>% 
  summary()

# 2. Look at comparisons across tasks

# need to make data set with subjects that have both task data
wideWIT = read.delim("PDPestimatesWITwide.txt")
wideAP = read.delim("PDPestimatesAPwide.txt")

pdpBoth = select(wideWIT, c(Subject, Observer, Black_C, Black_A, White_C, White_A, MeanC, Resids)) %>%
  rename(        WIT_BlackC = Black_C, 
                 WIT_BlackA = Black_A, 
                 WIT_WhiteA = White_A, 
                 WIT_WhiteC = White_C,
                 WIT_MeanC = MeanC,
                 WIT_AResid = Resids)
pdpBoth = pdpBoth[pdpBoth$Subject %in% wideAP$Subject,] %>%
  left_join(select(wideAP, c(Subject, Observer, Black_C, Black_A, White_C, White_A, MeanC, Resids)), by = "Subject")
pdpBoth = rename(pdpBoth, 
                 AP_BlackC = Black_C, 
                 AP_BlackA = Black_A, 
                 AP_WhiteA = White_A, 
                 AP_WhiteC = White_C,
                 AP_MeanC = MeanC, 
                 AP_AResid = Resids,
                 Observer = Observer.x) %>%
  select(-Observer.y)

# make standardized PDP estimates
pdpStand = pdpBoth[,1:2]  # make new data.frame, just bring in subject numbers and observer condition 

# add standardized estimates for MeanC and AResid for each task
pdpStand$WIT_MeanC = scale(pdpBoth$WIT_MeanC)   # scale() is equivalent to (x-mean(x))/sd(x)
pdpStand$WIT_AResid = scale(pdpBoth$WIT_AResid)

pdpStand$AP_MeanC = scale(pdpBoth$AP_MeanC)
pdpStand$AP_AResid = scale(pdpBoth$AP_AResid)

# look at correlations of estimates between tasks - Mean C
ggplot(pdpStand, aes(WIT_MeanC, AP_MeanC)) +
  geom_point() +
  ggtitle("Mean C estimate for WIT/AP (Stand data)") +
  geom_smooth(method = "lm") +
  labs(x = "Standardized Mean C for WIT", y = "Standardized Mean C for AP") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        title = element_text(size=20)
        #axis.text.x  = element_text(angle=90, vjust=0.5, size=16)
  )
lm(AP_MeanC ~ WIT_MeanC, data = pdpStand) %>%
  summary()

# look at correlations of estimates between tasks - AResid
ggplot(pdpStand, aes(WIT_AResid, AP_AResid)) +
  geom_point() +
  ggtitle("AResid estimate for WIT/AP (Stand)") +
  geom_smooth(method = "lm") +
  labs(x = "Standardized AResid for WIT", y = "Standardized AResid for AP") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        title = element_text(size=20)
        #axis.text.x  = element_text(angle=90, vjust=0.5, size=16)
  )
lm(WIT_AResid ~ AP_AResid, data = pdpStand) %>%
  summary()

# 3. Compare simple slopes of Mean C correlation and AResid correlation

# rearrange data so it can be plotted 
dat1 = select(pdpStand, Subject, contains("MeanC")) %>%
  rename(WIT = WIT_MeanC,
         AP = AP_MeanC)
dat1$Type = "MeanC"

dat2 = select(pdpStand, Subject, contains("AResid")) %>%
  rename(WIT = WIT_AResid,
         AP = AP_AResid)
dat2$Type = "AResid"

SSC = rbind(dat1, dat2)

# Visualize simple slopes- MeanC + AResid
ggplot(SSC, aes(WIT, AP, fill = Type, col = Type, pch = Type)) +
  geom_point() +
  ggtitle("Correlations across tasks for task control and auto estimates") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        title = element_text(size=20),
        legend.text = element_text(size = 16)
        #axis.text.x  = element_text(angle=90, vjust=0.5, size=16)
  ) +
  geom_smooth(method = "lm")

# Interaction represents whether simple slope of MeanC is different from AResid
lm(WIT ~ AP*Type, data = SSC) %>%
  summary()




#######################################################################
######################## Effect of observer ##############################
######################## on performance bias ##############################
#######################################################################

# Add observer information to perfBias data
dat.cond$Observer = as.character(dat.cond$Observer)
for (i in unique(perfBias$Subject)) {
  perfBias$Observer[perfBias$Subject == i] = dat.cond$Observer[dat.cond$TrialType == "WITnumErr_bw" &
                                                                 dat.cond$Subject == i]
}

# rearrange to long form (just standardized data)
pbLong = select(perfBias, -contains("con"), -contains("perfBias")) %>%
  gather(Task,pbStand,2:3)                   # "Task" is what previous column names go into
                                              # "pbStand" is what data points go into 


ggplot(pbLong, aes(Task, pbStand, fill = Observer)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  #  facet_wrap(~Task*Observer) + 
  ggtitle("Performance bias (P(errors|incon) - P(errors|congruent))") +
  labs(x = "Task", y = "Performance bias") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        title = element_text(size=20),
        axis.text.x  = element_text(vjust=0.5, size=16, color = "black")
  )


# change observer to factors for anovas
pbLong$Observer = as.character(pbLong$Observer)

# Omnibus ANOVA to examine effect of observer
aov(pbStand ~ (Observer*Task) +Error(Subject/(Task)), data = pbLong) %>% 
  summary()

# pairwise comparison of Observer Present and Observer Absent within AP
aov(pbStand ~ Observer, data = pbLong[pbLong$Task == "APStand",]) %>% 
  summary()

# pairwise comparison of Observer Present and Observer Absent within WIT
aov(pbStand ~ Observer, data = pbLong[pbLong$Task == "WITStand",]) %>% 
  summary()


#########################################################################
# Look at observer x IMS interaction in predicting Mean C, AResid

# rearrange data to half wide/half long form
# columns: Subject, Task, Observer, MeanC, ADiff, AResid

temp1 = pdpStand[,1:4] %>%    # Takes just WIT data
  rename(MeanC = WIT_MeanC,
         AResid = WIT_AResid)
temp1$Task = "WIT"

temp2 = rename(pdpStand[,c(1:2, 5:6)],     # Takes just AP data
               MeanC = AP_MeanC,
               AResid = AP_AResid)
temp2$Task = "AP"

# Bind WIT and AP data together
pdpStand2 = rbind(temp1, temp2)    


# Add IMS/EMS data
for (i in unique(pdpStand2$Subject)) {
  pdpStand2$IMS[pdpStand2$Subject == i] = dat.trial$IMS[dat.trial$Subject == i & 
                                                          dat.trial$SubTrial == 1 & 
                                                          dat.trial$blockName == "WIT"]
  pdpStand2$EMS[pdpStand2$Subject == i] = dat.trial$EMS[dat.trial$Subject == i & 
                                                          dat.trial$SubTrial == 1 & 
                                                          dat.trial$blockName == "WIT"]
}

# Adjust classes of variables
pdpStand2$Subject = factor(pdpStand2$Subject)
pdpStand2$MeanC = as.numeric(pdpStand2$MeanC)
pdpStand2$ADiff = as.numeric(pdpStand2$ADiff)
pdpStand2$AResid = as.numeric(pdpStand2$AResid)
pdpStand2$Task = factor(pdpStand2$Task)

#######################################################################
######################## IMS x Observer #################################
######################## on AResid, MeanC #########################
#######################################################################

# IMS and ADiff separated by observer
ggplot(pdpStand2, aes(IMS, ADiff, fill = Observer, col = Observer, pch = Observer)) +
  geom_point() +
  ggtitle("IMS/ADiff") +
  facet_wrap(~Task) +
  geom_smooth(method = "lm") +
  labs(y = "Stand. ADiff estimate") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=28),
        axis.title.y = element_text(face="bold", colour="#990000", size=28),
        plot.title = element_text(size=28, vjust = 2, color = "white"),
        plot.background = element_rect(fill = "black"),
        axis.text.x  = element_text(vjust=0.5, size=16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16),
        strip.text = element_text(size=24)
  )

# IMS*Observer interaction
## WIT - significant (p = .0496)
lm(ADiff ~ IMS*Observer, data = pdpStand2[pdpStand2$Task == "WIT",]) %>%
  summary()
## AP
lm(ADiff ~ IMS*Observer, data = pdpStand2[pdpStand2$Task == "AP",]) %>%
  summary()

# simple slopes for Observer Present condition
## WIT - marginally significant (p = .05)
lm(ADiff ~ IMS, data = pdpStand2[pdpStand2$Observer == "Present" &
                                   pdpStand2$Task == "WIT",]) %>% summary()
## AP
lm(ADiff ~ IMS, data = pdpStand2[pdpStand2$Observer == "Present" &
                                   pdpStand2$Task == "AP",]) %>% summary()



# IMS and AResid separated by observer
ggplot(pdpStand2, aes(IMS, AResid, fill = Observer, col = Observer, pch = Observer)) +
  geom_point() +
  ggtitle("IMS/AResid") +
  facet_wrap(~Task) +
  geom_smooth(method = "lm") +
  labs(y = "Stand. AResid estimate") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=28),
        axis.title.y = element_text(face="bold", colour="#990000", size=28),
        plot.title = element_text(size=28, vjust = 2, color = "white"),
        plot.background = element_rect(fill = "black"),
        axis.text.x  = element_text(vjust=0.5, size=16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16),
        strip.text = element_text(size=24)
  )

# IMS*Observer interaction
## WIT - significant (p = .03)
lm(AResid ~ IMS*Observer, data = pdpStand2[pdpStand2$Task == "WIT",]) %>%
  summary()
## AP
lm(AResid ~ IMS*Observer, data = pdpStand2[pdpStand2$Task == "AP",]) %>%
  summary()

# simple slopes for Observer Present condition
## WIT
lm(AResid ~ IMS, data = pdpStand2[pdpStand2$Observer == "Present" &
                                   pdpStand2$Task == "WIT",]) %>% summary()
## AP
lm(AResid ~ IMS, data = pdpStand2[pdpStand2$Observer == "Present" &
                                   pdpStand2$Task == "AP",]) %>% summary()


# IMS and MeanC separated by observer
ggplot(pdpStand2, aes(IMS, MeanC, fill = Observer, col = Observer, pch = Observer)) +
  geom_point() +
  ggtitle("IMS/MeanC") +
  facet_wrap(~Task) +
  geom_smooth(method = "lm") +
  labs(y = "Stand. MeanC estimate") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=28),
        axis.title.y = element_text(face="bold", colour="#990000", size=28),
        plot.title = element_text(size=28, vjust = 2, color = "white"),
        plot.background = element_rect(fill = "black"),
        axis.text.x  = element_text(vjust=0.5, size=16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16),
        strip.text = element_text(size=24)
  )


# Put two tasks in same graph

# IMS on ADiff, separated by task
# only for observer present condition
ggplot(pdpStand2[pdpStand2$Observer == "Present",], 
       aes(IMS, ADiff, fill = Task, col = Task, pch = Task)) +
  geom_point() +
  ggtitle("Observer present condition") +
  labs(y = "Stand. ADiff estimate") +
  geom_smooth(method = "lm") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=28),
        axis.title.y = element_text(face="bold", colour="#990000", size=28),
        plot.title = element_text(size=28, vjust = 2, color = "white"),
        plot.background = element_rect(fill = "black"),
        axis.text.x  = element_text(vjust=0.5, size=16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16)
  )

# interaction is significant - p = (.009)
lm(ADiff ~ IMS*Task, data = pdpStand2[pdpStand2$Observer == "Present",]) %>%  
  summary()

# IMS on AResid, separated by task
# only for observer present condition
ggplot(pdpStand2[pdpStand2$Observer == "Present",], 
       aes(IMS, AResid, fill = Task, col = Task, pch = Task)) +
  geom_point() +
  ggtitle("Observer present condition") +
  labs(y = "Stand. AResid estimate") +
  geom_smooth(method = "lm") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=28),
        axis.title.y = element_text(face="bold", colour="#990000", size=28),
        plot.title = element_text(size=28, vjust = 2, color = "white"),
        plot.background = element_rect(fill = "black"),
        axis.text.x  = element_text(vjust=0.5, size=16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16)
  )

# interaction is significant - p = (.027)
lm(AResid ~ IMS*Task, data = pdpStand2[pdpStand2$Observer == "Present",]) %>%  
  summary()


# IMS on MeanC, separated by task
# only for observer present condition
ggplot(pdpStand2[pdpStand2$Observer == "Present",], 
       aes(IMS, MeanC, fill = Task, col = Task, pch = Task)) +
  geom_point() +
  ggtitle("Observer present condition") +
  labs(y = "Stand. MeanC estimate") +
  geom_smooth(method = "lm") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=28),
        axis.title.y = element_text(face="bold", colour="#990000", size=28),
        plot.title = element_text(size=28, vjust = 2, color = "white"),
        plot.background = element_rect(fill = "black"),
        axis.text.x  = element_text(vjust=0.5, size=16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16)
  )

# interaction is significant - p = (.027)
lm(MeanC ~ IMS*Task, data = pdpStand2[pdpStand2$Observer == "Present",]) %>%  
  summary()
