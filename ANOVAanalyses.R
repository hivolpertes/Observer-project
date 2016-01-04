require(magrittr)
require(dplyr)
require(tidyr)
require(car)

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

############################################################################
###################### Priming effects + Observer ##########################
############################################################################

############# DV: Total errors ######################

# 1. Looking at priming effect (specifying race, valence as within Ss variables) in AP task

# Race x Valence on accuracy (AP)
aov(numErr ~ PrimeType*TargetType + # IVs of interest. In nonorthoganal design, order matters 
      Error(Subject/(PrimeType*TargetType)), # need to add error term for within subjects variables (ie repeated measures)
    data = dat.cond.AP) %>% 
  summary() # displays Type 1 SS

##### TargetType and TargetType*PrimeType interaction are significant ######

# means
print(model.tables(aov(numErr ~ PrimeType*TargetType + Error(Subject/(PrimeType*TargetType)), data = dat.cond.AP),
                   "means"),digits=3)

#### SIMPLE EFFECTS

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

# Testing Black vs White "congruent"- SIGNIFICANT (p = .005)
contr3 = c(1, 0, 0, -1)
xc3=cbind(flat.AP$black_negative, flat.AP$black_positive, flat.AP$white_negative, flat.AP$white_positive) %*% contr3
aov3=aov(xc3~1) 
summary(aov3,intercept=T)

# Testing Black vs White "incongruent"- SIGNIFICANT (p = .017)
contr4 = c(0, 1, -1, 0)
xc4=cbind(flat.AP$black_negative, flat.AP$black_positive, flat.AP$white_negative, flat.AP$white_positive) %*% contr4
aov4=aov(xc4~1) 
summary(aov4,intercept=T)





# 2. Looking at priming effect (specifying race, valence as within Ss variables) in WIT task

# Race x Valence on accuracy (WIT)
aov(numErr ~ PrimeType*TargetType + Error(Subject/(PrimeType*TargetType)), data = dat.cond.WIT) %>% 
  summary()
##### TargetType and TargetType*PrimeType interaction are significant ######

# means
print(model.tables(aov(numErr ~ PrimeType*TargetType + Error(Subject/(PrimeType*TargetType)), data = dat.cond.WIT),
                   "means"),digits=3)

#### SIMPLE EFFECTS

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

# Testing Black vs White "congruent"- SIGNIFICANT (p = .001)
contr3 = c(1, 0, 0, -1)
xc3=cbind(flat.WIT$black_weapon, flat.WIT$black_tool, flat.WIT$white_weapon, flat.WIT$white_tool) %*% contr3
aov3=aov(xc3~1) 
summary(aov3,intercept=T)

# Testing Black vs White "incongruent"- SIGNIFICANT (p < .0001)
contr4 = c(0, 1, -1, 0)
xc4=cbind(flat.WIT$black_weapon, flat.WIT$black_tool, flat.WIT$white_weapon, flat.WIT$white_tool) %*% contr4
aov4=aov(xc4~1) 
summary(aov4,intercept=T)




# 3. Look at Observer effect (specifying race, valence as within Ss variables, observer as between) in AP task

# Race x Valence x Observer on accuracy (AP)
aov(numErr ~ (PrimeType*TargetType*Observer)+Error(Subject/(PrimeType*TargetType)), data = dat.cond.AP) %>%
  summary()
### Observer effect not significant, nor any interactions with observer ####


# 4. Look at Observer effect (specifying race, valence as within Ss variables, observer as between) in WIT task

# Race x Valence x Observer on accuracy (WIT)
aov(numErr ~ (PrimeType*TargetType*Observer)+Error(Subject/(PrimeType*TargetType)), data = dat.cond.WIT) %>% 
  summary()
### Observer effect not significant, no interactions with observer ####



############# DV: Timeout errors ######################

# 5. Looking at priming effect (specifying race, valence as within Ss variables) in AP task

# Race x Valence on accuracy (AP)
aov(numTOErr ~ PrimeType*TargetType + Error(Subject/(PrimeType*TargetType)), data = dat.cond.AP) %>% 
  summary()
##### TargetType and TargetType*PrimeType interaction are significant ######

# means
print(model.tables(aov(numTOErr ~ PrimeType*TargetType + Error(Subject/(PrimeType*TargetType)), data = dat.cond.AP),
                   "means"),digits=3)


#### SIMPLE EFFECTS

dat2.AP = summarise(group_by(dat.trial[dat.trial$blockName == "AP",], TargetType, PrimeType, Subject), Correct = sum(TargetAP.ACC.TO))
dat2.AP = unite(dat2.AP, Condition, PrimeType, TargetType,  sep = "_")
flat.AP = spread(dat2.AP, Condition,  Correct)

# Order: Black neg, Black pos, White neg, White pos
# Testing Black pos vs neg- not significant
contr1 = c(1, -1, 0, 0)
xc1=cbind(flat.AP$black_negative, flat.AP$black_positive, flat.AP$white_negative, flat.AP$white_positive) %*% contr1 
aov1=aov(xc1~1) 
summary(aov1,intercept=T)

# Testing White pos vs neg- SIGNIFICANT (p < .0001)
contr2 = c(0, 0, 1, -1)
xc2=cbind(flat.AP$black_negative, flat.AP$black_positive, flat.AP$white_negative, flat.AP$white_positive) %*% contr2
aov2=aov(xc2~1) 
summary(aov2,intercept=T)

# Testing Black vs White "congruent"- SIGNIFICANT (p = .005)
contr3 = c(1, 0, 0, -1)
xc3=cbind(flat.AP$black_negative, flat.AP$black_positive, flat.AP$white_negative, flat.AP$white_positive) %*% contr3
aov3=aov(xc3~1) 
summary(aov3,intercept=T)

# Testing Black vs White "incongruent"- SIGNIFICANT (p = .017)
contr4 = c(0, 1, -1, 0)
xc4=cbind(flat.AP$black_negative, flat.AP$black_positive, flat.AP$white_negative, flat.AP$white_positive) %*% contr4
aov4=aov(xc4~1) 
summary(aov4,intercept=T)


# 6. Looking at priming effect (specifying race, valence as within Ss variables) in WIT task

# Race x Valence on accuracy (WIT)
aov(numTOErr ~ PrimeType*TargetType + Error(Subject/(PrimeType*TargetType)), data = dat.cond.WIT) %>% 
  summary()
##### TargetType and TargetType*PrimeType interaction are significant ######

# means
print(model.tables(aov(numTOErr ~ PrimeType*TargetType + Error(Subject/(PrimeType*TargetType)), data = dat.cond.WIT),
                   "means"),digits=3)



#### SIMPLE EFFECTS

dat2.WIT = summarise(group_by(dat.trial[dat.trial$blockName == "WIT",], TargetType, PrimeType, Subject), Correct = sum(TargetWIT.ACC.TO))
dat2.WIT = unite(dat2.WIT, Condition, PrimeType, TargetType,  sep = "_")
flat.WIT = spread(dat2.WIT, Condition,  Correct)

# Order: Black neg, Black pos, White neg, White pos
# Testing Black pos vs neg- SIGNIFICANT (p < .0001)
contr1 = c(1, -1, 0, 0)
xc1=cbind(flat.WIT$black_weapon, flat.WIT$black_tool, flat.WIT$white_weapon, flat.WIT$white_tool) %*% contr1 
aov1=aov(xc1~1) 
summary(aov1,intercept=T)

# Testing White pos vs neg- not significant
contr2 = c(0, 0, 1, -1)
xc2=cbind(flat.WIT$black_weapon, flat.WIT$black_tool, flat.WIT$white_weapon, flat.WIT$white_tool) %*% contr2
aov2=aov(xc2~1) 
summary(aov2,intercept=T)

# Testing Black vs White "congruent"- SIGNIFICANT (p = .001)
contr3 = c(1, 0, 0, -1)
xc3=cbind(flat.WIT$black_weapon, flat.WIT$black_tool, flat.WIT$white_weapon, flat.WIT$white_tool) %*% contr3
aov3=aov(xc3~1) 
summary(aov3,intercept=T)

# Testing Black vs White "incongruent"- SIGNIFICANT (p < .0001)
contr4 = c(0, 1, -1, 0)
xc4=cbind(flat.WIT$black_weapon, flat.WIT$black_tool, flat.WIT$white_weapon, flat.WIT$white_tool) %*% contr4
aov4=aov(xc4~1) 
summary(aov4,intercept=T)



# 7. Look at Observer effect (specifying race, valence as within Ss variables, observer as between) in AP task in TO errors

# Race x Valence x Observer on accuracy (AP)
aov(numTOErr ~ (PrimeType*TargetType*Observer)+Error(Subject/(PrimeType*TargetType)), data = dat.cond.AP) %>%
  summary()
### Observer effect marginally significant (p = .057), not any interactions with observer ####


# 8. Look at Observer effect (specifying race, valence as within Ss variables, observer as between) in WIT task in TO errors

# Race x Valence x Observer on accuracy (WIT)
aov(numTOErr ~ (PrimeType*TargetType*Observer)+Error(Subject/(PrimeType*TargetType)), data = dat.cond.WIT) %>% 
  summary()
### Observer effect SIGNIFICANT (p = .017), no interactions with observer ####





############# DV: Committed errors ######################

# 9. Looking at priming effect (specifying race, valence as within Ss variables) in AP task

# Race x Valence on accuracy (AP)
aov(numComErr ~ PrimeType*TargetType + Error(Subject/(PrimeType*TargetType)), data = dat.cond.AP) %>% 
  summary()
##### PrimeType and TargetType*PrimeType interaction are significant ######

# means
print(model.tables(aov(numComErr ~ PrimeType*TargetType + Error(Subject/(PrimeType*TargetType)), data = dat.cond.AP),
                   "means"),digits=3)



#### SIMPLE EFFECTS

dat2.AP = summarise(group_by(dat.trial[dat.trial$blockName == "AP",], TargetType, PrimeType, Subject), Correct = sum(TargetAP.ACC.Com))
dat2.AP = unite(dat2.AP, Condition, PrimeType, TargetType,  sep = "_")
flat.AP = spread(dat2.AP, Condition,  Correct)

# Order: Black neg, Black pos, White neg, White pos
# Testing Black pos vs neg- SIGNIFICANT (p < .0001)
contr1 = c(1, -1, 0, 0)
xc1=cbind(flat.AP$black_negative, flat.AP$black_positive, flat.AP$white_negative, flat.AP$white_positive) %*% contr1 
aov1=aov(xc1~1) 
summary(aov1,intercept=T)

# Testing White pos vs neg- SIGNIFICANT (p < .0001)
contr2 = c(0, 0, 1, -1)
xc2=cbind(flat.AP$black_negative, flat.AP$black_positive, flat.AP$white_negative, flat.AP$white_positive) %*% contr2
aov2=aov(xc2~1) 
summary(aov2,intercept=T)

# Testing Black vs White "congruent"- not significant
contr3 = c(1, 0, 0, -1)
xc3=cbind(flat.AP$black_negative, flat.AP$black_positive, flat.AP$white_negative, flat.AP$white_positive) %*% contr3
aov3=aov(xc3~1) 
summary(aov3,intercept=T)

# Testing Black vs White "incongruent"- not significant
contr4 = c(0, 1, -1, 0)
xc4=cbind(flat.AP$black_negative, flat.AP$black_positive, flat.AP$white_negative, flat.AP$white_positive) %*% contr4
aov4=aov(xc4~1) 
summary(aov4,intercept=T)


# 10. Looking at priming effect (specifying race, valence as within Ss variables) in WIT task

# Race x Valence on accuracy (WIT)
aov(numComErr ~ PrimeType*TargetType + Error(Subject/(PrimeType*TargetType)), data = dat.cond.WIT) %>% 
  summary()
##### TargetType*PrimeType interaction is significant ######

# means
print(model.tables(aov(numComErr ~ PrimeType*TargetType + Error(Subject/(PrimeType*TargetType)), data = dat.cond.WIT),
                   "means"),digits=3)



#### SIMPLE EFFECTS

dat2.WIT = summarise(group_by(dat.trial[dat.trial$blockName == "WIT",], TargetType, PrimeType, Subject), Correct = sum(TargetWIT.ACC.Com))
dat2.WIT = unite(dat2.WIT, Condition, PrimeType, TargetType,  sep = "_")
flat.WIT = spread(dat2.WIT, Condition,  Correct)

# Order: Black neg, Black pos, White neg, White pos
# Testing Black pos vs neg- SIGNIFICANT (p < .0001)
contr1 = c(1, -1, 0, 0)
xc1=cbind(flat.WIT$black_weapon, flat.WIT$black_tool, flat.WIT$white_weapon, flat.WIT$white_tool) %*% contr1 
aov1=aov(xc1~1) 
summary(aov1,intercept=T)

# Testing White pos vs neg- SIGNIFICANT (p < .001)
contr2 = c(0, 0, 1, -1)
xc2=cbind(flat.WIT$black_weapon, flat.WIT$black_tool, flat.WIT$white_weapon, flat.WIT$white_tool) %*% contr2
aov2=aov(xc2~1) 
summary(aov2,intercept=T)

# Testing Black vs White "congruent"- not significant
contr3 = c(1, 0, 0, -1)
xc3=cbind(flat.WIT$black_weapon, flat.WIT$black_tool, flat.WIT$white_weapon, flat.WIT$white_tool) %*% contr3
aov3=aov(xc3~1) 
summary(aov3,intercept=T)

# Testing Black vs White "incongruent"- not significant
contr4 = c(0, 1, -1, 0)
xc4=cbind(flat.WIT$black_weapon, flat.WIT$black_tool, flat.WIT$white_weapon, flat.WIT$white_tool) %*% contr4
aov4=aov(xc4~1) 
summary(aov4,intercept=T)



# 11. Look at Observer effect (specifying race, valence as within Ss variables, observer as between) in AP task

# Race x Valence x Observer on accuracy (AP)
aov(numComErr ~ (PrimeType*TargetType*Observer)+Error(Subject/(PrimeType*TargetType)), data = dat.cond.AP) %>%
  summary()

# Check if Congruence x Observer is the same (it is)
aov(numComErr ~ (ConType*Observer)+Error(Subject/ConType), data = dat.cond.AP) %>%
  summary()

# 12. Look at Observer effect (specifying race, valence as within Ss variables, observer as between) in WIT task

# Race x Valence x Observer on accuracy (WIT)
aov(numComErr ~ (PrimeType*TargetType*Observer)+Error(Subject/(PrimeType*TargetType)), data = dat.cond.WIT) %>% 
  summary()




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

# See if pattern of racial bias differs across two tasks- TIMEOUT ERRORS
aov(numTOErr ~ (PrimeType*ConType*Task)+Error(Subject/(PrimeType*ConType*Task)), data = dat.cond.nobs) %>%
  summary()

print(model.tables(aov(numTOErr ~ (PrimeType*ConType*Task)+Error(Subject/(PrimeType*ConType*Task)), data = dat.cond.nobs),
                   "means"),digits=3)

# See if pattern of racial bias differs across two tasks- COMMITTED ERRORS
aov(numComErr ~ (PrimeType*ConType*Task)+Error(Subject/(PrimeType*ConType*Task)), data = dat.cond.nobs) %>%
  summary()


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

# Look at correlation between tasks
ggplot(perfBias, aes(APperfBias, WITperfBias)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  #  ggtitle("Correlation between accuracy on WIT and accuracy on AP") +
  labs(x = "Performance bias on AP", y = "Performance bias on WIT") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        title = element_text(size=20)
        #axis.text.x  = element_text(angle=90, vjust=0.5, size=16)
  )

lm(APperfBias ~ WITperfBias, data = perfBias) %>%
  summary()


# Add observer condition for each subject

for (i in unique(perfBias$Subject)) {
  perfBias$Observer[perfBias$Subject == i] = as.character(dat.cond$Observer[dat.cond$Subject == i &
                                                                 dat.cond$Task == "WIT" &
                                                                 dat.cond$GenType == "black_con"])
}

write.table(perfBias, file = "performanceBiasEstimates.txt", sep = "\t", row.names = F)



# Look at performance bias on each task separated by observer condition

long = select(perfBias, -contains("con")) %>%
  gather(Task,perfBias,2:3)                   # "Task" is what previous column names go into
                                              # "perfBias" is what data points go into 


ggplot(long, aes(Task, perfBias, fill = Observer)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
#  facet_wrap(~Task*Observer) + 
  ggtitle("Performance bias (P(errors|incon) - P(errors|congruent)") +
  labs(x = "Task", y = "Performance bias") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        title = element_text(size=20),
        axis.text.x  = element_text(vjust=0.5, size=16, color = "black")
  )
        
aov(perfBias ~ Task*Observer + Error(Subject/(Task)), data = long) %>%
  summary()


ggplot(long, aes(Task, perfBias, color = Observer)) +
  geom_point() +
  ggtitle("Performance bias (P(errors|incon) - P(errors|congruent)") +
  labs(x = "Task", y = "Performance bias") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        title = element_text(size=20),
        axis.text.x  = element_text(vjust=0.5, size=16, color = "black")
  )

# Is there a difference in perf bias for high IMS/high EMS and high IMS/low EMS?
# Take out participants with no IMS/EMS scores

longSS = long[!is.na(long$IMS),]

# separate participants by median split
median(longSS$IMS, na.rm = T) # 7.8
median(longSS$EMS, na.rm = T) # 5

longSS$IMSsplit = NULL
for (i in unique(longSS$Subject)) {
  if (longSS$IMS[longSS$Subject == i & longSS$Task == "WITperfBias"] >= median(longSS$IMS, na.rm = T))
    longSS$IMSsplit[longSS$Subject == i] = "high"
  if (longSS$IMS[longSS$Subject == i & longSS$Task == "WITperfBias"] < median(longSS$IMS, na.rm = T))
    longSS$IMSsplit[longSS$Subject == i] = "low"
  if (longSS$EMS[longSS$Subject == i & longSS$Task == "WITperfBias"] >= median(longSS$EMS, na.rm = T))
    longSS$EMSsplit[longSS$Subject == i] = "high"
  if (longSS$EMS[longSS$Subject == i & longSS$Task == "WITperfBias"] < median(longSS$EMS, na.rm = T))
    longSS$EMSsplit[longSS$Subject == i] = "low"
}

# high IMS/low EMS = 21 participants
length(unique(longSS$Subject[longSS$IMSsplit == "high" & longSS$EMSsplit == "low"]))
# high IMS/high EMS = 19 participants
length(unique(longSS$Subject[longSS$IMSsplit == "high" & longSS$EMSsplit == "high"]))
# low IMS/low EMS = 16 participants
length(unique(longSS$Subject[longSS$IMSsplit == "low" & longSS$EMSsplit == "low"]))
# low IMS/high EMS = 22 participants
length(unique(longSS$Subject[longSS$IMSsplit == "low" & longSS$EMSsplit == "high"]))

ggplot(longSS, aes(IMSsplit, perfBias, fill = EMSsplit)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  #  facet_wrap(~Task*Observer) + 
#  ggtitle("Performance bias (P(errors|incon) - P(errors|congruent)") +
#  labs(x = "Task", y = "Performance bias") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        title = element_text(size=20),
        axis.text.x  = element_text(vjust=0.5, size=16, color = "black")
  )

# Look at how IMS/EMS interact with observer

# Add IMS/EMS data to perfBias data
for (i in unique(perfBias$Subject)) {
  perfBias$IMS[perfBias$Subject == i] = dat.trial$IMS[dat.trial$Subject == i & 
                                                        dat.trial$SubTrial == 1 & 
                                                        dat.trial$Block == 1]
  perfBias$EMS[perfBias$Subject == i] = dat.trial$EMS[dat.trial$Subject == i & 
                                                        dat.trial$SubTrial == 1 & 
                                                        dat.trial$Block == 1]
}

# Visualize relationship between IMS and AP performance Bias, separated by observer
ggplot(perfBias, aes(IMS, APperfBias, fill = Observer, col = Observer, pch = Observer)) +
  geom_point() +
  ggtitle("IMS and perfBias, separated by observer") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        title = element_text(size=20)
        #axis.text.x  = element_text(angle=90, vjust=0.5, size=16)
  ) +
  geom_smooth(method = "lm")
lm(APperfBias ~ IMS*Observer, data = perfBias) %>%
  summary()

# Visualize relationship between EMS and AP performance Bias, separated by observer
ggplot(perfBias, aes(EMS, APperfBias, fill = Observer, col = Observer, pch = Observer)) +
  geom_point() +
  ggtitle("EMS and perfBias, separated by observer") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        title = element_text(size=20)
        #axis.text.x  = element_text(angle=90, vjust=0.5, size=16)
  ) +
  geom_smooth(method = "lm")
lm(APperfBias ~ EMS*Observer, data = perfBias) %>%
  summary()

# Visualize relationship between IMS and WIT performance Bias, separated by observer
ggplot(perfBias, aes(IMS, WITperfBias, fill = Observer, col = Observer, pch = Observer)) +
  geom_point() +
  ggtitle("IMS and perfBias, separated by observer") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        title = element_text(size=20)
        #axis.text.x  = element_text(angle=90, vjust=0.5, size=16)
  ) +
  geom_smooth(method = "lm")
lm(WITperfBias ~ IMS*Observer, data = perfBias) %>%
  summary()

# Visualize relationship between EMS and WIT performance Bias, separated by observer
ggplot(perfBias, aes(EMS, WITperfBias, fill = Observer, col = Observer, pch = Observer)) +
  geom_point() +
  ggtitle("EMS and perfBias, separated by observer") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        title = element_text(size=20)
        #axis.text.x  = element_text(angle=90, vjust=0.5, size=16)
  ) +
  geom_smooth(method = "lm")
lm(WITperfBias ~ EMS*Observer, data = perfBias) %>%
  summary()



  

