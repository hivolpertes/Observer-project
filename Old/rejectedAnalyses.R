# ANOVAs examining effect of observer that were rejected from manuscript
# replaced with three multiple regressions: DVs are perfBias, task auto and task control



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
# Calculate partial etq-squared for main effect of observer
# SS.effect/(SS.effect + SS.total)
0.000001/(105.5) # partial eta-sq = 0

# partial eta square for 2 way interaction
2.17/(2.17+70.35)   # partial eta sq = .03



# Effect of observer on PDP estimates (auto)
ggplot(pdpStand2, aes(Task, AResid, fill = Observer)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  #  facet_wrap(~Task*Observer) + 
  ggtitle("AResid") +
  labs(x = "Task", y = "Task auto estimate") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        title = element_text(size=20),
        axis.text.x  = element_text(vjust=0.5, size=16, color = "black")
  )

# anova examine effect of observer on auto estimates in each task
aov(AResid ~ Observer*Task + Error(Subject/(Task)), data = pdpStand2) %>% 
  summary()
# partial eta-squared for main effect of observer
.34/(.34+109.56)

# Effect of observer on PDP estimates (control)
ggplot(pdpStand2, aes(Task, MeanC, fill = Observer)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  #  facet_wrap(~Task*Observer) + 
  ggtitle("MeanC") +
  labs(x = "Task", y = "Task control estimate") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        title = element_text(size=20),
        axis.text.x  = element_text(vjust=0.5, size=16, color = "black")
  )

# anova examining effect of observer on control estimates in each task
aov(MeanC ~ Observer*Task + Error(Subject/(Task)), data = pdpStand2) %>% 
  summary()
# partial eta-squared for main effect of observer
.34/(.34+143.46) # partial eta sq = .002



