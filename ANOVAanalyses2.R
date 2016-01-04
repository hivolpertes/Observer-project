# Looks at order effects

require(magrittr)
require(dplyr)
require(tidyr)
require(car)
require(ggplot2)

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

# separate by task
dat.cond.AP = dat.cond[dat.cond$Task == "AP",]
dat.cond.WIT = dat.cond[dat.cond$Task == "WIT",]

dat.cond.AP$TargetType = factor(dat.cond.AP$TargetType) # need to make TargetType have proper levels
dat.cond.WIT$TargetType = factor(dat.cond.WIT$TargetType)


# 1. Look at order effects on self-reported effort/attention

# have to take out subjects for which there's only one task
oneTaskSubs = c(4, 28, 34, 59, 64)

# Effort
aov(Effort ~ Block + # IVs of interest. In nonorthoganal design, order matters 
      Error(Subject/(Block)), # need to add error term for within subjects variables (ie repeated measures)
    data = dat.trial[!(dat.trial$Subject %in% oneTaskSubs),]) %>% 
  summary() # displays Type 1 SS

print(model.tables(aov(Effort ~ Block + Error(Subject/(Block)), 
                       data = dat.trial[!(dat.trial$Subject %in% oneTaskSubs),]),
                   "means"),digits=3)

# Attention
aov(Attend ~ Block + # IVs of interest. In nonorthoganal design, order matters 
      Error(Subject/(Block)), # need to add error term for within subjects variables (ie repeated measures)
    data = dat.trial[!(dat.trial$Subject %in% oneTaskSubs),]) %>% 
  summary() # displays Type 1 SS

print(model.tables(aov(Attend ~ Block + Error(Subject/(Block)), 
                       data = dat.trial[!(dat.trial$Subject %in% oneTaskSubs),]),
                   "means"),digits=3)

# 2. Look at order effect on general accuracy
# take out rows for bad subs
badSubsWIT = read.delim("badsubsWIT.txt")
badSubsAP = read.delim("badsubsAP.txt")

orderDat = dat.cond[!(dat.cond$Subject %in% badSubsWIT$Subject),]
orderDat = orderDat[!(orderDat$Subject %in% badSubsAP$Subject),]


# Need to add block order info to cond data
for (i in unique(orderDat$Subject)) {
  orderDat$Block[orderDat$Subject == i & orderDat$Task == "WIT"] = 
    dat.trial$Block[dat.trial$Subject == i & dat.trial$blockName == "WIT" & dat.trial$SubTrial == 1]
  orderDat$Block[orderDat$Subject == i & orderDat$Task == "AP"] = 
    dat.trial$Block[dat.trial$Subject == i & dat.trial$blockName == "AP" & dat.trial$SubTrial == 1]  
}

# 3. Look at IMS/EMS x Observer on effort/attention
datBoth = dat.trial[!(dat.trial$Subject %in% oneTaskSubs),]
datBoth = datBoth[datBoth$SubTrial == 1,]

# Effort/IMS
# Block 1
ggplot(datBoth[datBoth$Block == 1,], aes(Effort, IMS, fill = Observer)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  ggtitle("Effort/IMS for Block 1")
# Block 2
ggplot(datBoth[datBoth$Block == 2,], aes(Effort, IMS, fill = Observer)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  ggtitle("Effort/IMS for Block 2")

# WIT
ggplot(datBoth[datBoth$blockName == "WIT",], aes(Effort, IMS, fill = Observer)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  ggtitle("Effort/IMS for WIT task")
# AP
ggplot(datBoth[datBoth$blockName == "AP",], aes(Effort, IMS, fill = Observer)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  ggtitle("Effort/IMS for AP task")

lm(Effort ~ IMS*Observer, data = datBoth) %>%
  summary()


# Effort/EMS
# Block 1
ggplot(datBoth[datBoth$Block == 1,], aes(Effort, EMS, fill = Observer)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  ggtitle("Effort/EMS for Block 1")
# Block 2
ggplot(datBoth[datBoth$Block == 2,], aes(Effort, EMS, fill = Observer)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  ggtitle("Effort/EMS for Block 2")

# WIT
ggplot(datBoth[datBoth$blockName == "WIT",], aes(Effort, EMS, fill = Observer)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  ggtitle("Effort/EMS for WIT task")
# AP
ggplot(datBoth[datBoth$blockName == "AP",], aes(Effort, EMS, fill = Observer)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  ggtitle("Effort/EMS for AP task")

lm(Effort ~ EMS*Observer, data = datBoth) %>%
  summary()


# Attend/IMS
# Block 1
ggplot(datBoth[datBoth$Block == 1,], aes(Attend, IMS, fill = Observer)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  ggtitle("Attend/IMS for Block 1")
# Block 2
ggplot(datBoth[datBoth$Block == 2,], aes(Attend, IMS, fill = Observer)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  ggtitle("Attend/IMS for Block 2")

# WIT
ggplot(datBoth[datBoth$blockName == "WIT",], aes(Attend, IMS, fill = Observer)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  ggtitle("Attend/IMS for WIT task")
# AP
ggplot(datBoth[datBoth$blockName == "AP",], aes(Attend, IMS, fill = Observer)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  ggtitle("Attend/IMS for AP task")

lm(Attend ~ IMS*Observer, data = datBoth) %>%
  summary()


# Attend/EMS
# Block 1
ggplot(datBoth[datBoth$Block == 1,], aes(Attend, EMS, fill = Observer)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  ggtitle("Attend/EMS for Block 1")
# Block 2
ggplot(datBoth[datBoth$Block == 2,], aes(Attend, EMS, fill = Observer)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  ggtitle("Attend/EMS for Block 2")

# WIT
ggplot(datBoth[datBoth$blockName == "WIT",], aes(Attend, EMS, fill = Observer)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  ggtitle("Attend/EMS for WIT task")
# AP
ggplot(datBoth[datBoth$blockName == "AP",], aes(Attend, EMS, fill = Observer)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  ggtitle("Attend/EMS for AP task")

lm(Attend ~ EMS*Observer, data = datBoth) %>%
  summary()


