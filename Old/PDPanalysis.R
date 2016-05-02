require(ggplot2)
require(dplyr)

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


# make separate data sets for estimates that collapse across race 
# WIT
longWITtot = longWIT[(longWIT$Type == "PDPbiasDiff"),] %>%
  bind_rows(longWIT[(longWIT$Type == "MeanC"),])

longWITtot$Type = factor(longWITtot$Type)

# AP
longAPtot = longAP[(longAP$Type == "PDPbiasDiff"),] %>%
  bind_rows(longAP[(longAP$Type == "MeanC"),])

longAPtot$Type = factor(longAPtot$Type)




# make Subject a factor in order to do anovas
longWIT$Subject = as.factor(longWIT$Subject)
longAP$Subject = as.factor(longAP$Subject)

longWITsep$Subject = as.factor(longWITsep$Subject)
longAPsep$Subject = as.factor(longAPsep$Subject)

longWITtot$Subject = as.factor(longWITtot$Subject)
longAPtot$Subject = as.factor(longAPtot$Subject)

# 1. Look at means of estimates, separately for race of prime
# AP
ggplot(longAPsep, aes(PrimeType, value, fill = Estimate)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  #  facet_wrap(~Observer) + 
  scale_fill_manual(values=c("darkblue","lightblue")) +
#  ggtitle("PDP estimates for AP task") +
  labs(y = "Value of estimate", x = "Race of Prime") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        title = element_text(size=24),
        axis.text.x  = element_text(vjust=0.5, size=16, color="black"),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16)
#        strip.text.x = element_text(size = 16, face="bold")
        )        
        

aov(value ~ PrimeType*Estimate + Error(Subject/(PrimeType*Estimate)), data = longAPsep) %>% 
  summary()

# WIT
ggplot(longWITsep, aes(PrimeType, value, fill = Estimate)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  #  facet_wrap(~Observer) + 
  scale_fill_manual(values=c("darkblue","lightblue")) +
#  ggtitle("PDP estimates for WIT task") +
  labs(y = "Value of estimate", x = "Race of Prime") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        plot.title = element_text(size=24, vjust=2),
        axis.text.x  = element_text(vjust=0.5, size=16, color="black"),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16)
        #        strip.text.x = element_text(size = 16, face="bold")
  )   

aov(value ~ PrimeType*Estimate + Error(Subject/(PrimeType*Estimate)), data = longWITsep) %>% 
  summary()

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

# Combine data for both tasks into one file to look at 3 way interaction
longBoth = rbind(longAPsep, longWITsep)         # need to take out subs who only have one task
longBoth = longBoth[!(longBoth$Subject %in% c(4, 28, 34, 59, 64)),]

longBoth$Subject = factor(longBoth$Subject)

ggplot(longBoth, aes(PrimeType, value, fill = Estimate)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_wrap(~Task) + 
  scale_fill_manual(values=c("darkblue","lightblue")) +
  #  ggtitle("PDP estimates for WIT task") +
  labs(y = "Value of estimate", x = "Race of Prime") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        plot.title = element_text(size=24, vjust=2),
        axis.text.x  = element_text(vjust=0.5, size=16, color="black"),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16)
        #        strip.text.x = element_text(size = 16, face="bold")
  )   

aov(value ~ PrimeType*Estimate*Task + Error(Subject/(PrimeType*Estimate*Task)), data = longBoth) %>%
  summary()

# 2. Look at correlations of estimates within each task

# need to make data set with subjects that have both task data
wideWIT = read.delim("PDPestimatesWITwide.txt")
wideAP = read.delim("PDPestimatesAPwide.txt")

pdpBoth = rename(wideWIT, WIT_BlackC = Black_C, 
                          WIT_BlackA = Black_A, 
                          WIT_WhiteA = White_A, 
                          WIT_WhiteC = White_C,
                          WIT_TotalC = Total_C,
                          WIT_TotalA = Total_A,
                          WIT_biasDiff = PDPbiasDiff,
                          WIT_DiffC = DiffC)
pdpBoth = pdpBoth[pdpBoth$Subject %in% wideAP$Subject,] %>%
  select(-Task) %>%
  left_join(wideAP, by = "Subject")
pdpBoth = rename(pdpBoth, AP_BlackC = Black_C, 
                           AP_BlackA = Black_A, 
                           AP_WhiteA = White_A, 
                           AP_WhiteC = White_C,
                           AP_TotalC = Total_C,
                           AP_TotalA = Total_A,
                           AP_biasDiff = PDPbiasDiff,
                          AP_DiffC = DiffC) %>%
  select(-Observer.y)

# look at correlations between C estimates for WIT
ggplot(pdpBoth, aes(WIT_BlackC, WIT_WhiteC)) +
  geom_point() +
  geom_smooth(method = "lm") + 
#  ggtitle("Correlation between C estimates in WIT") +
  labs(x = "C estimate for Black trials", y = "C estimate for White trials") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        plot.title = element_text(size=20, vjust = 2)
        #axis.text.x  = element_text(angle=90, vjust=0.5, size=16)
  )

lm(WIT_BlackC ~ WIT_WhiteC, data = pdpBoth) %>%
  summary()

# look at correlations between C estimates for AP
ggplot(pdpBoth, aes(AP_BlackC, AP_WhiteC)) +
  geom_point() +
  geom_smooth(method = "lm") + 
#  ggtitle("Correlation between C estimates in AP") +
  labs(x = "C estimate for Black trials", y = "C estimate for White trials") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        plot.title = element_text(size=20, vjust = 2)
        #axis.text.x  = element_text(angle=90, vjust=0.5, size=16)
  )

lm(AP_BlackC ~ AP_WhiteC, data = pdpBoth) %>%
  summary()

# look at correlations between A estimates for WIT
ggplot(pdpBoth, aes(WIT_BlackA, WIT_WhiteA)) +
  geom_point() +
  geom_smooth(method = "lm") + 
#  ggtitle("Correlation between A estimates in WIT") +
  labs(x = "A estimate for Black trials", y = "A estimate for White trials") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        plot.title = element_text(size=20, vjust = 2)
        #axis.text.x  = element_text(angle=90, vjust=0.5, size=16)
  )

lm(WIT_BlackA ~ WIT_WhiteA, data = pdpBoth) %>%
  summary()

# look at correlations between A estimates for AP
ggplot(pdpBoth, aes(AP_BlackA, AP_WhiteA)) +
  geom_point() +
  geom_smooth(method = "lm") + 
#  ggtitle("Correlation between A estimates in AP") +
  labs(x = "A estimate for Black trials", y = "A estimate for White trials") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        plot.title = element_text(size=20, vjust = 2)
        #axis.text.x  = element_text(angle=90, vjust=0.5, size=16)
  )

lm(AP_BlackA ~ AP_WhiteA, data = pdpBoth) %>%
  summary()

# 3. Look at correlations of estimates between two tasks

# look at correlations of estimates between tasks - Black C
ggplot(pdpBoth, aes(WIT_BlackC, AP_BlackC)) +
  geom_point() +
  ggtitle("Black C estimate for WIT/AP") +
  geom_smooth(method = "lm")
lm(WIT_BlackC ~ AP_BlackC, data = pdpBoth) %>%
  summary()

# look at correlations of estimates between tasks - Black A
ggplot(pdpBoth, aes(WIT_BlackA, AP_BlackA)) +
  geom_point() +
  ggtitle("Black A estimate for WIT/AP") +
  geom_smooth(method = "lm")
lm(WIT_BlackA ~ AP_BlackA, data = pdpBoth) %>%
  summary()

# look at correlations of estimates between tasks - White C
ggplot(pdpBoth, aes(WIT_WhiteC, AP_WhiteC)) +
  geom_point() +
  ggtitle("White C estimate for WIT/AP") +
  geom_smooth(method = "lm")
lm(WIT_WhiteC ~ AP_WhiteC, data = pdpBoth) %>%
  summary()

# look at correlations of estimates between tasks - White A
ggplot(pdpBoth, aes(WIT_WhiteA, AP_WhiteA)) +
  geom_point() +
  ggtitle("White A estimate for WIT/AP") +
  geom_smooth(method = "lm")
lm(WIT_WhiteA ~ AP_WhiteA, data = pdpBoth) %>%
  summary()

# look at correlations of estimates between tasks - biasDiff
ggplot(pdpBoth, aes(WIT_biasDiff, AP_biasDiff)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  #  ggtitle("Correlation between C estimates in WIT") +
  labs(x = "PDP bias estimate for WIT", y = "PDP bias estimate for AP") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        plot.title = element_text(size=20, vjust = 2)
        #axis.text.x  = element_text(angle=90, vjust=0.5, size=16)
  )
lm(WIT_biasDiff ~ AP_biasDiff, data = pdpBoth) %>%
  summary()

# look at correlations of estimates between tasks - MeanC
ggplot(pdpBoth, aes(WIT_MeanC, AP_MeanC)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  #  ggtitle("Correlation between C estimates in WIT") +
  labs(x = "PDP control estimate for WIT", y = "PDP control estimate for AP") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
#        plot.background = element_rect(fill = "black"),
        plot.title = element_text(size=20, vjust = 2)
        #axis.text.x  = element_text(angle=90, vjust=0.5, size=16)
  )

lm(WIT_MeanC ~ AP_MeanC, data = pdpBoth) %>%
  summary()

# 4. Separate by observer
# AP
ggplot(longAPsep, aes(PrimeType, value, fill = Estimate)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_wrap(~Observer) + 
  ggtitle("PDP estimates for AP task")
aov(value ~ PrimeType*Estimate*Observer + Error(Subject/(PrimeType*Estimate)), data = longAPsep) %>%
  summary()

# effect of observer on C estimate specifically
aov(value ~ PrimeType*Observer + Error(Subject/(PrimeType)), 
    data = longAPsep[longAPsep$Estimate == "C",]) %>%
  summary()

print(model.tables(aov(value ~ PrimeType*Observer + Error(Subject/(PrimeType)), 
                       data = longAPsep[longAPsep$Estimate == "C",]),
                   "means"),digits=3)

# effect of observer on A estimate specifically
aov(value ~ PrimeType*Observer + Error(Subject/(PrimeType)), 
    data = longAPsep[longAPsep$Estimate == "A",]) %>%
  summary()

# WIT
ggplot(longWITsep, aes(PrimeType, value, fill = Estimate)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_wrap(~Observer) + 
  ggtitle("PDP estimates for WIT task")
aov(value ~ PrimeType*Estimate*Observer + Error(Subject/(PrimeType*Estimate)), data = longWITsep) %>%
  summary()

# effect of observer on C estimate specifically
aov(value ~ PrimeType*Observer + Error(Subject/(PrimeType)), 
    data = longWITsep[longWITsep$Estimate == "C",]) %>%
  summary()

print(model.tables(aov(value ~ PrimeType*Observer + Error(Subject/(PrimeType)), 
                       data = longWITsep[longWITsep$Estimate == "C",]),
                   "means"),digits=3)

# effect of observer on A estimate specifically
aov(value ~ PrimeType*Observer + Error(Subject/(PrimeType)), 
    data = longWITsep[longWITsep$Estimate == "A",]) %>%
  summary()
