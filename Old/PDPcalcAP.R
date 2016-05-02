# PDP analysis for AP task
require(ggplot2)
require(maggritr)

# 1. Read in errCountLong.txt
dat = read.delim("errCountLong.txt", stringsAsFactors = F)
# create data frame that takes out NA data points for bad subjects
noBS = dat[!is.na(dat$numErr),]
noBS.AP = noBS[noBS$Task == "AP",]

# create dataframe where PDP calculations are going to go for AP
pdpAP = data.frame( "Subject" = unique(noBS.AP$Subject), 
                     "Task" = "AP")
# add observer cond
for (i in unique(pdpAP$Subject)) {
  pdpAP$Observer[pdpAP$Subject == i] = noBS.AP$Observer[noBS.AP$Subject == i &
                                                             noBS.AP$GenType == "black_con"]
}

# 2. Calculate PDP estimates for Black primes
for (i in unique(pdpAP$Subject)) {
  # make columns with #correct trials in each condition
  pdpAP$BNcor[pdpAP$Subject == i] = (48 - noBS.AP$numErr[noBS.AP$Subject == i & 
                                                              noBS.AP$GenType == "black_con"])
  pdpAP$BPcor[pdpAP$Subject == i] = (48 - noBS.AP$numErr[noBS.AP$Subject == i & 
                                                              noBS.AP$GenType == "black_incon"])
  # make columns with #false alarm trials in each condition (committed errors, not timeouts)
  pdpAP$BNfa[pdpAP$Subject == i] = noBS.AP$numComErr[noBS.AP$Subject == i & 
                                                          noBS.AP$GenType == "black_con"]
  pdpAP$BPfa[pdpAP$Subject == i] = noBS.AP$numComErr[noBS.AP$Subject == i & 
                                                          noBS.AP$GenType == "black_incon"]  
  
  # calculate C and A estimates separately for each race prime
  pdpAP$Black_C[pdpAP$Subject == i] = pdpAP$BNcor[pdpAP$Subject == i]/48 - 
    pdpAP$BPfa[pdpAP$Subject == i]/48
  pdpAP$Black_A[pdpAP$Subject == i] = (pdpAP$BPfa[pdpAP$Subject == i]/48)/
    (1-pdpAP$Black_C[pdpAP$Subject == i])
}

# look at black PDP estimates, separated by observer
ggplot(pdpAP, aes(Black_C, Black_A)) +
  geom_point() +
  geom_smooth(method = "lm")
lm(Black_C ~ Black_A, data = pdpAP) %>%
  summary()

ggplot(pdpAP, aes(Black_C, Observer)) +
  geom_point() 

ggplot(pdpAP, aes(Black_A, Observer)) +
  geom_point() 


# 3. Calculate PDP estimates for White primes
for (i in unique(pdpAP$Subject)) {
  # make columns with #correct trials in each condition
  pdpAP$WPcor[pdpAP$Subject == i] = (48 - noBS.AP$numErr[noBS.AP$Subject == i & 
                                                              noBS.AP$GenType == "white_con"])
  pdpAP$WNcor[pdpAP$Subject == i] = (48 - noBS.AP$numErr[noBS.AP$Subject == i & 
                                                              noBS.AP$GenType == "white_incon"])  
  # make columns with #false alarm trials in each condition (committed errors, not timeouts)
  pdpAP$WPfa[pdpAP$Subject == i] = noBS.AP$numComErr[noBS.AP$Subject == i & 
                                                          noBS.AP$GenType == "white_con"]
  pdpAP$WNfa[pdpAP$Subject == i] = noBS.AP$numComErr[noBS.AP$Subject == i & 
                                                          noBS.AP$GenType == "white_incon"]  
  
  # calculate C and A estimates separately for each race prime
  pdpAP$White_C[pdpAP$Subject == i] = pdpAP$WPcor[pdpAP$Subject == i]/48 - 
    pdpAP$WNfa[pdpAP$Subject == i]/48
  pdpAP$White_A[pdpAP$Subject == i] = (pdpAP$WNfa[pdpAP$Subject == i]/48)/
    (1-pdpAP$White_C[pdpAP$Subject == i])
}

# look at white PDP estimates, separated by observer
ggplot(pdpAP, aes(White_C, White_A)) +
  geom_point(aes(color = factor(pdpAP$Observer))) 

ggplot(pdpAP, aes(White_C, Observer)) +
  geom_point() 

ggplot(pdpAP, aes(White_A, Observer)) +
  geom_point() 

# look at how A estimates for White and Black correlate

ggplot(pdpAP, aes(Black_A, White_A)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("A estimates for AP")

lm(White_A ~ Black_A, data = pdpAP) %>%
  summary()

# look at how C estimates for White and Black correlate

ggplot(pdpAP, aes(Black_C, White_C)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  ggtitle("C estimates for AP")

lm(White_C ~ Black_C, data = pdpAP) %>%
  summary()

# 4. Look at means of PDP estimates

# PDP estimates to Black primes
mean(pdpAP$Black_C)

mean(pdpAP$Black_C[pdpAP$Observer == "Present"])
mean(pdpAP$Black_C[pdpAP$Observer == "Absent"])

mean(pdpAP$Black_A)

mean(pdpAP$Black_A[pdpAP$Observer == "Present"])
mean(pdpAP$Black_A[pdpAP$Observer == "Absent"])


# PDP estimates to White primes
mean(pdpAP$White_C)

mean(pdpAP$White_C[pdpAP$Observer == "Present"])
mean(pdpAP$White_C[pdpAP$Observer == "Absent"])

mean(pdpAP$White_A)

mean(pdpAP$White_A[pdpAP$Observer == "Present"])
mean(pdpAP$White_A[pdpAP$Observer == "Absent"])


# 5. Calculate PDP estimates, collapsing across race prime
for (i in unique(pdpAP$Subject)) {
  
  # calculate probability of correct(congruent) and false alarm(incongruent)
  pdpAP$corCon[pdpAP$Subject == i] = (pdpAP$BNcor[pdpAP$Subject == i]
                                        + pdpAP$WPcor[pdpAP$Subject == i])/(48+48)
  pdpAP$faIncon[pdpAP$Subject == i] = (pdpAP$BPfa[pdpAP$Subject == i]
                                         + pdpAP$WNfa[pdpAP$Subject == i])/(48+48)
  
  # calculate C and A estimates
  pdpAP$Total_C[pdpAP$Subject == i] = pdpAP$corCon[pdpAP$Subject == i] - 
    pdpAP$faIncon[pdpAP$Subject == i]
  pdpAP$Total_A[pdpAP$Subject == i] = pdpAP$faIncon[pdpAP$Subject == i]/
    (1-pdpAP$Total_C[pdpAP$Subject == i])
}

# Visualize
ggplot(pdpAP, aes(Total_C, Total_A)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("AP")
lm(Total_C ~ Total_A, data = pdpAP) %>%
  summary()

ggplot(pdpAP, aes(Total_C, Observer)) +
  geom_point() 

ggplot(pdpAP, aes(Total_A, Observer)) +
  geom_point() 



# 6. Look at how C estimate relates to effort question
for (i in unique(pdpAP$Subject)) {
  pdpAP$Effort[pdpAP$Subject == i] = noBS.AP$Effort[noBS.AP$Subject == i &
                                                         noBS.AP$GenType == "black_con"]
  pdpAP$Attend[pdpAP$Subject == i] = noBS.AP$Attend[noBS.AP$Subject == i &
                                                         noBS.AP$GenType == "black_con"]
}

# Look at Effort
# Relationship between C estimate and self-reported effort
ggplot(pdpAP, aes(Total_C, Effort)) +
  geom_point(aes(color = factor(pdpAP$Observer))) +
  geom_smooth(method = "lm")
lm(Total_C ~ Effort, data = pdpAP) %>% summary() # marginally sig relationship between effort and C estimate

# Relationship between C estimate and self-reported effort within Obs Present cond
ggplot(pdpAP[pdpAP$Observer == "Present",], aes(Total_C, Effort)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Observer Present")
lm(Effort ~ Total_C, data = pdpAP[pdpAP$Observer == "Present",]) %>% summary()

# Relationship between C estimate and self-reported effort within Obs Absent cond
ggplot(pdpAP[pdpAP$Observer == "Absent",], aes(Total_C, Effort)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Observer Absent")
lm(Effort ~ Total_C, data = pdpAP[pdpAP$Observer == "Absent",]) %>% summary() # no relationship between effort and C estimate

# Relationship between A estimate and self-reported effort
ggplot(pdpAP, aes(Total_A, Effort)) +
  geom_point(aes(color = factor(pdpAP$Observer))) +
  geom_smooth(method = "lm")
lm(Effort ~ Total_A, data = pdpAP) %>% summary() # no relationship between effort and A estimate

# Relationship between A estimate and self-reported effort within Obs Present cond
ggplot(pdpAP[pdpAP$Observer == "Present",], aes(Total_A, Effort)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Observer Present")
lm(Effort ~ Total_A, data = pdpAP[pdpAP$Observer == "Present",]) %>% summary()

# Relationship between A estimate and self-reported effort within Obs Absent cond
ggplot(pdpAP[pdpAP$Observer == "Absent",], aes(Total_A, Effort)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Observer Absent")
lm(Effort ~ Total_A, data = pdpAP[pdpAP$Observer == "Absent",]) %>% summary() # no relationship between effort and C estimate



# Look at Attend
# Relationship between C estimate and self-reported attention
ggplot(pdpAP, aes(Total_C, Attend)) +
  geom_point(aes(color = factor(pdpAP$Observer))) +
  geom_smooth(method = "lm")
lm(Total_C ~ Attend, data = pdpAP) %>% summary() # SIGNIGICANT relationship between attention and C estimate

# Relationship between C estimate and self-reported attention within Obs Present cond
ggplot(pdpAP[pdpAP$Observer == "Present",], aes(Total_C, Attend)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Observer Present")
lm(Total_C ~ Attend, data = pdpAP[pdpAP$Observer == "Present",]) %>% summary()

# Relationship between C estimate and self-reported attention within Obs Absent cond
ggplot(pdpAP[pdpAP$Observer == "Absent",], aes(Total_C, Attend)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Observer Absent")
lm(Total_C ~ Attend, data = pdpAP[pdpAP$Observer == "Absent",]) %>% summary() # no relationship between effort and C estimate



# Relationship between A estimate and self-reported attention
ggplot(pdpAP, aes(Total_A, Attend)) +
  geom_point(aes(color = factor(pdpAP$Observer))) +
  geom_smooth(method = "lm")
lm(Total_A ~ Attend, data = pdpAP) %>% summary() # no relationship between attention and A estimate

# Relationship between A estimate and self-reported attention within Obs Present cond
ggplot(pdpAP[pdpAP$Observer == "Present",], aes(Total_A, Attend)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Observer Present")
lm(Total_A ~ Attend, data = pdpAP[pdpAP$Observer == "Present",]) %>% summary()

# Relationship between A estimate and self-reported attention within Obs Absent cond
ggplot(pdpAP[pdpAP$Observer == "Absent",], aes(Total_A, Attend)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Observer Absent")
lm(Total_A ~ Attend, data = pdpAP[pdpAP$Observer == "Absent",]) %>% summary() # no relationship between effort and C estimate


########### calculate PDP bias difference score, MeanC ####################
pdpAP = mutate(pdpAP, PDPbiasDiff = Black_A - White_A) %>%
  mutate(MeanC = (Black_C + White_C)/2) %>%
  mutate(MeanA = (Black_A + White_A)/2) %>%
  mutate(DiffC = Black_C - White_C)

hist(pdpAP$PDPbiasDiff, breaks = 30)
hist(pdpAP$MeanC, breaks = 30)
hist(pdpAP$DiffC, breaks = 30)

# 7. Look at how PDPbiasDiff, MeanC, and DiffC relate to effort

# Effort and PDPbiasDiff
ggplot(pdpAP, aes(PDPbiasDiff, Effort)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  #  ggtitle("Correlation between C estimates in WIT") +
#  labs(x = "C estimate for Black trials", y = "C estimate for White trials") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        plot.title = element_text(size=20, vjust = 2)
        #axis.text.x  = element_text(angle=90, vjust=0.5, size=16)
  )

# Effort and MeanC
ggplot(pdpAP, aes(MeanC, Effort)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  #  ggtitle("Correlation between C estimates in WIT") +
  #  labs(x = "C estimate for Black trials", y = "C estimate for White trials") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        plot.title = element_text(size=20, vjust = 2)
        #axis.text.x  = element_text(angle=90, vjust=0.5, size=16)
  )
lm(MeanC ~ Effort, data = pdpAP) %>%
  summary()



# 8. Calculate resid score (White A partialed out of Black A)

model1 = lm(Black_A ~ White_A, data = pdpAP)
pdpAP$AResids = model1$residuals

model2 = lm(Black_C ~ White_C, data = pdpAP)
pdpAP$CResids = model2$residuals

write.table(select(pdpAP, Subject, Task, Observer, Black_C, Black_A, White_C, White_A, MeanC, AResids, CResids), 
            file = "PDPestimatesAPwide.txt", sep = "\t", row.names = F)




# 7. Put into long form. Columns: Subject, PrimeType, PDPestimate
require(tidyr)
require(magrittr)
require(dplyr)

longAP = select(pdpAP, c(Black_C, Black_A, White_C, White_A, MeanC, AResids, CResids, Observer, Subject)) %>%
  gather(Subject, value, 1:7) # Subject is what you organize by, Estimate is new column that you create
# 1:6 selects columns that you want to gather into Estimate column
names(longAP)[3] = "Type"

# add column for race of prime
longAP$PrimeType = NA
longAP$PrimeType[grep("Black", longAP$Type)] = "Black"
longAP$PrimeType[grep("White", longAP$Type)] = "White"

# add column specifying what kind of estimate
longAP$Estimate = NA
longAP$Estimate[grep("C", longAP$Type)] = "C"
longAP$Estimate[grep("A", longAP$Type)] = "A"

# add task
longAP$Task = "AP"

write.table(longAP, file = "PDPestimatesAPlong.txt", sep = "\t", row.names = F)


