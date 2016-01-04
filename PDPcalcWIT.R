# PDP analysis for WIT task
require(ggplot2)
require(magrittr)

# 1. Read in errCountLong.txt
dat = read.delim("errCountLong.txt", stringsAsFactors = F)
# create data frame that takes out NA data points for bad subjects
noBS = dat[!is.na(dat$numErr),]
noBS.WIT = noBS[noBS$Task == "WIT",]

# create dataframe where PDP calculations are going to go for WIT
pdpWIT = data.frame( "Subject" = unique(noBS.WIT$Subject), 
                    "Task" = "WIT")
# add observer cond
for (i in unique(pdpWIT$Subject)) {
  pdpWIT$Observer[pdpWIT$Subject == i] = noBS.WIT$Observer[noBS.WIT$Subject == i &
                                                          noBS.WIT$GenType == "black_con"]
}

# 2. Calculate PDP estimates for Black primes
for (i in unique(pdpWIT$Subject)) {
  # make columns with #correct trials in each condition
  pdpWIT$BGcor[pdpWIT$Subject == i] = (48 - noBS.WIT$numErr[noBS.WIT$Subject == i & 
                                                           noBS.WIT$GenType == "black_con"])
  pdpWIT$BTcor[pdpWIT$Subject == i] = (48 - noBS.WIT$numErr[noBS.WIT$Subject == i & 
                                                           noBS.WIT$GenType == "black_incon"])
  # make columns with #false alarm trials in each condition (committed errors, not timeouts)
  pdpWIT$BGfa[pdpWIT$Subject == i] = noBS.WIT$numComErr[noBS.WIT$Subject == i & 
                                                       noBS.WIT$GenType == "black_con"]
  pdpWIT$BTfa[pdpWIT$Subject == i] = noBS.WIT$numComErr[noBS.WIT$Subject == i & 
                                                       noBS.WIT$GenType == "black_incon"]  
  
  # calculate C and A estimates separately for each race prime
  pdpWIT$Black_C[pdpWIT$Subject == i] = pdpWIT$BGcor[pdpWIT$Subject == i]/48 - 
    pdpWIT$BTfa[pdpWIT$Subject == i]/48
  pdpWIT$Black_A[pdpWIT$Subject == i] = (pdpWIT$BTfa[pdpWIT$Subject == i]/48)/
    (1-pdpWIT$Black_C[pdpWIT$Subject == i])
}

# look at black PDP estimates, separated by observer
ggplot(pdpWIT, aes(Black_C, Black_A)) +
  geom_point() +
  geom_smooth(method = "lm")
lm(Black_C ~ Black_A, data = pdpWIT) %>%
  summary()

ggplot(pdpWIT, aes(Black_C, Observer)) +
  geom_point() 

ggplot(pdpWIT, aes(Black_A, Observer)) +
  geom_point() 


# 3. Calculate PDP estimates for White primes
for (i in unique(pdpWIT$Subject)) {
  # make columns with #correct trials in each condition
  pdpWIT$WTcor[pdpWIT$Subject == i] = (48 - noBS.WIT$numErr[noBS.WIT$Subject == i & 
                                                           noBS.WIT$GenType == "white_con"])
  pdpWIT$WGcor[pdpWIT$Subject == i] = (48 - noBS.WIT$numErr[noBS.WIT$Subject == i & 
                                                           noBS.WIT$GenType == "white_incon"])  
  # make columns with #false alarm trials in each condition (committed errors, not timeouts)
  pdpWIT$WTfa[pdpWIT$Subject == i] = noBS.WIT$numComErr[noBS.WIT$Subject == i & 
                                                       noBS.WIT$GenType == "white_con"]
  pdpWIT$WGfa[pdpWIT$Subject == i] = noBS.WIT$numComErr[noBS.WIT$Subject == i & 
                                                       noBS.WIT$GenType == "white_incon"]  
  
  # calculate C and A estimates separately for each race prime
  pdpWIT$White_C[pdpWIT$Subject == i] = pdpWIT$WTcor[pdpWIT$Subject == i]/48 - 
    pdpWIT$WGfa[pdpWIT$Subject == i]/48
  pdpWIT$White_A[pdpWIT$Subject == i] = (pdpWIT$WGfa[pdpWIT$Subject == i]/48)/
    (1-pdpWIT$White_C[pdpWIT$Subject == i])
}

# look at white PDP estimates, separated by observer
ggplot(pdpWIT, aes(White_C, White_A)) +
  geom_point(aes(color = factor(pdpWIT$Observer))) 

ggplot(pdpWIT, aes(White_C, Observer)) +
  geom_point() 

ggplot(pdpWIT, aes(White_A, Observer)) +
  geom_point() 

# look at how A estimates for White and Black correlate

ggplot(pdpWIT, aes(Black_A, White_A)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  ggtitle("A estimates for WIT")

lm(White_A ~ Black_A, data = pdpWIT) %>%
  summary()

# look at how C estimates for White and Black correlate

ggplot(pdpWIT, aes(Black_C, White_C)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  ggtitle("C estimates for WIT")

lm(White_C ~ Black_C, data = pdpWIT) %>%
  summary()

# 4. Look at means of PDP estimates

# PDP estimates to Black primes
mean(pdpWIT$Black_C)

mean(pdpWIT$Black_C[pdpWIT$Observer == "Present"])
mean(pdpWIT$Black_C[pdpWIT$Observer == "Absent"])

mean(pdpWIT$Black_A)

mean(pdpWIT$Black_A[pdpWIT$Observer == "Present"])
mean(pdpWIT$Black_A[pdpWIT$Observer == "Absent"])


# PDP estimates to White primes
mean(pdpWIT$White_C)

mean(pdpWIT$White_C[pdpWIT$Observer == "Present"])
mean(pdpWIT$White_C[pdpWIT$Observer == "Absent"])

mean(pdpWIT$White_A)

mean(pdpWIT$White_A[pdpWIT$Observer == "Present"])
mean(pdpWIT$White_A[pdpWIT$Observer == "Absent"])


# 5. Calculate PDP estimates, collapsing across race prime
for (i in unique(pdpWIT$Subject)) {
  
  # calculate probability of correct(congruent) and false alarm(incongruent)
  pdpWIT$corCon[pdpWIT$Subject == i] = (pdpWIT$BGcor[pdpWIT$Subject == i]
                                      + pdpWIT$WTcor[pdpWIT$Subject == i])/(48+48)
  pdpWIT$faIncon[pdpWIT$Subject == i] = (pdpWIT$BTfa[pdpWIT$Subject == i]
                                       + pdpWIT$WGfa[pdpWIT$Subject == i])/(48+48)
  
  # calculate C and A estimates
  pdpWIT$Total_C[pdpWIT$Subject == i] = pdpWIT$corCon[pdpWIT$Subject == i] - 
    pdpWIT$faIncon[pdpWIT$Subject == i]
  pdpWIT$Total_A[pdpWIT$Subject == i] = pdpWIT$faIncon[pdpWIT$Subject == i]/
    (1-pdpWIT$Total_C[pdpWIT$Subject == i])
}

# Visualize
ggplot(pdpWIT, aes(Total_C, Total_A)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("WIT")
lm(Total_C ~ Total_A, data = pdpWIT) %>%
  summary()


ggplot(pdpWIT, aes(Total_C, Observer)) +
  geom_point() 

ggplot(pdpWIT, aes(Total_A, Observer)) +
  geom_point() 



# 6. Look at how C estimate relates to effort question
for (i in unique(pdpWIT$Subject)) {
  pdpWIT$Effort[pdpWIT$Subject == i] = noBS.WIT$Effort[noBS.WIT$Subject == i &
                                                      noBS.WIT$GenType == "black_con"]
  pdpWIT$Attend[pdpWIT$Subject == i] = noBS.WIT$Attend[noBS.WIT$Subject == i &
                                                      noBS.WIT$GenType == "black_con"]
}

# Look at Effort
# Relationship between C estimate and self-reported effort
ggplot(pdpWIT, aes(Total_C, Effort)) +
  geom_point(aes(color = factor(pdpWIT$Observer))) +
  geom_smooth(method = "lm")
lm(Total_C ~ Effort, data = pdpWIT) %>% summary() # marginally sig relationship between effort and C estimate

# Relationship between C estimate and self-reported effort within Obs Present cond
ggplot(pdpWIT[pdpWIT$Observer == "Present",], aes(Total_C, Effort)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Observer Present")
lm(Effort ~ Total_C, data = pdpWIT[pdpWIT$Observer == "Present",]) %>% summary()

# Relationship between C estimate and self-reported effort within Obs Absent cond
ggplot(pdpWIT[pdpWIT$Observer == "Absent",], aes(Total_C, Effort)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Observer Absent")
lm(Effort ~ Total_C, data = pdpWIT[pdpWIT$Observer == "Absent",]) %>% summary()

# Relationship between A estimate and self-reported effort
ggplot(pdpWIT, aes(Total_A, Effort)) +
  geom_point(aes(color = factor(pdpWIT$Observer))) +
  geom_smooth(method = "lm")
lm(Effort ~ Total_A, data = pdpWIT) %>% summary() # no relationship between effort and A estimate

# Relationship between A estimate and self-reported effort within Obs Present cond
ggplot(pdpWIT[pdpWIT$Observer == "Present",], aes(Total_A, Effort)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Observer Present")
lm(Effort ~ Total_A, data = pdpWIT[pdpWIT$Observer == "Present",]) %>% summary()

# Relationship between A estimate and self-reported effort within Obs Absent cond
ggplot(pdpWIT[pdpWIT$Observer == "Absent",], aes(Total_A, Effort)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Observer Absent")
lm(Effort ~ Total_A, data = pdpWIT[pdpWIT$Observer == "Absent",]) %>% summary() # no relationship between effort and C estimate



# Look at Attend
# Relationship between C estimate and self-reported attention
ggplot(pdpWIT, aes(Total_C, Attend)) +
  geom_point(aes(color = factor(pdpWIT$Observer))) +
  geom_smooth(method = "lm")
lm(Total_C ~ Attend, data = pdpWIT) %>% summary() # SIGNIGICANT relationship between attention and C estimate

# Relationship between C estimate and self-reported attention within Obs Present cond
ggplot(pdpWIT[pdpWIT$Observer == "Present",], aes(Total_C, Attend)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Observer Present")
lm(Total_C ~ Attend, data = pdpWIT[pdpWIT$Observer == "Present",]) %>% summary()

# Relationship between C estimate and self-reported attention within Obs Absent cond
ggplot(pdpWIT[pdpWIT$Observer == "Absent",], aes(Total_C, Attend)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Observer Absent")
lm(Total_C ~ Attend, data = pdpWIT[pdpWIT$Observer == "Absent",]) %>% summary() # no relationship between effort and C estimate



# Relationship between A estimate and self-reported attention
ggplot(pdpWIT, aes(Total_A, Attend)) +
  geom_point(aes(color = factor(pdpWIT$Observer))) +
  geom_smooth(method = "lm")
lm(Total_A ~ Attend, data = pdpWIT) %>% summary() # no relationship between attention and A estimate

# Relationship between A estimate and self-reported attention within Obs Present cond
ggplot(pdpWIT[pdpWIT$Observer == "Present",], aes(Total_A, Attend)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Observer Present")
lm(Total_A ~ Attend, data = pdpWIT[pdpWIT$Observer == "Present",]) %>% summary()

# Relationship between A estimate and self-reported attention within Obs Absent cond
ggplot(pdpWIT[pdpWIT$Observer == "Absent",], aes(Total_A, Attend)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Observer Absent")
lm(Total_A ~ Attend, data = pdpWIT[pdpWIT$Observer == "Absent",]) %>% summary() # no relationship between effort and C estimate


# calculate PDP bias difference score
pdpWIT = mutate(pdpWIT, PDPbiasDiff = Black_A - White_A) %>%
  mutate(MeanC = (Black_C + White_C)/2) %>%
  mutate(DiffC = Black_C - White_C)

hist(pdpWIT$PDPbiasDiff, breaks = 30)
hist(pdpWIT$Total_A, breaks = 30)
hist(pdpWIT$MeanC, breaks = 30)

# 8. Calculate resid score (White A partialed out of Black A)

model = lm(Black_A ~ White_A, data = pdpWIT)
pdpWIT$Resids = model$residuals

write.table(select(pdpWIT, Subject, Task, Observer, Black_C, Black_A, White_C, White_A, Total_C, Total_A, PDPbiasDiff, MeanC, Resids), 
            file = "PDPestimatesWITwide.txt", sep = "\t", row.names = F)

# 7. Put into long form. Columns: Subject, PrimeType, PDPestimate
require(tidyr)
require(magrittr)
require(dplyr)

longWIT = select(pdpWIT, c(Black_C, Black_A, White_C, White_A, PDPbiasDiff, Total_C, Total_A, Observer, Subject)) %>%
  gather(Subject, value, 1:7) # Subject is what you organize by, Estimate is new column that you create
# 1:4 selects columns that you want to gather into Estimate column
names(longWIT)[3] = "Type"

# add column for race of prime
longWIT$PrimeType = NA
longWIT$PrimeType[grep("Black", longWIT$Type)] = "Black"
longWIT$PrimeType[grep("White", longWIT$Type)] = "White"

# add column specifying what kind of estimate
longWIT$Estimate = NA
longWIT$Estimate[grep("C", longWIT$Type)] = "C"
longWIT$Estimate[grep("A", longWIT$Type)] = "A"

# add task
longWIT$Task = "WIT"

write.table(longWIT, file = "PDPestimatesWITlong.txt", sep = "\t", row.names = F)


