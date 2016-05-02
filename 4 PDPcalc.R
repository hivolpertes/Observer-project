require(dplyr)
require(tidyr)
require(maggritr)

################ AP #######################

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

# 4. Calculate MeanC, DiffA
pdpAP = mutate(pdpAP, DiffA = Black_A - White_A) %>%
  mutate(MeanC = (Black_C + White_C)/2)

# 5. Calculate resid score (White A partialed out of Black A)
model1 = lm(Black_A ~ White_A, data = pdpAP)
pdpAP$AResid = model1$residuals

write.table(select(pdpAP, Subject, Task, Observer, Black_C, Black_A, White_C, White_A, DiffA, MeanC, AResid), 
            file = "PDPestimatesAPwide.txt", sep = "\t", row.names = F)

# 6. Convert to long form

longAP = select(pdpAP, c(Black_C, Black_A, White_C, White_A, MeanC, AResid, DiffA, Observer, Subject)) %>%
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



################ WIT #######################

# 1. Create data frame that takes out NA data points for bad subjects
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

# 4. Calculate MeanC, DiffA
pdpWIT = mutate(pdpWIT, DiffA = Black_A - White_A) %>%
  mutate(MeanC = (Black_C + White_C)/2)

# 5. Calculate resid score (White A partialed out of Black A)

model1 = lm(Black_A ~ White_A, data = pdpWIT)
pdpWIT$AResid = model1$residuals

write.table(select(pdpWIT, Subject, Task, Observer, Black_C, Black_A, White_C, White_A, DiffA, MeanC, AResid), 
            file = "PDPestimatesWITwide.txt", sep = "\t", row.names = F)

# 6. Put into long form. Columns: Subject, PrimeType, PDPestimate

longWIT = select(pdpWIT, c(Black_C, Black_A, White_C, White_A, MeanC, AResid, DiffA, Observer, Subject)) %>%
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




