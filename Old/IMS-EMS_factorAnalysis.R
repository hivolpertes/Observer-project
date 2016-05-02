require(dplyr)

# Factor analysis of IMS/EMS scores

MTCP = read.delim("MTCPscores.txt")
# includes 5 subjects added after semester was over: 37, 45, 70, 73, 85

# reverse score IMS_1
MTCP$IMS_1.rev = NULL

for (i in unique(MTCP$Subject)) {
  MTCP$IMS_1.rev[MTCP$Subject == i] = 10 - MTCP$IMS_1[MTCP$Subject == i]
}

# take out columns that aren't questionnaire items
dat = select(MTCP, -c(Subject, Citizen, Native_eng, IMS_1))

# take out subjects without complete data
noNA = dat[!is.na(dat),]
noNA = noNA[!is.na(noNA$EMS_4),]
noNA = noNA[!is.na(noNA$IMS_2),]


# Factor analysis
# Maximum Likelihood Factor Analysis
# entering raw data and extracting 2 factors, 
# with varimax rotation 
fit <- factanal(noNA, 2, rotation="varimax", scores="regression")
print(fit, digits=2, cutoff=.001)                                   # confirms that IMS and EMS items load correctly


# for one factor solution
fit2 <- factanal(noNA, 1, rotation="varimax", scores="regression")
print(fit2, digits=2, cutoff=.001)
