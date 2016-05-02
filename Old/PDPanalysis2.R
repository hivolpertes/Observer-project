require(ggplot2)
require(magrittr)
# Look at how PDP estimates correlate with IMS/EMS
# See if that differs across observer condition

# read in wide form data for WIT and AP
wideWIT = read.delim("PDPestimatesWITwide.txt")
wideAP = read.delim("PDPestimatesAPwide.txt")


# 1. WIT Task
# add IMS/EMS scores as columns to wide form of data for WIT
dat = read.delim("ForAnalysis.txt")

for (i in unique(wideWIT$Subject)) {
  wideWIT$IMS[wideWIT$Subject == i] = dat$IMS[dat$Subject == i & dat$SubTrial == 1 & dat$blockName == "WIT"]
  wideWIT$EMS[wideWIT$Subject == i] = dat$EMS[dat$Subject == i & dat$SubTrial == 1 & dat$blockName == "WIT"]
}

# IMS and Black_C in WIT
ggplot(wideWIT, aes(IMS, Black_C)) +
  geom_point() +
  ggtitle("IMS on Black_C in WIT task") +
  geom_smooth(method = "lm")
lm(IMS ~ Black_C, data = wideWIT) %>% 
  summary()

# IMS and White_C in WIT
ggplot(wideWIT, aes(IMS, White_C)) +
  geom_point() +
  ggtitle("IMS on White_C in WIT task") +
  geom_smooth(method = "lm")

# IMS and Black_A in WIT
ggplot(wideWIT, aes(IMS, Black_A)) +
  geom_point() +
  ggtitle("IMS on Black_A in WIT task") +
  geom_smooth(method = "lm")

# IMS and White_A in WIT
ggplot(wideWIT, aes(IMS, White_A)) +
  geom_point() +
  ggtitle("IMS on White_A in WIT task") +
  geom_smooth(method = "lm")
lm(IMS ~ White_A, data = wideWIT) %>%       # marginally significant (p = .08)
  summary()

# IMS and BiasDiff in WIT
ggplot(wideWIT, aes(IMS, PDPbiasDiff)) +
  geom_point() +
  ggtitle("IMS/Bias in WIT task") +
  geom_smooth(method = "lm") +
  labs(x = "IMS score", y = "PDP bias estimate") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=28),
        axis.title.y = element_text(face="bold", colour="#990000", size=28),
        plot.title = element_text(size=28, vjust = 2, color = "white"),
        plot.background = element_rect(fill = "black")
        #axis.text.x  = element_text(angle=90, vjust=0.5, size=16)
  )
lm(IMS ~ PDPbiasDiff, data = wideWIT) %>%
  summary()

# IMS and MeanC in WIT
ggplot(wideWIT, aes(IMS, MeanC)) +
  geom_point() +
  ggtitle("IMS/Control in WIT task") +
  geom_smooth(method = "lm") +
  labs(x = "IMS score", y = "PDP control estimate") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=28),
        axis.title.y = element_text(face="bold", colour="darkgreen", size=28),
        plot.title = element_text(size=28, vjust = 2, color = "white"),
        plot.background = element_rect(fill = "black")
        #axis.text.x  = element_text(angle=90, vjust=0.5, size=16)
  )

lm(IMS ~ MeanC, data = wideWIT) %>%
  summary()



############
# EMS and Black_C in WIT
ggplot(wideWIT, aes(EMS, Black_C)) +
  geom_point() +
  ggtitle("EMS on Black_C in WIT task") +
  geom_smooth(method = "lm")

# EMS and White_C in WIT
ggplot(wideWIT, aes(EMS, White_C)) +
  geom_point() +
  ggtitle("EMS on White_C in WIT task") +
  geom_smooth(method = "lm")

# EMS and Black_A in WIT
ggplot(wideWIT, aes(EMS, Black_A)) +
  geom_point() +
  ggtitle("EMS on Black_A in WIT task") +
  geom_smooth(method = "lm")
lm(EMS ~ Black_A, data = wideWIT) %>%       # marginally significant (p = .055)
  summary()

# EMS and White_A in WIT
ggplot(wideWIT, aes(EMS, White_A)) +
  geom_point() +
  ggtitle("EMS on White_A in WIT task") +
  geom_smooth(method = "lm")
lm(EMS ~ White_A, data = wideWIT) %>%       # marginally significant (p = .07)
  summary()

# EMS and BiasDiff in WIT
ggplot(wideWIT, aes(EMS, PDPbiasDiff)) +
  geom_point() +
  ggtitle("EMS/Bias in WIT task") +
  geom_smooth(method = "lm") +
  labs(x = "EMS score", y = "PDP bias estimate") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=28),
        axis.title.y = element_text(face="bold", colour="#990000", size=28),
        plot.title = element_text(size=28, vjust = 2, color = "white"),
        plot.background = element_rect(fill = "black")
        #axis.text.x  = element_text(angle=90, vjust=0.5, size=16)
  )
lm(EMS ~ PDPbiasDiff, data = wideWIT) %>%
  summary()

# EMS and MeanC in WIT
ggplot(wideWIT, aes(EMS, MeanC)) +
  geom_point() +
  ggtitle("EMS/Control in WIT task") +
  geom_smooth(method = "lm") +
  labs(x = "EMS score", y = "PDP control estimate") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=28),
        axis.title.y = element_text(face="bold", colour="darkgreen", size=28),
        plot.title = element_text(size=28, vjust = 2, color = "white"),
        plot.background = element_rect(fill = "black")
        #axis.text.x  = element_text(angle=90, vjust=0.5, size=16)
  )

lm(EMS ~ MeanC, data = wideWIT) %>%
  summary()


##### Separated by observer condition#################

# IMS and Black_C in WIT separated by observer
ggplot(wideWIT, aes(IMS, Black_C, fill = Observer, col = Observer, pch = Observer)) +
  geom_point() +
  ggtitle("IMS on Black_C in WIT task separated by observer") +
  geom_smooth(method = "lm")

# IMS and White_C in WIT separated by observer
ggplot(wideWIT, aes(IMS, White_C, fill = Observer, col = Observer, pch = Observer)) +
  geom_point() +
  ggtitle("IMS on White_C in WIT task sep by Observer") +
  geom_smooth(method = "lm")
lm(IMS ~ White_C*Observer, data = wideWIT) %>%
  summary()

# IMS and Black_A in WIT separated by observer
ggplot(wideWIT, aes(IMS, Black_A, fill = Observer, col = Observer, pch = Observer)) +
  geom_point() +
  ggtitle("IMS on Black_A in WIT task sep by observer") +
  geom_smooth(method = "lm")
lm(IMS ~ Black_A*Observer, data = wideWIT) %>%              # marginal effect of observer, significant interaction
  summary()

# simple slope
lm(IMS ~ Black_A, data = wideWIT[wideWIT$Observer == "Present",]) %>%
  summary()

# IMS and White_A in WIT separated by observer
ggplot(wideWIT, aes(IMS, White_A, fill = Observer, col = Observer, pch = Observer)) +
  geom_point() +
  ggtitle("IMS on White_A in WIT task sep by observer") +
  geom_smooth(method = "lm")


############
# EMS and Black_C in WIT separated by observer
ggplot(wideWIT, aes(EMS, Black_C, fill = Observer, col = Observer, pch = Observer)) +
  geom_point() +
  ggtitle("EMS on Black_C in WIT task sep. by observer") +
  geom_smooth(method = "lm")
lm(EMS ~ Black_C*Observer, data = wideWIT) %>%
  summary()

# EMS and White_C in WIT separated by observer
ggplot(wideWIT, aes(EMS, White_C, fill = Observer, col = Observer, pch = Observer)) +
  geom_point() +
  ggtitle("EMS on White_C in WIT task sep. by observer") +
  geom_smooth(method = "lm")

# EMS and Black_A in WIT separated by observer
ggplot(wideWIT, aes(EMS, Black_A, fill = Observer, col = Observer, pch = Observer)) +
  geom_point() +
  ggtitle("EMS on Black_A in WIT task sep. by observer") +
  geom_smooth(method = "lm")
lm(EMS ~ Black_C*Observer, data = wideWIT) %>%
  summary()


# EMS and White_A in WIT separated by observer
ggplot(wideWIT, aes(EMS, White_A, fill = Observer, col = Observer, pch = Observer)) +
  geom_point() +
  ggtitle("EMS on White_A in WIT task sep. by observer") +
  geom_smooth(method = "lm")







# 2. AP Task
# add IMS/EMS scores as columns to wide form of data for AP

for (i in unique(wideAP$Subject)) {
  wideAP$IMS[wideAP$Subject == i] = dat$IMS[dat$Subject == i & dat$SubTrial == 1 & dat$blockName == "AP"]
  wideAP$EMS[wideAP$Subject == i] = dat$EMS[dat$Subject == i & dat$SubTrial == 1 & dat$blockName == "AP"]
}

# IMS and Black_C in AP
ggplot(wideAP, aes(IMS, Black_C)) +
  geom_point() +
  ggtitle("IMS on Black_C in AP task") +
  geom_smooth(method = "lm")
lm(IMS ~ Black_C, data = wideAP) %>% 
  summary()

# IMS and White_C in AP
ggplot(wideAP, aes(IMS, White_C)) +
  geom_point() +
  ggtitle("IMS on White_C in AP task") +
  geom_smooth(method = "lm")
lm(IMS ~ White_C, data = wideAP) %>% 
  summary()

# IMS and Black_A in AP
ggplot(wideAP, aes(IMS, Black_A)) +
  geom_point() +
  ggtitle("IMS on Black_A in AP task") +
  geom_smooth(method = "lm")
lm(IMS ~ Black_A, data = wideAP) %>% 
  summary()

# IMS and White_A in AP
ggplot(wideAP, aes(IMS, White_A)) +
  geom_point() +
  ggtitle("IMS on White_A in AP task") +
  geom_smooth(method = "lm")
lm(IMS ~ White_A, data = wideAP) %>%      
  summary()

# IMS and BiasDiff in AP
ggplot(wideAP, aes(IMS, PDPbiasDiff)) +
  geom_point() +
  ggtitle("IMS on BiasDiff in AP task") +
  geom_smooth(method = "lm")
lm(IMS ~ PDPbiasDiff, data = wideAP) %>%
  summary()

# IMS and MeanC in AP
ggplot(wideAP, aes(IMS, MeanC)) +
  geom_point() +
  ggtitle("IMS on MeanC in AP task") +
  geom_smooth(method = "lm")
lm(IMS ~ MeanC, data = wideAP) %>%
  summary()

############
# EMS and Black_C in AP
ggplot(wideAP, aes(EMS, Black_C)) +
  geom_point() +
  ggtitle("EMS on Black_C in AP task") +
  geom_smooth(method = "lm")
lm(EMS ~ Black_A, data = wideAP) %>%
  summary()

# EMS and White_C in AP
ggplot(wideAP, aes(EMS, White_C)) +
  geom_point() +
  ggtitle("EMS on White_C in AP task") +
  geom_smooth(method = "lm")

# EMS and Black_A in AP
ggplot(wideAP, aes(EMS, Black_A)) +
  geom_point() +
  ggtitle("EMS on Black_A in AP task") +
  geom_smooth(method = "lm")
lm(EMS ~ Black_A, data = wideAP) %>%       
  summary()

# EMS and White_A in AP
ggplot(wideAP, aes(EMS, White_A)) +
  geom_point() +
  ggtitle("EMS on White_A in AP task") +
  geom_smooth(method = "lm")
lm(EMS ~ White_A, data = wideAP) %>%       
  summary()

# EMS and BiasDiff in AP
ggplot(wideAP, aes(EMS, PDPbiasDiff)) +
  geom_point() +
  ggtitle("EMS on PDPbiasDiff in AP task") +
  geom_smooth(method = "lm")
lm(EMS ~ PDPbiasDiff, data = wideAP) %>%       
  summary()

# EMS and MeanC in AP
ggplot(wideAP, aes(EMS, MeanC)) +
  geom_point() +
  ggtitle("EMS on MeanC in AP task") +
  geom_smooth(method = "lm")
lm(EMS ~ MeanC, data = wideAP) %>%       
  summary()

##### Separated by observer condition#################

# IMS and Black_C in AP separated by observer
ggplot(wideAP, aes(IMS, Black_C, fill = Observer, col = Observer, pch = Observer)) +
  geom_point() +
  ggtitle("IMS on Black_C in AP task separated by observer") +
  geom_smooth(method = "lm")
lm(IMS ~ Black_C*Observer, data = wideAP) %>%       
  summary()

# IMS and White_C in AP separated by observer
ggplot(wideAP, aes(IMS, White_C, fill = Observer, col = Observer, pch = Observer)) +
  geom_point() +
  ggtitle("IMS on White_C in AP task sep by Observer") +
  geom_smooth(method = "lm")
lm(IMS ~ White_C*Observer, data = wideAP) %>%
  summary()

# IMS and Black_A in AP separated by observer
ggplot(wideAP, aes(IMS, Black_A, fill = Observer, col = Observer, pch = Observer)) +
  geom_point() +
  ggtitle("IMS on Black_A in AP task sep by observer") +
  geom_smooth(method = "lm")
lm(IMS ~ Black_A*Observer, data = wideAP) %>%              # marginal effect of observer, non significant interaction
  summary()

# simple slope
lm(IMS ~ Black_A, data = wideAP[wideAP$Observer == "Present",]) %>%
  summary()

# IMS and White_A in AP separated by observer
ggplot(wideAP, aes(IMS, White_A, fill = Observer, col = Observer, pch = Observer)) +
  geom_point() +
  ggtitle("IMS on White_A in AP task sep by observer") +
  geom_smooth(method = "lm")


############
# EMS and Black_C in AP separated by observer
ggplot(wideAP, aes(EMS, Black_C, fill = Observer, col = Observer, pch = Observer)) +
  geom_point() +
  ggtitle("EMS on Black_C in AP task sep. by observer") +
  geom_smooth(method = "lm")
lm(EMS ~ Black_C*Observer, data = wideAP) %>%
  summary()

# EMS and White_C in AP separated by observer
ggplot(wideAP, aes(EMS, White_C, fill = Observer, col = Observer, pch = Observer)) +
  geom_point() +
  ggtitle("EMS on White_C in AP task sep. by observer") +
  geom_smooth(method = "lm")

# EMS and Black_A in AP separated by observer
ggplot(wideAP, aes(EMS, Black_A, fill = Observer, col = Observer, pch = Observer)) +
  geom_point() +
  ggtitle("EMS on Black_A in AP task sep. by observer") +
  geom_smooth(method = "lm")
lm(EMS ~ Black_C*Observer, data = wideAP) %>%
  summary()

# EMS and White_A in AP separated by observer
ggplot(wideAP, aes(EMS, White_A, fill = Observer, col = Observer, pch = Observer)) +
  geom_point() +
  ggtitle("EMS on White_A in AP task sep. by observer") +
  geom_smooth(method = "lm")


#########################################################################################################
############################# GRAPHS OF INTEREST: Black_A/Black_C X IMS/EMS X OBSERVER ################
#########################################################################################################
# Figures in QuestionsforBruce.pptx

# IMS and Black_A in WIT separated by observer
ggplot(wideWIT, aes(IMS, Black_A, fill = Observer, col = Observer, pch = Observer)) +
  geom_point() +
  ggtitle("IMS/Black_A in WIT task") +
  geom_smooth(method = "lm") +
  labs(y = "Black A estimate") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=28),
        axis.title.y = element_text(face="bold", colour="#990000", size=28),
        plot.title = element_text(size=28, vjust = 2, color = "white"),
        plot.background = element_rect(fill = "black"),
        axis.text.x  = element_text(vjust=0.5, size=16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16)
  )

lm(IMS ~ Black_A*Observer, data = wideWIT) %>%
  summary()

# simple slopes
lm(IMS ~ Black_A, data = wideWIT[wideWIT$Observer == "Present",]) %>%
  summary()
lm(IMS ~ Black_A, data = wideWIT[wideWIT$Observer == "Absent",]) %>%
  summary()

# IMS and Black_C in WIT separated by observer
ggplot(wideWIT, aes(IMS, Black_C, fill = Observer, col = Observer, pch = Observer)) +
  geom_point() +
  ggtitle("IMS/Black_C in WIT task") +
  geom_smooth(method = "lm") +
  labs(y = "Black C estimate") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=28),
        axis.title.y = element_text(face="bold", colour="darkgreen", size=28),
        plot.title = element_text(size=28, vjust = 2, color = "white"),
        plot.background = element_rect(fill = "black"),
        axis.text.x  = element_text(vjust=0.5, size=16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16)
  )
lm(IMS ~ Black_C*Observer, data = wideWIT) %>%
  summary()

# IMS and Black_A in AP separated by observer
ggplot(wideAP, aes(IMS, Black_A, fill = Observer, col = Observer, pch = Observer)) +
  geom_point() +
  ggtitle("IMS/Black_A in AP task") +
  geom_smooth(method = "lm") +
  labs(y = "Black A estimate") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=28),
        axis.title.y = element_text(face="bold", colour="#990000", size=28),
        plot.title = element_text(size=28, vjust = 2, color = "white"),
        plot.background = element_rect(fill = "black"),
        axis.text.x  = element_text(vjust=0.5, size=16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16)
  )

lm(IMS ~ Black_A*Observer, data = wideAP) %>%
  summary()

# simple slopes
lm(IMS ~ Black_A, data = wideAP[wideAP$Observer == "Present",]) %>%
  summary()
lm(IMS ~ Black_A, data = wideAP[wideAP$Observer == "Absent",]) %>%
  summary()

# IMS and Black_C in AP separated by observer
ggplot(wideAP, aes(IMS, Black_C, fill = Observer, col = Observer, pch = Observer)) +
  geom_point() +
  ggtitle("IMS/Black_C in AP task") +
  geom_smooth(method = "lm") +
  labs(y = "Black C estimate") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=28),
        axis.title.y = element_text(face="bold", colour="darkgreen", size=28),
        plot.title = element_text(size=28, vjust = 2, color = "white"),
        plot.background = element_rect(fill = "black"),
        axis.text.x  = element_text(vjust=0.5, size=16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16)
  )
lm(IMS ~ Black_C*Observer, data = wideAP) %>%
  summary()

############################################################

# EMS and Black_A in WIT separated by observer
ggplot(wideWIT, aes(EMS, Black_A, fill = Observer, col = Observer, pch = Observer)) +
  geom_point() +
  ggtitle("EMS on Black_A in WIT task") +
  geom_smooth(method = "lm") +
  labs(y = "Black A estimate") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=28),
        axis.title.y = element_text(face="bold", colour="#990000", size=28),
        plot.title = element_text(size=28, vjust = 2, color = "white"),
        plot.background = element_rect(fill = "black"),
        axis.text.x  = element_text(vjust=0.5, size=16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16)
  )
lm(EMS ~ Black_A*Observer, data = wideWIT) %>%
  summary()

# EMS and Black_C in WIT separated by observer
ggplot(wideWIT, aes(EMS, Black_C, fill = Observer, col = Observer, pch = Observer)) +
  geom_point() +
  ggtitle("EMS/Black_C in WIT task") +
  geom_smooth(method = "lm") +
  labs(y = "Black C estimate") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=28),
        axis.title.y = element_text(face="bold", colour="darkgreen", size=28),
        plot.title = element_text(size=28, vjust = 2, color = "white"),
        plot.background = element_rect(fill = "black"),
        axis.text.x  = element_text(vjust=0.5, size=16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16)
  )
lm(EMS ~ Black_C*Observer, data = wideWIT) %>%
  summary()


# EMS and Black_A in AP separated by observer
ggplot(wideAP, aes(EMS, Black_A, fill = Observer, col = Observer, pch = Observer)) +
  geom_point() +
  ggtitle("EMS on Black_A in AP task") +
  geom_smooth(method = "lm") +
  labs(y = "Black A estimate") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=28),
        axis.title.y = element_text(face="bold", colour="#990000", size=28),
        plot.title = element_text(size=28, vjust = 2, color = "white"),
        plot.background = element_rect(fill = "black"),
        axis.text.x  = element_text(vjust=0.5, size=16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16)
  )
lm(EMS ~ Black_A*Observer, data = wideAP) %>%
  summary()

# EMS and Black_C in AP separated by observer
ggplot(wideAP, aes(EMS, Black_C, fill = Observer, col = Observer, pch = Observer)) +
  geom_point() +
  ggtitle("EMS/Black_C in AP task") +
  geom_smooth(method = "lm") +
  labs(y = "Black C estimate") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=28),
        axis.title.y = element_text(face="bold", colour="darkgreen", size=28),
        plot.title = element_text(size=28, vjust = 2, color = "white"),
        plot.background = element_rect(fill = "black"),
        axis.text.x  = element_text(vjust=0.5, size=16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16)
  )
lm(EMS ~ Black_C*Observer, data = wideAP) %>%
  summary()

##########################################################################




