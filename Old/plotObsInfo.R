# Take errCountLong.txt from calcRaceBias.R
# Adds observer condition
# Plots separate by observer condition
require(ggplot2)

long = read.delim("errCountLong.txt", stringsAsFactors=F)
# check how many subjects are included
length(unique(long$Subject[long$Task == "WIT" & !is.na(long$numErr)])) # 92 subjects
length(unique(long$Subject[long$Task == "AP" & !is.na(long$numErr)])) # 93 subjects 

# 1. Plot average errors in each trial across conditions (will throw errors about missing data)

# Total number of errors
ggplot(long, aes(PrimeType, numErr, fill = ConType)) +
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
        
# Total number of errors, just in AP
ggplot(long[long$Task == "AP",], aes(PrimeType, numErr, fill = TargetType)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
#  facet_wrap(~Task) + 
#  ggtitle("AP Task") +
  labs(y = "Number of errors", x = "Race of Prime") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        title = element_text(face="bold", size=24),
        axis.text.x  = element_text(vjust=0.5, size=16, color="black"),
        legend.title = element_blank(),
        legend.text = element_text(size=16)
#        strip.text.x = element_text(size = 16, face="bold")
  )

# Total number of errors, just in WIT
ggplot(long[long$Task == "WIT",], aes(PrimeType, numErr, fill = TargetType)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  #  facet_wrap(~Task) + 
#  ggtitle("WIT Task") +
  labs(y = "Number of errors", x = "Race of Prime") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        title = element_text(face="bold", size=24),
        axis.text.x  = element_text(vjust=0.5, size=16, color="black"),
        legend.title = element_blank(),
        legend.text = element_text(size=16)
        #        strip.text.x = element_text(size = 16, face="bold")
  )

# Timeout errors
ggplot(long, aes(PrimeType, numTOErr, fill = ConType)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_wrap(~Task) +
  ggtitle("Timeout errors")

# Committed errors
ggplot(long, aes(PrimeType, numComErr, fill = ConType)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_wrap(~Task) +
  ggtitle("Committed errors")


# 2. Plot again, this time separated by obs condition
# check how many subjects are included
length(unique(long$Subject[long$Task == "WIT" & long$Observer == "Present" & !is.na(long$numErr)])) # 45 subjects
length(unique(long$Subject[long$Task == "WIT" & long$Observer == "Absent" & !is.na(long$numErr)])) # 47 subjects

length(unique(long$Subject[long$Task == "AP" & long$Observer == "Present" & !is.na(long$numErr)])) # 46 subjects 
length(unique(long$Subject[long$Task == "AP" & long$Observer == "Absent" & !is.na(long$numErr)])) # 47 subjects

# To change facet labels
obsLabeller <- function(var, value){
  value <- as.character(value)
  if (var=="Observer") { 
    value[value=="Present"] <- "Observer Present"
    value[value=="Absent"]   <- "Observer Absent"
  }
  return(value)
}

# Total errors
ggplot(long, aes(PrimeType, numErr, fill = ConType)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_wrap(~Task*Observer) + 
  ggtitle("Total number of errors")

# Total number of errors, just in AP, separated by observer
ggplot(long[long$Task == "AP",], aes(PrimeType, numErr, fill = TargetType)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_grid(~Observer, labeller = obsLabeller) + 
  ggtitle("AP Task") +
  labs(y = "Number of errors", x = "Race of Prime") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        title = element_text(face="bold", size=24),
        axis.text.x  = element_text(vjust=0.5, size=16, color="black"),
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        strip.text.x = element_text(size = 16, face="bold")
  )

# Total number of errors, just in WIT, separated by observer
ggplot(long[long$Task == "WIT",], aes(PrimeType, numErr, fill = TargetType)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_grid(~Observer, labeller = obsLabeller) + 
  ggtitle("WIT Task") +
  labs(y = "Number of errors", x = "Race of Prime") +
  theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
        axis.title.y = element_text(face="bold", colour="#990000", size=20),
        title = element_text(face="bold", size=24),
        axis.text.x  = element_text(vjust=0.5, size=16, color="black"),
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        strip.text.x = element_text(size = 16, face="bold")
  )

# Time out errors
ggplot(long, aes(PrimeType, numTOErr, fill = ConType)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_wrap(~Task*Observer) + 
  ggtitle("Timeout errors")

# Committed errors
ggplot(long, aes(PrimeType, numComErr, fill = ConType)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=.9), width = .2) +
  facet_wrap(~Task*Observer) + 
  ggtitle("Committed errors")
