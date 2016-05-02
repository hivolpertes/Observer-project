require(lme4)
require(magrittr)
require(car)
require(arm)

# Read in file prepped for HLM (long form: each trial is a row, each column is a variable)
dat = read.delim("ForHLManalysis.txt")

# General form:
# lmer(y ~ c + (1|j) + (c|i), data = dat)
# i = participants
# j = stimuli
# participants are crossed with condition (within Ss)
# stimuli are not crossed with condition

# 1. Examine priming effect on RTs

# Looking at Race x Valence interaction 

# With HLM, each subject allowed to have own intercept
model1 = lmer(TargetAP.ACC ~ PrimeType + TargetType + PrimeType*TargetType + (1|Subject), data=dat) 
summary(model1)
display(model1) # gives AIC but less info in general
Anova(model1, type = 3) #Calculates type-II or type-III analysis-of-variance tables for HLM model

# compare to null model?
modelnull = lmer(TargetAP.ACC ~ 1 + (1|Subject), data=dat)
summary(modelnull)

AIC(model1) # Doesn't match with what's given in anova
AIC(modelnull)
anova(model1, modelnull) # gives AIC for each model- doesn't match display() or AIC() though. Also gives p value but not sure if to trust



# With linear regression, not allowing intercept to vary
lm(TargetAP.ACC ~ PrimeType + TargetType + PrimeType*TargetType, data=dat) %>%
  summary()

# With ANOVA, specifying Race and Valence as within Ss variables
aov(TargetAP.ACC ~ (PrimeType*TargetType)+Error(Subject/(PrimeType*TargetType)), data = dat) %>% 
  summary()


lmer(TargetAP.RT ~ Congruence + (1|Subject), data=dat) %>%
  summary()

lm(TargetAP.RT ~ Congruence, data=dat) %>%
  summary()




# 1. Looking at the effect of Observer condition on RT
# AP
fm.null.AP = lmer(TargetAP.RT ~ 1 + (1|Subject), dat[dat$blockName == "AP",])
fm1.AP = lmer(TargetAP.RT ~ Observer + (1|Subject), dat[dat$blockName == "AP",])

anova(fm.null.AP, fm1.AP)

# WIT
fm.null.WIT = lmer(TargetWIT.RT ~ 1 + (1|Subject), dat[dat$blockName == "WIT",])
fm1.WIT = lmer(TargetWIT.RT ~ Observer + (1|Subject), dat[dat$blockName == "WIT",])

anova(fm.null.WIT, fm1.WIT)

# No obeserver effect on RT in AP or WIT


# 2. Looking at the effect of Observer condition on accuracy
# AP
fm.null.AP = lmer(TargetAP.ACC ~ 1 + (1|Subject), dat[dat$blockName == "AP",])
fm2.AP = lmer(TargetAP.ACC ~ Observer + (1|Subject), dat[dat$blockName == "AP",])

anova(fm.null.AP, fm2.AP)

# WIT
fm.null.WIT = lmer(TargetWIT.ACC ~ 1 + (1|Subject), dat[dat$blockName == "WIT",])
fm2.WIT = lmer(TargetWIT.ACC ~ Observer + (1|Subject), dat[dat$blockName == "WIT",])

anova(fm.null.WIT, fm2.WIT)


# 3. Looking at the effect of attention on accuracy
fm.null = lmer(TargetAP.ACC ~ 1 + (1|Subject), dat[dat$blockName == "AP",])
fm2 = lmer(TargetAP.ACC ~ Observer + (1|Subject), dat[dat$blockName == "AP",])

anova(fm.null, fm2)
# No observer effect on accuracy?