#Bring in dataset "ind_month_feeding_proportionoftime"
malefemale<-read.csv(file.choose(), header = T)
head(malefemale)
names(malefemale)

attach(malefemale)
detach(malefemale)


###if want to check for outliers use other eulemur mixed model script not this one

###Run one or the other, can't run both bc lsmeans will be overridden? to just run the models can use lme4
###but to get p-values need to use lmerTest
library(lme4)
library(lmerTest)

names(malefemale)

###full lmm for ripe fruit
mf1<-lmer(logtransformed~ FemaleStatus + NumberOfFemales + (1|FemaleID) + (1|MaleID) + (1|Group) + (1|MonthYR) , data = malefemale, REML= F)
summary(mf1)
qqnorm(resid(mf1))

plot(Proportion~FemaleStatus, data = malefemale)
plot(Proportion~MaleStatus, data = malefemale)

boxplot(logtransformed~MaleStatus*FemaleStatus, data = malefemale)


 mf_null<-lmer(logtransformed~(1|FemaleID)+ (1|MaleID) + (1|Group) + (1|MonthYR) , data = malefemale, REML= F)
summary(mf_null)


#and reduced model using lmertest with p-values
a<-step(mf1)
a
anova(mf1, mf_null)
summary(mf1)


