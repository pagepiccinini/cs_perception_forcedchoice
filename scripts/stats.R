## SET WORKING DIRECTORY
setwd("~/Desktop/Experiments/CS E-S Perception 1 - like forced choice/Results/R - Raw Totals/Data")


## READ IN LIBRARIES
library(lme4)


## READ IN DATA AND ORGANIZE
data = read.table("all_num.txt", header=T, sep="\t")

supsw = subset(data, Group=="supsw")
	supsw$Group = factor(supsw$Group)
	supsw$Subject = factor(supsw$Subject)
	

## ORGANIZE FOR GLMER
	# Language heard
contrasts(supsw$Lg_Heard) = c(-0.5, 0.5) # E = -0.5, S = 0.5
supsw$Lg_HeardContrast <- contrasts(supsw$Lg_Heard)[,1][as.numeric(supsw$Lg_Heard)]

	# Context
contrasts(supsw$Main_Context) = c(0.5, -0.5) # ML = -0.5, CS = 0.5
supsw$Main_ContextContrast <- contrasts(supsw$Main_Context)[,1][as.numeric(supsw$Main_Context)]

	# Block
supsw$Block_Num = as.numeric(supsw$Block)

	# Response
contrasts(supsw$Response) = c(1, 0) # ML = 0, CS = 1
supsw$ResponseContrast <- contrasts(supsw$Response)[,1][as.numeric(supsw$Response)]


## RUN GLMERS FOR ANALYSIS #1
	# Full model
supsw_full.glmer = glmer(Correct ~ Lg_HeardContrast * Main_ContextContrast + (0+Lg_HeardContrast+Main_ContextContrast|Subject) + (1|Subject) + (1|Stimuli), family="binomial", data=supsw)

summary(supsw_full.glmer)

	# Test for language effect
supsw_nolg.glmer = glmer(Correct ~ Lg_HeardContrast * Main_ContextContrast - Lg_HeardContrast + (0+Lg_HeardContrast+Main_ContextContrast|Subject) + (1|Subject) + (1|Stimuli), family="binomial", data=supsw)

anova(supsw_full.glmer, supsw_nolg.glmer)

	# Test for context effect
supsw_nocontext.glmer = glmer(Correct ~ Lg_HeardContrast * Main_ContextContrast - Main_ContextContrast + (0+Lg_HeardContrast+Main_ContextContrast|Subject) + (1|Subject) + (1|Stimuli), family="binomial", data=supsw)

anova(supsw_full.glmer, supsw_nocontext.glmer)

	# Test for interaction of language and context
supsw_nolgxcontext.glmer = glmer(Correct ~ Lg_HeardContrast * Main_ContextContrast - Lg_HeardContrast:Main_ContextContrast + (0+Lg_HeardContrast+Main_ContextContrast|Subject) + (1|Subject) + (1|Stimuli), family="binomial", data=supsw)

anova(supsw_full.glmer, supsw_nolgxcontext.glmer)
	

## RUN GLMERS FOR ANALYSIS #2
	# Full model
supsw_resp_full.glmer = glmer(ResponseContrast ~ Lg_HeardContrast * Main_ContextContrast + (0+Lg_HeardContrast+Main_ContextContrast|Subject) + (1|Subject) + (1|Stimuli), family="binomial", data=supsw)

summary(supsw_resp_full.glmer)

	# Test for language effect
supsw_resp_nolg.glmer = glmer(ResponseContrast ~ Lg_HeardContrast * Main_ContextContrast - Lg_HeardContrast + (0+Lg_HeardContrast+Main_ContextContrast|Subject) + (1|Subject) + (1|Stimuli), family="binomial", data=supsw)

anova(supsw_resp_full.glmer, supsw_resp_nolg.glmer)

	# Test for context effect
supsw_resp_nocontext.glmer = glmer(ResponseContrast ~ Lg_HeardContrast * Main_ContextContrast - Main_ContextContrast + (0+Lg_HeardContrast+Main_ContextContrast|Subject) + (1|Subject) + (1|Stimuli), family="binomial", data=supsw)

anova(supsw_resp_full.glmer, supsw_resp_nocontext.glmer)

	# Test for interaction of language and context
supsw_resp_nolgxcontext.glmer = glmer(ResponseContrast ~ Lg_HeardContrast * Main_ContextContrast - Lg_HeardContrast:Main_ContextContrast + (0+Lg_HeardContrast+Main_ContextContrast|Subject) + (1|Subject) + (1|Stimuli), family="binomial", data=supsw)

anova(supsw_resp_full.glmer, supsw_resp_nolgxcontext.glmer)

	# Follow-up regressions for interaction
supsw_eng = subset(supsw, Lg_Heard=="Eng")
	supsw_eng$Lg_Heard = factor(supsw_eng$Lg_Heard)
supsw_sp = subset(supsw, Lg_Heard=="Sp")
	supsw_sp$Lg_Heard = factor(supsw_sp$Lg_Heard)
	
summary(glm(ResponseContrast ~ Main_Context, family="binomial", data=supsw_eng))
summary(glm(ResponseContrast ~ Main_Context, family="binomial", data=supsw_sp))
	











