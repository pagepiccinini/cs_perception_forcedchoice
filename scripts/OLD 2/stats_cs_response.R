	# Set working directory
setwd("~/Desktop/Experiments/CS E-S Perception 1 - like forced choice/Results/R - Raw Totals/Data")

	# READ IN PACKAGES
library(lme4)

	# READ IN DATA AND ORGANIZE
data = read.table("all_num.txt", header=T, sep="\t")
data = data[data$Subject!="BE_85",]
data = data[data$Group!="nonsw",]

data$Response_CS = 0
data$Response_CS[data$Response=="CS"] = data$Response_CS[data$Response=="CS"] + 1

	# MODIFY MAIN EFFECTS
contrasts(data$Lg_Heard) = c(-0.5, 0.5)
data$Lg_HeardContrast <- contrasts(data$Lg_Heard)[,1][as.numeric(data$Lg_Heard)]

contrasts(data$Main_Context) = c(-0.5, 0.5)
data$Main_ContextContrast <- contrasts(data$Main_Context)[,1][as.numeric(data$Main_Context)]

contrasts(data$Block) = c(1, 2, 3, 4)
data$BlockContrast <- contrasts(data$Block)[,1][as.numeric(data$Block)]

	# LMER TO TEST MAIN EFFECTS
data_full.lmer = glmer(Response_CS ~ Lg_HeardContrast * Main_ContextContrast + BlockContrast + (1+Lg_HeardContrast * Main_ContextContrast + BlockContrast|Subject), family="binomial", data=data, REML=F)

	# LMER WITHOUT LANGUAGE
data_nolg.lmer =glmer(Response_CS ~ Lg_HeardContrast * Main_ContextContrast + BlockContrast - Lg_HeardContrast + (1+Lg_HeardContrast * Main_ContextContrast + BlockContrast|Subject), family="binomial", data=data, REML=F)

anova(data_full.lmer, data_nolg.lmer)

	# LMER WITHOUT CONTEXT
data_nocontext.lmer =glmer(Response_CS ~ Lg_HeardContrast * Main_ContextContrast + BlockContrast - Main_ContextContrast + (1+Lg_HeardContrast * Main_ContextContrast + BlockContrast|Subject), family="binomial", data=data, REML=F)

anova(data_full.lmer, data_nocontext.lmer)

	# LMER WITHOUT BLOCK
data_noblock.lmer =glmer(Response_CS ~ Lg_HeardContrast * Main_ContextContrast + BlockContrast - BlockContrast + (1+Lg_HeardContrast * Main_ContextContrast + BlockContrast|Subject), family="binomial", data=data, REML=F)

anova(data_full.lmer, data_noblock.lmer)

	# LMER WITHOUT INTERACTION
data_noint.lmer =glmer(Response_CS ~ Lg_HeardContrast * Main_ContextContrast + BlockContrast - Lg_HeardContrast:Main_ContextContrast + (1+Lg_HeardContrast * Main_ContextContrast + BlockContrast|Subject), family="binomial", data=data, REML=F)

anova(data_full.lmer, data_noint.lmer)

	# FOLLOW UP REGRESSIONS FOR INTERACTION
data_e = subset(data, Lg_Heard=="Eng")
data_s = subset(data, Lg_Heard=="Sp")

data_e_full.lmer = glmer(Response_CS ~ Main_ContextContrast + BlockContrast + (1+Main_ContextContrast + BlockContrast|Subject), family="binomial", data=data_e)

	data_e_nocontext.lmer = glmer(Response_CS ~ Main_ContextContrast + BlockContrast - Main_ContextContrast + (1+Main_ContextContrast + BlockContrast|Subject), family="binomial", data=data_e)
	
	anova(data_e_full.lmer, data_e_nocontext.lmer)

data_s_full.lmer = glmer(Response_CS ~ Main_ContextContrast + BlockContrast + (1+Main_ContextContrast + BlockContrast|Subject), family="binomial", data=data_s)

	data_s_nocontext.lmer = glmer(Response_CS ~ Main_ContextContrast + BlockContrast - Main_ContextContrast + (1+Main_ContextContrast + BlockContrast|Subject), family="binomial", data=data_s)
	
	anova(data_s_full.lmer, data_s_nocontext.lmer)









