	# Set working directory
setwd("~/Desktop/Experiments/CS E-S Perception 1 - like forced choice/Results/R - Raw Totals/Data")

	# Read in libraries
library(lme4)
library(languageR)

	# Read in all text files
all_num 	= read.table("all_num.txt", header=T, sep="\t")
all_num		= all_num[all_num$Subject!="BE_85",]

	# Separate data by hearing English or hearing Spanish
all_num_e 		= all_num	[all_num$Lg_Heard=="Eng",]
all_num_s 		= all_num	[all_num$Lg_Heard=="Sp",]

	# Linear mixed effects models on ALL data together for RESPONSE, no CORRECT
all_num.lmer = lmer(relevel(Response, "ML") ~ relevel(Main_Context, "ML")*relevel(Lg_Heard, "Eng") + (1+Main_Context+Lg_Heard|Subject) + (1|Block), family="binomial", data=all_num)

summary(all_num.lmer)

	# Linear mixed effects models on language specific data for RESPONSE, no CORRECT
all_num_e.lmer = lmer(Response ~ relevel(Main_Context, "ML") + (1+Main_Context|Subject) + (1|Block), family="binomial", data=all_num_e)

all_num_s.lmer = lmer(Response ~ relevel(Main_Context, "ML") + (1+Main_Context|Subject) + (1|Block), family="binomial", data=all_num_s)

summary(all_num_e.lmer)
summary(all_num_s.lmer)




