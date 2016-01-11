	# Set working directory
setwd("~/Desktop/Experiments/CS E-S Perception 1 - like forced choice/Results/R - Raw Totals/Data")

	# Read in libraries
library(lme4)
library(languageR)

	# Read in all text files
all_num 	= read.table("all_num.txt", header=T, sep="\t")
nonsw_num 	= all_num[all_num$Group == "nonsw",]
sw_num		= all_num[all_num$Group == "sw",]
supsw_num	= all_num[all_num$Group == "supsw",]

	# Separate data by hearing English or hearing Spanish
all_num_e 		= all_num	[all_num$Lg_Heard=="Eng",]
all_num_s 		= all_num	[all_num$Lg_Heard=="Sp",]

nonsw_num_e 	= nonsw_num	[nonsw_num$Lg_Heard=="Eng",]
nonsw_num_s 	= nonsw_num	[nonsw_num$Lg_Heard=="Sp",]

sw_num_e 		= sw_num	[sw_num$Lg_Heard=="Eng",]
sw_num_s 		= sw_num	[sw_num$Lg_Heard=="Sp",]

supsw_num_e 	= supsw_num	[supsw_num$Lg_Heard=="Eng",]
supsw_num_s 	= supsw_num	[supsw_num$Lg_Heard=="Sp",]

	# Linear mixed effects models on ALL data together for RESPONSE, no CORRECT
all_num.lmer = lmer(Response ~ relevel(Main_Context, "ML")*relevel(Lg_Heard, "Eng")*relevel(Block, "Block1") + (Main_Context+Lg_Heard+Block|Subject), family="binomial", data=all_num)
summary(all_num.lmer)

nonsw_num.lmer = lmer(Response ~ relevel(Main_Context, "ML")*relevel(Lg_Heard, "Eng")*relevel(Block, "Block1") + (1|Subject), family="binomial", data=nonsw_num)
summary(nonsw_num.lmer)

sw_num.lmer = lmer(Response ~ relevel(Main_Context, "ML")*relevel(Lg_Heard, "Eng")*relevel(Block, "Block1") + (1|Subject), family="binomial", data=sw_num)
summary(sw_num.lmer)

supsw_num.lmer = lmer(Response ~ relevel(Main_Context, "ML")*relevel(Lg_Heard, "Eng")*relevel(Block, "Block1") + (1|Subject), family="binomial", data=supsw_num)
summary(supsw_num.lmer)

	# Linear mixed effects models on language specific data for RESPONSE, no CORRECT
all_num_e.lmer = lmer(Response ~ relevel(Main_Context, "ML")*relevel(Block, "Block1") + (1|Subject), family="binomial", data=all_num_e)
all_num_s.lmer = lmer(Response ~ relevel(Main_Context, "ML")*relevel(Block, "Block1") + (1|Subject), family="binomial", data=all_num_s)
summary(all_num_e.lmer)
summary(all_num_s.lmer)

nonsw_num_e.lmer = lmer(Response ~ relevel(Main_Context, "ML")*relevel(Block, "Block1") + (1|Subject), family="binomial", data=nonsw_num_e)
nonsw_num_s.lmer = lmer(Response ~ relevel(Main_Context, "ML")*relevel(Block, "Block1") + (1|Subject), family="binomial", data=nonsw_num_s)
summary(nonsw_num_e.lmer)
summary(nonsw_num_s.lmer)

sw_num_e.lmer = lmer(Response ~ relevel(Main_Context, "ML")*relevel(Block, "Block1") + (1|Subject), family="binomial", data=sw_num_e)
sw_num_s.lmer = lmer(Response ~ relevel(Main_Context, "ML")*relevel(Block, "Block1") + (1|Subject), family="binomial", data=sw_num_s)
summary(sw_num_e.lmer)
summary(sw_num_s.lmer)

supsw_num_e.lmer = lmer(Response ~ relevel(Main_Context, "ML")*relevel(Block, "Block1") + (1|Subject), family="binomial", data=supsw_num_e)
supsw_num_s.lmer = lmer(Response ~ relevel(Main_Context, "ML")*relevel(Block, "Block1") + (1|Subject), family="binomial", data=supsw_num_s)
summary(supsw_num_e.lmer)
summary(supsw_num_s.lmer)



