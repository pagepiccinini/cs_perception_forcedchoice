	# Set working directory
setwd("~/Desktop/Experiments/CS E-S Perception 1 - like forced choice/Results/R - Raw Totals/Data")

	# Read in libraries
library(lme4)
library(languageR)

	# Read in file and make groups
all_rt = read.table("all_noout.txt", header=T, sep="\t")
nonsw_rt = all_rt[all_rt$Group == "nonsw",]
sw_rt = all_rt[all_rt$Group == "sw",]
supsw_rt = all_rt[all_rt$Group == "supsw",]

	# Separate data by hearing English or hearing Spanish
all_rt_e 	= all_rt	[all_rt$Lg_Heard=="Eng",]
all_rt_s 	= all_rt	[all_rt$Lg_Heard=="Sp",]

nonsw_rt_e 	= nonsw_rt	[nonsw_rt$Lg_Heard=="Eng",]
nonsw_rt_s 	= nonsw_rt	[nonsw_rt$Lg_Heard=="Sp",]

sw_rt_e 	= sw_rt		[sw_rt$Lg_Heard=="Eng",]
sw_rt_s 	= sw_rt		[sw_rt$Lg_Heard=="Sp",]

supsw_rt_e 	= supsw_rt	[supsw_rt$Lg_Heard=="Eng",]
supsw_rt_s 	= supsw_rt	[supsw_rt$Lg_Heard=="Sp",]

	# LMERs on logarithimic RT data
		# Linear mixed effects model on ALL RT data logarithmic
all_log_rt.lmer = lmer(RT_log ~ relevel(Response, "ML")*relevel(Main_Context, "ML")*Lg_Heard*relevel(Group, "nonsw") + (1|Subject) + (1|Block), data=all_rt)
all_log_rt.pvals = pvals.fnc(all_log_rt.lmer)
all_log_rt.lmer
all_log_rt.pvals

nonsw_log_rt.lmer = lmer(RT_log ~ relevel(Response, ref="ML")*relevel(Main_Context, ref="ML")*Lg_Heard + (1|Subject) + (1|Block), data=nonsw_rt)
nonsw_log_rt.pvals = pvals.fnc(nonsw_log_rt.lmer)
nonsw_log_rt.lmer
nonsw_log_rt.pvals

sw_log_rt.lmer = lmer(RT_log ~ relevel(Response, ref="ML")*relevel(Main_Context, ref="ML")*Lg_Heard + (1|Subject) + (1|Block), data=sw_rt)
sw_log_rt.pvals = pvals.fnc(sw_log_rt.lmer)
sw_log_rt.lmer
sw_log_rt.pvals

supsw_log_rt.lmer = lmer(RT_log ~ relevel(Response, ref="ML")*relevel(Main_Context, ref="ML")*Lg_Heard + (1|Subject) + (1|Block), data=supsw_rt)
supsw_log_rt.pvals = pvals.fnc(supsw_log_rt.lmer)
supsw_log_rt.lmer
supsw_log_rt.pvals


