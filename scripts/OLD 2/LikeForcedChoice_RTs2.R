	# Set working directory
setwd("~/Desktop/Experiments/CS E-S Perception 1 - like forced choice/Results/R - Raw Totals/Data")

	# Read in libraries
library(lme4)
library(languageR)

	# Read in file and make groups
all_rt = read.table("all_noout.txt", header=T, sep="\t")
all_rt = all_rt[all_rt$Subject!="BE_85",]

all_rt_correct = all_rt[all_rt$Correct=="1",]

all_rt_respcs = all_rt[all_rt$Response=="CS",]
all_rt_respml = all_rt[all_rt$Response=="ML",]

	# LMERs on logarithimic RT data
		# Linear mixed effects model on ALL RT data logarithmic
all_log_rt.lmer = lmer(RT_log ~ relevel(Response, "ML")*relevel(Main_Context, "ML")*relevel(Lg_Heard,"Eng") + (1+Response+Main_Context+Lg_Heard|Subject) + (1|Block), data=all_rt)
summary(all_log_rt.lmer)

		# Linear mixed effects model on correct responses
all_log_rt_correct.lmer = lmer(RT_log ~ relevel(Main_Context, "ML")*relevel(Lg_Heard,"Eng") + (1+Main_Context+Lg_Heard|Subject) + (1|Block), data=all_rt_correct)
summary(all_log_rt_correct.lmer)

		# Linear mixed effects model on monolingual responses
all_log_rt_respml.lmer = lmer(RT_log ~ relevel(Main_Context, "ML")*relevel(Lg_Heard,"Eng") + (1+Main_Context+Lg_Heard|Subject) + (1|Block), data=all_rt_respml)
summary(all_log_rt_respml.lmer)	

		# Linear mixed effects model on code-switching responses
all_log_rt_respcs.lmer = lmer(RT_log ~ relevel(Main_Context, "ML")*relevel(Lg_Heard,"Eng") + (1+Main_Context+Lg_Heard|Subject) + (1|Block), data=all_rt_respcs)
summary(all_log_rt_respcs.lmer)		




