## SET WORKING DIRECTORY
setwd("~/Desktop/Experiments/CS E-S Perception 1 - like forced choice/Results/R - Raw Totals/Data")


## READ IN LIBRARIES
library(lme4)


## READ IN DATA AND ORGANIZE
data = read.table("all_num.txt", header=T, sep="\t")

supsw = subset(data, Group=="supsw" | Group=="sw")
	supsw$Group = factor(supsw$Group)
supsw = subset(supsw, Subject!="BE_85")
	supsw$Subject = factor(supsw$Subject)


## DO D-PRIME ANALYSIS
supsw_hit = supsw %>%
	filter(Main_Context=="CS") %>%
	group_by(Subject, Lg_Heard) %>%
	summarize(hit = mean(Correct)) %>%
	ungroup()
	
supsw_fa = supsw %>%
	filter(Main_Context=="ML") %>%
	group_by(Subject, Lg_Heard) %>%
	summarize(fa = 1 - mean(Correct)) %>%
	ungroup()
	
supsw_dprime = inner_join(supsw_hit, supsw_fa) %>%
	mutate(dprime = qnorm(hit) - qnorm(fa))
	
	
## RUN STATS ON D-PRIME
supsw_aov = aov(dprime ~ Lg_Heard + Error(Subject/Lg_Heard), data=supsw_dprime)
summary(supsw_aov)
