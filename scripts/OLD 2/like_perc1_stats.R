	# Set working directory
setwd("~/Desktop/Experiments/CS E-S Perception 1 - like forced choice/Results/R - Raw Totals/Data")

	# Read in libraries
library(lme4)
library(Hmisc)

	# Read in all text files
all_num 	= read.table("all_num.txt", header=T, sep="\t")
all_num	= all_num[all_num$Subject!="BE_85",]
all_num = all_num[all_num$Group!="nonsw",]

all_num_plot = read.table("all_num_plot.txt", header=T, sep="\t")
test = all_num_plot[,1:3]

all_rt 	= read.table("all_noout.txt", header=T, sep="\t")
all_rt	= all_rt[all_rt$Subject!="BE_85",]
all_rt = all_rt[all_rt$Group!="nonsw",]

all_rt_corr = all_rt[all_rt$Correct=="1",]
all_rt_ml = all_rt[all_rt$Response=="ML",]
all_rt_cs = all_rt[all_rt$Response=="CS",]

	# Model for Responses
all_num.lmer = lmer(relevel(Response, "ML") ~ relevel(Main_Context, "ML")*relevel(Lg_Heard, "Eng") + (1+Main_Context+Lg_Heard | Subject) + (1 | Block), family="binomial", data=all_num)
all_num.lmer

	# Models for RTs
all_rt_corr.lmer = lmer(RT_log ~ relevel(Main_Context, "ML")*relevel(Lg_Heard, "Eng") + (1+Main_Context+Lg_Heard | Subject) + (1 | Block), data=all_rt_corr)
all_rt_corr.lmer

all_rt_ml.lmer = lmer(RT_log ~ relevel(Main_Context, "ML")*relevel(Lg_Heard, "Eng") + (1+Main_Context+Lg_Heard | Subject) + (1 | Block), data=all_rt_ml)
all_rt_ml.lmer

all_rt_cs.lmer = lmer(RT_log ~ relevel(Main_Context, "ML")*relevel(Lg_Heard, "Eng") + (1+Main_Context+Lg_Heard | Subject) + (1|Block), data=all_rt_cs)
all_rt_cs.lmer

		# Grouped barplot for raw responses
data <- tapply(test$Percentage, list(test$Main_Context,test$Language), sum)
labels=c("monolingual", "code-switching")

like_num_plot_x = c(1.5, 2.5, 4.5, 5.5)
like_num_plot_y = c(all_num_plot$Percentage)
like_num_plot_sd = c(all_num_plot$Std_Error)
like_num_plot_sd_upper = c(like_num_plot_y+like_num_plot_sd)
like_num_plot_sd_lower = c(like_num_plot_y-like_num_plot_sd)

	pdf("../Figures/all_num.pdf")

barplot(data,beside=T,col=c("grey18","white"), main="Percentage of Time Giving CS Response",xlab="Language Utterance Begins In",ylab="Percentage of time", ylim=c(0,50), cex.main=1.7,cex.lab=1.5,cex.axis=1.5)

errbar(like_num_plot_x, like_num_plot_y, like_num_plot_sd_lower, like_num_plot_sd_upper, add=T, lty=1, lwd=2)

legend("topright", labels,fill=c("grey18","white"), cex=1.2, box.lwd=0)

	dev.off()
	
	
		# Line plots for RTs
x = c(1, 2)
labels_corr = c("monolingual", "code-switch")
labels_ml = c("correct", "incorrect")
labels_cs = c("incorrect", "correct")

eng_corr	= c(1443.610, 1370.762)
sp_corr	= c(1372.840, 1679.802)

eng_ml	= c(1443.610393, 1364.89738)
sp_ml		= c(1372.840238, 1466.932059)

eng_cs		= c(1489.848129, 1370.761822)
sp_cs		= c(1787.738529, 1679.801636)

corr_x = c(1, 2, 1, 2)
corr_y = c(1443.610, 1370.762, 1372.840, 1679.802)
corr_sd = c(1.03, 1.13, 1.13, 1.14)
corr_sd_upper = c(corr_y+corr_sd)
corr_sd_lower = c(corr_y-corr_sd)

	pdf("../Figures/rt_corr_plot.pdf")
plot(eng_corr ~ x, type="o", pch=19, lwd=2, col="black", ylim=c(1350, 1800), xaxt="n", main="Reaction Times for Correct Responses", xlab="Stimuli context", ylab="Reaction times in ms.", cex.main=1.7,cex.lab=1.5,cex.axis=1.5)
points(sp_corr ~ x, type="o", lwd=2, col="black", lty=2)
#errbar(corr_x, corr_y, corr_sd_lower, corr_sd_upper, add=T, lty=1, lwd=2)
axis(1, at=x, labels=labels_corr, cex.axis=1.4)
legend("topleft", c("English", "Spanish"), lty=c(1, 2), pch=c(19, 1), cex=1.2, box.lwd=0)
	dev.off()
	
	pdf("../Figures/rt_ml_plot.pdf")
plot(eng_ml ~ x, type="o", pch=19, lwd=2, col="black", ylim=c(1350, 1800), xaxt="n", main="Reaction Times for Monolingual Responses", xlab="Stimuli context", ylab="Reaction times in ms.", cex.main=1.7,cex.lab=1.5,cex.axis=1.5)
points(sp_ml ~ x, type="o", lwd=2, col="black", lty=2)
axis(1, at=x, labels=labels_ml, cex.axis=1.4)
legend("topleft", c("English", "Spanish"), lty=c(1, 2), pch=c(19, 1), cex=1.2, box.lwd=0)
	dev.off()
	
	pdf("../Figures/rt_cs_plot.pdf")
plot(eng_cs ~ x, type="o", pch=19, lwd=2, col="black", ylim=c(1350, 1800), xaxt="n", main="Reaction Times for Code-switching Responses", xlab="Stimuli context", ylab="Reaction times in ms.", cex.main=1.7,cex.lab=1.5,cex.axis=1.5)
points(sp_cs ~ x, type="o", lwd=2, col="black", lty=2)
axis(1, at=x, labels=labels_cs, cex.axis=1.4)
legend("topright", c("English", "Spanish"), lty=c(1, 2), pch=c(19, 1), cex=1.2, box.lwd=0)
	dev.off()











