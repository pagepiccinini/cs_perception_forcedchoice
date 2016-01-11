	# Set working directory
setwd("~/Desktop/Experiments/CS E-S Perception 1 - like forced choice/Results/R - Raw Totals/Data")

	# Read in libraries
library(ggplot2)
library(bear)

	# Read in all text files
data = read.table("all_num.txt", header=T, sep="\t")
data = data[data$Subject!="BE_85",]
data = data[data$Group!="nonsw",]

data$Response_CS = 0
data$Response_CS[data$Response=="CS"] = data$Response_CS[data$Response=="CS"] + 1

data2 = aggregate(data$Response_CS, by=list(data$Subject, data$Lg_Heard, data$Main_Context), FUN=mean)
	colnames(data2)[1] = "Subject"
	colnames(data2)[2] = "Lg_Heard"
	colnames(data2)[3] = "Main_Context"
	colnames(data2)[4] = "Response_CS"
data2$Response_CS = data2$Response_CS * 100

data2$Lg_Heard = factor(data2$Lg_Heard, labels=c("English", "Spanish"))
data2$Main_Context = factor(data2$Main_Context, c("ML", "CS"))
data2$Main_Context = factor(data2$Main_Context, labels=c("monolingual", "code-switching"))
	
	# MAKE BOXPLOT
	pdf("response_cs.pdf")
ggplot(data2, aes(factor(Lg_Heard), Response_CS)) +
	geom_boxplot(aes(fill=factor(Main_Context))) +
	ggtitle("Percent of Time Call Stimulus Code-switch\nby Language and Context of Stimuli") +
	xlab("Language of Stimuli") +
	ylab("Percent of time call stimulus code-switch") +
	scale_y_continuous(limits=c(0,100)) +
	geom_hline(yintercept=50) +
	scale_fill_manual(name="Context", values=c("white", "#525252")) +
	theme_bw() +
	theme(text=element_text(size=18),
		axis.line = element_line(color = "black"),
		plot.background = element_blank(),
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		panel.border = element_blank(),
		legend.key = element_rect(colour="white", fill="white"),
		plot.title=element_text(hjust=0.3))
	dev.off()

