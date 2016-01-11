## LOAD PACKAGES ####
library(dplyr)
library(ggplot2)
library(RColorBrewer)


## READ IN DATA AND ORGANIZE
data = read.table("data/all_num.txt", header=T, sep="\t")

supsw = subset(data, Group=="supsw" | Group=="sw")
	supsw$Group = factor(supsw$Group)
supsw = subset(supsw, Subject!="BE_85")
	supsw$Subject = factor(supsw$Subject)
	
# Change variable names and order
supsw$Lg_Heard = factor(supsw$Lg_Heard, levels=c("Eng", "Sp"), labels=c("English", "Spanish"))

supsw$Main_Context = factor(supsw$Main_Context, levels=c("ML", "CS"), labels=c("monolingual", "code-switching"))

# Summarize data for plotting percent correct
supsw_summ = supsw %>%
	group_by(Subject, Lg_Heard, Main_Context) %>%
	summarize(percent_correct = mean(Correct) * 100) %>%
	ungroup()
	
# Summarize data for plotting percent cs resposne
supsw$Response_CS = ifelse(supsw$Response=="CS", 1, 0)
	
supsw_cs_summ = supsw %>%
	group_by(Subject, Lg_Heard, Main_Context) %>%
	summarize(percent_correct = mean(Response_CS) * 100) %>%
	ungroup()


## MAKE COLORS
cols = brewer.pal(5, "PRGn")


## MAKE FIGURES ####
# Percent correct
percent_correct_plot <- ggplot(data = supsw_summ, aes(x=Lg_Heard, y=percent_correct)) + 
          geom_boxplot(aes(fill=Main_Context)) +
          geom_hline(yintercept=50) +
          theme_bw() +
          #scale_fill_manual(values=c("white", "grey")) +
          scale_fill_manual(values=c("white", "black")) +
					xlab("Language of Stimuli") +
					ylab("Percent correct") +
					ggtitle("Percent Correct by\nLanguage and Context of Stimuli") +
					guides(fill=guide_legend(title="Context")) +
					theme(text=element_text(size=18), title=element_text(size=18), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
					      legend.position="top", legend.key=element_blank()) +
					scale_y_continuous(limits=c(0, 100))

pdf("figures/percent_correct.pdf")
percent_correct_plot
dev.off()
	
# Percent correct for CS response
percent_cs_plot <- ggplot(data = supsw_cs_summ, aes(x=Lg_Heard, y=percent_correct)) + 
             		geom_boxplot(aes(fill=Main_Context)) +
             		geom_hline(yintercept=50) +
             		theme_bw() +
             		#scale_fill_manual(values=c("white", "grey")) +
          scale_fill_manual(values=c("white", "black")) +
					xlab("Language of Stimuli") +
					ylab("Percent code-switch response") +
					ggtitle("Percent of Time Call Stimulus Code-switch\nby Language and Context of Stimuli") +
					guides(fill=guide_legend(title="Context")) +
					theme(text=element_text(size=18), title=element_text(size=18), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
					      legend.position="top", legend.key=element_blank()) +
					scale_y_continuous(limits=c(0, 100))

pdf("figures/percent_cs.pdf")
percent_cs_plot
dev.off()
	
	
	
	
	
	
	