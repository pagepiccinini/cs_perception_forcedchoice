## LOAD PACKAGES ####
library(dplyr)
library(ggplot2)
library(RColorBrewer)


## READ IN DATA AND ORGANIZE ####
data = read.table("data/all_num.txt", header=T, sep="\t")

supsw = subset(data, Group=="supsw" | Group=="sw") %>%
  mutate(Group = factor(Group)) %>%
  filter(Subject!="BE_85") %>%
	mutate(Subject = factor(Subject)) %>%
  # Change variable names and order
  mutate(Lg_Heard = factor(Lg_Heard, levels=c("Eng", "Sp"), labels=c("English", "Spanish"))) %>%
  mutate(Main_Context = factor(Main_Context, levels=c("ML", "CS"), labels=c("monolingual", "code-switching"))) %>%
  mutate(Context = factor(Context, levels=c("E", "CSES", "S", "CSSE"), labels=c("Eng. ML", "Eng. CS", "Sp. ML", "Sp. CS")))

# Summarize data for plotting percent correct
supsw_summ = supsw %>%
	group_by(Subject, Lg_Heard, Main_Context) %>%
	summarize(percent_correct = mean(Correct) * 100) %>%
	ungroup() %>%
  mutate(Context = ifelse(Lg_Heard == "English" & Main_Context == "monolingual", "Eng. ML",
                   ifelse(Lg_Heard == "English" & Main_Context == "code-switching", "Eng. CS",
                   ifelse(Lg_Heard == "Spanish" & Main_Context == "monolingual", "Sp. ML", "Sp. CS")))) %>%
  mutate(Context = factor(Context, levels = c("Eng. ML", "Eng. CS", "Sp. ML", "Sp. CS")))
	
# Summarize data for plotting percent cs resposne
supsw_cs_summ = supsw %>%
  mutate(Response_CS = ifelse(Response == "CS", 1, 0)) %>%
	group_by(Subject, Lg_Heard, Main_Context) %>%
	summarize(percent_correct = mean(Response_CS) * 100) %>%
	ungroup() %>%
  mutate(Context = ifelse(Lg_Heard == "English" & Main_Context == "monolingual", "Eng. ML",
                          ifelse(Lg_Heard == "English" & Main_Context == "code-switching", "Eng. CS",
                                 ifelse(Lg_Heard == "Spanish" & Main_Context == "monolingual", "Sp. ML", "Sp. CS")))) %>%
  mutate(Context = factor(Context, levels = c("Eng. ML", "Eng. CS", "Sp. ML", "Sp. CS")))


## MAKE COLORS ####
cols = brewer.pal(5, "PRGn")
col_eng = cols[5]
col_sp = cols[1]
col_cses = cols[4]
col_csse = cols[2]


## MAKE FIGURES ####
# Percent correct
percent_correct_plot <- ggplot(data = supsw_summ, aes(x=Lg_Heard, y=percent_correct)) + 
          geom_boxplot(aes(fill=Context)) +
          geom_hline(yintercept=50) +
          theme_bw() +
          #scale_fill_manual(values=c("white", "grey")) +
          scale_fill_manual(values=c(col_eng, col_cses, col_sp, col_csse)) +
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
             		geom_boxplot(aes(fill=Context)) +
             		geom_hline(yintercept=50) +
             		theme_bw() +
             		#scale_fill_manual(values=c("white", "grey")) +
          scale_fill_manual(values=c(col_eng, col_cses, col_sp, col_csse)) +
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
	
	
	
	
	
	
	