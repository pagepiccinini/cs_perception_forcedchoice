## READ IN DATA ####
source("scripts/cs_perception_forcedchoice_cleaning.R")


## LOAD PACKAGES ####
library(ggplot2)
library(RColorBrewer)


## ORGANIZE DATA ####
data_figs = data_clean %>%
  # Change variable names and order
  mutate(Lg_Heard = factor(Lg_Heard, levels=c("Eng", "Sp"), labels=c("English", "Spanish"))) %>%
  mutate(Main_Context = factor(Main_Context, levels=c("ML", "CS"), labels=c("monolingual", "code-switching"))) %>%
  mutate(Context = factor(Context, levels=c("E", "CSES", "S", "CSSE"), labels=c("Eng. ML", "Eng. CS", "Sp. ML", "Sp. CS")))

# Summarize data for plotting percent correct
data_accuracy_figs = data_figs %>%
  group_by(Subject, Lg_Heard, Main_Context) %>%
  summarize(percent_correct = mean(Correct) * 100) %>%
  ungroup() %>%
  mutate(Context = ifelse(Lg_Heard == "English" & Main_Context == "monolingual", "Eng. ML",
                          ifelse(Lg_Heard == "English" & Main_Context == "code-switching", "Eng. CS",
                                 ifelse(Lg_Heard == "Spanish" & Main_Context == "monolingual", "Sp. ML", "Sp. CS")))) %>%
  mutate(Context = factor(Context, levels = c("Eng. ML", "Eng. CS", "Sp. ML", "Sp. CS")))

# Summarize data for plotting percent cs resposne
data_response_figs = data_figs %>%
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
# Accuracy
accuracy.plot = ggplot(data = data_accuracy_figs, aes(x=Lg_Heard, y=percent_correct)) + 
  geom_boxplot(aes(fill=Context)) +
  geom_hline(yintercept=50) +
  scale_y_continuous(limits=c(0, 100)) +
  #scale_fill_manual(values=c("white", "grey")) +
  scale_fill_manual(values=c(col_eng, col_cses, col_sp, col_csse)) +
  ggtitle("Percent Correct by\nLanguage and Context of Stimuli") +
  xlab("Language of Stimuli") +
  ylab("Percent correct") +
  guides(fill=guide_legend(title="Context")) +
  theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18), panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), legend.position="top",
        legend.key=element_blank())

pdf("figures/accuracy.pdf")
accuracy.plot
dev.off()

# Response
response.plot = ggplot(data = data_response_figs, aes(x = Lg_Heard, y = percent_correct)) + 
  geom_boxplot(aes(fill=Context)) +
  geom_hline(yintercept=50) +
  scale_y_continuous(limits=c(0, 100)) +
  #scale_fill_manual(values=c("white", "grey")) +
  scale_fill_manual(values=c(col_eng, col_cses, col_sp, col_csse)) +
  ggtitle("Percent of Time Call Stimulus Code-switch\nby Language and Context of Stimuli") +
  xlab("Language of Stimuli") +
  ylab("Percent code-switch response") +
  guides(fill=guide_legend(title="Context")) +
  theme_bw() +
  theme(text=element_text(size=18), title=element_text(size=18), panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), legend.position="top",
        legend.key=element_blank())

pdf("figures/response.pdf")
response.plot
dev.off()