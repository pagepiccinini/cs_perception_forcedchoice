## READ IN DATA ####
source("scripts/cs_perception_forcedchoice_cleaning.R")


## LOAD PACKAGES ####
library(lme4)


## ORGANIZE DATA ####
# Make hit data
data_dprime_hit_stats = data_clean %>%
  filter(Main_Context=="CS") %>%
  group_by(Subject, Lg_Heard) %>%
  summarize(hit = mean(Correct)) %>%
  ungroup()

# Make false alarm data
data_dprime_fa_stats = data_clean %>%
  filter(Main_Context=="ML") %>%
  group_by(Subject, Lg_Heard) %>%
  summarize(fa = 1 - mean(Correct)) %>%
  ungroup()

# Compute d prime
data_dprime_stats = data_dprime_hit_stats %>%
  inner_join(data_dprime_fa_stats) %>%
  mutate(dprime = qnorm(hit) - qnorm(fa))


## RUN ANOVA ON D PRIME SCORES ####
accuracy.aov = aov(dprime ~ Lg_Heard + Error(Subject/Lg_Heard),
                   data = data_dprime_stats)
accuracy.aov_sum = summary(accuracy.aov)
