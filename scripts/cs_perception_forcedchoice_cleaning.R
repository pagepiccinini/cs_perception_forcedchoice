## LOAD PACKAGES ####
library(dplyr)


## READ IN DATA ####
data = read.table("data/data.txt", header=T, sep="\t")


## CLEAN DATA ####
data_clean = data %>%
  filter(Group == "supsw" | Group == "sw") %>%
  mutate(Group = factor(Group)) %>%
  filter(Subject != "BE_85") %>%
  mutate(Subject = factor(Subject))