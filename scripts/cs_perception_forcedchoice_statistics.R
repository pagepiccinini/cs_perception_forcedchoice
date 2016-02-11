## READ IN DATA ####
source("scripts/cs_perception_forcedchoice_cleaning.R")


## LOAD PACKAGES ####
library(lme4)


## ORGANIZE DATA ####
data_stats = data_clean %>%
  # Contrast coding for language heard (baseline English)
  mutate(Lg_HeardContrast = ifelse(Lg_Heard == "Eng", -0.5, 0.5)) %>%
  # Contrast coding for context (baseline monolingual)
  mutate(Main_ContextContrast = ifelse(Main_Context == "ML", -0.5, 0.5)) %>%
  # Make block numeric
  mutate(Block_Num = as.numeric(Block)) %>%
  # Make response binary (0 set to monolingual)
  mutate(ResponseContrast = ifelse(Response == "ML", 0, 1))


## BUILD MODELS FOR ANALYSIS #1 (ACCURACY) ####
# Full model
accuracy.glmer = glmer(Correct ~ Lg_HeardContrast * Main_ContextContrast +
                         (0+Lg_HeardContrast*Main_ContextContrast|Subject) +
                         (1|Stimuli), family = "binomial", data = data_stats)

accuracy.glmer_sum = summary(accuracy.glmer)

# Test for effect of language heard
accuracy_nolg.glmer = glmer(Correct ~ Lg_HeardContrast * Main_ContextContrast - Lg_HeardContrast +
                              (0+Lg_HeardContrast*Main_ContextContrast|Subject) +
                              (1|Stimuli), family = "binomial", data = data_stats)

accuracy_nolg.anova = anova(accuracy.glmer, accuracy_nolg.glmer)

# Test for effect of context
accuracy_nocontext.glmer = glmer(Correct ~ Lg_HeardContrast * Main_ContextContrast - Main_ContextContrast +
                              (0+Lg_HeardContrast*Main_ContextContrast|Subject) +
                              (1|Stimuli), family = "binomial", data = data_stats)

accuracy_nocontext.anova = anova(accuracy.glmer, accuracy_nocontext.glmer)

# Test for interaction of language heard x context
accuracy_nolgxcontext.glmer = glmer(Correct ~ Lg_HeardContrast * Main_ContextContrast - Lg_HeardContrast:Main_ContextContrast +
                              (0+Lg_HeardContrast*Main_ContextContrast|Subject) +
                              (1|Stimuli), family = "binomial", data = data_stats)

accuracy_nolgxcontext.anova = anova(accuracy.glmer, accuracy_nolgxcontext.glmer)


## BUILD MODELS FOR ANALYSIS #2 (RESPONSE BIAS) ####
# Full model
response.glmer = glmer(ResponseContrast ~ Lg_HeardContrast * Main_ContextContrast +
                         (1+Lg_HeardContrast*Main_ContextContrast|Subject) +
                         (1|Stimuli), family = "binomial", data = data_stats)

response.glmer_sum = summary(response.glmer)

# Test for effect of language heard
response_nolg.glmer = glmer(ResponseContrast ~ Lg_HeardContrast * Main_ContextContrast - Lg_HeardContrast +
                              (1+Lg_HeardContrast*Main_ContextContrast|Subject) +
                              (1|Stimuli), family = "binomial", data = data_stats)

response_nolg.anova = anova(response.glmer, response_nolg.glmer)

# Test for effect of context
response_nocontext.glmer = glmer(ResponseContrast ~ Lg_HeardContrast * Main_ContextContrast - Main_ContextContrast +
                              (1+Lg_HeardContrast*Main_ContextContrast|Subject) +
                              (1|Stimuli), family = "binomial", data = data_stats)

response_nocontext.anova = anova(response.glmer, response_nocontext.glmer)

# Test for interaction of language heard x context
response_nolgxcontext.glmer = glmer(ResponseContrast ~ Lg_HeardContrast * Main_ContextContrast - Lg_HeardContrast:Main_ContextContrast +
                              (1+Lg_HeardContrast*Main_ContextContrast|Subject) +
                              (1|Stimuli), family = "binomial", data = data_stats)

response_nolgxcontext.anova = anova(response.glmer, response_nolgxcontext.glmer)

## FOLLOW-UP SIMPLE REGRESSIONS FOR INTERACTION OF LANGUAGE HEARD X CONTEXT ####
# Organize data
data_eng_stats = data_stats %>%
  filter(Lg_Heard == "Eng") %>%
  mutate(Lg_Heard = factor(Lg_Heard))

data_sp_stats = data_stats %>%
  filter(Lg_Heard == "Sp") %>%
  mutate(Lg_Heard = factor(Lg_Heard))

# Build models
response_eng.glm = glm(ResponseContrast ~ Main_ContextContrast,
                       family="binomial", data = data_eng_stats)
response_eng.glm_sum = summary(response_eng.glm)

response_sp.glm = glm(ResponseContrast ~ Main_ContextContrast,
                       family="binomial", data = data_sp_stats)
response_sp.glm_sum = summary(response_sp.glm)
