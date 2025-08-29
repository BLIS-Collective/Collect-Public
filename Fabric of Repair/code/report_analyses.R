################################################################################
######################## Fabric of Repair: Key Analyses ########################
################################################################################

# Created By: Christina Pao
# Last Update: 05/02/2025

# Updated By: Justin Morgan
# Last Update: 08/28/2025
# Update Note: Converting code file for public use
# Inputs: replication_swayable_2025.csv

# File Description: 
# read in cleaned public data file
# reproduce analyses from Fabric of Repair report
# Notes : Future update is to translate this into interactive QMD file for 
# output folder, as well as (potentially) update analyses production code

################################################################################
#Packages
library(tidyverse)
library(magrittr)
library(arsenal)
library(ggeffects)
library(broom)
library(dotwhisker)
library(forcats)
#library(stargazer)
#library(showtext)
library(kableExtra)

# Data
df <- read_csv("data/replication_swayable_2025.csv")

### Calculating baseline support

# Reparations baseline support
df_reparations_baseline <- 
  df %>% 
  group_by(race_single) %>% 
  count(rep_baseline_categorical) %>% 
  mutate(Percentage = n/sum(n) * 100) %>% 
  rename(baseline_support = rep_baseline_categorical) %>% 
  add_column(Movement = "Reparations")

# Land Back baseline support
df_landback_baseline <- 
  df %>% 
  group_by(race_single) %>% 
  count(landback_baseline_categorical) %>% 
  mutate(Percentage = n/sum(n) * 100) %>% 
  rename(baseline_support = landback_baseline_categorical) %>% 
  add_column(Movement = "Land Back")

# Merging baseline support dataframes
df_baseline <- bind_rows(df_reparations_baseline, df_landback_baseline)

# Viewing Baseline support
df_baseline

### Calculating predictions by race and video treatment

# Movement regressions
model_landback_race <- lm(Land.Back.Support ~ treatment*race + video_perception, df)
model_reparations_race <- lm(Reparations.Support ~ treatment*race + video_perception, df)

# Movement predictions
predictions_landback_race <- tibble(ggpredict(model_landback_race, terms = c("treatment", "race"))) %>%
  mutate(outcome = "Land Back")
predictions_reparations_race <- tibble(ggpredict(model_reparations_race, terms = c("treatment", "race"))) %>%
  mutate(outcome = "Reparations")

# Merged predictions
predictions_race <- rbind(predictions_landback_race, predictions_reparations_race)

# Black support for reparations with braided video
(predictions_race$predicted[predictions_race$x == "Both" & predictions_race$group == "Black" & predictions_race$outcome == "Reparations"] - predictions_race$predicted[predictions_race$x == "Control" & predictions_race$group == "Black" & predictions_race$outcome == "Reparations"]) / predictions_race$predicted[predictions_race$x == "Control" & predictions_race$group == "Black" & predictions_race$outcome == "Reparations"]

# Black support for landback with braided video
(predictions_race$predicted[predictions_race$x == "Both" & predictions_race$group == "Black" & predictions_race$outcome == "Land Back"] - predictions_race$predicted[predictions_race$x == "Control" & predictions_race$group == "Black" & predictions_race$outcome == "Land Back"]) / predictions_race$predicted[predictions_race$x == "Control" & predictions_race$group == "Black" & predictions_race$outcome == "Land Back"]

# Native support for reparations with braided video
(predictions_race$predicted[predictions_race$x == "Both" & predictions_race$group == "Indigenous" & predictions_race$outcome == "Reparations"] - predictions_race$predicted[predictions_race$x == "Control" & predictions_race$group == "Indigenous" & predictions_race$outcome == "Reparations"]) / predictions_race$predicted[predictions_race$x == "Control" & predictions_race$group == "Indigenous" & predictions_race$outcome == "Reparations"]

# Native support for landback with braided video
(predictions_race$predicted[predictions_race$x == "Both" & predictions_race$group == "Indigenous" & predictions_race$outcome == "Land Back"] - predictions_race$predicted[predictions_race$x == "Control" & predictions_race$group == "Indigenous" & predictions_race$outcome == "Land Back"]) / predictions_race$predicted[predictions_race$x == "Control" & predictions_race$group == "Indigenous" & predictions_race$outcome == "Land Back"]

# Black support for landback increase with reparations video
(predictions_race$predicted[predictions_race$x == "Reparations" & predictions_race$group == "Black" & predictions_race$outcome == "Land Back"] - predictions_race$predicted[predictions_race$x == "Control" & predictions_race$group == "Black" & predictions_race$outcome == "Land Back"]) / predictions_race$predicted[predictions_race$x == "Control" & predictions_race$group == "Black" & predictions_race$outcome == "Land Back"]

# Native support for reparations increase with landback video
(predictions_race$predicted[predictions_race$x == "Land Back" & predictions_race$group == "Indigenous" & predictions_race$outcome == "Reparations"] - predictions_race$predicted[predictions_race$x == "Control" & predictions_race$group == "Indigenous" & predictions_race$outcome == "Reparations"]) / predictions_race$predicted[predictions_race$x == "Control" & predictions_race$group == "Indigenous" & predictions_race$outcome == "Reparations"]

### Calculating predictions by race, partisanship, and video treatment

# Movement regressions
model_landback_raceparty <- lm(Land.Back.Support ~ race*summarized_party*treatment + video_perception, data = df) 
model_reparations_raceparty <- lm(Reparations.Support ~ race*summarized_party*treatment + video_perception, data = df)

# Movement predictions
predictions_landback_raceparty <- tibble(ggpredict(model_landback_raceparty, terms = c("treatment", "race", "summarized_party"))) %>%
  mutate(outcome = "Land Back")
predictions_reparations_raceparty <- tibble(ggpredict(model_reparations_raceparty, terms = c("treatment", "race", "summarized_party"))) %>%
  mutate(outcome = "Reparations")

# Merged predictions
predictions_raceparty <- rbind(predictions_landback_raceparty, predictions_reparations_raceparty)

# Native Democrats' support for landback with braided video
(predictions_raceparty$predicted[predictions_raceparty$x == "Both" & predictions_raceparty$group == "Indigenous" & predictions_raceparty$facet == "Democrat" & predictions_raceparty$outcome == "Land Back"] - predictions_raceparty$predicted[predictions_raceparty$x == "Control" & predictions_raceparty$group == "Indigenous" & predictions_raceparty$facet == "Democrat" & predictions_raceparty$outcome == "Land Back"]) / predictions_raceparty$predicted[predictions_raceparty$x == "Control" & predictions_raceparty$group == "Indigenous" & predictions_raceparty$facet == "Democrat" & predictions_raceparty$outcome == "Land Back"]

# Native Democrats' support for reparations increase with landback video
(predictions_raceparty$predicted[predictions_raceparty$x == "Both" & predictions_raceparty$group == "Indigenous" & predictions_raceparty$facet == "Democrat" & predictions_raceparty$outcome == "Reparations"] - predictions_raceparty$predicted[predictions_raceparty$x == "Control" & predictions_raceparty$group == "Indigenous" & predictions_raceparty$facet == "Democrat" & predictions_raceparty$outcome == "Reparations"]) / predictions_raceparty$predicted[predictions_raceparty$x == "Control" & predictions_raceparty$group == "Indigenous" & predictions_raceparty$facet == "Democrat" & predictions_raceparty$outcome == "Reparations"]

# Native Republicans' support for reparations increase with landback video
(predictions_raceparty$predicted[predictions_raceparty$x == "Land Back" & predictions_raceparty$group == "Indigenous" & predictions_raceparty$facet == "Republican" & predictions_raceparty$outcome == "Reparations"] - predictions_raceparty$predicted[predictions_raceparty$x == "Control" & predictions_raceparty$group == "Indigenous" & predictions_raceparty$facet == "Republican" & predictions_raceparty$outcome == "Reparations"]) / predictions_raceparty$predicted[predictions_raceparty$x == "Control" & predictions_raceparty$group == "Indigenous" & predictions_raceparty$facet == "Republican" & predictions_raceparty$outcome == "Reparations"]

# Black Democrats' support for Land Back increase with Reparations video
(predictions_raceparty$predicted[predictions_raceparty$x == "Reparations" & predictions_raceparty$group == "Black" & predictions_raceparty$facet == "Democrat" & predictions_raceparty$outcome == "Land Back"] - predictions_raceparty$predicted[predictions_raceparty$x == "Control" & predictions_raceparty$group == "Black" & predictions_raceparty$facet == "Democrat" & predictions_raceparty$outcome == "Land Back"]) / predictions_raceparty$predicted[predictions_raceparty$x == "Control" & predictions_raceparty$group == "Black" & predictions_raceparty$facet == "Democrat" & predictions_raceparty$outcome == "Land Back"]

# Black Democrats' support for Reparations increase with Braided video
(predictions_raceparty$predicted[predictions_raceparty$x == "Both" & predictions_raceparty$group == "Black" & predictions_raceparty$facet == "Democrat" & predictions_raceparty$outcome == "Reparations"] - predictions_raceparty$predicted[predictions_raceparty$x == "Control" & predictions_raceparty$group == "Black" & predictions_raceparty$facet == "Democrat" & predictions_raceparty$outcome == "Reparations"]) / predictions_raceparty$predicted[predictions_raceparty$x == "Control" & predictions_raceparty$group == "Black" & predictions_raceparty$facet == "Democrat" & predictions_raceparty$outcome == "Reparations"]

# Black Democrats' support for Land Back with Braided
(predictions_raceparty$predicted[predictions_raceparty$x == "Both" & predictions_raceparty$group == "Black" & predictions_raceparty$facet == "Democrat" & predictions_raceparty$outcome == "Land Back"] - predictions_raceparty$predicted[predictions_raceparty$x == "Control" & predictions_raceparty$group == "Black" & predictions_raceparty$facet == "Democrat" & predictions_raceparty$outcome == "Land Back"]) / predictions_raceparty$predicted[predictions_raceparty$x == "Control" & predictions_raceparty$group == "Black" & predictions_raceparty$facet == "Democrat" & predictions_raceparty$outcome == "Land Back"]

# Indigenous Democrats' support for Reparations with Braided
(predictions_raceparty$predicted[predictions_raceparty$x == "Both" & predictions_raceparty$group == "Indigenous" & predictions_raceparty$facet == "Democrat" & predictions_raceparty$outcome == "Reparations"] - predictions_raceparty$predicted[predictions_raceparty$x == "Control" & predictions_raceparty$group == "Indigenous" & predictions_raceparty$facet == "Democrat" & predictions_raceparty$outcome == "Reparations"]) / predictions_raceparty$predicted[predictions_raceparty$x == "Control" & predictions_raceparty$group == "Indigenous" & predictions_raceparty$facet == "Democrat" & predictions_raceparty$outcome == "Reparations"]

# Indigenous Republicans' support for Reparations with Reparations
(predictions_raceparty$predicted[predictions_raceparty$x == "Reparations" & predictions_raceparty$group == "Indigenous" & predictions_raceparty$facet == "Republican" & predictions_raceparty$outcome == "Reparations"] - predictions_raceparty$predicted[predictions_raceparty$x == "Control" & predictions_raceparty$group == "Indigenous" & predictions_raceparty$facet == "Republican" & predictions_raceparty$outcome == "Reparations"]) / predictions_raceparty$predicted[predictions_raceparty$x == "Control" & predictions_raceparty$group == "Indigenous" & predictions_raceparty$facet == "Republican" & predictions_raceparty$outcome == "Reparations"]
