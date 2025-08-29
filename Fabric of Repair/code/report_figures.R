################################################################################
################## Fabric of Repair: Producing Report Figures ##################
################################################################################

# Created By: Christina Pao
# Last Update: 05/02/2025

# Updated By: Justin Morgan
# Last Update: 08/28/2025
# Update Note: Converting code file for public use
# Inputs: replication_swayable_2025.csv

# File Description: 
# read in cleaned public data file
# reproduce plots from Fabric of Repair report
# Notes : Future update is to translate this into interactive QMD file for 
# output folder, as well as (potentially) update figure production code

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

########################### Figure 1: Title of Figure ##########################

# Create summary dataframe for Reparations baseline
df_repbaseline_plot <- 
  df %>% 
  group_by(race_single) %>% 
  count(rep_baseline_categorical) %>% 
  mutate(Percentage = n/sum(n) * 100) %>% 
  rename(baseline_support = rep_baseline_categorical) %>% 
  add_column(Movement = "Reparations")

# Create summary dataframe for Land Back baseline
df_landbackbaseline_plot <- 
  df %>% 
  group_by(race_single) %>% 
  count(landback_baseline_categorical) %>% 
  mutate(Percentage = n/sum(n) * 100) %>% 
  rename(baseline_support = landback_baseline_categorical) %>% 
  add_column(Movement = "Land Back")

# Merge summary dataframes for Reparations and Land Back
df_baselinesupport_plot <- bind_rows(df_repbaseline_plot, df_landbackbaseline_plot)

# Plot Figure 1
figure1 <- df_baselinesupport_plot %>% 
  mutate(race_label = case_when(race_single == "black-or-african-american" ~ "Black Respondent",
                                race_single == "american-indian-or-alaska-native" ~ "Native Respondent"))  %>%
  ggplot(aes(x = baseline_support, y = Percentage, fill = Movement)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Baseline Support for Each Movement",
       x = "Baseline Support",
       y = "Percentage",
       fill = "Movement") +  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = c("#1DA595", "#F94B01")) + 
  facet_wrap(~race_label) + 
  theme_bw()

figure1

########################### Figure 2: Title of Figure ##########################

# Creating a summary dataframe of baseline Reparations support by political party
df_baseline_party_reparations <- 
  df %>% 
  group_by(race, summarized_party) %>% 
  count(rep_baseline_categorical) %>% 
  mutate(Percentage = n/sum(n) * 100) %>% 
  rename(baseline_support = rep_baseline_categorical) %>% 
  add_column(Movement = "Reparations")

# Creating a summary dataframe of baseline Land Back support by political party
dF_baseline_party_landback <- 
  df %>% 
  group_by(race, summarized_party) %>% 
  count(landback_baseline_categorical) %>% 
  mutate(Percentage = n/sum(n) * 100) %>% 
  rename(baseline_support = landback_baseline_categorical) %>% 
  add_column(Movement = "Land Back")

# Combining both Reparations and Land Back dataframes
df_baseline_party <- bind_rows(df_baseline_party_reparations, df_baseline_party_landback)

# Plotting Figure 2
figure2 <- df_baseline_party %>% 
  ggplot(aes(x = summarized_party, y = Percentage, fill = baseline_support)) +
  geom_bar(stat = "identity") +
  labs(title = "Baseline Support by Partisanship",
       x = "Partisanship",
       y = "Percentage",
       fill = "Support") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  facet_grid(vars(Movement), vars(race)) + 
  theme(axis.text.x = element_text(angle = 45)) +
  scale_fill_manual(values = c('#F6A40E', '#F35018', '#5AAE7D')) + 
  theme_bw() 

figure2

########################### Figure 3: Title of Figure ##########################

# Creating a summary dataframe of baseline Reparations support by age groups
df_baseline_age_reparations <- 
  df %>% 
  group_by(race, age_groups) %>% 
  count(rep_baseline_categorical) %>% 
  mutate(Percentage = n/sum(n) * 100) %>% 
  rename(baseline_support = rep_baseline_categorical) %>% 
  add_column(Movement = "Reparations")

# Creating a summary dataframe of baseline Land Back support by age
df_baseline_age_landback <- 
  df %>% 
  group_by(race, age_groups) %>% 
  count(landback_baseline_categorical) %>% 
  mutate(Percentage = n/sum(n) * 100) %>% 
  rename(baseline_support = landback_baseline_categorical) %>% 
  add_column(Movement = "Land Back")

# Combining both Reparations and Land Back dataframes
df_baseline_age <- bind_rows(df_baseline_age_reparations, df_baseline_age_landback)

# Plotting Figure 3
figure3 <- df_baseline_age %>% 
  ggplot(aes(x = age_groups, y = Percentage, fill = baseline_support)) +
  geom_bar(stat = "identity") +
  labs(title = "Baseline Support by Age",
       x = "Age",
       y = "Percentage",
       fill = "Support") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  facet_grid(vars(Movement), vars(race)) + 
  theme(axis.text.x = element_text(angle = 45)) +
  scale_fill_manual(values = c('#F6A40E', '#F35018', '#5AAE7D')) + 
  theme_bw()

figure3

########################### Figure 4: Title of Figure ##########################

# Regression model for Land Back support
model_landback_ovr <- lm(Land.Back.Support ~ treatment + video_perception, df)

# Making dataframe of regression coefficients for Land Back model
df_landback_model <- tidy(model_landback_ovr) %>%
  mutate(model = "Land Back Support")

# Regression model for Reparations support
model_reparations_ovr <- lm(Reparations.Support ~ treatment + video_perception, df)

# Making dataframe of regression coefficients for Reparations model
df_reparations_model <- tidy(model_reparations_ovr) %>%
  mutate(model = "Reparations Support")

# Binding regression coefficient dataframes together
df_combined_models <- bind_rows(df_landback_model, df_reparations_model) %>%
  relabel_predictors(c("treatmentReparations" = "Reparations Video",
                       "treatmentLand Back" = "Land Back Video",
                       "treatmentBoth" = "Both Video")) %>%
  filter(term != "video_perception")

# Plot Figure 4 from regression coefficients
figure4 <- dwplot(df_combined_models, 
       # here are our regular aesthetics
       dot_args = list(aes(colour = model)), 
       size = 3) + 
  theme_bw() + 
  labs(title = "Persuasion Effects of Garrison's Videos", subtitle = "Referenced Against Control Video", 
       y = "", caption = "Model includes a control for respondents' perceived production quality.") +
  scale_x_continuous("Coefficient Estimate with 95% CIs") +
  geom_vline(xintercept = 0, colour="grey", linetype = "longdash") +
  scale_colour_manual(values = c("#1DA595", "#F94B01")) # start/end for light/dark greys

figure4

########################### Figure 5: Title of Figure ##########################

# Regression model stratified by race for Land Back support
model_landback_race <- lm(Land.Back.Support ~ treatment*race + video_perception, df)

# Predictions by race for Land Back support
predictions_landback_race <- tibble(ggpredict(model_landback_race, terms = c("treatment", "race"))) %>% 
  mutate(outcome = "Land Back")

# Regression model stratified by race for Reparations support
model_reparations_race <- lm(Reparations.Support ~ treatment*race + video_perception, df)

# Predictions by race for Reparations support
predictions_reparations_race <- tibble(ggpredict(model_reparations_race, terms = c("treatment", "race"))) %>%
  mutate(outcome = "Reparations")

# Merging predictions
predictions_support <- rbind(predictions_landback_race, predictions_reparations_race)

# Plotting Figure 5
figure5 <- predictions_support %>%
  ggplot(aes(x = x, y = predicted, fill = outcome)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), position=position_dodge(.9), width=.2) +
  labs(x = "Video", y = "Average Support", fill = "Movement", caption = "Note: Perceived production quality control added.") +
  facet_wrap(~group) +
  scale_fill_manual(values = c("#1DA595", "#F94B01")) +
  theme_bw() +
  labs(title = "Movement Support Among Black & Indigenous Audiences") + 
  scale_y_continuous(limits = c(0, 10), breaks = c(0, 2.5, 5, 7.5, 10), 
                     labels = c("0, Definitely No", "2.5", "5", "7.5", "10, Definitely Yes"))

figure5

########################### Figure 6: Title of Figure ##########################

# Regression model stratified by race and party, Land Back support
model_landback_raceparty <- lm(Land.Back.Support ~ race*summarized_party*treatment + video_perception, data = df) 

# Predictions by race and party, Land Back support
predictions_landback_raceparty <- ggpredict(model_landback_raceparty, terms = c("treatment", "race", "summarized_party")) %>%
  tibble() %>% 
  mutate(outcome = "Land Back")

# Regression model stratified by race and party, Reparations support
model_reparations_raceparty <- lm(Reparations.Support ~ race*summarized_party*treatment + video_perception, data = df)

# Predictions by race and party, Reparations support
predictions_reparations_raceparty <- tibble(ggpredict(model_reparations_raceparty, terms = c("treatment", "race", "summarized_party"))) %>%
  mutate(outcome = "Reparations")

# Merging predictions
predictions_raceparty <- rbind(predictions_landback_raceparty, predictions_reparations_raceparty)

# Plotting Figure 6
figure6 <- predictions_raceparty %>%
  ggplot(aes(x = x, y = predicted, fill = group)) + 
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), position=position_dodge(.9), width=.2) +
  labs(x = "Video", y = "Average Support", fill = "Race", caption = "Note: Model includes a control for respondents' perceived production quality.") +
  facet_grid(vars(outcome),vars(facet)) +
  scale_fill_manual(values = c('Black' = '#4C230A', 'Indigenous' = '#F4C900')) + 
  theme_bw() +
  scale_y_continuous(limits = c(0, 10), breaks = c(0, 2.5, 5, 7.5, 10)) +
  labs(title = "Movement Support Across Political Ideologies")

figure6

########################### Figure 7: Title of Figure ##########################

# Calculating movement support
df <- df %>%
  mutate(support_reparations = ifelse(rep_baseline_categorical == "Support", 1, 0),
         support_landback = ifelse(landback_baseline_categorical == "Support", 1, 0))

# Calculating movememt feasibility
df <- df %>%
  mutate(feasibility_landback = ifelse(Feasibility.Land.Back == "Yes", 1, 0),
         feasibility_reparations = ifelse(Feasibility.Reparations == "Yes", 1, 0))

# Calculating Landback feasibility for Indigenous respondents, Reparations feasibility for Black respondents
df_ownmovement_feasibility <- df %>%
  filter((race == "Indigenous" & support_landback == 1) | (race == "Black" & support_reparations == 1)) %>%
  mutate(hope = case_when(race == "Indigenous" ~ feasibility_landback,
                          race == "Black" ~ feasibility_reparations)) %>%
  group_by(race) %>%
  count(hope) %>% 
  mutate(Percentage = n/sum(n) * 100) %>%
  mutate(hope = ifelse(hope == 1, "Feasibile", "Hope Gap")) %>%
  mutate(movement = "Own Movement")

# Changing factor levels
df_ownmovement_feasibility$hope <- factor(df_ownmovement_feasibility$hope, levels = c("Hope Gap", "Feasibile"))

# Plotting Figure 7
figure7 <- df_ownmovement_feasibility %>% 
  ggplot(aes(x = race, y = Percentage, fill = hope)) +
  geom_bar(stat = "identity") +
  labs(title = "Hope Gap",
       x = "Identity",
       y = "Percentage",
       fill = "Feasibility") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_fill_manual(values = c('#F35018', '#5AAE7D')) + 
  theme_bw()

figure7
