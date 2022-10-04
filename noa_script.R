# Initial script for Noa's thesis
# Elise Lesage (elise.r.d.lesage@gmail.com)

# load some basic libraries
library(Rmisc) # for summarySE
library(tidyverse)
library(data.table)
library(ggplot2)
library(ggdist)
library(readxl)
#source("~/r_scripts/gorilla_scripts/SageRUtils/SageThemesNSchemes.R")

# load in the data ----
D_ <- readxl::read_xlsx('data/data_kopie.xlsx', .name_repair = "universal")

# clean so that the relevant lines and columns are left ----
# name of the columns you need for the analysis
ColumnsToKeep <- c("Participant.Private.ID", "BlockNum", "Trial.Number", "Spreadsheet.Row", "StimLeft", "StimRight", "Congruency", "Condition","randomise_trials", "Screen.Name", "display",  "Response", "ProbabilisticAccuracy", "Attempt","Correct", "Reaction.Time")
D_ <- D_ %>% select(all_of(ColumnsToKeep))
# get rid of rows that are not from a trial
D_ <-filter(D_, Screen.Name=="Choice")

# Quick analysis ----
# accuracy and RT per condition; RT only for correct choices
# Because Condition is not filled in properly, make a "new" condition variable
D_ <- D_ %>% mutate(Condition2 = ifelse(randomise_trials==1, "compare", "habit_train"))

# ! Data cleaning: exclude RTs > 10s
hist(D_$Reaction.Time) # this shows the problem
D <- D_ %>% filter(Reaction.Time < 10000) 

# compute average accuracy and RT
Acc <- summarySE(D, measurevar="Correct", groupvars = c("Condition2"), na.rm=TRUE)
RT <- summarySE(filter(D, Correct == 1), measurevar="Reaction.Time", groupvars = c("Condition2"), na.rm=TRUE)

# illustrate w boxplots + dots ----
RTboxplot <- ggplot(D, aes(x = Condition2, y = Reaction.Time)) +
  scale_y_continuous(limits = c(0, 3000))+
  geom_boxplot(fill="grey", alpha=0.5, notch=TRUE, outlier.shape=NA)+
  geom_point(aes(color=Condition2), alpha=0.5, size = 2, position = position_jitter(seed=1, width=0.2))+
  theme_classic()
RTboxplot

