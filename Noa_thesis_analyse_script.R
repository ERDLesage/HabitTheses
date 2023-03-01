# Analysis script for Noa's thesis


# load some basic libraries
# Noa: deze zal je wellicht moeten installeren
library(rstatix)
library(Rmisc) # for summarySE
library(tidyverse)
library(data.table)
library(ggplot2)
library(ggdist)
library(readxl)
#library(SageRUtils)
library(gtsummary) # for nice tables
library(pracma) # for disp
library(afex)
library(phia)
library(lmerTest)

#source("SageDataCleanUtils.R")

# de data inlezen
D <- read.csv('noa_alldata.csv', sep=',')
D$BlockDaily <- D$Block
D$Block <- (D$Day-1)*18+D$BlockDaily
D <- filter(D, Reaction.Time<3000)
D$PI <- D$PropIncongr
D$PropIncongr <- as.numeric(substring(D$PI, 2, 2))
D$ID <- D$Participant.Private.ID
D$ID <- factor(D$ID)
D <- filter(D, ID!=7404185)

# code om te filteren
D_train <- filter(D, Condition == "Train")
D_compare <- filter(D, Condition == "Compare")
D_pctest <- filter(D, Condition == "PCTest")
D_pctest$BlockBig <- D_pctest$Block
D_pctest$Block <- D_pctest$BlockBig-238
## some figures for exploration


# ~~~~~~~~~~~~
# Training: acc en RT : change over blocks ####
# ~~~~~~~~~~~~

Acc_train_grp <- SageSummarySEwithin(D_train, dv="Correct", wv= c("Block"), subv = "ID")
Acc_train_sub <- SageSummarySEwithin(D_train, dv="Correct", wv= c("ID", "Block"), subv = "ID")
#Acc_train <- SageSummarySEwithin(D_train, dv="Correct", wv= c("ID"), subv = "ID")
rt_train_grp <- SageSummarySEwithin(filter(D_train, Correct==1), dv="Reaction.Time", wv= c("Block"), subv = "ID")
rt_train_sub <- SageSummarySEwithin(filter(D_train, Correct==1), dv="Reaction.Time", wv= c("ID", "Block"), subv = "ID")


train_ACC_line <- ggplot() + 
  geom_line(data=Acc_train_sub, aes(x=Block, y=Correct, group=ID), color = "Black", size = .5, alpha=0.2)+
  geom_line(data=Acc_train_grp, aes(x=Block, y=Correct, group=1), size = 2, position=position_dodge(.5)) +
  geom_errorbar(data=Acc_train_grp, aes(x=Block, group=1, ymin=Correct-se, ymax=Correct+se), width=0, size =1, position=position_dodge(.5)) +
  geom_point(data=Acc_train_grp, aes(x=Block, y=Correct),stroke=1, position=position_dodge(.5), size = 3) +
  xlab("Block") + ylab("accuracy")+ggtitle("Accuracy over blocks")+
  #scale_color_manual(values=c("black", "white") ) + scale_shape_manual(values=c(21, 21)) + scale_fill_manual(values=c("black", "white"))+
  theme_sage_simple(base_size=20) 
train_ACC_line

train_RT_line <- ggplot() + 
  geom_line(data=rt_train_sub, aes(x=Block, y=Reaction.Time, group=ID), color = "grey", size = .5, alpha=0.2)+
  geom_line(data=rt_train_grp, aes(x=Block, y=Reaction.Time, group=1), size = 2, position=position_dodge(.5)) +
  geom_errorbar(data=rt_train_grp, aes(x=Block, group=1, ymin=Reaction.Time-se, ymax=Reaction.Time+se), width=0, size =1, position=position_dodge(.5)) +
  geom_point(data=rt_train_grp, aes(x=Block, y=Reaction.Time),stroke=1, position=position_dodge(.5), size = 3) +
  xlab("Block") + ylab("RT")+ggtitle("RT over blocks")+
  #scale_color_manual(values=c("black", "white") ) + scale_shape_manual(values=c(21, 21)) + scale_fill_manual(values=c("black", "white"))+
  theme_sage_simple(base_size=20) 
train_RT_line

# ~~~~~~~~~~~~
# PC: scores en optimaliteit ####
# ~~~~~~~~~~~~

## calculating the actual scores
scores <- D_pctest %>% summarySE(measurevar = "Reaction.Time", groupvars = c("ID","PropIncongr", "Correct"), na.rm = TRUE) %>%
  filter(Correct==1) %>%
  select(ID, PropIncongr, N) %>%
  rename(Score = N) %>%
  mutate(ScorePerBlock = Score/4) %>%
  mutate(TimeCost = 60/ScorePerBlock)

## calculate scores and real
habit_scores <- SageSummarySEwithin(filter(D_train, Correct == 1, Day==3), dv="Reaction.Time", wv = c("ID"), subv = "ID") %>%
  rename(Score=N) %>%
  mutate(ScorePerBlock = Score) %>%
  mutate(TimeCost = 60/ScorePerBlock)

GD_scores <- SageSummarySEwithin(filter(D_compare, Correct == 1, Day==3), dv="Reaction.Time", wv = c("ID"), subv = "ID") %>%
  rename(Score=N) %>%
  mutate(ScorePerBlock = Score) %>%
  mutate(TimeCost = 60/ScorePerBlock)

score_average <- SageSummarySEwithin(scores, dv="Score", wv = c("PropIncongr"), subv = "ID")
acc_average <- SageSummarySEwithin(D_pctest, dv="Correct", wv = c("PropIncongr", "Congruency"), subv = "ID")
rt_average <- SageSummarySEwithin(D_pctest, dv="Reaction.Time", wv = c("PropIncongr", "Congruency"), subv = "ID")

## Simulation: let's look at projected scores if ppl were entirely automatic of entirely goal-directed ----

## very rough, group-wise
# - training rt's on day 3 (habit) and compare rts on day3 (GD)

habit_scr_grp <- summarySE(habit_scores, measurevar="ScorePerBlock")
GD_scr_grp <- summarySE(GD_scores, measurevar="ScorePerBlock")
habit_rt_grp <- summarySE(habit_scores, measurevar="TimeCost")
GD_rt_grp <- summarySE(GD_scores, measurevar="TimeCost")
real_rt_grp <- summarySE(scores, measurevar="TimeCost")
# - "simulate" the scores at different levels of prop incongr
pi_levels <- c(0.1, 0.2, 0.3, 0.4, 0.5)
habitsim<-data.frame(PropIncongr =rep(as.double(NA),length(pi_levels)), meanScore = rep(as.double(NA),length(pi_levels)), lowerScore=rep(as.double(NA),length(pi_levels)), upperScore=rep(as.double(NA),length(pi_levels)))
habitsim$PropIncongr <- pi_levels
habitsim$meanScore = (1-pi_levels)*habit_scr_grp$ScorePerBlock
habitsim$lowerScore <- habitsim$meanScore - habit_scr_grp$sd
habitsim$upperScore <- habitsim$meanScore + habit_scr_grp$sd
GDsim<-data.frame(PropIncongr =rep(as.double(NA),length(pi_levels)), meanScore = rep(as.double(NA),length(pi_levels)), lowerScore=rep(as.double(NA),length(pi_levels)), upperScore=rep(as.double(NA),length(pi_levels)))
GDsim$PropIncongr <- pi_levels
GDsim$meanScore <- GD_scr_grp$ScorePerBlock
GDsim$lowerScore <- GD_scr_grp$ScorePerBlock - GD_scr_grp$sd
GDsim$upperScore <- GD_scr_grp$ScorePerBlock + GD_scr_grp$sd

# avg scores and average 'simulated' scores
plot_scores_sim <- ggplot() + #scale_y_continuous(limits = c(0.75, 1.1))+
  geom_line(data = habitsim, aes(x=PropIncongr, y=meanScore), color="cyan4")+
  geom_line(data = GDsim, aes(x=PropIncongr, y=meanScore), color="tomato")+
  geom_ribbon(data = habitsim, aes(x=PropIncongr, ymin=lowerScore, ymax=upperScore, xmin=0, xmax=Inf), fill="cyan4", alpha=.5)+
  geom_ribbon(data = GDsim, aes(x=PropIncongr, ymin=lowerScore, ymax=upperScore, xmin=0, xmax=Inf), fill="tomato", alpha=.5)+
  geom_line(data = score_average, aes(x=pi_levels, y=Score), size = 1) +
  geom_errorbar(data = score_average,aes(x=pi_levels,ymin=Score-se, ymax=Score+se), width=0, size =1, position=position_dodge(.5)) +
  #geom_point(data = acc_average,aes(x=PropIncongr, y=Correct),stroke=1, position=position_dodge(.5), size = 3) +
  #geom_line(data = score_average2, aes(x=PropIncongr, y=Score, group = 1), color = "blue", size = 2, position=position_dodge(.5)) +
  xlab("Proportion Incongruent") + ylab("avg score per block")+
  ggtitle("Average Point Yield")+
  #scale_color_manual(values=c("black", "white") ) + scale_shape_manual(values=c(21, 21)) + scale_fill_manual(values=c("black", "white"))+
  theme_sage_simple(base_size=20) 
plot_scores_sim

train_RT_line <- ggplot(RT_grp, aes(x=Block, y=Reaction.Time)) + #scale_y_continuous(limits = c(0.75, 1.1))+
  geom_line(data=RT_allsubj, aes(x=Block, y=Reaction.Time, group=Participant.Private.ID), size = .5, alpha=0.6, color="Grey")+
  geom_line(data=RT_grp,aes(x=Block, y=Reaction.Time, group=1), size = 2, position=position_dodge(.5), stat="identity") +
  geom_errorbar(data=RT_grp, aes(ymin=Reaction.Time-se, ymax=Reaction.Time+se), width=0, size =1, position=position_dodge(.5)) +
  geom_point(stroke=1, position=position_dodge(.5), size = 3) +
  xlab("Block") + ylab("RT (ms.)")+ggtitle("RT over blocks")+
  #scale_color_manual(values=c("black", "white") ) + scale_shape_manual(values=c(21, 21)) + scale_fill_manual(values=c("black", "white"))+
  theme_sage_simple(base_size=20) 
train_RT_line


## the main analysis ####
acc_pctest <- glmer(Correct~PropIncongr*Congruency+(1|ID), data=D_pctest, family=binomial)
acc_pctest <- mixed(Correct~PropIncongr*Congruency+(1|ID), data=D_pctest, family=binomial, method = "PB")

acc_pctest <- glmer(Correct~PropIncongr*Congruency+(1|ID), data=D_pctest, family=binomial)
summary(acc_pctest)
Anova(acc_pctest)
rt_pctest <- lmer(Reaction.Time~PropIncongr*Congruency+(1|Participant.Private.ID), data=D_pctest)
summary(rt_pctest)

# En daar een figuur van

acc_pctest <- SageSummarySEwithin(D_pctest, dv = "Correct", wv = c("PropIncongr", "Congruency"), subv = "ID")
acc_pctest_allsub <- SageSummarySEwithin(D_pctest, dv = "Correct", wv = c("PropIncongr", "Congruency", "Participant.Private.ID"), subv = "ID")
rt_pctest <- SageSummarySEwithin(D_pctest, dv = "Reaction.Time", wv = c("PropIncongr", "Congruency"), subv = "ID")
rt_pctest_allsub <- SageSummarySEwithin(D_pctest, dv = "Reaction.Time", wv = c("PropIncongr", "Congruency", "Participant.Private.ID"), subv = "ID")


PCTest_line <- ggplot(acc_pctest, aes(x=PropIncongr, y=Correct, group=Congruency)) + #scale_y_continuous(limits = c(0.75, 1.1))+
  geom_line(aes(x=PropIncongr, y=Correct, group=Congruency, color=Congruency), size = 1)+
  geom_errorbar(aes(ymin=Correct-se, ymax=Correct+se), width=0) +
  geom_point(data = acc_pctest_allsub,  aes(x=PropIncongr, y=Correct, group=Congruency, color=Congruency), size =.3, alpha=0.3, stroke=1, position=position_dodge(.5)) +
  xlab("Proportion Incongruent") + ylab("Accuracy")+ggtitle("Accuracy over blocks")+
  theme_sage_simple(base_size=20) +
  scale_color_manual(values=c("cyan4", "tomato") ) + scale_fill_manual(values=c("cyan4", "tomato"))
PCTest_line 

PCTest_rt <- ggplot() + #scale_y_continuous(limits = c(0.75, 1.1))+
  geom_line(data=rt_pctest, aes(x=PropIncongr, y=Reaction.Time, group=Congruency, color=Congruency), color="Black", size = 1)+
  geom_errorbar(data=rt_pctest, aes(x=PropIncongr*10, y=Score/0.06, ymax=Reaction.Time+se, group=Congruency),color="Black", width=0) +
  #geom_rect(data = habit_rt_grp, aes(ymin=(ScorePerBlock-se)/0.06, ymax=(ScorePerBlock+se)/0.06, xmin=0, xmax=Inf), fill="pink", alpha=.5)+
  #geom_rect(data = GD_rt_grp, aes(ymin=Reaction.Time-sd, ymax=Reaction.Time+sd, xmin=0, xmax=Inf), fill="gold", alpha=.5)+
  geom_point(data = rt_pctest_allsub,  aes(x=PropIncongr, y=Reaction.Time, group=Congruency, color=Congruency), color= "grey", size =.3, alpha=0.3, stroke=1, position=position_dodge(.5)) +
  xlab("Proportion Incongruent") + ylab("RT")+ggtitle("RT over blocks")+
  theme_sage_simple(base_size=20) +
  scale_color_manual(values=c("cyan4", "tomato") ) + scale_fill_manual(values=c("cyan4", "tomato"))
PCTest_rt

# 
acc_long <- SageSummarySEwithin(D_pctest, dv="Correct", wv = c("PropIncongr", "Congruency"), bv="ID", subv = "ID")
acc_long <- select(acc_long, -N,-sd,-se,-ci)
acc_long$PropIncongr <- numeric(acc_long$PropIncongr)


acc_test_rm <- anova_test(data=acc_long, dv=Correct, wid=ID, within = c(PropIncongr, Congruency), effect.size = "pes")
acc_test_rm
tukey_hsd(acc_test_rm)

acc_test_lm <- lm(Correct~Congruency*PropIncongr, data=acc_long)
summary(acc_test_lm)

## Single-subject look at strategy and optimality ####
# a little function that looks per person

idlist <- unique(Dh$ID)
for (sub in 1:length(idlist)) {
  dd <- filter(D, ID==idlist[sub])
  # now we extract
  dd$ID <- factor(dd$ID)
  dd$Trial <- 1:nrow(dd)
  alld <- bind_rows(alld, dd)
  rm(dd)
}

## Habits? ---- NOPE
Dh <- D %>% filter(Condition=="Devaluation") %>% select(ID, Congruency, Correct, Reaction.Time)
idlist <- unique(Dh$ID)
alld <- data.frame(matrix(ncol = ncol(Dh), nrow = 0))
colnames(alld)<- colnames(Dh)
alld$ID <- factor(alld$ID)
alld$Congruency <- factor(alld$Congruency)

for (sub in 1:length(idlist)) {
  dd <- filter(Dh, ID==idlist[sub])
  dd$ID <- factor(dd$ID)
  dd$Trial <- 1:nrow(dd)
  alld <- bind_rows(alld, dd)
  rm(dd)
}
Dh <- alld
acc_deval_all <- SageSummarySEwithin(Dh, dv="Correct", wv=c("Trial", "Congruency"), subv="ID")
acc_deval <- SageSummarySEwithin(Dh, dv="Correct", wv=c("Congruency"), bv="ID", subv="ID")
rt_deval <- SageSummarySEwithin(filter(Dh, Correct==1), dv="Reaction.Time", bv="ID",wv=c("Congruency"), subv="ID")
deval <- acc_deval %>%
  select(ID, Congruency, Correct) %>%
  bind_cols(rt_deval[3])
rm(acc_deval, rt_deval)

# plot throughout the block
habit_overtime <- ggplot() + scale_y_continuous(limits = c(0.5, 1.1))+
  geom_line(data=acc_deval_all, aes(x=Trial, y=Correct, group=Congruency, color=Congruency), size = 1)+
  geom_errorbar(data=acc_deval_all, aes(x=Trial, ymin=Correct-se, ymax=Correct+se, group=Congruency, color=Congruency), width=0) +
  #geom_rect(data = habit_rt_grp, aes(ymin=Reaction.Time-sd, ymax=Reaction.Time+sd, xmin=0, xmax=Inf), fill="pink", alpha=.5)+
  #geom_rect(data = GD_rt_grp, aes(ymin=Reaction.Time-sd, ymax=Reaction.Time+sd, xmin=0, xmax=Inf), fill="gold", alpha=.5)+
  #geom_point(data = rt_pctest_allsub,  aes(x=PropIncongr, y=Reaction.Time, group=Congruency, color=Congruency), color= "grey", size =.3, alpha=0.3, stroke=1, position=position_dodge(.5)) +
  xlab("Trial") + ylab("Accuracy")+ggtitle("Devaluation insensitivity")+
  theme_sage_simple(base_size=20) +
  scale_color_manual(values=c("tomato", "cyan4") ) + scale_fill_manual(values=c("tomato","cyan4"))
habit_overtime

# DI per subject
DI_wide <- deval %>% 
  pivot_wider(id_cols = "ID", names_from = "Congruency", values_from = c("Correct", "Reaction.Time"))
DI_wide$DI_acc <- DI_wide$Correct_Valued - DI_wide$Correct_Devalued
DI_wide$DI_rt <- DI_wide$Reaction.Time_Valued - DI_wide$Reaction.Time_Devalued

# ----------------------------------------------------------------------------- -
# OLD / commented ----
# als je data wil wegschrijven, eventueel om in jasp te openen
# geef zelf een naam die duideliijk is voor jezelf
# write.csv(Acc_persubj, 'noa_pctest_persubj.csv')

# ------------------------------------------------------------------------------ ---
# Dit deel in de comments heb ik gebruikt om van de ruwe files de csv file te maken
# # read in the data
# D1 <- read.csv('data/data_exp_86658-v11_task-pufp.csv', sep=',')
# D2 <- read.csv('data/data_exp_86658-v11_task-9zau.csv', sep=',')
# D3 <- read.csv('data/data_exp_86658-v11_task-vwnp.csv', sep=',')
# D3$Reaction.Time <- as.double(D3$Reaction.Time)
# D1$Reaction.Time <- as.double(D1$Reaction.Time)
# D2$Reaction.Time <- as.double(D2$Reaction.Time)
# D_ <- bind_rows(D1, D2, D3)
# rm(D1, D2, D3)
# #
# # # clean so that the relevant lines and columns are left
# # # name of the columns you need for the analysis
# ColumnsToKeep <- c("Participant.Private.ID", "Trial.Number", "Spreadsheet.Row","Spreadsheet.Name", "StimLeft", "StimRight", "Congruency", "Condition","randomise_trials", "Screen.Name", "display",  "Response", "ProbabilisticAccuracy","PropIncongr", "Attempt","Correct", "Reaction.Time")
# D_ <- D_ %>% select(all_of(ColumnsToKeep))
# # get rid of rows that are not from a trial
# D <-filter(D_, Screen.Name=="Choice")
# D$Block <- D$randomise_trials
# rm(D_)
# 
# # make columns for day and counterbalancing group
# D$CBgrp <- substring(D$Spreadsheet.Name, 4, 4)
# D$Day <- substring(D$Spreadsheet.Name, 9, 9)
# 
# 
# # write away the csv
# write.csv(D, 'noa_alldata.csv')
