# Analysis script for Ophelie's thesis


# load some basic libraries
# Ophelie: deze zal je wellicht moeten installeren
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

library(ggplot2); theme_set(theme_sage_contexthabit())

#source("SageDataCleanUtils.R")

# de data inlezen (zorg dat je in de juiste folder zit/zoekt)
# D <- read.csv('ophelie_alldata.csv', sep=',')
# 
# # voorbeeld van hoe je kan filteren
# D_train <- filter(D, Condition != "mixed")
# 
# #voorbeeld van hoe je kan samenvatten per persoon
# acc_train_persubj <- summarySE(D_train, measurevar = "Correct", groupvars = c("Participant.Private.ID", "Block", "Condition"))
# rt_train_persubj <- summarySE(D_train, measurevar = "Reaction.Time", groupvars = c("Participant.Private.ID", "Block", "Condition"))
# 
# # gewoon per conditie (gemiddelde over personen heen)
# acc_train_avg <- summarySE(D_train, measurevar = "Correct", groupvars = c("Block", "Condition"))
# 
# # voorbeeld van hoe je data kan wegschrijven. Moet je bvb doen om in jasp een analyse te maken waarbij 
# write.csv(acc_train_persubj, 'train_correct_pergroep.csv')

# ---------------------------------------------------------------------------------
# Dit deel in de comments heb ik gebruikt om van de ruwe files de csv file te maken
# read in the data

D1 <- read.csv('data_ophelie/data_exp_86657-v14_task-wzds.csv', sep=',')
D2 <- read.csv('data_ophelie/data_exp_86657-v14_task-9zau.csv', sep=',')
D3 <- read.csv('data_ophelie/data_exp_86657-v12_task-wzds.csv', sep=',')
D4 <- read.csv('data_ophelie/data_exp_86657-v12_task-9zau.csv', sep=',')

D1$Reaction.Time <- as.double(D1$Reaction.Time)
D2$Reaction.Time <- as.double(D2$Reaction.Time)
D3$Reaction.Time <- as.double(D3$Reaction.Time)
D4$Reaction.Time <- as.double(D4$Reaction.Time)
D_ <- bind_rows(D1, D2, D3, D4)
rm(D1, D2, D3, D4)
#
# # clean so that the relevant lines and columns are left ----
# # name of the columns you need for the analysis
ColumnsToKeep <- c("Participant.Private.ID", "Trial.Number", "Spreadsheet.Row","Spreadsheet.Name", "StimLeft", "StimRight","ANSWER", "ValueLeft", "ValueRight", "ValueANSWER", "Condition","randomise_trials", "Screen.Name", "display",  "Response","Attempt","Correct", "Reaction.Time")
D_ <- D_ %>% select(all_of(ColumnsToKeep))
# get rid of rows that are not from a trial
D <-filter(D_, Screen.Name=="Choice", Participant.Private.ID!=7724286)
D$Block <- D$randomise_trials
D$ID <- factor(D$Participant.Private.ID)
rm(D_)

# make columns for day and counterbalancing group
D$CBgrp <- substring(D$Spreadsheet.Name, 4, 4)
D$Day <- substring(D$Spreadsheet.Name, 9, 9)
D$Day <- as.numeric(D$Day)

# make a column that is the overall block number
D$BlockPerDay <- D$Block
D$Block <- (D$Day-1)*18 + D$BlockPerDay

# make some value-related columns (chosen and nonchosen)
D <- D %>% rowwise() %>% mutate(ValueMin = min(c(ValueLeft, ValueRight)))
D <- D %>% rowwise() %>% mutate(ValueMax = max(c(ValueLeft, ValueRight)))
D$ValueChosen <- ifelse(D$Correct==1, D$ValueMax, D$ValueMin)
D$ValueNonChosen <- ifelse(D$Correct==1, D$ValueMin, D$ValueMax)

# a dependent variable that is choice (not accuracy) - I will go w left or right
D$CHOICEleft0 <- ifelse(D$Response == D$StimLeft, 0, 1)
D$CHOICEleft0[D$Response == 'Missed'] <- NA

# # Congruency on Correct-incorrect basis
# D$TrainValueCorrect[D$ValueMax<5] <- (D$ValueMax[D$ValueMax<5]-1)/3
# D$TrainValueCorrect[D$ValueMax>4] <- (D$ValueMax[D$ValueMax>4]-5)/3
# 
# D$TrainValueIncorrect[D$ValueMin<5] <- (D$ValueMin[D$ValueMin<5]-1)/3
# D$TrainValueIncorrect[D$ValueMin>4] <- (D$ValueMin[D$ValueMin>4]-5)/3
# # do this for the chosen value (intermediate step for seeing if they picked the trained best)
# D$TrainValueChosen[D$ValueMax<5] <- (D$ValueChosen[D$ValueMax<5]-1)/3
# D$TrainValueChosen[D$ValueMax>4] <- (D$ValueChosen[D$ValueMax>4]-5)/3
# D$MixedValueCorrect <- (D$ValueMax-1)/7
# D$MixedValueIncorrect <- (D$ValueMin-1)/7

# Congruency on Correct-incorrect basis
D$TrainValueMax[D$ValueMax<5] <- D$ValueMax[D$ValueMax<5]
D$TrainValueMax[D$ValueMax>4] <- D$ValueMax[D$ValueMax>4]-4

D$TrainValueMin[D$ValueMin<5] <- D$ValueMin[D$ValueMin<5]
D$TrainValueMin[D$ValueMin>4] <- D$ValueMin[D$ValueMin>4]-4
# do this for the chosen value (intermediate step for seeing if they picked the trained best)
D$TrainValueChosen[D$ValueMax<5] <- D$ValueChosen[D$ValueMax<5]
D$TrainValueChosen[D$ValueMax>4] <- D$ValueChosen[D$ValueMax>4]-4


D$TrainValueDiff <- D$TrainValueMax - D$TrainValueMin
D$MixedValueDiff <- D$ValueMax - D$ValueMin
# alternative "Correct" -- correct in accordance to the training
D$TrainCorrect <- ifelse(D$TrainValueMax ==D$TrainValueChosen, 1, 0)
  # if chosen value is the same as 

#center the value differences
D$CNTRDTrainValueDiff = D$TrainValueDiff-mean(D$TrainValueDiff)
D$CNTRDMixedValueDiff = D$MixedValueDiff-mean(D$MixedValueDiff)

#old versus new combos
D$TrainedPair <- NA
D$TrainedPair[D$ValueMax<5] <- "old"
D$TrainedPair[D$ValueMin>4] <- "old"
D$TrainedPair[(D$ValueMin<5)&(D$ValueMax>4)&(D$TrainValueDiff<0)] <- "new_incongruent"
D$TrainedPair[(D$ValueMin<5)&(D$ValueMax>4)&(D$TrainValueDiff>0)] <- "new_congruent"
D$TrainedPair[(D$ValueMin<5)&(D$ValueMax>4)&(D$TrainValueDiff==0)] <- "new_same"

D_train <- filter(D, Condition != "mixed")
D_mixed <- filter(D, Condition == "mixed")
## change over blocks
D_mixed$BlockRescaled <- D_mixed$Block-33

# compute a regressor for the number of responses (per person) during training
freq<- D_train %>% summarySE(measurevar= "Correct", groupvars = c("ID", "Response", "CBgrp")) %>%
  select("ID", "Response", "CBgrp", "N")

D_mixed <- D_mixed %>%
  left_join(freq, by=c("StimLeft" = "Response", "ID", "CBgrp")) %>%
  rename(LeftHist=N) %>%
  left_join(freq, by=c("StimRight" = "Response", "ID", "CBgrp")) %>%
  rename(RightHist=N) %>%
  left_join(freq, by=c("Response" = "Response", "ID", "CBgrp")) %>%
  rename(ChosenHist=N)
D_mixed$NonChosenHist <- ifelse(D_mixed$StimLeft == D_mixed$Response, D_mixed$RightHist, D_mixed$LeftHist)
D_mixed$CHOICEHIST <- (D_mixed$ChosenHist - D_mixed$NonChosenHist)/100
D_mixed$CHOICEHISTleft0 <- (D_mixed$RightHist - D_mixed$LeftHist)/100

# ~~~~~~~~~~~~~~
## Analyses ####
# ~~~~~~~~~~~~~~


## ~ paired t tests (histograms show more of less normal distribution) ----
newcon <- filter(accuracy_persub, TrainedPair=="new_congruent")
newincon <- filter(accuracy_persub, TrainedPair=="new_incongruent")
newsame <- filter(accuracy_persub, TrainedPair=="new_same")
old <- filter(accuracy_persub, TrainedPair=="old")

newcon1 <- filter(accuracy_persubandblock, TrainedPair=="new_congruent", BlockRescaled==0)
newincon1 <- filter(accuracy_persubandblock, TrainedPair=="new_incongruent", BlockRescaled==0)
newsame1 <- filter(accuracy_persubandblock, TrainedPair=="new_same", BlockRescaled==0)
old1 <- filter(accuracy_persubandblock, TrainedPair=="old", BlockRescaled==0)

t.test(newcon$Correct, newincon$Correct, 'two.sided', paired = TRUE)
t.test(newsame$Correct, newincon$Correct, 'two.sided', paired = TRUE)
t.test(newsame$Correct, newcon$Correct, 'two.sided', paired = TRUE)
t.test(old$Correct, newincon$Correct, 'two.sided', paired = TRUE)
t.test(old$Correct, newsame$Correct, 'two.sided', paired = TRUE)

t.test(newcon1$Correct, newincon1$Correct, 'two.sided', paired = TRUE)
t.test(newsame1$Correct, newincon1$Correct, 'two.sided', paired = TRUE)
t.test(newsame1$Correct, newcon1$Correct, 'two.sided', paired = TRUE)
t.test(old1$Correct, newincon1$Correct, 'two.sided', paired = TRUE)
t.test(old1$Correct, newsame1$Correct, 'two.sided', paired = TRUE)

## ~ mm across the combined condition ----
acc1 <- glmer(Correct~CNTRDTrainValueDiff + (1|ID), data=D_mixed, family=binomial)
summary(acc1) # BIC: 22121
acc2 <- glmer(Correct~CNTRDMixedValueDiff + (1|ID), data=D_mixed, family=binomial)
summary(acc2) # BIC: 21371 **
acc3 <- glmer(Correct~CNTRDTrainValueDiff*CNTRDMixedValueDiff + (1|ID), data=D_mixed, family=binomial)
summary(acc3) # BIC: 20911

## ~ mm on just the first block ----
acc1B1 <- glmer(Correct~CNTRDTrainValueDiff + (1|ID), data=filter(D_mixed, BlockRescaled==0), family=binomial)
summary(acc1B1) #BIC: 4073 **
acc2B1 <- glmer(Correct~CNTRDMixedValueDiff + (1|ID), data=filter(D_mixed, BlockRescaled==0), family=binomial)
summary(acc2B1) #BIC: 4282
acc3B1 <- glmer(Correct~CNTRDMixedValueDiff*CNTRDTrainValueDiff + (1|ID), data=filter(D_mixed, BlockRescaled==0), family=binomial)
summary(acc3B1) #BIC: 3941

## ~ mm only congruent versus incongruent ----
acc_onlymixedval <- glmer(Correct~CNTRDMixedValueDiff + (1|ID), data=filter(D_mixed, TrainedPair!="old", TrainedPair!="new_same"), family=binomial)
summary(acc_onlymixedval)
acc_congruency<- glmer(Correct~Congruency*CNTRDMixedValueDiff + (1|ID), data=filter(D_mixed, TrainedPair!="old", TrainedPair!="new_same"), family=binomial)
summary(acc_congruency)
# better w congruency added
anova(acc_onlymixedval, acc_congruency)

## ~ mm effect of congruency over time ----
acc_congruencyovertime<- glmer(Correct~Congruency*BlockRescaled+CNTRDMixedValueDiff + (1|ID), data=filter(D_mixed, TrainedPair!="old", TrainedPair!="new_same"), family=binomial)
summary(acc_congruencyovertime)
Anova(acc_congruencyovertime)
testInteractions(acc_congruencyovertime, pairwise=c("Congruency", "BlockRescaled"), residual =  "BlockRescaled")

# ~ posthoc: isolating the first block ----
## isolate the first block
acc_congruencyB1<- glmer(Correct~Congruency+CNTRDMixedValueDiff + (1|ID), data=filter(D_mixed, TrainedPair!="old", TrainedPair!="new_same", BlockRescaled==0), family=binomial)
summary(acc_congruencyB1)
acc_congruencyB2<- glmer(Correct~Congruency+CNTRDMixedValueDiff + (1|ID), data=filter(D_mixed, TrainedPair!="old", TrainedPair!="new_same", BlockRescaled==1), family=binomial)
summary(acc_congruencyB2)
acc_congruencyB3<- glmer(Correct~Congruency+CNTRDMixedValueDiff + (1|ID), data=filter(D_mixed, TrainedPair!="old", TrainedPair!="new_same", BlockRescaled==2), family=binomial)
summary(acc_congruencyB3)
acc_congruencyB4<- glmer(Correct~Congruency+CNTRDMixedValueDiff + (1|ID), data=filter(D_mixed, TrainedPair!="old", TrainedPair!="new_same", BlockRescaled==3), family=binomial)
summary(acc_congruencyB4)
acc_congruencyB5<- glmer(Correct~Congruency+CNTRDMixedValueDiff + (1|ID), data=filter(D_mixed, TrainedPair!="old", TrainedPair!="new_same", BlockRescaled==4), family=binomial)
summary(acc_congruencyB5)
acc_congruencyB6<- glmer(Correct~Congruency+CNTRDMixedValueDiff + (1|ID), data=filter(D_mixed, TrainedPair!="old", TrainedPair!="new_same", BlockRescaled==5), family=binomial)
summary(acc_congruencyB6)

rt1 <- mixed(Reaction.Time~CNTRDTrainValueDiff + (1|ID), data=D_mixed)
summary(rt1)
rt2 <- lmer(Reaction.Time~CNTRDMixedValueDiff + (1|ID), data=D_mixed)
summary(rt2)
rt3 <- lmer(Reaction.Time~CNTRDMixedValueDiff*CNTRDTrainValueDiff + (1|ID), data=D_mixed)
summary(rt3)

# ~~~~~~~~~~~~~~
## Figures ####
# ~~~~~~~~~~~~~~

# ~ Training

acc_train <- SageSummarySEwithin(D_train, dv="Correct", wv=c("Block", "Condition"), subv="ID")
rt_train <- SageSummarySEwithin(D_train, dv="Reaction.Time", wv=c("Block", "Condition"), subv="ID")

traincols <- c("lightsteelblue","steelblue")
acc_block_train <- ggplot(acc_train, aes(x=Block, y=Correct, group=Condition)) + scale_y_continuous(limits = c(0.5, 1.1))+
  geom_line(aes(x=Block, y=Correct, group=Condition, color = Condition), size = 1, position=position_dodge(.4), stat="identity") +
  geom_errorbar(aes(ymin=Correct-se, ymax=Correct+se, color = Condition), width=0, position=position_dodge(.4), size = 1) +
  geom_point(aes(fill=Condition, group=Condition, color = Condition),stroke =1, position=position_dodge(.4), stat="identity") +
  xlab("Block") + ylab("Proportion correct")+
  scale_color_manual(values=traincols) + scale_fill_manual(values=traincols)+
  theme_sage_simple()
acc_block_train
rt_block_train <- ggplot(rt_train, aes(x=Block, y=Reaction.Time, group=Condition)) +# scale_y_continuous(limits = c(0.5, 1.1))+
  geom_line(aes(x=Block, y=Reaction.Time, group=Condition, color = Condition), size = 1, position=position_dodge(.4), stat="identity") +
  geom_errorbar(aes(ymin=Reaction.Time-se, ymax=Reaction.Time+se, color = Condition), width=0, position=position_dodge(.4), size = 1) +
  geom_point(aes(fill=Condition, group=Condition, color = Condition),stroke =1, position=position_dodge(.4), stat="identity") +
  xlab("Block") + ylab("rt")+
  scale_color_manual(values=traincols) + scale_fill_manual(values=traincols)+
  theme_sage_simple()
rt_block_train

## ~ Mixed condition
accuracy_persub <- SageSummarySEwithin(filter(D_mixed,Attempt==1), dv="Correct", wv = c("TrainedPair"), bv = "ID", subv = "ID")
accuracy_grped <- SageSummarySEwithin(filter(D_mixed,Attempt==1), dv="Correct", wv = c("TrainedPair"), subv = "ID")
accuracy_perblock <- SageSummarySEwithin(filter(D_mixed,Attempt==1), dv="Correct", wv = c("BlockRescaled", "TrainedPair"), subv = "ID")
accuracy_persubandblock <- SageSummarySEwithin(filter(D_mixed,Attempt==1), dv="Correct", wv = c("BlockRescaled", "TrainedPair"), bv = "ID", subv = "ID")

rt_grped <- SageSummarySEwithin(filter(D_mixed,Attempt==1, Correct==1), dv="Reaction.Time", wv = c("TrainedPair"), subv = "ID")
rt_persub <- SageSummarySEwithin(filter(D_mixed,Attempt==1, Correct==1), dv="Reaction.Time", wv = c("TrainedPair"), bv = "ID", subv = "ID")
rt_perblock <- SageSummarySEwithin(filter(D_mixed,Attempt==1, Correct==1), dv="Reaction.Time", wv = c("BlockRescaled", "TrainedPair"), subv = "ID")
rt_persubandblock <- SageSummarySEwithin(filter(D_mixed,Attempt==1, Correct==1), dv="Reaction.Time", wv = c("BlockRescaled", "TrainedPair"), bv = "ID", subv = "ID")
rt_perblock_incl_incorr <- rt_perblock <- SageSummarySEwithin(filter(D_mixed,Attempt==1), dv="Reaction.Time", wv = c("BlockRescaled", "TrainedPair", "Correct"), subv = "ID")


# ~ the ones about accuracy ----
condcols <- c("chartreuse4","tomato", "grey", "wheat")
acc_point <- ggplot(accuracy_grped, aes(x=TrainedPair, y=Correct)) + #scale_y_continuous(limits = c(0.4, 1.1))+
  geom_boxplot(data= accuracy_persub, aes(x=TrainedPair, y=Correct))+
  geom_errorbar(aes(ymin=Correct-se, ymax=Correct+se), width=0, position=position_dodge(.4), size = 3) +
  geom_point(aes(color=TrainedPair),stroke =1, size = 3, position=position_dodge(.4), stat="identity") +
  geom_point(data = accuracy_persub, aes(fill=TrainedPair, group=TrainedPair, color = TrainedPair),stroke =.5, alpha=.3, position=position_jitter(.25), stat="identity") +
  #facet_grid(.~BlockRescaled)+
  xlab("Trained & Congruency") + ylab("Proportion correct")+
  scale_color_manual(values=condcols) + scale_fill_manual(values=condcols)+
  theme_sage_simple()
acc_point

# ~~ acc_point_isolate_outlier ----
  ggplot(accuracy_grped, aes(x=TrainedPair, y=Correct)) + #scale_y_continuous(limits = c(0.4, 1.1))+
  geom_boxplot(data= accuracy_persub, aes(x=TrainedPair, y=Correct))+
  geom_errorbar(aes(ymin=Correct-se, ymax=Correct+se), width=0, position=position_dodge(.4), size = 3) +
  #geom_point(aes(color=TrainedPair),stroke =1, size = 3, position=position_dodge(.4), stat="identity") +
  geom_point(data = accuracy_persub, aes(fill=TrainedPair, group=TrainedPair, color = ID),stroke =1, position=position_jitter(.25), stat="identity") +
  #facet_grid(.~BlockRescaled)+
  xlab("Trained & Congruency") + ylab("Proportion correct")+
  ggtitle("Detect outliers")+
  #scale_color_manual(values=condcols) + scale_fill_manual(values=condcols)+
  theme_sage_simple()


acc_pointB1 <- ggplot(filter(accuracy_perblock, BlockRescaled==0), aes(x=TrainedPair, y=Correct)) + scale_y_continuous(limits = c(0.15, 1.1))+
  geom_boxplot(data= filter(accuracy_persubandblock, BlockRescaled==0), aes(x=TrainedPair, y=Correct))+
  geom_errorbar(aes(ymin=Correct-se, ymax=Correct+se), width=0, position=position_dodge(.4), size = 3) +
  geom_point(aes(color=TrainedPair),stroke =1, size = 3, position=position_dodge(.4), stat="identity") +
  geom_point(data= filter(accuracy_persubandblock, BlockRescaled==0), aes(fill=TrainedPair, group=TrainedPair, color = TrainedPair),stroke =.5, alpha=.5, position=position_jitter(.25), stat="identity") +
  #facet_grid(.~BlockRescaled)+
  xlab("Trained & Congruency") + ylab("Prop. correct")+ggtitle("Accuracy in Block 1")+
  scale_color_manual(values=condcols) + scale_fill_manual(values=condcols)+
  theme_sage_simple()
acc_pointB1

acc_pointB5 <- ggplot(filter(accuracy_perblock, BlockRescaled==5), aes(x=TrainedPair, y=Correct)) + scale_y_continuous(limits = c(0.15, 1.1))+
  geom_boxplot(data= filter(accuracy_persubandblock, BlockRescaled==5), aes(x=TrainedPair, y=Correct))+
  geom_errorbar(aes(ymin=Correct-se, ymax=Correct+se), width=0, position=position_dodge(.4), size = 3) +
  geom_point(aes(color=TrainedPair),stroke =1, size = 3, position=position_dodge(.4), stat="identity") +
  geom_point(data= filter(accuracy_persubandblock, BlockRescaled==5), aes(fill=TrainedPair, group=TrainedPair, color = TrainedPair),stroke =.5, alpha=.5, position=position_jitter(.25), stat="identity") +
  #facet_grid(.~BlockRescaled)+
  xlab("Trained & Congruency") + ylab("Prop. correct")+ggtitle("Accuracy in Block 5")+
  scale_color_manual(values=condcols) + scale_fill_manual(values=condcols)+
  theme_sage_simple()
acc_pointB5

condcols <- c("chartreuse4","tomato", "grey", "wheat")
acc_block_effect <- ggplot(accuracy_perblock, aes(x=BlockRescaled, y=Correct, group=TrainedPair)) + #scale_y_continuous(limits = c(0.5, 1.1))+
  geom_line(aes(x=BlockRescaled, y=Correct, group=TrainedPair, color = TrainedPair), size = 2, position=position_dodge(.4), stat="identity") +
  geom_errorbar(aes(ymin=Correct-se, ymax=Correct+se, color = TrainedPair), width=0, position=position_dodge(.4), size = 2) +
  geom_point(aes(fill=TrainedPair, group=TrainedPair, color = TrainedPair),stroke =2, position=position_dodge(.4), stat="identity") +
  geom_point(data = accuracy_persubandblock, aes(fill=TrainedPair, group=TrainedPair, color = TrainedPair),stroke =.5, alpha=.1, position=position_jitter(.25), stat="identity") +
  #facet_grid(.~TrainedPair)+
  xlab("Block") + ylab("Proportion correct")+
  scale_color_manual(values=condcols) + scale_fill_manual(values=condcols)+
  theme_sage_simple()
acc_block_effect

# ~ the ones about RT ----
rt_point <- ggplot(rt_grped, aes(x=TrainedPair, y=Reaction.Time)) + #scale_y_continuous(limits = c(0.4, 1.1))+
  geom_boxplot(data= rt_persub, aes(x=TrainedPair, y=Reaction.Time))+
  geom_errorbar(aes(ymin=Reaction.Time-se, ymax=Reaction.Time+se), width=0, position=position_dodge(.4), size = 3) +scale_y_continuous(limits = c(300, 2000))+
  geom_point(aes(color=TrainedPair),stroke =1, size = 3, position=position_dodge(.4), stat="identity") +
  geom_point(data = rt_persub, aes(fill=TrainedPair, group=TrainedPair, color = TrainedPair),stroke =.5, alpha=.3, position=position_jitter(.25), stat="identity") +
  #facet_grid(.~BlockRescaled)+
  xlab("Trained & Congruency") + ylab("RT (ms)")+
  scale_color_manual(values=condcols) + scale_fill_manual(values=condcols)+
  theme_sage_simple()
rt_point

rt_pointB1 <- ggplot(filter(rt_perblock, BlockRescaled==0), aes(x=TrainedPair, y=Reaction.Time)) + scale_y_continuous(limits = c(300, 2000))+
  geom_boxplot(data= filter(rt_persubandblock, BlockRescaled==0), aes(x=TrainedPair, y=Reaction.Time))+
  geom_errorbar(aes(ymin=Reaction.Time-se, ymax=Reaction.Time+se), width=0, position=position_dodge(.4), size = 3) +
  geom_point(aes(color=TrainedPair),stroke =1, size = 3, position=position_dodge(.4), stat="identity") +
  geom_point(data= filter(rt_persubandblock, BlockRescaled==0), aes(fill=TrainedPair, group=TrainedPair, color = TrainedPair),stroke =.5, alpha=.5, position=position_jitter(.25), stat="identity") +
  #facet_grid(.~BlockRescaled)+
  xlab("Trained & Congruency") + ylab("RT")+ggtitle("RT in block 1")+
  scale_color_manual(values=condcols) + scale_fill_manual(values=condcols)+
  theme_sage_simple()
rt_pointB1

rt_pointB5 <- ggplot(filter(rt_perblock, BlockRescaled==5), aes(x=TrainedPair, y=Reaction.Time)) + scale_y_continuous(limits = c(300, 2000))+
  geom_boxplot(data= filter(rt_persubandblock, BlockRescaled==5), aes(x=TrainedPair, y=Reaction.Time))+
  geom_errorbar(aes(ymin=Reaction.Time-se, ymax=Reaction.Time+se), width=0, position=position_dodge(.4), size = 3) +
  geom_point(aes(color=TrainedPair),stroke =1, size = 3, position=position_dodge(.4), stat="identity") +
  geom_point(data= filter(rt_persubandblock, BlockRescaled==5), aes(fill=TrainedPair, group=TrainedPair, color = TrainedPair),stroke =.5, alpha=.3, position=position_jitter(.25), stat="identity") +
  #facet_grid(.~BlockRescaled)+
  xlab("Trained & Congruency") + ylab("RT")+ggtitle("RT in block 5")+
  scale_color_manual(values=condcols) + scale_fill_manual(values=condcols)+
  theme_sage_simple()
rt_pointB5

rt_block_effect <- ggplot(rt_perblock, aes(x=BlockRescaled, y=Reaction.Time, group=TrainedPair)) + #scale_y_continuous(limits = c(0.5, 1.1))+
  geom_line(aes(x=BlockRescaled, y=Reaction.Time, group=TrainedPair, color = TrainedPair), size = 2, position=position_dodge(.4), stat="identity") +
  geom_errorbar(aes(ymin=Reaction.Time-se, ymax=Reaction.Time+se, color = TrainedPair), width=0, position=position_dodge(.4), size = 2) +
  geom_point(aes(fill=TrainedPair, group=TrainedPair, color = TrainedPair),stroke =2, position=position_dodge(.4), stat="identity") +
  geom_point(data = rt_persubandblock, aes(fill=TrainedPair, group=TrainedPair, color = TrainedPair),stroke =.5, alpha=.3, position=position_jitter(.25), stat="identity") +
  facet_grid(TrainedPair~.)+
  xlab("Block") + ylab("Response Time")+
  scale_color_manual(values=condcols) + scale_fill_manual(values=condcols)+
  theme_sage_simple()
rt_block_effect

## including the incorrect ones 
corrcols <- c("salmon","darkolivegreen2")
rt_block_effect <- ggplot(rt_perblock_incl_incorr, aes(x=BlockRescaled, y=Reaction.Time, group=Correct)) + #scale_y_continuous(limits = c(0.5, 1.1))+
  geom_line(aes(x=BlockRescaled, y=Reaction.Time, group=Correct, color = Correct), size = 2, position=position_dodge(.4), stat="identity") +
  geom_errorbar(aes(ymin=Reaction.Time-se, ymax=Reaction.Time+se, color = Correct), width=0, position=position_dodge(.4), size = 2) +
  geom_point(aes(fill=Correct, group=Correct, color = Correct),stroke =2, position=position_dodge(.4), stat="identity") +
  #geom_point(data = rt_persubandblock, aes(fill=TrainedPair, group=TrainedPair, color = TrainedPair),stroke =.5, alpha=.3, position=position_jitter(.25), stat="identity") +
  facet_grid(.~TrainedPair)+
  xlab("Block") + ylab("Response Time")+
  scale_color_manual(values=corrcols) + scale_fill_manual(values=corrcols)+
  theme_sage_simple()
rt_block_effect


# ~ in function of value/train differences
D_mixed$MixedValueDiffRound <- round(D_mixed$MixedValueDiff*100)
D_mixed$TrainValueDiffRound <- round(D_mixed$TrainValueDiff*100)
acc_diffs <- SageSummarySEwithin(D_mixed, dv="Correct", wv=c("MixedValueDiffRound", "TrainedPair"), subv="ID")
acc_diffs2 <- SageSummarySEwithin(D_mixed, dv="Correct", wv=c("MixedValueDiffRound"), subv="ID")
acc_histdiffs <- SageSummarySEwithin(D_mixed, dv="Correct", wv=c("TrainValueDiffRound", "TrainedPair"), subv="ID")
acc_histdiffs2 <- SageSummarySEwithin(D_mixed, dv="Correct", wv=c("TrainValueDiffRound"), subv="ID")

habacc_diffs <- SageSummarySEwithin(D_mixed, dv="TrainCorrect", wv=c("MixedValueDiffRound", "TrainedPair"), subv="ID")
habacc_diffs2 <- SageSummarySEwithin(D_mixed, dv="TrainCorrect", wv=c("MixedValueDiffRound"), subv="ID")
habacc_histdiffs <- SageSummarySEwithin(D_mixed, dv="TrainCorrect", wv=c("TrainValueDiffRound", "TrainedPair"), subv="ID")
habacc_histdiffs2 <- SageSummarySEwithin(D_mixed, dv="TrainCorrect", wv=c("TrainValueDiffRound"), subv="ID")

condcols <- c("chartreuse4","tomato", "grey", "Black")
acc_valuediff <- ggplot(acc_diffs, aes(x=MixedValueDiffRound, y=Correct, group=TrainedPair)) + #scale_y_continuous(limits = c(0.5, 1.1))+
  geom_line(data=acc_diffs2, aes(x=MixedValueDiffRound, y=Correct, group=1), line=1.5)+
  geom_line(aes(x=MixedValueDiffRound, y=Correct, group=TrainedPair, color = TrainedPair), size = 1.5, position=position_dodge(.4), stat="identity") +
  geom_errorbar(aes(ymin=Correct-se, ymax=Correct+se, color = TrainedPair), width=0, position=position_dodge(.4), size = 1.5) +
  geom_point(aes(fill=TrainedPair, group=TrainedPair, color = TrainedPair),stroke =2, position=position_dodge(.4), stat="identity") +
  #geom_point(data = acc_all_sub, aes(fill=TrainedPair, group=TrainedPair, color = TrainedPair),stroke =.5, alpha=.3, position=position_jitter(.25), stat="identity") +
  #facet_grid(.~TrainedPair)+
  xlab("ValueDifference") + ylab("Accuracy")+
  scale_color_manual(values=condcols) + scale_fill_manual(values=condcols)+
  theme_sage_simple()
acc_valuediff

acc_histdiff <- ggplot(acc_histdiffs, aes(x=TrainValueDiffRound, y=Correct, group=TrainedPair)) + #scale_y_continuous(limits = c(0.5, 1.1))+
  geom_line(data=acc_histdiffs2, aes(x=TrainValueDiffRound, y=Correct, group=1))+
  geom_line(aes(x=TrainValueDiffRound, y=Correct, group=TrainedPair, color = TrainedPair), size = 1.5, position=position_dodge(.4), stat="identity") +
  geom_errorbar(aes(ymin=Correct-se, ymax=Correct+se, color = TrainedPair), width=0, position=position_dodge(.4), size = 1.5) +
  geom_point(aes(fill=TrainedPair, group=TrainedPair, color = TrainedPair),stroke =2, position=position_dodge(.4), stat="identity") +
  #geom_point(data = acc_all_sub, aes(fill=TrainedPair, group=TrainedPair, color = TrainedPair),stroke =.5, alpha=.3, position=position_jitter(.25), stat="identity") +
  #facet_grid(.~TrainedPair)+
  xlab("Trained Difference") + ylab("Accuracy")+
  scale_color_manual(values=condcols) + scale_fill_manual(values=condcols)+
  theme_sage_simple()
acc_histdiff

habacc_histdiff <- ggplot(habacc_histdiffs, aes(x=TrainValueDiffRound, y=TrainCorrect, group=TrainedPair)) + #scale_y_continuous(limits = c(0.5, 1.1))+
  #geom_line(data=acc_histdiffs2, aes(x=TrainValueDiffRound, y=Correct, group=1))+
  geom_line(aes(x=TrainValueDiffRound, y=TrainCorrect, group=TrainedPair, color = TrainedPair), size = 1.5, position=position_dodge(.4), stat="identity") +
  geom_errorbar(aes(ymin=TrainCorrect-se, ymax=TrainCorrect+se, color = TrainedPair), width=0, position=position_dodge(.4), size = 1.5) +
  geom_point(aes(fill=TrainedPair, group=TrainedPair, color = TrainedPair),stroke =2, position=position_dodge(.4), stat="identity") +
  #geom_point(data = acc_all_sub, aes(fill=TrainedPair, group=TrainedPair, color = TrainedPair),stroke =.5, alpha=.3, position=position_jitter(.25), stat="identity") +
  #facet_grid(.~TrainedPair)+
  xlab("Trained Difference") + ylab("responses in line with training")+
  scale_color_manual(values=condcols) + scale_fill_manual(values=condcols)+
  theme_sage_simple()
habacc_histdiff

## looking not at correct, but trainedcorrect


rt_valuediff <- ggplot(acc_diffs, aes(x=MixedValueDiffRound, y=Correct, group=TrainedPair)) + #scale_y_continuous(limits = c(0.5, 1.1))+
  geom_line(data=acc_diffs2, aes(x=MixedValueDiffRound, y=Correct, group=1), line=1.5)+
  geom_line(aes(x=MixedValueDiffRound, y=Correct, group=TrainedPair, color = TrainedPair), size = 1.5, position=position_dodge(.4), stat="identity") +
  geom_errorbar(aes(ymin=Correct-se, ymax=Correct+se, color = TrainedPair), width=0, position=position_dodge(.4), size = 1.5) +
  geom_point(aes(fill=TrainedPair, group=TrainedPair, color = TrainedPair),stroke =2, position=position_dodge(.4), stat="identity") +
  #geom_point(data = acc_all_sub, aes(fill=TrainedPair, group=TrainedPair, color = TrainedPair),stroke =.5, alpha=.3, position=position_jitter(.25), stat="identity") +
  #facet_grid(.~TrainedPair)+
  xlab("ValueDifference") + ylab("Accuracy")+
  scale_color_manual(values=condcols) + scale_fill_manual(values=condcols)+
  theme_sage_simple()
rt_valuediff



# ~ how about training versus mixed? ----
acc_all <- SageSummarySEwithin(D, dv="Correct", wv=c("TrainedPair", "Block"), subv="ID")
acc_all_sub <- SageSummarySEwithin(D, dv="Correct", wv=c("TrainedPair", "Block"), bv = "ID", subv="ID")

rt_all <- SageSummarySEwithin(filter(D, Correct==1), dv="Reaction.Time", wv=c("TrainedPair", "Block"), subv="ID")
rt_all_sub <- SageSummarySEwithin(filter(D, Correct==1), dv="Reaction.Time", wv=c("TrainedPair", "Block"), bv = "ID", subv="ID")


condcols <- c("chartreuse4","tomato", "grey", "Black")
acc_block_effect <- ggplot(acc_all, aes(x=Block, y=Correct, group=TrainedPair)) + #scale_y_continuous(limits = c(0.5, 1.1))+
  geom_line(aes(x=Block, y=Correct, group=TrainedPair, color = TrainedPair), size = 1.5, position=position_dodge(.4), stat="identity") +
  geom_errorbar(aes(ymin=Correct-se, ymax=Correct+se, color = TrainedPair), width=0, position=position_dodge(.4), size = 1.5) +
  geom_point(aes(fill=TrainedPair, group=TrainedPair, color = TrainedPair),stroke =2, position=position_dodge(.4), stat="identity") +
  #geom_point(data = acc_all_sub, aes(fill=TrainedPair, group=TrainedPair, color = TrainedPair),stroke =.5, alpha=.3, position=position_jitter(.25), stat="identity") +
  #facet_grid(.~TrainedPair)+
  xlab("Block") + ylab("Accuracy")+
  scale_color_manual(values=condcols) + scale_fill_manual(values=condcols)+
  theme_sage_simple()
acc_block_effect

rt_block_effect <- ggplot(rt_all, aes(x=Block, y=Reaction.Time, group=TrainedPair)) + #scale_y_continuous(limits = c(0.5, 1.1))+
  geom_line(aes(x=Block, y=Reaction.Time, group=TrainedPair, color = TrainedPair), size = 1.5, position=position_dodge(.4), stat="identity") +
  geom_errorbar(aes(ymin=Reaction.Time-se, ymax=Reaction.Time+se, color = TrainedPair), width=0, position=position_dodge(.4), size = 1.5) +
  geom_point(aes(fill=TrainedPair, group=TrainedPair, color = TrainedPair),stroke =2, position=position_dodge(.4), stat="identity") +
  geom_point(data = rt_all_sub, aes(fill=TrainedPair, group=TrainedPair, color = TrainedPair),stroke =.5, alpha=.3, position=position_jitter(.25), stat="identity") +
  #facet_grid(.~TrainedPair)+
  xlab("Block") + ylab("RT")+
  scale_color_manual(values=condcols) + scale_fill_manual(values=condcols)+
  theme_sage_simple()
rt_block_effect

# # write away the csv
# write.csv(D, 'ophelie_alldata.csv')
# write.csv(D_mixed, 'ophelie_mixeddata.csv')
# write.csv(D_train, 'ophelie_traindata.csv')
