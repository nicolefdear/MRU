#ECHO discontinuation study descriptives
#Created by: Nicole Dear
#Created on: April 8, 2019

install.packages("magrittr")
install.packages("tidyr")
install.packages("DescTools")
install.packages("gmodels")
install.packages("knitr")
install.packages("Hmisc")
install.packages("summarytools")
install.packages("stringr")
library(magrittr)
library(tidyr)
library(tidyverse)
library(dplyr)
library(DescTools)
library(gmodels)
library(knitr)
library(Hmisc)
library(summarytools)
library(stringr)

#setwd and import dataset
setwd("H:/ECHO/Discontinuation")
exit <- read.csv("ECHOContraceptiveMet_DATA_2019-04-08_1121.csv") %>%
  subset(redcap_event_name=="followup_1_study_e_arm_1")
exit <- select(exit, c(1:79))

sixmo <- read.csv("ECHOContraceptiveMet_DATA_2019-04-08_1121.csv") %>%
  subset(redcap_event_name=="followup_2_6_month_arm_1")
sixmo <- select(sixmo, c(1:2,81:101))

merge <- merge(exit, sixmo, by="pid", all = T)
all <- select(merge, -c("redcap_event_name.x", "redcap_event_name.y"))

#new variables
all$dmpa_only[all$q1_method_wanted___1==1&all$q1_method_wanted___2==0&all$q1_method_wanted___3==0&all$q1_method_wanted___4==0&all$q1_method_wanted___5==0] <- 1
all$implant_only[all$q1_method_wanted___1==0&all$q1_method_wanted___2==1&all$q1_method_wanted___3==0&all$q1_method_wanted___4==0&all$q1_method_wanted___5==0] <- 1
all$iud_only[all$q1_method_wanted___1==0&all$q1_method_wanted___2==0&all$q1_method_wanted___3==1&all$q1_method_wanted___4==0&all$q1_method_wanted___5==0] <- 1
all$dmpa_implant[all$q1_method_wanted___1==1&all$q1_method_wanted___2==1&all$q1_method_wanted___3==0&all$q1_method_wanted___4==0&all$q1_method_wanted___5==0] <- 1
all$dmpa_iud[all$q1_method_wanted___1==1&all$q1_method_wanted___3==1&all$q1_method_wanted___2==0&all$q1_method_wanted___4==0&all$q1_method_wanted___5==0] <- 1
all$implant_iud[all$q1_method_wanted___2==1&all$q1_method_wanted___3==1&all$q1_method_wanted___1==0&all$q1_method_wanted___4==0&all$q1_method_wanted___5==0] <- 1

all$method_wanted[all$dmpa_only==1] <- 1
all$method_wanted[all$implant_only==1] <- 2
all$method_wanted[all$iud_only==1] <- 3
all$method_wanted[all$dmpa_implant==1] <- 4
all$method_wanted[all$dmpa_iud==1] <- 5
all$method_wanted[all$implant_iud==1] <- 6
all$method_wanted[all$q1_method_wanted___4==1] <- 7 #Can't remember
all$method_wanted[all$q1_method_wanted___5==1] <- 8 #Any of the 3 methods

all$dmpa_only2[all$q2_method_unwanted___1==1&all$q2_method_unwanted___2==0&all$q2_method_unwanted___3==0&all$q2_method_unwanted___4==0&all$q2_method_unwanted___5==0] <- 1
all$implant_only2[all$q2_method_unwanted___1==0&all$q2_method_unwanted___2==1&all$q2_method_unwanted___3==0&all$q2_method_unwanted___4==0&all$q2_method_unwanted___5==0] <- 1
all$iud_only2[all$q2_method_unwanted___1==0&all$q2_method_unwanted___2==0&all$q2_method_unwanted___3==1&all$q2_method_unwanted___4==0&all$q2_method_unwanted___5==0] <- 1
all$dmpa_implant2[all$q2_method_unwanted___1==1&all$q2_method_unwanted___2==1&all$q2_method_unwanted___3==0&all$q2_method_unwanted___4==0&all$q2_method_unwanted___5==0] <- 1
all$dmpa_iud2[all$q2_method_unwanted___1==1&all$q2_method_unwanted___3==1&all$q2_method_unwanted___2==0&all$q2_method_unwanted___4==0&all$q2_method_unwanted___5==0] <- 1
all$implant_iud2[all$q2_method_unwanted___2==1&all$q2_method_unwanted___3==1&all$q2_method_unwanted___1==0&all$q2_method_unwanted___4==0&all$q2_method_unwanted___5==0] <- 1

all$method_unwanted[all$dmpa_only2==1] <- 1
all$method_unwanted[all$implant_only2==1] <- 2
all$method_unwanted[all$iud_only2==1] <- 3
all$method_unwanted[all$dmpa_implant2==1] <- 4
all$method_unwanted[all$dmpa_iud2==1] <- 5
all$method_unwanted[all$implant_iud2==1] <- 6
all$method_unwanted[all$q2_method_unwanted___4==1] <- 7 #Can't remember
all$method_unwanted[all$q2_method_unwanted___5==1] <- 8 #Any of the 3 methods

all$mo6method[all$q1_current_mthd_v2==1] <- 1
all$mo6method[all$q1_current_mthd_v2==5] <- 2
all$mo6method[all$q1_current_mthd_v2==3] <- 3
all$mo6method[all$q1_current_mthd_v2==2|all$q1_current_mthd_v2==4|all$q1_current_mthd_v2==6|all$q1_current_mthd_v2==7|all$q1_current_mthd_v2==8|all$q1_current_mthd_v2==9|all$q1_current_mthd_v2==10|all$q1_current_mthd_v2==11|all$q1_current_mthd_v2==13] <- 4
all$mo6method[all$q1_current_mthd_v2==12] <- 5

all$random_eq_exit <- ifelse(all$q3_random_method==all$q4_current_method,1,0)
all$random_eq_6mo <- ifelse(all$q3_random_method==all$mo6method,1,0)
all$exit_eq_6mo <- ifelse(all$q4_current_method==all$mo6method,1,0)

#clean up var 'q3_impl_remov_loc_v2'
b <- str_detect(all$q3_impl_remov_loc_v2, regex("mru", ignore_case = TRUE))

#label variables
label(all$q3_random_method)="Method randomised to at start of study"
label(all$q4_current_method)="Method at exit"
label(all$q5_continue_method)="Decided to continue with your current method"
label(all$q6_recommend_method)="Would recommend this method to other women"
label(all$age)="Age"
label(all$preg)="Number of pregnancies"
label(all$children)="Number of living children"
label(all$q2_change_mthd_v2)="Changed methods from the time ppt left the study"
label(all$method_wanted)="Method wanted to be randomised to at the beginning of the study"
label(all$method_unwanted)="Method not wanted to be randomised to at the beginning of the study"
label(all$mo6method)="Method at 6 month follow up"
label(all$random_eq_exit)="Method randomized to same as method at exit"
label(all$random_eq_6mo)="Method randomized to same as method at 6 mo follow up"
label(all$exit_eq_6mo)="Method at exit same as method at 6 mo follow up"

#label categories
all$q3_random_method <- factor(all$q3_random_method,
                               levels = c(1,2,3),
                               labels = c("DMPA","Jadelle Implant","IUD"))
all$q4_current_method <- factor(all$q4_current_method,
                                levels = c(1,2,3,4,5),
                                labels = c("DMPA","Jadelle Implant","IUD","Other","None"))
all$mo6method <- factor(all$mo6method,
                        levels = c(1,2,3,4,5),
                        labels = c("DMPA","Jadelle Implant","IUD","Other","None"))
all$random_eq_exit <- factor(all$random_eq_exit,
                             levels = c(0,1),
                             labels = c("No", "Yes"))
all$random_eq_6mo <- factor(all$random_eq_6mo,
                            levels = c(0,1),
                            labels = c("No", "Yes"))
all$q5_continue_method <- factor(all$q5_continue_method,
                                 levels = c(0,1),
                                 labels = c("No", "Yes"))
all$q6_recommend_method <- factor(all$q6_recommend_method,
                                  levels = c(0,1,2,3),
                                  labels = c("No", "Yes","Maybe","Don't know"))
all$q2_change_mthd_v2 <- factor(all$q2_change_mthd_v2,
                                levels = c(0,1,2,3),
                                labels = c("No", "Yes","Cannot remember","Don't want to answer"))
all$exit_eq_6mo <- factor(all$exit_eq_6mo,
                          levels = c(0,1),
                          labels = c("No", "Yes"))
all$method_wanted <- factor(all$method_wanted,
                            levels = c(1,2,3,4,5,6,7,8),
                            labels = c("DMPA only","Implant only","IUD only","DMPA or Implant","DMPA or IUD","Implant or IUD","Can't remember","Any of the 3 methods"))
all$method_unwanted <- factor(all$method_unwanted,
                              levels = c(1,2,3,4,5,6,7,8),
                              labels = c("DMPA only","Implant only","IUD only","DMPA or Implant","DMPA or IUD","Implant or IUD","Can't remember","Any of the 3 methods"))

#subset and save dataset
a <- select(all, c(1,12:15,61:63,79,81:82,106,113:117))
saveRDS(a, "echodiscont.rds")

#freq tables
summarytools::freq(a$q3_random_method, order = "freq")
ctable(a$q3_random_method, a$q4_current_method, prop = "r")