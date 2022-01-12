#Wondaleaf analysis for SAAIDS poster
#Created by: Nicole Dear
#Created on: May 6, 2019

install.packages("tidyr")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("gmodels")
install.packages("knitr")
install.packages("Hmisc")
library(tidyr)
library(tidyverse)
library(dplyr)
library(gmodels)
library(knitr)
library(Hmisc)

#setwd and import dataset
setwd("H:/Wondaleaf")
all <- read.csv("WondaleafCondomMainS_DATA_2019-05-14_1511.csv")
names(all)
#subset dataset
screen <- all %>%
  subset(redcap_event_name=="screening_arm_1") %>% 
  select(c(1,3:28)) %>% 
  subset(scc_q20_eligible==1)
baseline <- all %>%
  subset(redcap_event_name=="enrolment_arm_1") %>% 
  select(c(1,29:81))
fu <- all %>%
  subset(redcap_event_name=="followup_1_arm_1") %>% 
  select(c(1,83:189))
fc1 <- all %>%
  subset(redcap_event_name=="condom_log_f_1_arm_1") %>% 
  select(c(1,191:263))
fc2 <- all %>%
  subset(redcap_event_name=="condom_log_f_2_arm_1") %>% 
  select(c(1,191:263))
fc3 <- all %>%
  subset(redcap_event_name=="condom_log_f_3_arm_1") %>% 
  select(c(1,191:263))
fc4 <- all %>%
  subset(redcap_event_name=="condom_log_f_4_arm_1") %>% 
  select(c(1,191:263))
fc5 <- all %>%
  subset(redcap_event_name=="condom_log_f_5_arm_1") %>% 
  select(c(1,191:263))
wl1 <- all %>%
  subset(redcap_event_name=="condom_log_w_1_arm_1") %>% 
  select(c(1,265:339))
wl2 <- all %>%
  subset(redcap_event_name=="condom_log_w_2_arm_1") %>% 
  select(c(1,265:339))
wl3 <- all %>%
  subset(redcap_event_name=="condom_log_w_3_arm_1") %>% 
  select(c(1,265:339))
wl4 <- all %>%
  subset(redcap_event_name=="condom_log_w_4_arm_1") %>% 
  select(c(1,265:339))
wl5 <- all %>%
  subset(redcap_event_name=="condom_log_w_5_arm_1") %>% 
  select(c(1,265:339))

fc_all <- all %>%
  subset(redcap_event_name=="condom_log_f_1_arm_1"|redcap_event_name=="condom_log_f_2_arm_1"|redcap_event_name=="condom_log_f_3_arm_1"|redcap_event_name=="condom_log_f_4_arm_1"|redcap_event_name=="condom_log_f_5_arm_1") %>% 
  select(c(1:2,191:263))
wl_all <- all %>%
  subset(redcap_event_name=="condom_log_w_1_arm_1"|redcap_event_name=="condom_log_w_2_arm_1"|redcap_event_name=="condom_log_w_3_arm_1"|redcap_event_name=="condom_log_w_4_arm_1"|redcap_event_name=="condom_log_w_5_arm_1") %>% 
  select(c(1:2,265:339))

ae <- all %>%
  subset(redcap_event_name=="ae_1_arm_1") %>% 
  select(c(1,341:356))

#add number to end of all variable names - FC
names(fc1) <- paste0(names(fc1), "1")
names(fc2) <- paste0(names(fc2), "2")
names(fc3) <- paste0(names(fc3), "3")
names(fc4) <- paste0(names(fc4), "4")
names(fc5) <- paste0(names(fc5), "5")

#merge all condom log datasets - FC
fc <- cbind(fc1, fc2, fc3, fc4, fc5)

#add number to end of all variable names - WL
names(wl1) <- paste0(names(wl1), "1")
names(wl2) <- paste0(names(wl2), "2")
names(wl3) <- paste0(names(wl3), "3")
names(wl4) <- paste0(names(wl4), "4")
names(wl5) <- paste0(names(wl5), "5")
colnames(wl5)[colnames(wl5)=="clw_pid5"] <- "clw_pid4"

#merge all condom log datasets - WL
wl_4_5 <- merge(wl4, wl5, by="clw_pid4", all=T)
wl <- cbind(wl1, wl2, wl3, wl_4_5)

##BASELINE ANALYSIS##

#subset baseline dataset
x <- select(baseline, c(1,4:5,7:8,10:12,54))
write.csv(x, "baseline_subset.csv")

#new var for age cat
x$age_cat <- ifelse(x$bl_q4_age>=20&x$bl_q4_age<=29,0,1)

#basic descriptives
mean(x$bl_q4_age)
min(x$bl_q4_age)
median(x$bl_q4_age)
max(x$bl_q4_age)
sd(x$bl_q4_age)
mean(x$bl_q8_numchild)
sd(x$bl_q8_numchild)
CrossTable(x$bl_q6_edu)
CrossTable(x$bl_q7_occupation)
CrossTable(x$bl_q9_rship)
CrossTable(x$bl_q10_rship_length)
CrossTable(x$bl_q20_phair)

#summarise by group
a <- x %>% count(bl_q20_phair, bl_q4_age, sort = TRUE)
b <- x %>% group_by(bl_q20_phair) %>% 
  summarise(n = n(), average = mean(bl_q4_age), median = median(bl_q4_age), 
            min = min(bl_q4_age), max = max(bl_q4_age))

#chi-square tests
CrossTable(x$bl_q6_edu, x$bl_q20_phair, expected = TRUE, chisq = TRUE, fisher = TRUE, format="SAS")
CrossTable(x$bl_q7_occupation, x$bl_q20_phair, expected = TRUE, chisq = TRUE, fisher = TRUE, format="SAS")
CrossTable(x$bl_q9_rship, x$bl_q20_phair, expected = TRUE, chisq = TRUE, fisher = TRUE, format="SAS")
CrossTable(x$bl_q10_rship_length, x$bl_q20_phair, expected = TRUE, chisq = TRUE, fisher = TRUE, format="SAS")
CrossTable(x$age_cat, x$bl_q20_phair, expected = TRUE, chisq = TRUE, fisher = TRUE, format="SAS")

#ttests
t.test(x$bl_q4_age~x$bl_q20_phair)
t.test(x$bl_q8_numchild~x$bl_q20_phair)

##INTERIM REPORT##

#Table 4: Mean failure of Wondaleaf in relation to FC2
#calc denominator
fc_all$condoms_used_sex <- ifelse(fc_all$clf_q6_vaginal_cdm==1,1,0)
wl_all$condoms_used_sex <- ifelse(wl_all$clw_q7_vaginal_cdm==1,1,0)
table(fc_all$condoms_used_sex)
table(wl_all$condoms_used_sex)

#calc numerators for each failure type
#non-clinical breakage
table(fc_all$clf_q2_package_tear)
table(wl_all$clw_q2_package_tear)
#clinical breakage (during intercourse(5)/during withdrawal of penis(6)/during removal of condom from vagina(7))
table(fc_all$clf_q11_break)
table(wl_all$clw_q12_break)
#invagination
table(fc_all$clf_q9_invag)
table(wl_all$clw_q10_invag)
#misdirection
fc_all$misdirect <- ifelse(fc_all$clf_q8_misdirect___2==1|fc_all$clf_q8_misdirect___3==1|fc_all$clf_q8_misdirect___4==1|fc_all$clf_q8_misdirect___5==1,1,0)
table(fc_all$misdirect)
wl_all$misdirect <- ifelse(wl_all$clw_q9_misdirect___2==1|wl_all$clw_q9_misdirect___3==1|wl_all$clw_q9_misdirect___4==1|wl_all$clw_q9_misdirect___5==1,1,0)
table(wl_all$misdirect)
#slippage
table(fc_all$clf_q10_slip)
table(wl_all$clw_q11_slip)
#total clinical failure (break, invag, misdirection, slippage)
fc_all$anyclinical <- ifelse(fc_all$clf_q11_break==5|fc_all$clf_q11_break==6|fc_all$clf_q11_break==7|fc_all$clf_q9_invag==2|fc_all$clf_q9_invag==3|
                               fc_all$clf_q8_misdirect___2==1|fc_all$clf_q8_misdirect___3==1|fc_all$clf_q8_misdirect___4==1|fc_all$clf_q8_misdirect___5==1|
                               fc_all$clf_q10_slip==2|fc_all$clf_q10_slip==3|fc_all$clf_q10_slip==4|fc_all$clf_q10_slip==5,1,0)
table(fc_all$anyclinical)
wl_all$anyclinical <- ifelse(wl_all$clw_q12_break==5|wl_all$clw_q12_break==6|wl_all$clw_q12_break==7|wl_all$clw_q10_invag==2|wl_all$clw_q10_invag==3|
                               wl_all$clw_q9_misdirect___2==1|wl_all$clw_q9_misdirect___3==1|wl_all$clw_q9_misdirect___4==1|wl_all$clw_q9_misdirect___5==1|
                               wl_all$clw_q11_slip==2|wl_all$clw_q11_slip==3|wl_all$clw_q11_slip==4|wl_all$clw_q11_slip==5,1,0)
table(wl_all$anyclinical)
#total female condom failure (any)
fc_all$anyfail <- ifelse(fc_all$clf_q11_break==5|fc_all$clf_q11_break==6|fc_all$clf_q11_break==7|fc_all$clf_q9_invag==2|fc_all$clf_q9_invag==3|
                           fc_all$clf_q8_misdirect___2==1|fc_all$clf_q8_misdirect___3==1|fc_all$clf_q8_misdirect___4==1|fc_all$clf_q8_misdirect___5==1|
                           fc_all$clf_q10_slip==2|fc_all$clf_q10_slip==3|fc_all$clf_q10_slip==4|fc_all$clf_q10_slip==5|fc_all$clf_q2_package_tear==1,1,0)
table(fc_all$anyfail)
wl_all$anyfail <- ifelse(wl_all$clw_q12_break==5|wl_all$clw_q12_break==6|wl_all$clw_q12_break==7|wl_all$clw_q10_invag==2|wl_all$clw_q10_invag==3|
                           wl_all$clw_q9_misdirect___2==1|wl_all$clw_q9_misdirect___3==1|wl_all$clw_q9_misdirect___4==1|wl_all$clw_q9_misdirect___5==1|
                           wl_all$clw_q11_slip==2|wl_all$clw_q11_slip==3|wl_all$clw_q11_slip==4|wl_all$clw_q11_slip==5|wl_all$clw_q2_package_tear==1,1,0)
table(wl_all$anyfail)

#Table 5: detail of WL condom failures
wl_sub <- wl_all%>% subset(clw_q2_package_tear==1)
wl_sub <- wl_all%>% subset(wl_all$clw_q10_invag==2|wl_all$clw_q10_invag==3)
wl_sub <- wl_all%>% subset(wl_all$clw_q11_slip==2|wl_all$clw_q11_slip==3|wl_all$clw_q11_slip==4|wl_all$clw_q11_slip==5) %>% 
  select(c(1:4,44:49,78:81))
wl_sub <- wl_all%>% subset(wl_all$clw_q11_slip==2|wl_all$clw_q11_slip==3|wl_all$clw_q11_slip==4|wl_all$clw_q11_slip==5)

#subset follow-up surveys by condom type
fu_fc <- fu %>% subset(fu_ctype==1)
fu_wl <- fu %>% subset(fu_ctype==2)

#Table 8: lubricant experience with Wondaleaf
table(fu_wl$fu_q7_lubricant)
table(fu_wl$fu_q10_lube_appl)

#Table 9: shield adhesiveness
wl_sub <- wl_all%>% subset(clw_q4_shield==1)
table(fu_wl$fu_q17_wl_adherence)
table(fu_wl$fu_q18_wl_appl_practice)

#Table 10: Acceptability of different features of study condoms
#FC
table(fu_fc$fu_q25_rate_feel)
table(fu_fc$fu_q25_rate_length)
table(fu_fc$fu_q25_rate_lube)
table(fu_fc$fu_q25_rate_app)
table(fu_fc$fu_q25_rate_ease)
table(fu_fc$fu_q25_rate_insert)
table(fu_fc$fu_q25_rate_scent)
table(fu_fc$fu_q25_rate_colour)
table(fu_fc$fu_q25_rate_fit)
#WL
table(fu_wl$fu_q25_rate_feel)
table(fu_wl$fu_q25_rate_length)
table(fu_wl$fu_q25_rate_lube)
table(fu_wl$fu_q25_rate_app)
table(fu_wl$fu_q25_rate_ease)
table(fu_wl$fu_q25_rate_insert)
table(fu_wl$fu_q25_rate_scent)
table(fu_wl$fu_q25_rate_colour)
table(fu_wl$fu_q25_rate_fit)

#Table 11: Preference
table(fu$fu_q29_fc_like)
table(fu$fu_q30_fc_partner_like)

#AEs
ae_records <- wl %>% subset(record_id1==7|record_id1==26|record_id1==38) %>% select(c(1:3,55:65,79,131:141))