#CUBE QC report: South Africa
#Created by: Nicole Dear
#Created on: 2019-04-03

install.packages("tidyverse")
install.packages("lubridate")
install.packages("gmodels")
install.packages("rmarkdown")
library(tidyverse)
library(lubridate)
library(gmodels)
library(rmarkdown)

#set working directory
setwd("H:/CUBE/QC")

#load and reduce dataset
sa<-read.csv("CUBEContraceptiveUse_SA_2019-04-04_1231.csv")
sa_mo6<-sa[c(1:346)]
names(sa_mo6)

#internal consistency checks
#criteria 1: exit method must match method at 1mo post exit visit
sa_mo6$flag_exitmethod<-ifelse((sa_mo6$contraception_at_exit==1&sa_mo6$month_injectable3___1==1)|(sa_mo6$contraception_at_exit==2&sa_mo6$copper_iud___1==1)|(sa_mo6$contraception_at_exit==3&sa_mo6$rod_implant_jadelle___1==1),1,0)
table(sa_mo6$flag_exitmethod)
a<-subset(sa_mo6, sa_mo6$flag_exitmethod==FALSE)
print(a$pid)

#criteria 2: exited on iud or implant, discontinued iud or implant
#for those discont iud
sa_mo6_exitiud<-subset(sa_mo6, sa_mo6$contraception_at_exit==2&any_method_discont==1)
sa_mo6_exitiud$flag_discontiud<-ifelse(sa_mo6_exitiud$method_discontinued==5,1,0)
b<-subset(sa_mo6_exitiud, sa_mo6_exitiud$flag_discontiud==FALSE)
print(b$pid)

sa_mo6_exitiud$flag_discontuse<-ifelse(sa_mo6_exitiud$discont_use_iud==1,1,0)
c<-subset(sa_mo6_exitiud, sa_mo6_exitiud$flag_discontuse==FALSE)
print(c$pid)

#for those discont implant
sa_mo6_exitimp<-subset(sa_mo6, sa_mo6$contraception_at_exit==3&any_method_discont==1)
sa_mo6_exitimp$flag_discontimp<-ifelse(sa_mo6_exitimp$method_discontinued==3,1,0)
d<-subset(sa_mo6_exitimp, sa_mo6_exitimp$flag_discontimp==FALSE)
print(d$pid)

sa_mo6_exitimp$flag_discontuse<-ifelse(sa_mo6_exitimp$discont_use_iud==1,1,0)
e<-subset(sa_mo6_exitimp, sa_mo6_exitimp$flag_discontuse==FALSE)
print(e$pid)

#criteria 3: exited on iud or implant, did not discontinue iud or implant
#for those exited on iud and did not discont iud
sa_mo6_exitstayiud<-subset(sa_mo6, sa_mo6$contraception_at_exit==2&any_method_discont==0)
sa_mo6_exitstayiud$flag108<-ifelse(sa_mo6_exitstayiud$current_iud==1,1,0)
f<-subset(sa_mo6_exitstayiud, sa_mo6_exitstayiud$flag108==FALSE)
print(f$pid)

sa_mo6_exitstayiud$flag109<-ifelse(sa_mo6_exitstayiud$discont_use_iud==0,1,0)
g<-subset(sa_mo6_exitstayiud, sa_mo6_exitstayiud$flag109==FALSE)
print(g$pid)

#for those exited on implant and did not discont implant
sa_mo6_exitstayimp<-subset(sa_mo6, sa_mo6$contraception_at_exit==3&any_method_discont==0)
sa_mo6_exitstayimp$flag108<-ifelse(sa_mo6_exitstayimp$current_iud==1,1,0)
h<-subset(sa_mo6_exitstayimp, sa_mo6_exitstayimp$flag108==FALSE)
print(h$pid)

sa_mo6_exitstayimp$flag109<-ifelse(sa_mo6_exitstayimp$discont_use_iud==0,1,0)
i<-subset(sa_mo6_exitstayimp, sa_mo6_exitstayimp$flag109==FALSE)
print(i$pid)

#criteria 4: how long remain on method vs time until next baby
sa_mo6$timestaymethod[sa_mo6$how_long_plan_use_curr==1]<-0
sa_mo6$timestaymethod[sa_mo6$how_long_plan_use_curr==2]<-1
sa_mo6$timestaymethod[sa_mo6$how_long_plan_use_curr==3]<-3
sa_mo6$timestaymethod[sa_mo6$how_long_plan_use_curr==4]<-5.1

sa_mo6$flagbaby<-ifelse(sa_mo6$timestaymethod<=sa_mo6$how_long_wait,1,0)
j<-subset(sa_mo6, sa_mo6$flagbaby==FALSE)
print(j$pid)

#criteria 5: calendar is blank at 1 month post exit visit
sa_mo6$flagcalendarblank<-ifelse((sa_mo6$none___1==0&sa_mo6$month_injectable3___1==0&sa_mo6$month_injectable2___1==0&sa_mo6$rod_implant_jadelle___1==0&sa_mo6$rod_implant_implanon___1==0&sa_mo6$copper_iud___1==0&sa_mo6$hormonal_iud___1==0&sa_mo6$oral_pills___1==0&sa_mo6$male_condoms___1==0&sa_mo6$female_condoms___1==0&sa_mo6$emergency_contraception___1==0&sa_mo6$sterilization___1==0&sa_mo6$other___1==0),1,0)
k<-subset(sa_mo6, sa_mo6$flagcalendarblank==TRUE)
print(k$pid)

#criteria 6: calendar 'other'
sa_mo6$flagcalendarother<-ifelse((sa_mo6$other___1==1|sa_mo6$other___2==1|sa_mo6$other___3==1|sa_mo6$other___4==1|sa_mo6$other___5==1|sa_mo6$other___6==1|sa_mo6$other___7==1|sa_mo6$other___8==1),1,0)
l<-subset(sa_mo6, sa_mo6$flagcalendarother==TRUE)
print(l$pid)

#criteria 7: no reason for method discontinuation given
sa_mo6_discont<-subset(sa_mo6, sa_mo6$any_method_discont==1)
sa_mo6_discont$flag<-ifelse((sa_mo6_discont$reason_for_discont___1==1|sa_mo6_discont$reason_for_discont___2==1|sa_mo6_discont$reason_for_discont___3==1|sa_mo6_discont$reason_for_discont___4==1|sa_mo6_discont$reason_for_discont___5==1|sa_mo6_discont$reason_for_discont___6==1|sa_mo6_discont$reason_for_discont___7==1|sa_mo6_discont$reason_for_discont___8==1|sa_mo6_discont$reason_for_discont___9==1|sa_mo6_discont$reason_for_discont___10==1|sa_mo6_discont$reason_for_discont___11==1|sa_mo6_discont$reason_for_discont___12==1|sa_mo6_discont$reason_for_discont___13==1|sa_mo6_discont$reason_for_discont___14==1|sa_mo6_discont$reason_for_discont___15==1|sa_mo6_discont$reason_for_discont___16==1|sa_mo6_discont$reason_for_discont___17==1|sa_mo6_discont$reason_for_discont___18==1|sa_mo6_discont$reason_for_discont___19==1|sa_mo6_discont$reason_for_discont___20==1|sa_mo6_discont$reason_for_discont___21==1|sa_mo6_discont$reason_for_discont___22==1|sa_mo6_discont$reason_for_discont___23==1|sa_mo6_discont$reason_for_discont___24==1|sa_mo6_discont$reason_for_discont___25==1|sa_mo6_discont$reason_for_discont___99==1),1,0)
m<-subset(sa_mo6_discont, sa_mo6_discont$flag==FALSE)
print(m$pid)
