# GAM models for DTG uptake at ART initiation, by district

library(data.table)
library(tictoc)
library(mgcv)
library(emmeans)
library(readxl)

tic()

filepath_load <- "C:/ISPM/Data/SolidarMed/processed"
filepath_out <- "C:/ISPM/Data/SolidarMed/output"
filepath_lookup <- "C:/ISPM/HomeDir/SolidarMed report/Repository/R/Lookup"

load(file=file.path(filepath_load,"DTG_init_cohort.RData"))
df_choices <- data.table(read_xlsx(file.path(filepath_lookup,"df_choice_DTG.xlsx")))

fup_grid1 <- seq(0,7,by=1/365.25)
fup_grid2 <- seq(0,7,by=1/50)

DT[,age_base_cat:=cut(age_base,breaks=c(0,20,30,40,50,70,Inf),right=FALSE)]

########## absolute probabilities

pred_list <- list()

pred_grid <- expand.grid(delta_init=fup_grid1,district=factor(c(0,1)))

for(prog in c("SMARTLES","SMARTMOZ","SMARTZIM"))
{
  df <- df_choices[country==prog,init]
  
  DT_temp <- DT[program==prog]
  DT_temp[,district_num:=as.numeric(NA)]
  DT_temp[district%in%c("Butha-Buthe","Ancuabe","Bikita"),district_num:=0]
  DT_temp[district%in%c("Mokhotlong","Chiure","Zaka"),district_num:=1]
  DT_temp[,district:=factor(district_num)]
  DT_temp[,district_num:=NULL]
  
  greg <- gam(dtg_ind ~ s(delta_init, by=district, k=df) + district, family=binomial(link="logit"), data=DT_temp)
  
  pred_prob <- predict(greg,newdata=pred_grid,type="link",se.fit=TRUE)
  pred_prob <- data.table(Estimate=c(pred_prob$fit),SE=c(pred_prob$se.fit))
  pred_prob[,`:=`(Estimate=plogis(Estimate),lower=plogis(Estimate-qnorm(0.975)*SE),upper=plogis(Estimate+qnorm(0.975)*SE))]
  pred_prob <- cbind(data.table(pred_grid,pred_prob))
  pred_prob[,SE:=NULL]
  
  pred_list[[prog]] <- pred_prob
  
  rm(greg,pred_prob,DT_temp,df)
}

save(pred_list,file=file.path(filepath_out,"DTG_probs_init_cohort.RData"))
rm(pred_list)

###### mean probability differences for DTG uptake, over time

pred_list <- list()

for(prog in c("SMARTLES","SMARTMOZ","SMARTZIM"))
{
  df <- df_choices[country==prog,init]
  
  DT_temp <- DT[program==prog]
  DT_temp[,district_num:=as.numeric(NA)]
  DT_temp[district%in%c("Butha-Buthe","Ancuabe","Bikita"),district_num:=0]
  DT_temp[district%in%c("Mokhotlong","Chiure","Zaka"),district_num:=1]
  DT_temp[,district:=factor(district_num)]
  DT_temp[,district_num:=NULL]
  
  greg <- gam(dtg_ind ~ s(delta_init, by=district, k=df) + district + sex + age_base_cat, family=binomial(link="logit"), data=DT_temp)
  emm <- emmeans(greg, ~district | delta_init, at = list(delta_init=fup_grid2))    
  emm <- regrid(emm)         # switching to probabilities
  contr <- as.data.table(confint(contrast(emm, method = "revpairwise")))
  
  pred_list[[prog]] <- contr[,.(delta_init=as.numeric(as.character(delta_init)),Estimate=estimate,lower=lower.CL,upper=upper.CL)]
  
  rm(greg,DT_temp,contr,df)
}

save(pred_list,file=file.path(filepath_out,"DTG_probdiffs_init_cohort.RData"))
rm(pred_list)

toc()