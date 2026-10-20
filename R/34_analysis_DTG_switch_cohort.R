# Royston-Parmer models of time to first DTG uptake, by district

library(data.table)
library(rstpm2)
library(tictoc)

tic()

filepath_load <- "C:/ISPM/Data/SolidarMed/processed"
filepath_out <- "C:/ISPM/Data/SolidarMed/output"
filepath_lookup <- "C:/ISPM/HomeDir/SolidarMed report/Repository/R/Lookup"

load(file=file.path(filepath_load,"DTG_switch_cohort.RData"))
df_choices <- data.table(read_xlsx(file.path(filepath_lookup,"df_choice_DTG.xlsx")))

fup_grid <- seq(0,7,by=1/365.25)

#### absolute probabilities 

pred_list <- list()
pred_grid <- expand.grid(tstop=fup_grid,district_num=c(0,1))

for(prog in c("SMARTLES","SMARTMOZ","SMARTZIM"))
{
  df <- df_choices[country==prog,switch0]
  df_int <- df_choices[country==prog,switch_int]
  
  DT_temp <- DT[program==prog]
  DT_temp[,district_num:=as.numeric(NA)]
  DT_temp[district%in%c("Butha-Buthe","Ancuabe","Bikita"),district_num:=0]
  DT_temp[district%in%c("Mokhotlong","Chiure","Zaka"),district_num:=1]
  stopifnot(DT_temp[,all(!is.na(district_num))])
  
  sreg <- stpm2(Surv(tstart,tstop,status)~district_num,df=df,tvc=list(district_num=df_int),data=DT_temp)
  
  # predicted survival probabilities (cumulative probabilties with type="fail" leads to problems)
  pred_prob <- data.table(predict(sreg,newdata=pred_grid,type="surv",full=TRUE,se.fit=TRUE))
  pred_prob[Estimate==1 & is.nan(lower) & is.nan(upper),`:=`(lower=1,upper=1)]

  
  # switching to cumulative probability of DTG uptake
  pred_prob <- pred_prob[,.(tstop,district=factor(district_num),Estimate=1-Estimate,lower=1-upper,upper=1-lower)]
  
  pred_list[[prog]] <- pred_prob
  rm(DT_temp,sreg,pred_prob,df,df_int)
}

save(pred_list,file=file.path(filepath_out,"DTG_cumulprobs_switch_cohort.RData"))
rm(pred_grid,pred_list)

#### cumulative probability differences

pred_list <- list()
pred_grid <- expand.grid(tstop=fup_grid,district_num=0,sex=factor(c("Male", "Female"),levels=c("Male","Female")),age_group_current=factor(1:6,levels = 1:6))

for(prog in c("SMARTLES","SMARTMOZ","SMARTZIM"))
{
  df <- df_choices[country==prog,switch0]
  df_int <- df_choices[country==prog,switch_int]
  
  DT_temp <- DT[program==prog]
  DT_temp[,district_num:=as.numeric(NA)]
  DT_temp[district%in%c("Butha-Buthe","Ancuabe","Bikita"),district_num:=0]
  DT_temp[district%in%c("Mokhotlong","Chiure","Zaka"),district_num:=1]
  stopifnot(DT_temp[,all(!is.na(district_num))])
  
  sreg <- stpm2(Surv(tstart,tstop,status)~district_num + sex + age_group_current, df=df, tvc=list(district_num=df_int), data=DT_temp)
  pred_diff <- data.table(predict(sreg,newdata=pred_grid,type="meansurvdiff",var="district_num",full=TRUE,se.fit=TRUE))
  
  # switching to cumulative probability of DTG uptake
  pred_diff <- pred_diff[,.(tstop,Estimate=-Estimate,lower=-upper,upper=-lower)]
  
  pred_list[[prog]] <- pred_diff
  rm(DT_temp,sreg,pred_diff,df,df_int)
}

save(pred_list,file=file.path(filepath_out,"DTG_cumulprobdiffs_switch_cohort.RData"))
rm(pred_grid,pred_list)

toc()