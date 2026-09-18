# Royston-Parmer models of time to first DTG uptake

library(data.table)
library(rstpm2)
library(tictoc)

filepath_load <- "C:/ISPM/Data/SolidarMed/processed"
filepath_out <- "C:/ISPM/Data/SolidarMed/output"

load(file=file.path(filepath_load,"DTG_switch_cohort.RData"))

fup_grid <- seq(0,7,by=1/50)

#### absolute probabilities 

pred_list <- list()

for(prog in c("SMARTLES","SMARTMOZ","SMARTZIM"))
{
  DT_temp <- DT[program==prog]
  DT_temp[,district_num:=as.numeric(NA)]
  DT_temp[district%in%c("Butha-Buthe","Ancuabe","Bikita"),district_num:=0]
  DT_temp[district%in%c("Mokhotlong","Chiure","Zaka"),district_num:=1]
  stopifnot(DT_temp[,all(!is.na(district_num))])
  
  sreg <- stpm2(Surv(tstart,tstop,status)~district_num,df=5,tvc=list(district_num=3),data=DT_temp)
  
  # predicted survival probabilities (cumulative probabilties with type="fail" leads to problems)
  pred_prob <- data.table(predict(sreg,newdata=data.frame(tstop=rep(fup_grid,2),district_num=rep(c(0,1),rep(length(fup_grid),2))),
                                  type="surv",full=TRUE,se.fit=TRUE))
  pred_prob[Estimate==1 & is.nan(lower) & is.nan(upper),`:=`(lower=1,upper=1)]
  stopifnot(pred_prob[,all(!is.nan(lower) & !is.nan(upper))])
  
  # switching to cumulative probability of DTG uptake
  pred_prob <- pred_prob[,.(tstop,district=factor(district_num),Estimate=1-Estimate,lower=1-upper,upper=1-lower)]
  
  pred_list[[prog]] <- pred_prob
  rm(DT_temp,sreg,pred_prob)
}

save(pred_list,file=file.path(filepath_out,"DTG_cumulprobs_switch_cohort.RData"))

#### probability differences between districts, adjusting for sex and age

diff_list <- list()
pred_grid <- data.frame(tstop=fup_grid,district_num=0,sex=factor("Male",levels=c("Male","Female")),age_group_current=factor(1,levels=1:6))

for(prog in c("SMARTLES","SMARTMOZ","SMARTZIM"))
{
  DT_temp <- DT[program==prog]
  DT_temp[,district_num:=as.numeric(NA)]
  DT_temp[district%in%c("Butha-Buthe","Ancuabe","Bikita"),district_num:=0]
  DT_temp[district%in%c("Mokhotlong","Chiure","Zaka"),district_num:=1]
  stopifnot(DT_temp[,all(!is.na(district_num))])
  
  sreg <- stpm2(Surv(tstart,tstop,status)~district_num + sex + age_group_current,df=5,tvc=list(district_num=3),data=DT_temp)
  
  # adjusted differences in cumulative probability of DTG uptakes between districts
  pred_sdiff <- data.table(predict(sreg,newdata=pred_grid,type="sdiff",var="district_num",full=TRUE,se.fit=TRUE))
  pred_sdiff <- pred_sdiff[,.(tstop,Estimate=-Estimate,lower=-upper,upper=-lower)]
  
  diff_list[[prog]] <- pred_sdiff
  rm(DT_temp,sreg,pred_sdiff)
}

save(diff_list,file=file.path(filepath_out,"DTG_adj_cumulprobdiff_switch_cohort.RData"))