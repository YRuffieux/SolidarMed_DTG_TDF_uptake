library(data.table)
library(tictoc)
library(mgcv)

filepath_load <- "C:/ISPM/Data/SolidarMed/processed"
filepath_out <- "C:/ISPM/Data/SolidarMed/output"

load(file=file.path(filepath_load,"DTG_init_cohort.RData"))

fup_grid <- seq(0,7,by=1/50)

########## absolute probabilities

pred_list <- list()

pred_grid <- data.frame(delta_init=rep(fup_grid,2),district=factor(c(rep(0,length(fup_grid)),rep(1,length(fup_grid)))))

for(prog in c("SMARTLES","SMARTMOZ","SMARTZIM"))
{
  DT_temp <- DT[program==prog]
  DT_temp[,district_num:=as.numeric(NA)]
  DT_temp[district%in%c("Butha-Buthe","Ancuabe","Bikita"),district_num:=0]
  DT_temp[district%in%c("Mokhotlong","Chiure","Zaka"),district_num:=1]
  DT_temp[,district:=factor(district_num)]
  DT_temp[,district_num:=NULL]
  
  greg <- gam(dtg_ind ~ s(delta_init, by=district), family=binomial(link="logit"), data=DT_temp)
  
  pred_prob <- predict(greg,newdata=pred_grid,type="response",se.fit=TRUE)
  pred_prob <- data.table(Estimate=c(pred_prob$fit),SE=c(pred_prob$se.fit))
  pred_prob <- cbind(data.table(pred_grid),pred_prob)
  pred_prob[,`:=`(lower=Estimate-qnorm(0.975)*SE,upper=Estimate+qnorm(0.975)*SE)]
  pred_prob[,SE:=NULL]
  pred_prob[upper>1,upper:=1]
  pred_prob[lower<0,lower:=0]
  
  pred_list[[prog]] <- pred_prob
  
  rm(greg,pred_prob,DT_temp)
}

save(pred_list,file=file.path(filepath_out,"DTG_probs_init_cohort.RData"))