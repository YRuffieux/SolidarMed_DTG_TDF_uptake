# GAM models for DTG uptake at ART initiation, by district
# bootstrap sampling, estimating delay in reaching uptake thresholds between disctricts
# 10 iterations = 90 seconds

library(data.table)
library(tictoc)
library(mgcv)
library(emmeans)
library(readxl)

filepath_load <- "C:/ISPM/Data/SolidarMed/processed"
filepath_save <- "C:/ISPM/Data/SolidarMed/output"

load(file=file.path(filepath_load,"DTG_init_cohort.RData"))

tic()

N_boot <- 1000

set.seed(122410)
fup_grid <- seq(0,7,by=1/365.25)

########## absolute probabilities

list_out <- list()

pred_grid <- expand.grid(delta_init=fup_grid,district=factor(c(0,1)))

for(prog in c("SMARTLES","SMARTMOZ","SMARTZIM"))
{
  print(prog)
  df_temp <- data.table(P0.5=rep(as.numeric(NA),N_boot),P0.75=rep(as.numeric(NA),N_boot),warn=rep(0,N_boot))
  for(i in 1:N_boot)
  {
    if(i%%10==0)
      print(i)
    DT_temp <- DT[program==prog][sample(1:.N,replace=TRUE)]
    DT_temp[,district_num:=as.numeric(NA)]
    DT_temp[district%in%c("Butha-Buthe","Ancuabe","Bikita"),district_num:=0]
    DT_temp[district%in%c("Mokhotlong","Chiure","Zaka"),district_num:=1]
    DT_temp[,district:=factor(district_num)]
    DT_temp[,district_num:=NULL]
    
    warning_flag <- 0
    
    greg <- tryCatch(
      withCallingHandlers(
        gam(dtg_ind ~ s(delta_init, by=district, k=6) + district, family=binomial(link="logit"), data=DT_temp),
        warning = function(w) {
          warning_flag <<- 1
        }
      ),
      error=function(e){NULL}
    )
    
    if(!is.null(greg))
    {
      if(warning_flag==1)   # possibly convergence issues
        df_temp[i,warn:=1]  
      
      pred_prob <- data.table(pred_grid)
      pred_prob[,Estimate:=plogis(predict(greg,newdata=pred_grid,type="link",se.fit=FALSE))]
      X0 <- pred_prob[district==0]
      X1 <- pred_prob[district==1]
      for(thresh in c(0.5,0.75))
      {
        t0 <- X0[match(TRUE,X0[,Estimate]>thresh),delta_init]  # first time above threshold
        t1 <- X1[match(TRUE,X1[,Estimate]>thresh),delta_init]
        df_temp[i,paste0("P",thresh):=t1-t0]   # delay in reaching threshold, district 1 vs. 0 (in years)
        rm(t0,t1)
      }
      rm(greg,pred_prob,DT_temp,X0,X1)
    }
  }
  list_out[[prog]] <- df_temp
  rm(df_temp)
}

save(list_out,file=file.path(filepath_save,"DTG_bootstrap_init_cohort.RData"))

toc()