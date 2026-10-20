# Royston-Parmar models for switching to DTG, by district
# bootstrap sampling, estimating delay in reaching uptake thresholds between disctricts
# 10 iterations = 1 minute

library(data.table)
library(tictoc)
library(rstpm2)
library(readxl)

filepath_load <- "C:/ISPM/Data/SolidarMed/processed"
filepath_save <- "C:/ISPM/Data/SolidarMed/output"
filepath_lookup <- "C:/ISPM/HomeDir/SolidarMed report/Repository/R/Lookup"

load(file=file.path(filepath_load,"DTG_switch_cohort.RData"))
df_choices <- data.table(read_xlsx(file.path(filepath_lookup,"df_choice_DTG.xlsx")))

tic()

N_boot <- 1000

set.seed(123410)
fup_grid <- seq(0,7,by=1/365.25)

########## absolute probabilities

list_out <- list()

pred_grid <- expand.grid(tstop=fup_grid,district_num=c(0,1))

for(prog in c("SMARTLES","SMARTMOZ","SMARTZIM"))
{
  print(prog)
  df_temp <- data.table(P0.5=rep(as.numeric(NA),N_boot),P0.75=rep(as.numeric(NA),N_boot),
                        warn=rep(0,N_boot))
  df <- df_choices[country==prog,switch0]
  df_int <- df_choices[country==prog,switch_int]
  for(i in 1:N_boot)
  {
    if(i%%10==0)
      print(i)
    DT_temp <- DT[program==prog][sample(1:.N,replace=TRUE)]
    DT_temp[,district_num:=as.numeric(NA)]
    DT_temp[district%in%c("Butha-Buthe","Ancuabe","Bikita"),district_num:=0]
    DT_temp[district%in%c("Mokhotlong","Chiure","Zaka"),district_num:=1]
    stopifnot(DT_temp[,all(!is.na(district_num))])
    
    sreg <- stpm2(Surv(tstart,tstop,status)~district_num,df=df,tvc=list(district_num=df_int),data=DT_temp)
    
    pred_prob <- data.table(predict(sreg,newdata=pred_grid,type="surv",full=TRUE,se.fit=FALSE))
    pred_prob[,Estimate:=1-Estimate]
    
    if(pred_prob[,all(!is.nan(Estimate))])
    {
      X0 <- pred_prob[district_num==0]
      X1 <- pred_prob[district_num==1]
      for(thresh in c(0.5,0.75))
      {
        t0 <- X0[match(TRUE,X0[,Estimate]>thresh),tstop]  # first time above threshold
        t1 <- X1[match(TRUE,X1[,Estimate]>thresh),tstop]
        df_temp[i,paste0("P",thresh):=t1-t0]   # delay in reaching threshold, district 1 vs. 0 (in years)
        rm(t0,t1)
      }
      rm(X0,X1)
    } else{
      df_temp[i,warn:=1] 
    }
    rm(sreg,pred_prob,DT_temp)
  }
  list_out[[prog]] <- df_temp
  rm(df_temp,df)
}

save(list_out,file=file.path(filepath_save,"DTG_bootstrap_switch_cohort.RData"))

toc()