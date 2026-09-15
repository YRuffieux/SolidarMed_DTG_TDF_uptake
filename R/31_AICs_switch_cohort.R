library(data.table)
library(rstpm2)
library(tictoc)
library(writexl)

filepath_load <- "C:/ISPM/Data/SolidarMed/processed"
filepath_tables <- "C:/ISPM/HomeDir/SolidarMed report/Output/Tables"

tic()

load(file=file.path(filepath_load,"DTG_switch_cohort.RData"))

DT <- DT[district!="Unknown"]

df_AIC <- data.table(df_base=3:15,dfI0=as.numeric(NA),dfI1=as.numeric(NA),dfI2=as.numeric(NA),dfI3=as.numeric(NA))

AIC_list <- list("SMARTLES"=copy(df_AIC),"SMARTMOZ"=copy(df_AIC),"SMARTZIM"=copy(df_AIC))

for(prog in c("SMARTLES","SMARTMOZ","SMARTZIM"))
{
  DT_temp <- DT[program==prog]
  DT_temp[,district_num:=as.numeric(NA)]
  DT_temp[district%in%c("Butha-Buthe","Ancuabe","Bikita"),district_num:=0]
  DT_temp[district%in%c("Mokhotlong","Chiure","Zaka"),district_num:=1]
  stopifnot(DT_temp[,all(!is.na(district_num))])
  
  for(df in 3:15)
  {
    # no interaction
    sreg <-  stpm2(Surv(tstart,tstop,status)~district_num,df=df,data=DT_temp)
    if(all(!is.nan(summary(sreg)@coef)))
      AIC_list[[prog]][df_base==df,dfI0:=round(AIC(sreg))]
    rm(sreg)
    
    # with interaction
    for(df_int in 1:3)
      {
        sreg <- stpm2(Surv(tstart,tstop,status)~district_num,df=df,tvc=list(district_num=df_int),data=DT_temp)
        if(all(!is.nan(summary(sreg)@coef)))
          AIC_list[[prog]][df_base==df,paste0("dfI",df_int):=round(AIC(sreg))]
        rm(sreg)
      }
  }
  rm(DT_temp)
}

write_xlsx(x=AIC_list,path=file.path(filepath_tables,"AIC_DTG_switch_absolute.xlsx"))

toc()