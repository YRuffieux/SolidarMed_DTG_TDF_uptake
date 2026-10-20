library(data.table)
library(mgcv)
library(tictoc)
library(writexl)

filepath_load <- "C:/ISPM/Data/SolidarMed/processed"
filepath_tables <- "C:/ISPM/HomeDir/SolidarMed report/Output/Tables"

tic()

load(file=file.path(filepath_load,"DTG_init_cohort.RData"))

df_AIC <- data.table(df_base=c(-1,3:10),AIC=as.numeric(NA),BIC=as.numeric(NA),k_index=as.numeric(NA))    # -1 -> default choice

AIC_list <- list("SMARTLES"=copy(df_AIC),"SMARTMOZ"=copy(df_AIC),"SMARTZIM"=copy(df_AIC))

for(prog in c("SMARTLES","SMARTMOZ","SMARTZIM"))
{
  DT_temp <- DT[program==prog]
  DT_temp[,district_num:=as.numeric(NA)]
  DT_temp[district%in%c("Butha-Buthe","Ancuabe","Bikita"),district_num:=0]
  DT_temp[district%in%c("Mokhotlong","Chiure","Zaka"),district_num:=1]
  DT_temp[,district:=factor(district_num)]
  DT_temp[,district_num:=NULL]
  
  for(df in c(-1,3:10))
  {
    greg <- gam(dtg_ind ~ s(delta_init, by=district, k=df), family=binomial(link="logit"), data=DT_temp)
    AIC_list[[prog]][df_base==df,`:=`(AIC=round(AIC(greg)),BIC=round(BIC(greg)),k_index=round(k.check(greg)[2,3],digits=3))]
    rm(greg)
  }
  rm(DT_temp)
}

write_xlsx(x=AIC_list,path=file.path(filepath_tables,"BIC_DTG_init.xlsx"))

toc()