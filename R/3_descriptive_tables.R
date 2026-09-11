library(data.table)
library(tableone)
library(writexl)
library(ggplot2)
library(scales)
library(survival)

filepath_source <- "C:/ISPM/Data/SolidarMed/source"
filepath_processed <- "C:/ISPM/Data/SolidarMed/processed"
filepath_out <- "C:/ISPM/HomeDir/SolidarMed report/Output"

load(file=file.path(filepath_source,"tblBAS.RData"))
load(file=file.path(filepath_source,"tblCENTER.RData"))
load(file=file.path(filepath_processed,"tblART_agg.RData"))
load(file=file.path(filepath_processed,"tblOUTCOMES.RData"))
source("C:/ISPM/HomeDir/SolidarMed report/Code/R/timeSplit_DT.R")

# appending first district
#   SMARTLES: Butha-Buthe, Mokhotlong
#   SMARTMOZ: Ancuabe, Chiure
#   SMARTZIM: Bikita, Zaka
tblBAS[is.na(center_enrol),center_enrol:=center_last]

tblBAS <- merge(tblBAS,tblCENTER[,.(center_enrol=center,district)],by="center_enrol",all.x=TRUE)
tblBAS[district=="" | is.na(district),district:="Unknown"]  # note: only SMARTMOZ has people with missing district
tblBAS[district=="Unknown",table(program)]
tblBAS[,`:=`(district=factor(district,levels=c("Butha-Buthe","Mokhotlong","Ancuabe","Chiure","Unknown","Bikita","Zaka")),
             program=factor(program))]
tblBAS[,year_base:=year(enrol_d)]

# sex
tblBAS[,sex:=factor(sex,levels=c(1,2),labels=c("Male","Female"))]

# age
tblBAS[,age_base:=as.numeric(enrol_d-birth_d)/365.25]
