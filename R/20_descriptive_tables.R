library(data.table)
library(tableone)
library(writexl)

filepath_source <- "C:/ISPM/Data/SolidarMed/source"
filepath_processed <- "C:/ISPM/Data/SolidarMed/processed"
filepath_out <- "C:/ISPM/HomeDir/SolidarMed report/Output"

load(file=file.path(filepath_source,"tblBAS.RData"))
load(file=file.path(filepath_source,"tblCENTER.RData"))
load(file=file.path(filepath_processed,"tblART_agg.RData"))
load(file=file.path(filepath_processed,"tblOUTCOMES.RData"))
source("C:/ISPM/HomeDir/SolidarMed report/Repository/R/Utils/timeSplit_DT.R")

# country DTG adoption dates
LES_DTG_ADOPT_D <- as.Date("2018-11-01")
MOZ_DTG_ADOPT_D <- as.Date("2019-05-01")
ZIM_DTG_ADOPT_D <- as.Date("2019-05-01")

DT <- tblBAS[,.(patient,program,birth_d,enrol_d,sex,center_enrol,center_last)]

# appending first district
#   SMARTLES: Butha-Buthe, Mokhotlong
#   SMARTMOZ: Ancuabe, Chiure
#   SMARTZIM: Bikita, Zaka
DT[is.na(center_enrol),center_enrol:=center_last]
DT[,center_last:=NULL]

DT <- merge(DT,tblCENTER[,.(center_enrol=center,district)],by="center_enrol",all.x=TRUE)
DT[district=="" | is.na(district),district:="Unknown"]  # note: only SMARTMOZ has people with missing district
DT[district=="Unknown",table(program)]
DT[,`:=`(district=factor(district,levels=c("Butha-Buthe","Mokhotlong","Ancuabe","Chiure","Unknown","Bikita","Zaka")),program=factor(program))]
DT[,year_base:=year(enrol_d)]

# appending DTG adoption dates, by country
DT[,dtg_adopt_d:=as.Date(NA)]
DT[program=="SMARTLES",dtg_adopt_d:=LES_DTG_ADOPT_D]
DT[program=="SMARTMOZ",dtg_adopt_d:=MOZ_DTG_ADOPT_D]
DT[program=="SMARTZIM",dtg_adopt_d:=ZIM_DTG_ADOPT_D]

# sex
DT[,sex:=factor(sex,levels=c(1,2),labels=c("Male","Female"))]

# age
DT[,age_base:=as.numeric(enrol_d-birth_d)/365.25]

# censor date + reason
DT <- merge(DT,tblOUTCOMES[,.(patient,rev_outcome,rev_outcome_d)],by="patient")

# removing individuals who leave the study prior to DTG introduction
DT <- DT[rev_outcome_d>dtg_adopt_d]

# excluding individuals enrolled in 2026, and individuals with no follow-up (mainly: people lost to follow-up after one visit)
DT <- DT[year_base<2026 & rev_outcome_d>enrol_d]

# date of first DTG regimen per person
DT <- tblART_agg[grepl("DTG",regimen),.(patient,first_dtg_d=start)][DT,on="patient",mult="first"]

# removing individuals who initiaited DTG prior to country-wide DTG introduction
DT <- DT[is.na(first_dtg_d) | first_dtg_d>=dtg_adopt_d]

# removing individuals whose initial ART regimen includes DTG
DT <- DT[is.na(first_dtg_d) | first_dtg_d>enrol_d]

# indicator=1 if enrolled prior to DTG adoption date, otherwise 0
DT[,pre_adopt_ind:=enrol_d<dtg_adopt_d]

DT[,dtg_ever:=!is.na(first_dtg_d)]

tab <- CreateTableOne(vars=c("pre_adopt_ind","dtg_ever"),strata="district",test=FALSE,addOverall=TRUE,data=DT)
tab <- print(tab,showAllLevels=FALSE,quote=FALSE,printToggle=FALSE,noSpaces=TRUE)

# time between ART initiation and switching to DTG in people initiating ART after DTG introduction
DT[pre_adopt_ind==0 & !is.na(first_dtg_d),quantile(as.numeric(first_dtg_d-enrol_d),probs=c(0,0.25,0.5,0.75,1))]

# time between DTG introduction and switching to DTG for people who iniated ART prior to DTG introduction
DT[pre_adopt_ind==1 & !is.na(first_dtg_d),quantile(as.numeric(first_dtg_d-dtg_adopt_d),probs=c(0,0.25,0.5,0.75,1))]

# time from ART initiation to DTG adoption date, in ART-experienced patients at DTG adoption
DT[pre_adopt_ind==1, median(as.numeric(dtg_adopt_d-enrol_d)/365.25),by="program"]

