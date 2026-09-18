# creating "switching cohort" of people who are on (non-DTG) ART at time of DTG adoption
# survival format, time-updated with respect to age
# t=0 correspond to date of DTG adoption

library(data.table)
library(survival)

filepath_source <- "C:/ISPM/Data/SolidarMed/source"
filepath_processed <- "C:/ISPM/Data/SolidarMed/processed"

load(file=file.path(filepath_source,"tblBAS.RData"))
load(file=file.path(filepath_source,"tblCENTER.RData"))
load(file=file.path(filepath_processed,"tblART_agg.RData"))
load(file=file.path(filepath_processed,"tblOUTCOMES.RData"))

# country DTG adoption dates
LES_DTG_ADOPT_D <- as.Date("2018-11-01")
MOZ_DTG_ADOPT_D <- as.Date("2019-05-01")
ZIM_DTG_ADOPT_D <- as.Date("2019-05-01")

# age cutoffs
age_cutoffs <- c(20,30,40,50,70)

DT <- tblBAS[,.(patient,program,birth_d,enrol_d,sex,center_enrol,center_last)]

# appending first district
#   SMARTLES: Butha-Buthe, Mokhotlong
#   SMARTMOZ: Ancuabe, Chiure
#   SMARTZIM: Bikita, Zaka
DT[is.na(center_enrol),center_enrol:=center_last]
DT[,center_last:=NULL]

DT <- merge(DT,tblCENTER[,.(center_enrol=center,district)],by="center_enrol",all.x=TRUE)
DT[,`:=`(district=factor(district,levels=c("Butha-Buthe","Mokhotlong","Ancuabe","Chiure","Bikita","Zaka")),program=factor(program))]

# appending DTG adoption dates, by country
DT[,dtg_adopt_d:=as.Date(NA)]
DT[program=="SMARTLES",dtg_adopt_d:=LES_DTG_ADOPT_D]
DT[program=="SMARTMOZ",dtg_adopt_d:=MOZ_DTG_ADOPT_D]
DT[program=="SMARTZIM",dtg_adopt_d:=ZIM_DTG_ADOPT_D]

# sex
DT[,sex:=factor(sex,levels=c(1,2),labels=c("Male","Female"))]

# age at DTG adoption date
DT[,age_dtg_adopt:=as.numeric(dtg_adopt_d-birth_d)/365.25]

# censor date
DT <- merge(DT,tblOUTCOMES[,.(patient,rev_outcome_d)],by="patient")

# removing individuals who initiate ART after date of country's DTG adoption
DT <- DT[enrol_d<dtg_adopt_d]

# removing individuals who leave the study prior to DTG introduction
DT <- DT[rev_outcome_d>dtg_adopt_d]

# date of first DTG regimen for each individual (if received)
DT <- tblART_agg[grepl("DTG",regimen),.(patient,first_dtg_d=start)][DT,on="patient",mult="first"]

# removing individuals who initiaited DTG prior to country-wide DTG adoption
DT <- DT[is.na(first_dtg_d) | first_dtg_d>=dtg_adopt_d]

# time-to-event format in years (t=0 corresponds to date of DTG adoption)
DT[,`:=`(start_d=dtg_adopt_d,stop_d=pmin(rev_outcome_d,first_dtg_d,na.rm=TRUE))]
DT[,`:=`(tstart=0,tstop=as.numeric(stop_d-start_d)/365.25)]
DT[,status:=as.numeric(!is.na(first_dtg_d) & stop_d==first_dtg_d)]
DT[,`:=`(start_d=NULL,stop_d=NULL)]

# age scale -> time-updating
DT[,`:=`(tstart=age_dtg_adopt,tstop=tstop+age_dtg_adopt)]
DT <- data.table(survSplit(Surv(tstart,tstop,status)~.,cut=age_cutoffs,episode="age_group_current",data=DT))
DT[,age_group_current:=factor(age_group_current)]

# back to calendar scale
DT[,`:=`(tstart=tstart-age_dtg_adopt,tstop=tstop-age_dtg_adopt)]

# saving
DT <- DT[,.(patient,program,district,sex,age_group_current,tstart,tstop,status)]
save(DT,file=file.path(filepath_processed,"DTG_switch_cohort.RData"))