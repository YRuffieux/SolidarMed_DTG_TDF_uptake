# creating "initiation cohort" of people who initiate ART after their country's DTG adoption

library(data.table)

filepath_source <- "C:/ISPM/Data/SolidarMed/source"
filepath_processed <- "C:/ISPM/Data/SolidarMed/processed"

load(file=file.path(filepath_source,"tblBAS.RData"))
load(file=file.path(filepath_source,"tblCENTER.RData"))
load(file=file.path(filepath_processed,"tblART_agg.RData"))

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

DT <- DT[!is.na(center_enrol)]   # removing individuals with no declared clinic

# appending DTG adoption dates, by country
DT[,dtg_adopt_d:=as.Date(NA)]
DT[program=="SMARTLES",dtg_adopt_d:=LES_DTG_ADOPT_D]
DT[program=="SMARTMOZ",dtg_adopt_d:=MOZ_DTG_ADOPT_D]
DT[program=="SMARTZIM",dtg_adopt_d:=ZIM_DTG_ADOPT_D]

# sex
DT[,sex:=factor(sex,levels=c(1,2),labels=c("Male","Female"))]

# age
DT[,age_base:=as.numeric(enrol_d-birth_d)/365.25]

# appending first regimen (should correspond to enrol_d!)
setorder(tblART_agg,"patient","start")
DT <- tblART_agg[,.(patient,start,dtg_ind=as.numeric(grepl("DTG",regimen)))][DT,on="patient",mult="first"]
DT <- DT[!is.na(start)]   # a small number of individuals do not show up in the ART table, excluding them
stopifnot(DT[,all(start==enrol_d)])
DT[,start:=NULL]

# years between DTG adoption and ART initiation
DT[,delta_init:=as.numeric(enrol_d-dtg_adopt_d)/365.25]

# restricting to individuals initiating ART after DTG adoption
DT <- DT[delta_init>=0]

# saving
DT <- DT[,.(patient,program,district,sex,age_base,delta_init,dtg_ind)]
save(DT,file=file.path(filepath_processed,"DTG_init_cohort.RData"))
