# creating cohort of people on DTG (either at initiation or having switched)
# descriptive results produced here as well

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
DT[,`:=`(district=factor(district,levels=c("Butha-Buthe","Mokhotlong","Ancuabe","Chiure","Bikita","Zaka")),program=factor(program))]
DT[,year_base:=year(enrol_d)]

DT <- DT[!is.na(center_enrol)]   # removing individuals with no declared clinic

# appending DTG adoption dates, by country
DT[,dtg_adopt_d:=as.Date(NA)]
DT[program=="SMARTLES",dtg_adopt_d:=LES_DTG_ADOPT_D]
DT[program=="SMARTMOZ",dtg_adopt_d:=MOZ_DTG_ADOPT_D]
DT[program=="SMARTZIM",dtg_adopt_d:=ZIM_DTG_ADOPT_D]

# indentifying switches from DTG-based to non DTG-based regimen
tblART_agg[,dtg_ind:=as.numeric(grepl("DTG",regimen))]
tblART_agg[,dtg_switch_ind:=dtg_ind - data.table::shift(dtg_ind),by="patient"]

# first date of DTG-based regimen
DT <- tblART_agg[grepl("DTG",regimen),.(patient,first_dtg_d=start)][DT,on="patient",mult="first"]

# indicator for switch away from DTG-based regimen
DT <- tblART_agg[dtg_switch_ind==-1,.(patient,first_off_dtg_d=start)][DT,on="patient",mult="first"]

# restricting to individuals on DTG in post-DTG adoption era, and prior to start of 2025
DT <- DT[!is.na(first_dtg_d) & first_dtg_d>=dtg_adopt_d & year(first_dtg_d)<2025]
stopifnot(DT[,all(is.na(first_off_dtg_d) | (first_off_dtg_d>first_dtg_d))])

# restricting to individuals aged 16 or above at DTG initiation
DT <- DT[as.numeric(first_dtg_d-birth_d)/365.25>=16]

# descriptive results
print(DT[,table(!is.na(first_off_dtg_d))])
print(DT[,prop.table(table(!is.na(first_off_dtg_d)))])
print(DT[!is.na(first_off_dtg_d),table(district)])
print(DT[!is.na(first_off_dtg_d),quantile(as.numeric(first_off_dtg_d-first_dtg_d),probs=c(0.25,0.5,0.75))])


