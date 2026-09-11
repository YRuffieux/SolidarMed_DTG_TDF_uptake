# aggregating tblART into a time-updated, long-format dataset
# tracks regimen changes in patients

library(data.table)
library(readr)
library(survival)
library(tictoc)
library(reshape2)

filepath_source <- "C:/ISPM/Data/SolidarMed/source"
filepath_write <- "C:/ISPM/Data/SolidarMed/processed"

tic()

load(file=file.path(filepath_source,"tblART.RData"))
load(file=file.path(filepath_source,"tblBAS.RData"))
druglist <- data.table(read_csv("https://raw.githubusercontent.com/IeDEA-SA/IeDEA_ART/main/data-raw/IeDEA_druglist.csv",show_col_types = FALSE))

# appending drug information to ART table
tblART <- merge(tblART,druglist[,.(art_id,drug,arv_class)],by="art_id",all.x=TRUE)
setorder(tblART,"patient","art_sd")
tblART <- unique(tblART,by=c("patient","art_sd","drug"))
tblART[,drug:=trimws(drug, which = "both")]

tblART[,table(drug,useNA="a")]
tblART[,table(arv_class,useNA="a")]

tblART[is.na(art_ed),art_ed:=as.Date("2029-12-31")]

# need to split LPV + RTV single entries into separate ones
drugtemp <-  data.table(tblART[, reshape2::colsplit(drug," ",c("drug1","drug2"))])  # should only be LPV + RTV with two drugs
drugtemp[,`:=`(drug1=as.character((drug1)),drug2=as.character((drug2)))] 
tblART <- cbind(tblART,drugtemp)
rm(drugtemp)
tblART[drug1=="",drug1 := NA]
tblART[drug2=="",drug2 := NA]

tblART <- data.table::melt(tblART, 
                           id.vars = c("patient","art_sd","art_ed","arv_class"), 
                           measure.vars =  c("drug1","drug2"), 
                           na.rm = TRUE,
                           value.name="drug")
tblART[,variable:=NULL]
setorder(tblART,"patient","art_sd")
tblART[drug%in%c("LPV","RTV"),arv_class:="PI"]

tblART <- merge(tblART,tblBAS[,.(patient,enrol_d)],by="patient")

# aggregating ART table by regimen
create_regimen_long <- function(patient,art_sd,art_ed,drug,arv_class,enrol_d)
{
  start0 <- as.numeric(enrol_d[1])            # start of follow-up for that individual
  Z <- data.table(patient,start=as.numeric(art_sd),stop=as.numeric(art_ed)+1,drug,arv_class,switch=1)
  Z <- rbind(Z,data.table(patient=Z[1,patient],start=start0,stop=Z[,max(stop)],drug="",arv_class="",switch=1)) # adding background "empty" regimen to capture gaps with no meds
  Z <- data.table(survSplit(Surv(start,stop,switch)~.,cut=Z[,sort(unique(c(start,stop)))],data=Z)) # splitting at each regimen change
  # aggregation: medications and medication type
  Z <- Z[,.(regimen=paste0(sort(drug),collapse=" "),regimen_class=paste0(sort(arv_class),collapse=" "),
            nb_ii=sum(arv_class=="II"),nb_nnrti=sum(arv_class=="NNRTI"),nb_nrti=sum(arv_class=="NRTI"),nb_pi=sum(arv_class=="PI")),
         by=.(patient,start,stop)]
  Z[,`:=`(start=as.Date(start),stop=as.Date(stop),regimen=trimws(regimen,"both"),regimen_class=trimws(regimen_class,"both"))]
  Z[,patient:=NULL]
  setorder(Z,"start")
  Z
}

tblART_agg <- tblART[,create_regimen_long(patient,art_sd,art_ed,drug,arv_class,enrol_d),by="patient"] # aggregating full ART table, takes ~10 minutes

save(tblART_agg,file=file.path(filepath_write,"tblART_agg.RData"))

toc()
