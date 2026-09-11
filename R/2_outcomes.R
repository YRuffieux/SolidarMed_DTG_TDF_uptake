# censoring dates and revised outcomes (Death, Transfer, LTFU, RIC)
# if still in care: censor date = database closure
# if LTFU (90 days late to last scheduled visit): censor date = date of last visit
# otherwise date of Death/Transfer

library(data.table)

filepath_source <- "C:/ISPM/Data/SolidarMed/source"
filepath_write <- "C:/ISPM/Data/SolidarMed/processed"

load(file=file.path(filepath_source,"tblART.RData"))
load(file=file.path(filepath_source,"tblBAS.RData"))
load(file=file.path(filepath_source,"tblLAB_CD4.RData"))
load(file=file.path(filepath_source,"tblLAB_RNA.RData"))
load(file=file.path(filepath_source,"tblLTFU.RData"))
load(file=file.path(filepath_source,"tblVIS.RData"))

DB_CLOSING_D <- as.Date("2025-12-31")

tblLTFU[,`:=`(outcome="In care",outcome_d=as.Date(NA))]
tblLTFU[drop_rs==1,`:=`(outcome="Transfer",outcome_d=drop_d)]
tblLTFU[drop_rs==4,`:=`(outcome="LTFU",outcome_d=drop_d)]
tblLTFU[!is.na(death_d),`:=`(outcome="Dead",outcome_d=death_d)]
tblLTFU[,outcome:=factor(outcome,levels=c("In care","Transfer","LTFU","Dead"))]

# determining median time between scheduled visits, by program
tblVIS <- merge(tblVIS,tblBAS[,.(patient,program)],by="patient")
tblVIS[,delta:=as.numeric(next_visit_d-vis_d)]
tblVIS[delta<=0,`:=`(delta=NA,next_visit_d=as.Date(NA))]  # invalidating next scheduled visits that precede the actual visit
tblVIS[,median(delta,na.rm=T),by="program"] # LES -> 63 days, MOZ -> 31 days, ZIM -> 88 days

# removing visits past Januaary 2026 - probably erroneous
tblVIS <- tblVIS[year(vis_d)<=2025 | (year(vis_d)==2026 & month(vis_d)==1)]

# last visits per person
setorder(tblVIS,"patient","vis_d")
tblVISlast <- unique(tblVIS,by="patient",fromLast=TRUE)

# if person is missing next scheduled visit, use program-level median time to next visit
tblVISlast[is.na(next_visit_d) & program=="SMARTLES",next_visit_d:=vis_d+63]
tblVISlast[is.na(next_visit_d) & program=="SMARTMOZ",next_visit_d:=vis_d+31]
tblVISlast[is.na(next_visit_d) & program=="SMARTZIM",next_visit_d:=vis_d+88]

# assigning new outcomes, no change to Death and Transfer, person is LTFU if they are 90+ days late to their last scheduled visit, RIC otherwise
# LTFU -> censor date is at the last visit
# RIC -> censor date is at the last scheduled visit or database closure, whichever comes first
tblOUTCOMES <- tblLTFU[,.(patient,outcome,outcome_d)]
tblOUTCOMES <- merge(tblOUTCOMES,tblVISlast[,.(patient,last_visit_d=vis_d,next_visit_d)],by="patient")
tblOUTCOMES[,`:=`(rev_outcome=outcome,rev_outcome_d=outcome_d)]
tblOUTCOMES[!outcome%in%c("Transfer","Dead") & next_visit_d+90<DB_CLOSING_D,`:=`(rev_outcome="LTFU",rev_outcome_d=last_visit_d)]
tblOUTCOMES[!outcome%in%c("Transfer","Dead") & next_visit_d+90>=DB_CLOSING_D,`:=`(rev_outcome="In care",rev_outcome_d=pmin(next_visit_d,DB_CLOSING_D))]
tblOUTCOMES[,`:=`(last_visit_d=NULL,next_visit_d=NULL)]
stopifnot(tblOUTCOMES[,all(!is.na(rev_outcome_d))])

save(tblOUTCOMES,file=file.path(filepath_write,"tblOUTCOMES.RData"))

X <- rbind(tblART[,.(patient,enc_d=art_sd,type="ART")],
           tblLAB_CD4[,.(patient,enc_d=cd4_d,type="CD4")],
           tblLAB_RNA[,.(patient,enc_d=rna_d,type="RNA")],
           tblVIS[,.(patient,enc_d=vis_d,type="VIS")])
X <- merge(X,tblVISlast[,.(patient,vis_d,next_visit_d)],by="patient")
X[,delta:=as.numeric(enc_d-vis_d)]
setorder(X,"patient","enc_d")
tblOUTCOMES <- X[,.(patient,last_enc_d=enc_d)][tblOUTCOMES,on="patient",mult="last"]
tblOUTCOMES[last_enc_d>rev_outcome_d]
