library(readstata13)
library(data.table)
library(tictoc)

filepath_source <- "Y:/IeDEA/IeDEA_Science/Datasets/Stata_Files/Stata_202606_Jun"
filepath_write <- "C:/ISPM/Data/SolidarMed/source"

tic()

tblART <- data.table(read.dta13(file.path(filepath_source,"tblART.dta")))
tblART <- tblART[substr(patient,1,3)%in%c("SML","SMM","SMZ")]
save(tblART,file=file.path(filepath_write,"tblART.RData"))
rm(tblART)

tblBAS <- data.table(read.dta13(file.path(filepath_source,"tblBAS.dta")))
tblBAS <- tblBAS[program%in%c("SMARTLES","SMARTMOZ","SMARTZIM")]
tblBAS[center_enrol=="NA",center_enrol:=as.character(NA)]
tblBAS[center_last=="NA",center_last:=as.character(NA)]
save(tblBAS,file=file.path(filepath_write,"tblBAS.RData"))
rm(tblBAS)

tblLTFU <- data.table(read.dta13(file.path(filepath_source,"tblLTFU.dta")))
tblLTFU <- tblLTFU[substr(patient,1,3)%in%c("SML","SMM","SMZ")]
save(tblLTFU,file=file.path(filepath_write,"tblLTFU.RData"))
rm(tblLTFU)

tblVIS <- data.table(read.dta13(file.path(filepath_source,"tblVIS.dta")))
tblVIS <- tblVIS[substr(patient,1,3)%in%c("SML","SMM","SMZ")]
save(tblVIS,file=file.path(filepath_write,"tblVIS.RData"))
rm(tblVIS)

tblLAB_CD4 <- data.table(read.dta13(file.path(filepath_source,"tblLAB_CD4.dta")))
tblLAB_CD4 <- tblLAB_CD4[substr(patient,1,3)%in%c("SML","SMM","SMZ")]
save(tblLAB_CD4,file=file.path(filepath_write,"tblLAB_CD4.RData"))
rm(tblLAB_CD4)

tblLAB_RNA <- data.table(read.dta13(file.path(filepath_source,"tblLAB_RNA.dta")))
tblLAB_RNA <- tblLAB_RNA[substr(patient,1,3)%in%c("SML","SMM","SMZ")]
save(tblLAB_RNA,file=file.path(filepath_write,"tblLAB_RNA.RData"))
rm(tblLAB_RNA)

tblCENTER <- data.table(read.dta13(file.path(filepath_source,"tblCENTER.dta")))
tblCENTER <- tblCENTER[program%in%c("SMARTMOZ","SMARTLES","SMARTZIM")]
save(tblCENTER,file=file.path(filepath_write,"tblCENTER.RData"))
rm(tblCENTER)