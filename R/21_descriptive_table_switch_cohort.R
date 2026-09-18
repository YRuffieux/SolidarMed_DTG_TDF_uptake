library(data.table)
library(tableone)
library(writexl)

filepath_processed <- "C:/ISPM/Data/SolidarMed/processed"
filepath_tables <- "C:/ISPM/HomeDir/SolidarMed report/Output/Tables"

load(file=file.path(filepath_processed,"DTG_switch_cohort.RData"))

DTu <- unique(DT,by="patient")
DTu[,age_dtg_adopt_cat:=cut(age_dtg_adopt,breaks=c(0,20,30,40,50,70,Inf),right=FALSE)]

tab <- CreateTableOne(vars=c("sex","age_dtg_adopt_cat","age_dtg_adopt"),strata="district",test=FALSE,data=DTu)
tab <- print(tab,nonnormal="age_dtg_adopt",showAllLevels=FALSE,quote=FALSE,printToggle=FALSE,noSpaces=TRUE)
tab <- data.table(cbind(row.names(tab),tab))

write_xlsx(tab,path=file.path(filepath_tables,"descriptive_DTG_switch_cohort.xlsx"))