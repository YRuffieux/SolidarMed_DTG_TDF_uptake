library(data.table)
library(tableone)
library(writexl)

filepath_processed <- "C:/ISPM/Data/SolidarMed/processed"
filepath_tables <- "C:/ISPM/HomeDir/SolidarMed report/Output/Tables"

load(file=file.path(filepath_processed,"DTG_init_cohort.RData"))

DT[,`:=`(age_base_cat=cut(age_base,breaks=c(0,20,30,40,50,70,Inf),right=FALSE),
         dtg_ind=factor(dtg_ind))]

tab <- CreateTableOne(vars=c("sex","age_base_cat","age_base","dtg_ind"),strata="district",test=FALSE,data=DT)
tab <- print(tab,nonnormal="age_base",showAllLevels=FALSE,quote=FALSE,printToggle=FALSE,noSpaces=TRUE)
tab <- data.table(cbind(row.names(tab),tab))

write_xlsx(tab,path=file.path(filepath_tables,"descriptive_DTG_init_cohort.xlsx"))