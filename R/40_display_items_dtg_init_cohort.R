library(data.table)
library(ggplot2)
library(scales)
library(patchwork)
library(writexl)

filepath_read <- "C:/ISPM/Data/SolidarMed/output"
filepath_plot <- "C:/ISPM/HomeDir/SolidarMed report/Output/Plots"
filepath_tables <- "C:/ISPM/HomeDir/SolidarMed report/Output/Tables"

# colorblind-friendly palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

##### probabilities at ART initiation

load(file=file.path(filepath_read,"DTG_probs_init_cohort.RData"))
load(file=file.path(filepath_read,"DTG_bootstrap_init_cohort.RData"))

pp_les <- ggplot(pred_list[["SMARTLES"]],aes(x=delta_init,y=Estimate,color=district)) +
  geom_line(aes(linetype=district)) +
  geom_ribbon(aes(ymin=lower,ymax=upper,fill=district),linetype=0,alpha=0.2) +
  labs(x="Years since DTG adoption (2018-11-01)",title="(A) Lesotho") +
  scale_color_manual(values=cbPalette[c(4,6)],name="District",labels=c("Butha-Buthe","Mokhotlong")) + 
  scale_fill_manual(values=cbPalette[c(4,6)],name="District",labels=c("Butha-Buthe","Mokhotlong")) +
  scale_linetype_manual(values=c(1,2),name="District",labels=c("Butha-Buthe","Mokhotlong"))

pp_moz <- ggplot(pred_list[["SMARTMOZ"]],aes(x=delta_init,y=Estimate,color=district)) +
  geom_line(aes(linetype=district)) +
  geom_ribbon(aes(ymin=lower,ymax=upper,fill=district),linetype=0,alpha=0.2) +
  labs(x="Years since DTG adoption (2019-05-01)",title="(B) Mozambique") +
  scale_color_manual(values=cbPalette[c(3,7)],name="District",labels=c("Ancuabe","Chiure")) + 
  scale_fill_manual(values=cbPalette[c(3,7)],name="District",labels=c("Ancuabe","Chiure")) +
  scale_linetype_manual(values=c(1,2),name="District",labels=c("Ancuabe","Chiure"))

pp_zim <- ggplot(pred_list[["SMARTZIM"]],aes(x=delta_init,y=Estimate,color=district)) +
  geom_line(aes(linetype=district)) +
  geom_ribbon(aes(ymin=lower,ymax=upper,fill=district),linetype=0,alpha=0.2) +
  labs(x="Years since DTG adoption (2019-05-01)",title="(C) Zimbabwe") +
  scale_color_manual(values=cbPalette[c(2,8)],name="District",labels=c("Bikita","Zaka")) + 
  scale_fill_manual(values=cbPalette[c(2,8)],name="District",labels=c("Bikita","Zaka")) +
  scale_linetype_manual(values=c(1,2),name="District",labels=c("Bikita","Zaka"))

pp <- pp_les + pp_moz + pp_zim + plot_layout(ncol=1, axis_titles="collect_y") &
  theme_bw() &
  theme(panel.grid.minor=element_blank(),legend.position=c(0.8,0.4)) &
  scale_y_continuous(labels=scales::percent,limits=c(0,1)) &
  scale_x_continuous(breaks=0:7) &
  labs(y="Percentage initiating ART on DTG")

ggsave(pp,filename=file.path(filepath_plot,"DTG_probs_init_cohort.png"),width=7,height=7,dpi=600)
rm(pp_les,pp_moz,pp_zim,pp)

# delay (in months) in reaching % uptake thresholds

df_delay <- data.table(program=c("SMARTLES","SMARTMOZ","SMARTZIM"),P0.5=as.character(NA),P0.75=as.character(NA))
for(prog in c("SMARTLES","SMARTMOZ","SMARTZIM"))
{
  X0 <- pred_list[[prog]][district==0]
  X1 <- pred_list[[prog]][district==1]
  for(thresh in c(0.5,0.75))
  {
    t0 <- X0[match(TRUE,X0[,Estimate]>thresh),delta_init]  # first time above threshold
    t1 <- X1[match(TRUE,X1[,Estimate]>thresh),delta_init]
    lower <- list_out[[prog]][,quantile(get(paste0("P",thresh)),probs=0.025)]
    upper <- list_out[[prog]][,quantile(get(paste0("P",thresh)),probs=0.975)]
    # delay in reaching threshold, district 1 vs. 0 (in years), with 95% CIs derived from bootstrap samples
    df_delay[program==prog,paste0("P",thresh):=paste0(round(12*(t1-t0),digits=1)," (",round(12*lower,digits=1),"; ",round(12*upper,digits=1),")")]   
    rm(t0,t1)
  }
  rm(X0,X1)
}

write_xlsx(df_delay,path=file.path(filepath_tables,"DTG_delay_init_cohort.xlsx"))

rm(pred_list)

##### mean probability differences

load(file=file.path(filepath_read,"DTG_probdiffs_init_cohort.RData"))

pp_les <- ggplot(pred_list[["SMARTLES"]],aes(x=delta_init,y=Estimate)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower,ymax=upper),linetype=0,alpha=0.2) +
  labs(x="Years since DTG adoption (2018-11-01)",title="(A) Lesotho: Mokhotlong vs. Butha-Buthe")

pp_moz <- ggplot(pred_list[["SMARTMOZ"]],aes(x=delta_init,y=Estimate)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower,ymax=upper),linetype=0,alpha=0.2) +
  labs(x="Years since DTG adoption (2019-05-01)",title="(B) Mozambique: Chiure vs. Ancuabe")

pp_zim <- ggplot(pred_list[["SMARTZIM"]],aes(x=delta_init,y=Estimate)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower,ymax=upper),linetype=0,alpha=0.2) +
  labs(x="Years since DTG adoption (2019-05-01)",title="(C) Zimbabwe: Zaka vs. Bikita")

pp <- pp_les + pp_moz + pp_zim + plot_layout(ncol=1, axis_titles="collect_y") &
  theme_bw() &
  theme(panel.grid.minor=element_blank(),legend.position=c(0.8,0.4)) &
  scale_y_continuous(labels=scales::percent,limits=c(-0.3,0.3)) &
  scale_x_continuous(breaks=0:7) &
  labs(y="Mean difference in percentage of DTG uptake")

ggsave(pp,filename=file.path(filepath_plot,"DTG_probdiffs_init_cohort.png"),width=7,height=7,dpi=600)
rm(pp_les,pp_moz,pp_zim,pp,pred_list)

##### probabilities at ART initiaiton, by sex within districts

load(file=file.path(filepath_read,"DTG_probs_init_cohort_by_sex_within_districts.RData"))

# SMARTLES: Butha-Buthe & Mokhotlong

pp_1 <- ggplot(pred_list[["Butha-Buthe"]],aes(x=delta_init,y=Estimate,color=sex)) +
  geom_line(aes(linetype=sex)) +
  geom_ribbon(aes(ymin=lower,ymax=upper,fill=sex),linetype=0,alpha=0.2) +
  labs(x="Years since DTG adoption (2018-11-01)",title="Butha-Buthe")

pp_2 <- ggplot(pred_list[["Mokhotlong"]],aes(x=delta_init,y=Estimate,color=sex)) +
  geom_line(aes(linetype=sex)) +
  geom_ribbon(aes(ymin=lower,ymax=upper,fill=sex),linetype=0,alpha=0.2) +
  labs(x="Years since DTG adoption (2018-11-01)",title="Mokhotlong")

pp <- pp_1 + pp_2 + plot_layout(ncol=1, axis_titles="collect_y") &
  theme_bw() &
  theme(panel.grid.minor=element_blank(),legend.position=c(0.8,0.4),legend.title=element_blank()) &
  scale_y_continuous(labels=scales::percent,limits=c(0,1)) &
  scale_x_continuous(breaks=0:7) &
  scale_color_manual(values=cbPalette[c(4,6)]) &
  scale_fill_manual(values=cbPalette[c(4,6)]) &
  scale_linetype_manual(values=c(1,2)) &
  labs(y="Percentage initiating ART on DTG")

ggsave(pp,filename=file.path(filepath_plot,"DTG_probs_init_cohort_by_sex_within_districts_LES.png"),width=7,height=7,dpi=600)
rm(pp_1,pp_2,pp)

# SMARTMOZ: Ancuabe & Chiure

pp_1 <- ggplot(pred_list[["Ancuabe"]],aes(x=delta_init,y=Estimate,color=sex)) +
  geom_line(aes(linetype=sex)) +
  geom_ribbon(aes(ymin=lower,ymax=upper,fill=sex),linetype=0,alpha=0.2) +
  labs(x="Years since DTG adoption (2019-05-01)",title="Ancuabe")

pp_2 <- ggplot(pred_list[["Chiure"]],aes(x=delta_init,y=Estimate,color=sex)) +
  geom_line(aes(linetype=sex)) +
  geom_ribbon(aes(ymin=lower,ymax=upper,fill=sex),linetype=0,alpha=0.2) +
  labs(x="Years since DTG adoption (2019-05-01)",title="Chiure")

pp <- pp_1 + pp_2 + plot_layout(ncol=1, axis_titles="collect_y") &
  theme_bw() &
  theme(panel.grid.minor=element_blank(),legend.position=c(0.8,0.4),legend.title=element_blank()) &
  scale_y_continuous(labels=scales::percent,limits=c(0,1)) &
  scale_x_continuous(breaks=0:7) &
  scale_color_manual(values=cbPalette[c(4,6)]) &
  scale_fill_manual(values=cbPalette[c(4,6)]) &
  scale_linetype_manual(values=c(1,2)) &
  labs(y="Percentage initiating ART on DTG")

ggsave(pp,filename=file.path(filepath_plot,"DTG_probs_init_cohort_by_sex_within_districts_MOZ.png"),width=7,height=7,dpi=600)
rm(pp_1,pp_2,pp)

# SMARTZIM: Bikita & Zaka

pp_1 <- ggplot(pred_list[["Bikita"]],aes(x=delta_init,y=Estimate,color=sex)) +
  geom_line(aes(linetype=sex)) +
  geom_ribbon(aes(ymin=lower,ymax=upper,fill=sex),linetype=0,alpha=0.2) +
  labs(x="Years since DTG adoption (2019-05-01)",title="Bikita")

pp_2 <- ggplot(pred_list[["Zaka"]],aes(x=delta_init,y=Estimate,color=sex)) +
  geom_line(aes(linetype=sex)) +
  geom_ribbon(aes(ymin=lower,ymax=upper,fill=sex),linetype=0,alpha=0.2) +
  labs(x="Years since DTG adoption (2019-05-01)",title="Zaka")

pp <- pp_1 + pp_2 + plot_layout(ncol=1, axis_titles="collect_y") &
  theme_bw() &
  theme(panel.grid.minor=element_blank(),legend.position=c(0.8,0.4),legend.title=element_blank()) &
  scale_y_continuous(labels=scales::percent,limits=c(0,1)) &
  scale_x_continuous(breaks=0:7) &
  scale_color_manual(values=cbPalette[c(4,6)]) &
  scale_fill_manual(values=cbPalette[c(4,6)]) &
  scale_linetype_manual(values=c(1,2)) &
  labs(y="Percentage initiating ART on DTG")

ggsave(pp,filename=file.path(filepath_plot,"DTG_probs_init_cohort_by_sex_within_districts_ZIM.png"),width=7,height=7,dpi=600)
rm(pp_1,pp_2,pp)
