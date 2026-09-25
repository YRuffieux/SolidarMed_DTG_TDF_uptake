library(data.table)
library(ggplot2)
library(scales)
library(patchwork)

filepath_read <- "C:/ISPM/Data/SolidarMed/output"
filepath_plot <- "C:/ISPM/HomeDir/SolidarMed report/Output/Plots"

# colorblind-friendly palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

##### cumulative probabilities

load(file=file.path(filepath_read,"DTG_probs_init_cohort.RData"))

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

##### adjusted probability differences

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
  labs(y="Percent difference in percentage of DTG uptake")

ggsave(pp,filename=file.path(filepath_plot,"DTG_probdiffs_init_cohort.png"),width=7,height=7,dpi=600)
rm(pp_les,pp_moz,pp_zim,pp)