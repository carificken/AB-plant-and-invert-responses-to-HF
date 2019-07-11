# This script plots all figures

library(tidyverse); library(cowplot); library(ggrepel)
library(lme4)

rm(list=ls())

# graphing specs
{
  transparent_legend =  theme(
    legend.position = "top",
    legend.background = element_rect(fill = NA),
    legend.key = element_rect(fill = NA, 
                              color = NA))
  transparent_plot =  theme(
    plot.background = element_rect(fill = "white", color="white"),
    panel.background = element_rect(fill = "white", color="white"),
    panel.grid = element_blank())
  
  font_sizes <- theme(
    axis.text=element_text(size=6, color="black"),
    axis.title=element_text(size=8),
    legend.text=element_text(size=6),
    legend.title=element_text(size=8),
    strip.text=element_text(size=6))
}

# prep CSI, species richness, and exotic species data frames ####
{
  # plant data ####
  veg_pa <- read.csv("/users/carif/Dropbox/Desktop/Waterloo/AB plant and invert responses to HF/data/cleaned/ABMI veg cleaned_latlong.csv")

  # load HF data ####
  hf <- read.csv("/users/carif/Dropbox/Desktop/Waterloo/AB plant and invert responses to HF/data/cleaned/Alb wetlands HF_latlong.csv") 
  # calculate total disturbance ####
  hf_tot <- hf %>% group_by(Latitude, Longitude, Protocol, NRNAME, WetlandType, Site, Year) %>% summarize(totdist_percent=sum(Area_percent))
 
  # SSI from 1000 randomizations
  {
    sp_SSI <- read.csv("/users/carif/Dropbox/Desktop/Waterloo/AB plant and invert responses to HF/data/cleaned/ssi_final.csv", sep=";")
    colnames(sp_SSI) <- c("Species", "CV")
  }
  
  # calculate CSI : mean CV of each community (also compare the summed CV of each community) ####
  {
    veg_CSI_HF <- left_join(veg_pa, sp_SSI)
    veg_CSI_HF <- veg_CSI_HF %>% 
      group_by(Protocol,NRNAME, WetlandType,Site,Year) %>% 
      summarize(CSI=mean(CV, na.rm = T)) # 32 sites have na value for at least 1 sp
    
    veg_CSI_HF <- left_join(veg_CSI_HF,hf_tot, by=c("NRNAME", "Protocol", "WetlandType", "Site", "Year")) 
    veg_CSI_HF$UniqueID <- paste(veg_CSI_HF$Protocol, veg_CSI_HF$Site, sep="_")
    
  }
  
  # make sp richness df
  {
    spR <- veg_pa %>% group_by(Latitude, Longitude, Protocol, NRNAME, WetlandType, Site, Year) %>% 
      summarize(rich=sum(PA))
    spR <- inner_join(spR, hf_tot, by=c("Latitude", "Longitude", "Protocol", "NRNAME", "WetlandType", "Site", "Year"))
    spR$UniqueID <- paste(spR$Protocol, spR$Site, sep="_")
    
  }
  
  # exotic species
  {
    exotics <- read.csv("data/cleaned/exotic_plants_ab.csv", sep=";")
    veg_exot <- left_join(veg_pa, exotics, by=c("Species"="SPECIES")) %>% select(-TYPE)
    veg_exot <- veg_exot %>% 
      group_by(Latitude, Longitude, Protocol, Site,Year, ORIGIN) %>% 
      tally() %>% 
      spread(key=ORIGIN, value=n) %>% 
      replace_na(list(Exotic=0, Native=0,`Unknown/Undetermined`=0)) %>% 
      rowwise() %>% 
      mutate(rich=sum(Exotic, Native, `Unknown/Undetermined`),
             propexotic=100*(Exotic/rich))
    
    veg_exot <- left_join(veg_CSI_HF, 
                          veg_exot, 
                          by=c("Latitude", "Longitude", "Protocol", "Site", "Year")) %>% 
      select(-Exotic, -Native, -`Unknown/Undetermined`)
  }
  
}

# prep binnned df for NMDS
{
  # prep data frames with 2  or 3 bins
  {
    library(vegan)
    veg_hf <- veg_pa %>% 
      select(Latitude, Longitude, NRNAME, Protocol, WetlandType, Site, Year, Species, PA)
    veg_hf <- veg_hf %>% 
      spread(key=Species, value=PA) %>% 
      gather(key=Species, value=PA, 8:ncol(.)) %>% 
      replace_na(list(PA=0)) %>% 
      spread(key=Species, value=PA) 
    hf_bin <- hf_tot
    hf_bin$HFbin <- ntile(hf_bin$totdist_percent, n=10) 
    hf_bin %>% group_by(as.factor(HFbin)) %>% summarize(meandist=mean(totdist_percent),
                                                        meddist=median(totdist_percent))
    veg_hf <- left_join(veg_hf, 
                        select(hf_bin, -totdist_percent), 
                        by=c("Latitude","Longitude", "NRNAME", "Protocol", "WetlandType", "Site", "Year"))
    
    # veg_hf2 has most & least dist communities
    veg_hf2 <- veg_hf %>% filter(HFbin==1 | HFbin==10) %>% 
      select(Latitude, Longitude, NRNAME, Protocol, WetlandType, Site, Year, HFbin, everything())
    sptokeep2 <- colSums(veg_hf2[,9:ncol(veg_hf2)]) %>% data.frame() 
    sptokeep2$Species <- rownames(sptokeep2)
    colnames(sptokeep2) <- c("Obs", "Species")
    sptokeep2 <-  filter(sptokeep2, Obs > 1)
    veg_hf2 <- veg_hf2 %>% 
      gather(Species, PA, 9:ncol(.)) %>% 
      filter(Species %in% sptokeep2$Species) %>% 
      spread(Species, PA)
    
    # veg_hf3 has most, least, and intermed dist communities
    veg_hf3 <- veg_hf %>% filter(HFbin==1 | HFbin==8 | HFbin==10) %>% 
      select(Latitude, Longitude, NRNAME, Protocol, WetlandType, Site, Year, HFbin, everything())
    sptokeep3 <- colSums(veg_hf3[,9:ncol(veg_hf3)]) %>% data.frame() 
    sptokeep3$Species <- rownames(sptokeep3)
    colnames(sptokeep3) <- c("Obs", "Species")
    sptokeep3 <-  filter(sptokeep3, Obs > 1)
    veg_hf3 <- veg_hf3 %>% 
      gather(Species, PA, 9:ncol(.)) %>% 
      filter(Species %in% sptokeep3$Species) %>% 
      spread(Species, PA)
    
  }
  
  
}

# 1. sp richness vs HF (i.e. IDH) ####
{
  fig1 <- ggplot(spR, aes(x=totdist_percent, y=rich)) +
    labs(x="Total Human Development (%)", y="Species Richness") +
    geom_point(alpha=0.5, color="grey70") + 
    geom_smooth(method="lm", formula=y~poly(x,2), se=F, color=1) +
    geom_smooth(data=spR, aes(x=totdist_percent, y=rich, linetype=Protocol), 
                method="lm", formula=y~poly(x,2), se=F, color=1, size=0.5) +
    scale_linetype_manual(values=c("dashed", "dotdash")) +
    theme_classic() +
    theme(legend.position = "top") + font_sizes
  
  # ggsave(plot=fig1,
  #        filename="results/figs/fig1.jpeg",
  #        width=10, height=8, units="cm")
  
}

# 2 CSI vs. HF (multiple plots) ####
{
  # black and white
  fig2 <- ggplot(veg_CSI_HF, aes(x=totdist_percent, y=CSI)) +
    labs(x="Total Human Development (%)", y="CSI to HD") +
    geom_point(alpha=0.5, color="grey70") + 
    geom_smooth(method="lm", formula=y~poly(x,2), se=F, color=1) +
    geom_smooth(data=veg_CSI_HF, aes(x=totdist_percent, y=CSI, linetype=Protocol), 
                method="lm", formula=y~poly(x,2), se=F, color=1, size=0.5) +
    scale_linetype_manual(values=c("dashed", "dotdash")) +
    theme_classic() +
    theme(legend.position = "top") + font_sizes
  
  # ggsave(plot=fig2,
  #        filename="results/figs/fig2.jpeg",
  #        width=10, height=8, units="cm")
  
  # two panel figure
  myleg <- get_legend(fig1)
  fig1_twopanel <- plot_grid(fig1 + theme(legend.position = "none"),
            fig2 + theme(legend.position="none"),
            ncol=1, nrow=2,
            labels="auto", label_size = 12)
  
  fig1_twopanel <- plot_grid(myleg, fig1_twopanel, 
            ncol=1, nrow=2,
            rel_heights=c(0.1,1))
  ggsave(plot=fig1_twopanel,
         filename = "results/figs/fig1 - two panel.jpeg",
         width=5, height=10, units="cm")
  
  # colored and faceted by protocol
  ggplot(veg_CSI_HF,aes(x=totdist_percent,y=CSI, color=Protocol)) +
    ggtitle("Protocol") +
    labs(x="Total Human Development (%)", y="CSI to human develpoment") +
    geom_point(alpha=0.5) + 
    geom_smooth(method="lm", formula=y~poly(x,2), se=F) +
    facet_wrap(~Protocol) +
    theme_classic() +
    theme(legend.position = "none")
  
  # colored and faceted by Wetland Class
  ggplot(veg_CSI_HF,aes(x=totdist_percent,y=CSI, color=WetlandType)) +
    ggtitle("Wetland Class") +
    labs(x="Total Human Development (%)", y="CSI to human develpoment") +
    geom_point(alpha=0.5) +
    geom_smooth(method="lm", formula=y~poly(x,2), se=F) +
    theme_classic() +
    facet_wrap(~WetlandType) +
    theme(legend.position = "none")
  
  # colored and faceted by Natural Region
  ggplot(veg_CSI_HF,aes(x=totdist_percent,y=CSI, color=NRNAME)) +
    ggtitle("Natural Region") +
    labs(x="Total Human Development (%)", y="CSI to human develpoment") +
    geom_point(alpha=0.5) +
    geom_smooth(method="lm", formula=y~poly(x,2), se=F) +
    theme_classic() +
    facet_wrap(~NRNAME) +
    theme(legend.position = "none")
}

# exotic species vs HF, sp richness, and CSI ####
{
  exotic1 <- ggplot(veg_exot, aes(x=totdist_percent, y=propexotic)) +
    geom_point(alpha=0.5, color="grey70") +
    geom_smooth(method="lm", formula=y~poly(x,2), se=F, color=1) +
    geom_smooth(data=veg_exot, aes(x=totdist_percent, y=propexotic,linetype=Protocol), 
                method="lm", formula=y~poly(x,2), se=F, size=0.5, color=1) +
    scale_linetype_manual(values=c("dashed", "dotdash")) +
    labs(x="Total Development (%)", y="Exotic Sp. (%)") +
    theme(legend.position = "top")
  
  exotic2 <- ggplot(veg_exot, aes(x=rich, y=propexotic)) +
    geom_point(alpha=0.5, color="grey70") +
    labs(x="Sp. Rich.", y="Exotic Sp. (%)") 
  
  exotic3 <- ggplot(veg_exot, aes(x=CSI, y=propexotic)) +
    geom_point(alpha=0.5, color="grey70") +
    geom_smooth(method="lm",se=F, color=1) +
    geom_smooth(data=veg_exot, aes(x=CSI, y=propexotic, linetype=Protocol),
                method="lm", se=F, size=0.5, color=1) +
    scale_linetype_manual(values=c("dashed", "dotdash")) +
    labs(x="CSI", y="Exotic Sp. (%)") +
    theme(legend.position = "top")
  myleg <- get_legend(exotic1)
  exotp <- plot_grid(exotic1 + theme(legend.position = "none"), 
                     exotic2,
                     exotic3 + theme(legend.position = "none"), 
                     nrow=1, ncol=3,
                     labels = "auto")
  plot_grid(myleg, exotp, ncol=1, nrow=2, rel_heights = c(0.05,1))
  
}

# 3. rich vs HF x Exotics ####
{
  fig3 <- ggplot(veg_exot, aes(x=totdist_percent, y=rich)) +
    geom_point(alpha=0.8, aes(color=propexotic)) +
    labs(x="Total Human Development (%)", y="Species Richness") +
    geom_smooth(method="lm", formula=y~poly(x,2),  color=1, se=F) + 
    geom_smooth(data=veg_exot, aes(x=totdist_percent, y=rich, linetype=Protocol), 
                method="lm", formula=y~poly(x,2), se=F, color=1, size=0.5) +
    scale_linetype_manual(values=c("dashed", "dotdash")) +
    scale_color_gradient(low="yellow", high="red", name="% Exotics") +
    theme_classic() +
    theme(legend.position = "top")
  
  # ggsave(plot=fig3,
  #        filename="results/figs/fig3.jpeg",
  #        width=10, height=8, units="cm")
  
}

# 4. CSI vs HF x Exotics ####
{
  fig4 <- ggplot(veg_exot, aes(x=totdist_percent, y=CSI)) +
    geom_point(alpha=0.8, aes(color=propexotic)) +
    labs(x="Total Human Development (%)", y="CSI") +
    geom_smooth(method="lm", formula=y~poly(x,2), se=F, color=1) +
    geom_smooth(data=veg_exot, aes(x=totdist_percent, y=CSI, linetype=Protocol), 
                method="lm",se=F, formula=y~poly(x,2), size=0.5, color=1) +  
    scale_linetype_manual(values=c("dashed", "dotdash")) +
    scale_color_gradient(low="yellow", high="red", name="% Exotics") +
    theme(legend.position = "top") 
  
  # ggsave(plot=fig4,
  #        filename="results/figs/fig4.jpeg",
  #        width=10, height=8, units="cm")
  
}

# 5. NMDS ordinations ####
{
  # NMDS for most vs least dist sites
  {
    veg.nmds_final2 <- metaMDS(veg_hf2[,9:ncol(veg_hf2)], 
                               distance="jaccard", 
                               binary=T, k=5,
                               trymax=100,
                               maxit=500, sratmax=0.999999)
    
    # extract site scores and convert to df
    veg.scores2 <- data.frame(scores(veg.nmds_final2, "sites"))
    # add in protocol, site, year, 
    veg.scores2$Protocol <- as.factor(veg_hf2$Protocol)
    veg.scores2$Site <- veg_hf2$Site
    veg.scores2$Year <- veg_hf2$Year
    veg.scores2$NRNAME <- veg_hf2$NRNAME
    veg.scores2$HFbin <- veg_hf2$HFbin
    veg.scores2$HFbin <- recode(veg.scores2$HFbin, "1"="Low", "10"="High")

    spscores2 <- data.frame(scores(veg.nmds_final2, display="species"))
    spscores2$Species <- rownames(spscores2)
    
    # add 5 sp most strongly post & neg cor with axis 1
    impsp_5 <- bind_rows(top_n(spscores2, 5, wt=NMDS1),
                         top_n(spscores2, -5, wt=NMDS1))
    
    # note 5 axes; no convergence
    nmds2grps <- ggplot(data=impsp_5) +
      geom_point(data=veg.scores2, 
                 aes(x=NMDS1, y=NMDS2, 
                     color=as.factor(HFbin),
                     shape=as.factor(Protocol)),
                 size=3) +
      stat_ellipse(data=veg.scores2, 
                   aes(x=NMDS1, y=NMDS2, 
                       color=as.factor(HFbin))) +
      scale_color_manual(values=c("#1b9e77", "#d95f02"), name="Dist.") +
      scale_shape_manual(values=c(1,16), name="Protocol") +
      # geom_segment(aes(x=0,y=0,xend=NMDS1,yend=NMDS2),
      #              color=1,
      #              arrow=arrow(length=unit(0.3, "cm"))) +
      # geom_label_repel(aes(x=NMDS1,y=NMDS2,label=Species),
      #                  box.padding=1, size=3.5) +
      theme(legend.position="top")
    nmds2grps
    ggsave(plot=nmds2grps,
           filename="results/figs/nmds2grps.jpeg",
           width=12, height=10, units="cm")
    
  }
  
  # NMDS of most, least, and intermediate dist sites
  {
    veg.nmds_final3 <- metaMDS(veg_hf3[,9:ncol(veg_hf3)], 
                               distance="jaccard", binary=T, 
                               k=5,trymax=100,
                               maxit=500, sratmax=0.999999)
    
    # extract site scores and convert to df
    veg.scores3 <- data.frame(scores(veg.nmds_final3, "sites"))
    # add in protocol, site, year, 
    veg.scores3$Protocol <- as.factor(veg_hf3$Protocol)
    veg.scores3$Site <- veg_hf3$Site
    veg.scores3$Year <- veg_hf3$Year
    veg.scores3$NRNAME <- veg_hf3$NRNAME
    veg.scores3$HFbin <- veg_hf3$HFbin
    veg.scores3$HFbin <- recode(veg.scores3$HFbin, "1"="Low", "8"="Int.", "10"="High")
    
    spscores3 <- data.frame(scores(veg.nmds_final3, display="species"))
    spscores3$Species <- rownames(spscores3)
    
    impsp_5b <- bind_rows(top_n(spscores3, 5, wt=NMDS1),
                          top_n(spscores3, -5, wt=NMDS1))
    
    nmds3grps <- ggplot(data=impsp_5b) +
      geom_point(data=veg.scores3, 
                 aes(x=NMDS1, y=NMDS2, 
                     color=as.factor(HFbin), 
                     shape=as.factor(Protocol)),
                 size=3) +
      stat_ellipse(data=veg.scores3, 
                   aes(x=NMDS1, y=NMDS2, 
                       color=as.factor(HFbin))) +
      scale_color_manual(values=c("#1b9e77","#7570b3", "#d95f02"), name="Dist.") +
      scale_shape_manual(values=c(1,16), name="Protocol") +
      # geom_segment(aes(x=0,y=0,xend=NMDS1,yend=NMDS2), 
      #              color=1,
      #              arrow=arrow(length=unit(0.3, "cm"))) +
      # geom_label_repel(aes(x=NMDS1,y=NMDS2,label=Species),
      #                  box.padding=1, size=3.5) +
      theme(legend.position="top")
    
    # ggsave(plot=nmds3grps,
    #        filename="results/figs/nmds3grps.jpeg",
    #        width=12, height=10, units="cm")
  }
}

# 6. HF across bins ###
{
  hf_bin2 <- hf_bin %>% filter(HFbin==1 | HFbin==10)
  hf_bin2$UniqueID <- paste(hf_bin2$Protocol, hf_bin2$Site, sep="_")

  
  hf_bin3 <- hf_bin %>% filter(HFbin==1 | HFbin==8 | HFbin==10)
  hf_bin3$UniqueID <- paste(hf_bin3$Protocol, hf_bin3$Site, sep="_")
  hf_bin3$HFbin <- recode(hf_bin3$HFbin, "1"="Low", "8"="Int.", "10"="High")
  hf_bin3$HFbin <- factor(hf_bin3$HFbin, ordered=T, levels=c("Low", "Int.", "High"))
  
  fig6 <- ggplot(hf_bin3, aes(x=HFbin, y=totdist_percent)) +
    geom_boxplot(fill="grey80") +
    labs(x="Disturbance Level", y="Human Development (%)")
  
  # ggsave(plot=fig6, 
  #        filename="results/figs/fig6.jpeg",
  #        width=12, height=10, units="cm")
  
}

# 7. prop exotic across bins ###
{
  exot_bin <- left_join(select(ungroup(hf_bin),Protocol, WetlandType, Year, Site, HFbin ), 
            veg_exot, 
            by=c("Protocol", "WetlandType", "Year", "Site"))
  
  
  exot_bin2 <- exot_bin %>% filter(HFbin==1 | HFbin==10)
  exot_bin2$UniqueID <- paste(exot_bin2$Protocol, exot_bin2$Site, sep="_")
  
  
  exot_bin3 <- exot_bin %>% filter(HFbin==1 | HFbin==8 | HFbin==10)
  exot_bin3$UniqueID <- paste(exot_bin3$Protocol, exot_bin3$Site, sep="_")
  exot_bin3$HFbin <- recode(exot_bin3$HFbin, "1"="Low", "8"="Int.", "10"="High")
  exot_bin3$HFbin <- factor(exot_bin3$HFbin, ordered=T, levels=c("Low", "Int.", "High"))
  
  fig7 <- ggplot(exot_bin3, aes(x=HFbin, y=totdist_percent)) +
    geom_boxplot(fill="grey80") +
    labs(x="Disturbance Level", y="Prop. Exotics (%)") + font_sizes
  
  ggsave(plot=fig7,
         filename="results/figs/fig7.jpeg",
         width=8, height=8, units="cm")
  
}
