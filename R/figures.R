# This script plots all figures

library(tidyverse); library(cowplot); library(ggrepel)
library(lme4)
library(vegan)

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
    axis.text=element_text(size=10, color="black"),
    axis.title=element_text(size=12),
    legend.text=element_text(size=10),
    legend.title=element_text(size=12),
    strip.text=element_text(size=10))
}

# load df with richness, CSI, and prop exotic sp
{
  veg_df <- read.csv("data/cleaned/veg_rich_CSI_exot.csv")
  veg_df$Year <- as.factor(veg_df$Year) # convert to factor so it can be modeled with random intercept
}

# prep df for NMDS with HD levels based on hd %
# load veg site x sp matrices with only sites in 2 or 3 HF groups; calc chao dissimilarity
{
  veg_2groups <- read.csv("data/cleaned/ordination data/veg site x sp mat - 2 HF groups.csv")
  veg_2groups$Year <- as.factor(veg_2groups$Year)
  # seaparate protocols
  veg_2groups_ter <- veg_2groups %>% filter(Protocol=="Terrestrial")
  veg_2groups_wet <- veg_2groups %>% filter(Protocol=="Wetland")
  
  veg_3groups <- read.csv("data/cleaned/ordination data/veg site x sp mat - 3 HF groups.csv")
  veg_3groups$Year <- as.factor(veg_3groups$Year)
  # seaparate protocols
  veg_3groups_ter <- veg_3groups %>% filter(Protocol=="Terrestrial")
  veg_3groups_wet <- veg_3groups %>% filter(Protocol=="Wetland")
  
  veg_distance_2groups <-  vegdist(veg_2groups[,9:ncol(veg_2groups)], method="chao", binary=T)
  veg_distance_2groups_ter <- vegdist(veg_2groups_ter[,9:ncol(veg_2groups_ter)], method="chao", binary=T)
  veg_distance_2groups_wet <- vegdist(veg_2groups_wet[,9:ncol(veg_2groups_wet)], method="chao", binary=T)
  
  veg_distance_3groups <-  vegdist(veg_3groups[,9:ncol(veg_3groups)], method="chao", binary=T)
  veg_distance_3groups_ter <- vegdist(veg_3groups_ter[,9:ncol(veg_3groups_ter)], method="chao", binary=T)
  veg_distance_3groups_wet <- vegdist(veg_3groups_wet[,9:ncol(veg_3groups_wet)], method="chao", binary=T)
}

# Fig 1a - sp richness vs HF (i.e. IDH) ####
{
  fig1a <- ggplot(veg_df, aes(x=totdist_percent, y=rich_observed)) +
    labs(x="Total Human Development (%)", y="Species Richness") +
    geom_point(color="grey70", alpha=0.5) + 
    geom_smooth(method="lm", formula=y~poly(x,2, raw=T), se=F, color=1) +
    geom_smooth(data=veg_df, aes(x=totdist_percent, y=rich_observed, linetype=Protocol), 
                method="lm", formula=y~poly(x,2, raw=T), se=F, color=1, size=0.5) +
    scale_linetype_manual(values=c("dashed", "dotted")) +
    theme_classic() +
    theme(legend.position = "top") + font_sizes
  
  fig1a

  # colored and faceted by NR
  ggplot(spR, aes(x=totdist_percent, y=rich)) +
    labs(x="Total Human Development (%)", y="Species Richness") +
    geom_point(alpha=0.5, aes(color=NRNAME), show.legend = F) + 
    geom_smooth(method="lm", formula=y~poly(x,2, raw=T), se=F, color=1) +
    geom_smooth(method="lm", se=F, color=1, linetype="dashed") +
    # geom_smooth(data=spR, aes(x=totdist_percent, y=rich, linetype=Protocol), 
    #             method="lm", formula=y~poly(x,2, raw=T), se=F, color=1, size=0.5) +
    scale_linetype_manual(values=c("dashed", "dotdash")) +
    facet_wrap(~NRNAME) +
    theme_classic() +
    theme(legend.position = "top") + font_sizes
  
  # colored and faceted by wetland class
  ggplot(spR, aes(x=totdist_percent, y=rich)) +
    labs(x="Total Human Development (%)", y="Species Richness") +
    geom_point(alpha=0.5, aes(color=WetlandType), show.legend = F) + 
    geom_smooth(method="lm", formula=y~poly(x,2, raw=T), se=F, color=1) +
    geom_smooth(method="lm", se=F, color=1, linetype="dashed") +
    # geom_smooth(data=spR, aes(x=totdist_percent, y=rich, linetype=Protocol), 
    #             method="lm", formula=y~poly(x,2, raw=T), se=F, color=1, size=0.5) +
    scale_linetype_manual(values=c("dashed", "dotdash")) +
    facet_wrap(~WetlandType) +
    theme_classic() +
    theme(legend.position = "top") + font_sizes
  
}

# Fig 1b - CSI vs. HF (multiple plots) ####
{
  # black and white
  fig1b <- ggplot(veg_df, aes(x=totdist_percent, y=CSI)) +
    labs(x="Total Human Development (%)", y="Niche Specialization") +
    geom_point(alpha=0.5, color="grey70") + 
    geom_smooth(method="lm", formula=y~poly(x,2, raw=T), se=F, color=1) +
    geom_smooth(data=veg_df, aes(x=totdist_percent, y=CSI, linetype=Protocol), 
                method="lm", formula=y~poly(x,2, raw=T), se=F, color=1, size=0.5) +
    scale_linetype_manual(values=c("dashed", "dotted")) +
    theme_classic() +
    theme(legend.position = "top") + font_sizes
  
  fig1b
  
  # two panel figure
  myleg <- get_legend(fig1a)
  fig1_twopanel <- plot_grid(fig1a + theme(legend.position = "none"),
            fig1b + theme(legend.position="none"),
            ncol=1, nrow=2,
            labels="auto", label_size = 12)
  
  fig1_twopanel <- plot_grid(myleg, fig1_twopanel, 
            ncol=1, nrow=2,
            rel_heights=c(0.1,1))
  
  fig1_twopanel
  ggsave(plot=fig1_twopanel,
         filename = "manuscript/fig1ab_updated.jpeg",
         width=10, height=15, units="cm")
  
  # colored and faceted by protocol
  ggplot(veg_CSI_HF,aes(x=totdist_percent,y=CSI, color=Protocol)) +
    ggtitle("Protocol") +
    labs(x="Total Human Development (%)", y="CSI to human develpoment") +
    geom_point(alpha=0.5) + 
    geom_smooth(method="lm", formula=y~poly(x,2, raw=T), se=F) +
    facet_wrap(~Protocol) +
    theme_classic() +
    theme(legend.position = "none")
  
  # colored and faceted by Wetland Class
  ggplot(veg_CSI_HF,aes(x=totdist_percent,y=CSI, color=WetlandType)) +
    ggtitle("Wetland Class") +
    labs(x="Total Human Development (%)", y="CSI to human develpoment") +
    geom_point(alpha=0.5) +
    geom_smooth(method="lm", formula=y~poly(x,2, raw=T), se=F) +
    theme_classic() +
    facet_wrap(~WetlandType) +
    theme(legend.position = "none")
  
  # colored and faceted by Natural Region
  ggplot(veg_CSI_HF,aes(x=totdist_percent,y=CSI, color=NRNAME)) +
    ggtitle("Natural Region") +
    labs(x="Total Human Development (%)", y="CSI to human develpoment") +
    geom_point(alpha=0.5) +
    geom_smooth(method="lm", formula=y~poly(x,2, raw=T), se=F) +
    theme_classic() +
    facet_wrap(~NRNAME) +
    theme(legend.position = "none")
}

# Fig 2a - exotic species vs HF ####
{
  fig2a <- ggplot(veg_df, aes(x=totdist_percent, y=propexotic)) +
    geom_point(alpha=0.5, color="grey70") +
    geom_smooth(method="lm", formula=y~poly(x,2, raw=T), se=F, color=1) +
    geom_smooth(data=veg_df, aes(x=totdist_percent, y=propexotic,linetype=Protocol), 
                method="lm", formula=y~poly(x,2, raw=T), se=F, size=0.5, color=1) +
    
    scale_linetype_manual(values=c("dashed", "dotted")) +
    labs(x="Total Development (%)", y="Nonnative Sp. (%)") +
    theme_classic() +
    theme(legend.position = "top") + font_sizes
  fig2a
}

# Fig 2b - prop exotic across bins ####
{
  # make new df only with sites in 3 hf bins
  veg_df_3groups <- left_join(veg_df, 
                              select(veg_3groups, Protocol, Site, Year, HFbin),
                              by=c("Protocol", "Site", "Year")) %>% # add bin number to HF df
    filter(!is.na(HFbin))
  # make HF bin and ordered factor
  veg_df_3groups$HFbin <- factor(veg_df_3groups$HFbin, ordered=T, levels=c("Low", "Int.", "High"))
  

  fig2b <-   ggplot(veg_df_3groups, aes(x=HFbin, y=propexotic)) +
    geom_boxplot(fill="grey80") +
    labs(x="Human Development Level", y="Nonnative Sp. (%)") + 
    theme_classic() +
    font_sizes + transparent_plot
  fig2b
  
  myleg <- get_legend(fig2a)
  
  fig2 <- plot_grid(fig2a + theme(legend.position = "none"), 
                    fig2b, 
                        ncol=1, nrow=2, align="v", labels = "auto")
  fig2 <- plot_grid(myleg, fig2, 
                        ncol=1, nrow=2,
                        rel_heights=c(0.1,1))
  fig2
  # ggsave(plot=fig2,
  #        filename="manuscript/fig2ab_updated.jpeg",
  #        width=10, height=15, units="cm")
  
}

# fig s1 - correlelograms from martin ####

# fig s2 - NMDS ordinations ####
{
  
  # NMDS for most vs least and most, int and least dist sites based on HD %
  {
    
    # run ord 2 levels
    {
      veg.nmds_final2 <- metaMDS(veg_2groups[,9:ncol(veg_2groups)], 
                                 distance="raup", 
                                 binary=T, k=7,
                                 trymax=100,
                                 sratmax=0.999999,
                                 sfgrmin = 1e-8,
                                 maxit=500)
      
      # veg.nmds_final2a <- metaMDS(veg_2groups[,9:ncol(veg_2groups)],
      #                             distance="raup",
      #                             binary=T, k=6,
      #                             trymax=100,
      #                             sratmax=0.999999,
      #                             maxit=500,
      #                             previous.best = veg.nmds_final2)
      
      
      # extract site scores and convert to df
      veg.scores2 <- data.frame(scores(veg.nmds_final2, "sites"))
      # add in protocol, site, year, 
      veg.scores2$Protocol <- as.factor(veg_2groups$Protocol)
      veg.scores2$Site <- veg_2groups$Site
      veg.scores2$Year <- veg_2groups$Year
      veg.scores2$NRNAME <- veg_2groups$NRNAME
      veg.scores2$HFbin <- veg_2groups$HFbin
      # veg.scores2$HFbin <- recode(veg_2groups$HFbin, "1"="Low", "10"="High")
      
      spscores2 <- data.frame(scores(veg.nmds_final2, display="species"))
      spscores2$Species <- rownames(spscores2)
      
      # add 5 sp most strongly post & neg cor with axis 1
      impsp_5 <- bind_rows(top_n(spscores2, 5, wt=NMDS1),
                           top_n(spscores2, -5, wt=NMDS1))
      impsp_5 %>% arrange(desc(NMDS1)) # high NMDS1 values = high HD; low NMDS1 vals = low HD 
      
      # note 7 axes; convergence
      nmds2grps <- ggplot(data=impsp_5) +
        geom_point(data=veg.scores2, 
                   aes(x=NMDS1, y=NMDS2, 
                       color=as.factor(HFbin),
                       fill=as.factor(HFbin),
                       shape=as.factor(Protocol)),
                   size=2) +
        stat_ellipse(data=veg.scores2, 
                     aes(x=NMDS1, y=NMDS2, 
                         color=as.factor(HFbin))) +
        scale_color_manual(values=c("#1b9e77", "#d95f02"), name="Human Dev. Level", guide=guide_legend(title.position = "top")) +
        scale_fill_manual(values=c("#1b9e7750", "#d95f0250"), name="Human Dev. Level", guide=guide_legend(title.position = "top")) +
        scale_shape_manual(values=c(21,24), name="Protocol", guide=guide_legend(title.position = "top")) +
        # geom_segment(aes(x=0,y=0,xend=NMDS1,yend=NMDS2),
        #              color=1,
        #              arrow=arrow(length=unit(0.3, "cm"))) +
        # geom_label_repel(aes(x=NMDS1,y=NMDS2,label=Species),
        #                  box.padding=1, size=3.5) +
        theme_classic() + 
        theme(legend.position="top") + font_sizes
      nmds2grps
      
      # ggsave(plot=nmds2grps,
      #        filename="manuscript/nmds2grps - updated.jpeg",
      #        width=12, height=10, units="cm")
    }
    
    # run ord 3 levels
    {
      # 
      veg.nmds_final3 <- metaMDS(veg_3groups[,9:ncol(veg_3groups)], 
                                 distance="raup", binary=T, 
                                 k=5,trymax=200,
                                 maxit=500, sratmax=0.99999999, sfgrmin=1e-8)
      veg.nmds_final3a <- metaMDS(veg_3groups[,9:ncol(veg_3groups)], 
                                 distance="raup", binary=T, 
                                 k=5,trymax=200,
                                 maxit=500, sratmax=0.99999999, sfgrmin=1e-8,
                                 previous.best = veg.nm)
      
      # extract site scores and convert to df
      veg.scores3 <- data.frame(scores(veg.nmds_final3, "sites"))
      # add in protocol, site, year, 
      veg.scores3$Protocol <- as.factor(veg_hf3a$Protocol)
      veg.scores3$Site <- veg_hf3a$Site
      veg.scores3$Year <- veg_hf3a$Year
      veg.scores3$NRNAME <- veg_hf3a$NRNAME
      veg.scores3$HFbin <- veg_hf3a$HFbin
      
      spscores3 <- data.frame(scores(veg.nmds_final3, display="species"))
      spscores3$Species <- rownames(spscores3)
      
      impsp_5b <- bind_rows(top_n(spscores3, 5, wt=NMDS1),
                            top_n(spscores3, -5, wt=NMDS1))
      
      nmds3grps <- ggplot(data=impsp_5b) +
        geom_point(data=veg.scores3, 
                   aes(x=NMDS1, y=NMDS2, 
                       color=as.factor(HFbin),
                       shape=as.factor(Protocol)),
                   size=2) +
        stat_ellipse(data=veg.scores3, 
                     aes(x=NMDS1, y=NMDS2, 
                         color=as.factor(HFbin))) +
        scale_color_manual(values=c("#1b9e77","#7570b3", "#d95f02"), name="HD Level") +
        scale_shape_manual(values=c(1,16), name="Protocol") +
        # geom_segment(aes(x=0,y=0,xend=NMDS1,yend=NMDS2), 
        #              color=1,
        #              arrow=arrow(length=unit(0.3, "cm"))) +
        # geom_label_repel(aes(x=NMDS1,y=NMDS2,label=Species),
        #                  box.padding=1, size=3.5) +
        theme(legend.position="top") + font_sizes
      nmds3grps
      
      # ggsave(plot=nmds3grps,
      #        filename="results/figs/nmds3grps.jpeg",
      #        width=12, height=10, units="cm")
      
      # combined ord figs
      myleg <- get_legend(nmds3grps + theme(legend.justification = "center", legend.direction="horizontal"))
      ordfigs <- plot_grid(nmds2grps + theme(legend.position="none"), 
                           nmds3grps + theme(legend.position="none"), 
                           nrow=1, ncol=2, labels="auto", align="h") 
      
      ordfigs <- plot_grid(myleg, ordfigs, nrow=2, ncol=1, 
                           rel_heights=c(0.2,1) )
      ordfigs
      ggsave(ordfigs, 
             filename="/results/figs/Figure S2.jpeg",
             height=8, width=16, units="cm")
    }
    
  }
  
  # old NMDS ords based on HD bins - ignore
  {
    # prep
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
                                                            meddist=median(totdist_percent),
                                                            min=min(totdist_percent),
                                                            max=max(totdist_percent))
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
                   size=2) +
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
        theme(legend.position="top") + font_sizes
      nmds2grps
      
      # ggsave(plot=nmds2grps,
      #        filename="results/figs/nmds2grps.jpeg",
      #        width=12, height=10, units="cm")
      
    }
    
    # NMDS of most, least, and intermediate dist sites
    {
      # can't get this to converge
      veg.nmds_final3X <- metaMDS(veg_hf3[,9:ncol(veg_hf3)], 
                                  distance="jaccard", binary=T, 
                                  k=5,trymax=500,
                                  maxit=500, sratmax=0.99999999, sfgrmin=1e-8)
      
      veg.nmds_final3Xa <- metaMDS(veg_hf3[,9:ncol(veg_hf3)], 
                                   distance="jaccard", binary=T, 
                                   k=5,trymax=500,
                                   maxit=500, sratmax=0.9999999, sfgrmin=1e-8, previous.best=veg.nmds_final3X)
      
      # this converges
      veg.nmds_final3 <- metaMDS(veg_hf3[,9:ncol(veg_hf3)], 
                                 distance="jaccard", binary=T, 
                                 k=6,trymax=100,
                                 maxit=500, sratmax=0.99999999, sfgrmin=1e-8)
      # veg.nmds_final3a <-  metaMDS(veg_hf3[,9:ncol(veg_hf3)], 
      #                             distance="jaccard", binary=T, 
      #                             k=6,trymax=100,
      #                             maxit=500, sratmax=0.99999999, sfgrmin=1e-8, previous.best = veg.nmds_final3)
      
      
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
                   size=2) +
        stat_ellipse(data=veg.scores3, 
                     aes(x=NMDS1, y=NMDS2, 
                         color=as.factor(HFbin))) +
        scale_color_manual(values=c("#1b9e77","#7570b3", "#d95f02"), name="HD Level") +
        scale_shape_manual(values=c(1,16), name="Protocol") +
        # geom_segment(aes(x=0,y=0,xend=NMDS1,yend=NMDS2), 
        #              color=1,
        #              arrow=arrow(length=unit(0.3, "cm"))) +
        # geom_label_repel(aes(x=NMDS1,y=NMDS2,label=Species),
        #                  box.padding=1, size=3.5) +
        theme(legend.position="top") + font_sizes
      nmds3grps
      
      # ggsave(plot=nmds3grps,
      #        filename="results/figs/nmds3grps.jpeg",
      #        width=12, height=10, units="cm")
      
      # combined ord figs
      myleg <- get_legend(nmds3grps + theme(legend.justification = "center", legend.direction="horizontal"))
      ordfigs <- plot_grid(nmds2grps + theme(legend.position="none"), 
                           nmds3grps + theme(legend.position="none"), 
                           nrow=1, ncol=2, labels="auto", align="h") 
      
      ordfigs <- plot_grid(myleg, ordfigs, nrow=2, ncol=1, 
                           rel_heights=c(0.2,1) )
      ordfigs
      # ggsave(ordfigs, filename="/Users/carif/Dropbox/Desktop/Waterloo/AB plant and invert responses to HF/results/figs/Combined ords.jpeg",
      #        height=8, width=16, units="cm")
    }
  }
}

# fig s3 - CSI across HD levels ####
{
  hf_bin3 <- hf_tot %>% filter(totdist_percent==0 | 
                                 totdist_percent>=90 | 
                                 totdist_percent>=45 & totdist_percent<=55)  %>% 
    mutate(HFbin = ifelse(totdist_percent==0, "Low", 
                          ifelse(totdist_percent>=90, "High", "Int."))) %>% 
    select(Latitude, Longitude, NRNAME, Protocol, WetlandType, Site, Year, HFbin, everything())
  
  csi_bin <- left_join(select(ungroup(hf_bin3),Protocol, WetlandType, Year, Site, HFbin ), 
                       veg_CSI_HF, 
                       by=c("Protocol", "WetlandType", "Year", "Site"))
  
  ggplot(csi_bin, aes(x=HFbin, y=CSI)) +
    labs(x="Human Development Level", y="Niche Specialization") +
    geom_boxplot(fill="grey90") + font_sizes
  
  # ggsave(filename="results/figs/Figure S3.jpeg",
  #        height=7, width=7, units="cm")
  
}

# fig s4. native vs nonnative vs total richness (not prop) across gradient
{
  veg_exot2 <- veg_exot %>% mutate(richexot=round(rich*(propexotic/100),0)) %>% 
    select(NRNAME, Protocol, WetlandType, Site, Year, totdist_percent, "Total"=rich, "Nonnative"=richexot) %>% 
    mutate(Native=Total-Nonnative) %>% 
    gather(., key="Type", value="Richness", 7:9)
  
  nat_rich <- ggplot(filter(veg_exot2, Type=="Native"), aes(x=totdist_percent, y=Richness)) +
    geom_point(alpha=0.5, size=1, col="grey70") +
    geom_smooth(method="lm", formula=y~poly(x,2, raw=T), se=F, col=1) +
    labs(x="Total Human Development (%)", y="Native Species Richness") +
    # lims(y=c(0,105)) +
    theme_classic() +
    font_sizes +  transparent_legend + transparent_plot +
    theme(legend.position = "top",
          legend.title = element_blank())
  nat_rich
  nonnat_rich <- ggplot(filter(veg_exot2, Type=="Nonnative"), aes(x=totdist_percent, y=Richness)) +
    geom_point(alpha=0.5, size=1, col="grey70") +
    geom_smooth(method="lm", formula=y~poly(x,2, raw=T), se=F, col=1) +
    labs(x="Total Human Development (%)", y="Nonnative Species Richness") +
    lims(y=c(0,105)) +
    theme_classic() +
    font_sizes +  transparent_legend + transparent_plot +
    theme(legend.position = "top",
          legend.title = element_blank())
  nonnat_rich
  native_nonnative_richness <- plot_grid(nat_rich, nonnat_rich, labels="auto", ncol=2)
  
  # ggsave(plot=native_nonnative_richness,
  #        filename = "results/figs/native and nonnative richness.jpeg",
  #        width=15, height=8, units="cm")
}

# rich vs HF x Exotics ####
{
  fig3 <- ggplot(veg_exot, aes(x=totdist_percent, y=rich)) +
    geom_point(alpha=0.8, aes(color=propexotic), size=1) +
    labs(x="Total Human Development (%)", y="Species Richness") +
    geom_smooth(method="lm", formula=y~poly(x,2, raw=T),  color=1, se=F) + 
    geom_smooth(data=veg_exot, aes(x=totdist_percent, y=rich, linetype=Protocol), 
                method="lm", formula=y~poly(x,2, raw=T), se=F, color=1, size=0.5) +
    scale_linetype_manual(values=c("dashed", "dotdash")) +
    scale_color_gradient(low="skyblue", high="red", name="% Exotics") +
    theme_classic() +
    theme(legend.position = "top") + font_sizes + 
    guides(linetype=guide_legend(title.position="top"),
           color=guide_colorbar(barwidth = 5, barheight=.8, title.position = "top"))
  fig3
  fig3 <- fig3 + coord_cartesian(ylim=c(0,125), clip="off") +
    annotate("segment", x=-1, xend=1, y=-1, yend=-1, size=1.2) +
    annotate("segment", x=25.5, xend=60.7, y=-1, yend=-1, size=1.2) +
    annotate("segment", x=89.2, xend=101, y=-1, yend=-1, size=1.2)
  
  # ggsave(plot=fig3,
  #        filename="results/figs/fig3.jpeg",
  #        width=10, height=8, units="cm")
  
}

# CSI vs HF x Exotics ####
{
  fig4 <- ggplot(veg_exot, aes(x=totdist_percent, y=CSI)) +
    geom_point(alpha=0.8, aes(color=propexotic), size=3) +
    labs(x="Total Human Development (%)", y="CSI") +
    geom_smooth(method="lm", formula=y~poly(x,2, raw=T), se=F, color=1) +
    geom_smooth(data=veg_exot, aes(x=totdist_percent, y=CSI, linetype=Protocol), 
                method="lm",se=F, formula=y~poly(x,2, raw=T), size=0.5, color=1) +  
    scale_linetype_manual(values=c("dashed", "dotdash")) +
    scale_color_gradient(low="yellow", high="red", name="% Exotics") +
    theme(legend.position = "top",
          legend.direction = "horizontal",
          legend.box="vertical",
          legend.box.just = "left",
          axis.title = element_text(size=22),
          axis.text=element_text(size=20)) 
  fig4
  
  # ggsave(plot=fig4,
  #        filename="results/figs/fig4.jpeg",
  #        width=10, height=8, units="cm")
  
}

# HF across HD levels ####
{
  head(hf_tot)
  hf_bin3 <- hf_tot %>% filter(totdist_percent==0 | 
                                 totdist_percent>=90 | 
                                 totdist_percent>=45 & totdist_percent<=55)  %>% 
    mutate(HFbin = ifelse(totdist_percent==0, "Low", 
                          ifelse(totdist_percent>=90, "High", "Int."))) %>% 
    select(Latitude, Longitude, NRNAME, Protocol, WetlandType, Site, Year, HFbin, everything())
  
  hf_bin3$UniqueID <- paste(hf_bin3$Protocol, hf_bin3$Site, sep="_")
  hf_bin3$HFbin <- factor(hf_bin3$HFbin, ordered=T, levels=c("Low", "Int.", "High"))
  
  fig6 <- ggplot(hf_bin3, aes(x=HFbin, y=totdist_percent)) +
    geom_boxplot(fill="grey80") +
    labs(x="Development Level", y="Human Development (%)") + 
    font_sizes
  fig6
  
  # ggsave(plot=fig6,
  #        filename="results/figs/fig6.jpeg",
  #        width=12, height=10, units="cm")
  
}

