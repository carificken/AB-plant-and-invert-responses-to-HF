rm(list=ls())
library(tidyverse); library(cowplot)

# load plant data
veg_pa <- read.csv("/Users/cari/Desktop/Waterloo/AB plant and invert responses to HF/data/cleaned/ABMI veg cleaned.csv")
veg_pa %>% distinct(Protocol, Site, Year) %>% nrow()

# load HF data 
hf <- read.csv("/Users/cari/Desktop/Waterloo/AB plant and invert responses to HF/data/cleaned/Alb wetlands HF.csv") 
hf %>% distinct(Protocol, Site, Year) %>% nrow()

# summary of site breakdown
veg_pa %>% distinct(Protocol,WetlandType,Site,Year, NRNAME) %>% group_by(Protocol) %>% tally()
veg_pa %>% distinct(Protocol,WetlandType,Site,Year, NRNAME) %>% group_by(WetlandType) %>% tally()
veg_pa %>% distinct(Protocol,WetlandType,Site, Year, NRNAME) %>% group_by(NRNAME) %>% tally()


# compute total dist of each HF Cat - this doesn't make much sense b/c we are summing percents
head(hf)
ptotdist <- hf %>% group_by(NRNAME, HFCategory) %>% 
  summarize(totdist=sum(((Area_percent/100)*(pi*.25^2))) ) %>% 
  ggplot(aes(x=NRNAME, y=totdist, fill=HFCategory)) +
  geom_bar(stat="identity", position = position_stack()) +
  labs(x=NULL, y="Total Area (down-scaled km2)") +
  theme_classic() +
  theme(legend.position = "top",
        legend.title=element_blank())

ppropdist <- hf %>% group_by(NRNAME, HFCategory) %>% 
  summarize(totdist=sum(((Area_percent/100)*(pi*.25^2))) ) %>% 
  group_by(NRNAME) %>%  
  mutate(totdist_all = sum(totdist),
         propdist=100*totdist/totdist_all) %>% 
  ggplot(aes(x=NRNAME, y=propdist, fill=HFCategory)) +
  geom_bar(stat="identity", position = position_stack()) +
  labs(x=NULL, y="Proportional Developed Area (down-scaled %)") +
  theme_classic() +
  theme(legend.position = "top",
        legend.title=element_blank())

hfcats <- plot_grid(ptotdist + theme(legend.position = "none"),
          ppropdist + theme(legend.position = "none"), nrow=1, ncol=2 )

myleg <- get_legend(ptotdist)

hfcats <- cowplot::plot_grid(myleg,
                             hfcats,
                             ncol=1,rel_heights = c(0.2,2))
hfcats


# calculate total disturbance
hf_tot <- hf %>% group_by(Protocol, NRNAME, WetlandType, Site, Year) %>% summarize(totdist_percent=sum(Area_percent))
ggplot(hf_tot, aes(x=totdist_percent)) + 
  geom_histogram(fill="grey80", color="black") + 
  labs(x="Total Developed Area (% of plot)", y="Num. Wetlands") +
  ggtitle ("Distribution of total human development in AB") +
  theme_classic() +
  theme(legend.position = "none")


# calculate SSI for each speceis
{
  # first create 10 bins
  hf_tot$HFbin <- ntile(hf_tot$totdist_percent, n=10) 
  
  hf_tot %>%
    ggplot(aes(x=HFbin)) +
    geom_histogram(bins=10, color=1, fill="white") # approx 200 sites per bin
  
  veg_hf <- left_join(veg_pa, hf_tot, by=c("NRNAME", "WetlandType", "Protocol", "Site", "Year"))
  
  # plot the occurrence frequency of example species 
  veg_hf %>% 
    group_by(HFbin,Species) %>% summarize(occ_freq=sum(PA)) %>% 
    arrange(Species,desc(occ_freq)) %>% 
    filter(Species=="Typha latifolia") %>% 
    ggplot(aes(x=HFbin,y=occ_freq)) + 
    ggtitle("Typha latifolia") +
    geom_bar(stat="identity")
  
  # calc occurrence freq of each sp in each bin
  # must exclude species which were found in sits w/o climate data, and therefore were not assigned a bin 
  occfreq_HFbin <- veg_hf %>% 
      group_by(HFbin,Species) %>% 
      summarize(occ_freq=sum(PA))

  # exclude species which occur only 1x - they will have high sensitivity
  occfreq_HFbin %>% ungroup() %>% distinct(Species) %>% nrow() # 905 species total; 2 sp occur 1x 
  # occfreq_HFbin %>%
  #   arrange(Species) %>%
  #   group_by(Species) %>%
  #   summarize(cum_occ_freq = sum(occ_freq)) %>%
  #   filter(cum_occ_freq<=1) %>%
  #   nrow()
  
  occfreq_HFbin <- occfreq_HFbin %>% 
    arrange(Species) %>% 
    group_by(Species) %>% 
    mutate(cum_occ_freq = sum(occ_freq)) %>% 
    filter(cum_occ_freq>1) %>% 
    select(-cum_occ_freq)
  
  # expand the bin categories so each species has an occ freq value for every bin #1:10
  occfreq_HFbin <- occfreq_HFbin %>% 
    spread(key=HFbin, value=occ_freq) %>% 
    gather(key=HFbin, value=occ_freq, 2:ncol(.)) %>% 
    mutate(occ_freq=replace_na(occ_freq,0))
  
  # now calculate species sensitivitiy index (=CV) for each species across each HF bin gradient
  head(occfreq_HFbin)
  sp_SSI <- occfreq_HFbin %>% 
    group_by(Species) %>%
    summarize(CV=sd(occ_freq)/mean(occ_freq))
  sp_SSI  
  
}

# explore SSI of each species
{
  # distribution of CV for each species
  ggplot(sp_SSI, aes(x=CV)) + geom_histogram(bins=50)
  
  # which species have very high SSI?
  sp_SSI %>% filter(CV>3) %>% select(Species) -> tmp
  occfreq_HFbin %>% 
    arrange(Species) %>% 
    group_by(Species) %>% 
    mutate(cum_occ_freq = sum(occ_freq)) %>% 
    filter(Species %in% tmp$Species) %>% 
    distinct(Species, cum_occ_freq) %>% arrange(desc(cum_occ_freq)) %>% data.frame()
  
  occfreq_HFbin %>% 
    arrange(Species) %>% 
    group_by(Species) %>% 
    mutate(cum_occ_freq = sum(occ_freq)) %>% 
    filter(Species %in% tmp$Species) %>% 
    ggplot(aes(x=HFbin, y=occ_freq)) +
    geom_bar(stat="identity") +
    scale_x_discrete(limits=c(1:10)) +
    facet_wrap(~Species) +
    theme_bw()
  
  # relationship between cum occ freq and CV scores
  left_join(sp_SSI, occfreq_HFbin) %>%  
    group_by(Species, CV) %>% summarize(cum_occ_freq = sum(occ_freq)) %>% 
    ggplot(aes(x=CV, y=cum_occ_freq)) + 
    ggtitle("Human Footprint") +
    geom_point() 
}

# calculate CSI : mean CV of each community (also compare the summed CV of each community)
{
  veg_CSI_HF <- left_join(veg_pa, sp_SSI)
  head(veg_CSI_HF)
  veg_CSI_HF <- veg_CSI_HF %>% 
    group_by(Protocol,NRNAME, WetlandType,Site,Year) %>% 
    summarize(CSI=mean(CV, na.rm = T)) # 32 sites have na value for at least 1 sp
  
  veg_CSI_HF <- left_join(veg_CSI_HF,hf_tot, by=c("NRNAME", "Protocol", "WetlandType", "Site", "Year")) 
}


# plotting relatinoships bw CSI and HF gradient
# distribution of site-level CSI
{
  ggplot(veg_CSI_HF, aes(x=CSI)) +
    geom_histogram(bins=30) 

# relationship between veg community specialization and HF
ggplot(veg_CSI_HF,aes(x=totdist_percent,y=CSI, color=Protocol)) +
  ggtitle("Protocol") +
  labs(x="Total Human Development (%)", y="CSI to human develpoment") +
  geom_point(alpha=0.5) + 
  geom_smooth(method="lm", formula=y~poly(x,2), se=F) +
  facet_wrap(~Protocol) +
  theme_classic() +
  theme(legend.position = "none")

ggplot(veg_CSI_HF,aes(x=totdist_percent,y=CSI, color=WetlandType)) +
  ggtitle("Wetland Class") +
  labs(x="Total Human Development (%)", y="CSI to human develpoment") +
  geom_point(alpha=0.5) + 
  geom_smooth(method="lm", formula=y~poly(x,2), se=F) +
  theme_classic() +
  facet_wrap(~WetlandType) +
  theme(legend.position = "none")

ggplot(veg_CSI_HF,aes(x=totdist_percent,y=CSI, color=NRNAME)) +
  ggtitle("Natural Region") +
  labs(x="Total Human Development (%)", y="CSI to human develpoment") +
  geom_point(alpha=0.5) + 
  geom_smooth(method="lm", formula=y~poly(x,2), se=F) +
  theme_classic() +
  facet_wrap(~NRNAME) +
  theme(legend.position = "none")
}

# compare sp richness across HF gradient (intermediate disturbance hypothesis)
{
  head(veg_pa)
  spR <- veg_pa %>% group_by(Protocol, NRNAME, WetlandType, Site, Year) %>% summarize(rich=sum(PA))
  spR <- inner_join(spR, hf_tot, by=c("Protocol", "NRNAME", "WetlandType", "Site", "Year"))
  
  ggplot(spR, aes(x=totdist_percent, y=rich)) +
    labs(x="Total Human Development (%)", y="Species Richness") +
    geom_point(alpha=0.5) + 
    geom_smooth(method="lm", formula=y~poly(x,2), se=F) +
    theme_classic()
}


# now compare how plant comm sensitivity to HF varies across clim gradients
{
  # load ABMI climate data
  # clim vars - extracted from ABMI data; locations inaccurate
  {
    clim <- read.csv("/Users/cari/Desktop/Waterloo/AB plant and invert responses to HF/data/raw/climate/ABMI terrestrial + wetland sites climate.csv") %>% select(-Year, -Latitude, -Longitude)
    tail(clim,10)
    veg_pa %>% distinct(Protocol,Site) %>% tail(10)
    
    # remove "W-" from climate sites
    clim$ABMI.Site <- clim$ABMI.Site %>% str_remove(pattern="W") %>% str_remove(pattern="-*")
    
    # create ABMI main site matching ID in veg_pa (veg pa has sites w/ extra numbers at end)
    veg_pa$tmpSiteMatch <- paste(str_extract(string=veg_pa$Site, pattern="OG-"), str_extract(string=veg_pa$Site, pattern="\\d+"), sep="") 
    veg_pa$tmpSiteMatch <- veg_pa$tmpSiteMatch %>% str_remove(pattern="NA")
    
    # sites to keep
    sites <- veg_pa %>% ungroup() %>% select(Protocol, NRNAME, WetlandType, Site, tmpSiteMatch,Year) %>% distinct()
    
    tail(clim,20)
    tail(sites, 20)
    
    clim2 <- left_join(sites,clim, by=c("Protocol", "tmpSiteMatch"="ABMI.Site")) %>% select(-tmpSiteMatch)
    tail(clim2)
    clim2 %>% distinct(NRNAME, Protocol, WetlandType, Site, Year) %>% dim() # good 1653
    
    clim2 %>% gather(key=ClimateVar, value=Value, 6:ncol(clim2)) %>%
      ggplot(aes(x=Value, fill=ClimateVar)) +
      geom_histogram(bins=30) +
      facet_wrap(~ClimateVar, scales="free") + theme_classic() + theme(legend.position="none")
}

  # no correlation between total development and clim conditions
  left_join(veg_CSI_HF, clim2, by=c("NRNAME","WetlandType", "Protocol", "Site", "Year")) %>% 
    gather(key="ClimVar", value=Value, 9:ncol(.)) %>% 
    ggplot(aes(x=totdist_percent, y=Value, color=ClimVar)) +
    geom_point(alpha=0.5) +
    facet_wrap(~ClimVar, scales="free_y") +
    # geom_smooth(method="lm", formula=y~poly(x,2), se=F) +
    theme_classic() +
    theme(legend.position = "none")
  
  cmd <- left_join(veg_CSI_HF, clim2, by=c("NRNAME","WetlandType", "Protocol", "Site", "Year")) %>% 
      ggplot(aes(x=climatic_moisture_defecit, y=CSI)) +
      geom_point(alpha=0.5) + 
      geom_smooth(method="lm", formula=y~poly(x,2), se=F) +
      theme_classic() 

  ffp <- left_join(veg_CSI_HF, clim2, by=c("NRNAME","WetlandType", "Protocol", "Site", "Year")) %>% 
    ggplot(aes(x=FFP, y=CSI)) +
    geom_point(alpha=0.5) + 
    geom_smooth(method="lm", formula=y~poly(x,2), se=F) +
    theme_classic() 
  
  map <- left_join(veg_CSI_HF, clim2, by=c("NRNAME","WetlandType", "Protocol", "Site", "Year"))%>% 
    ggplot(aes(x=MAP, y=CSI)) +
    geom_point(alpha=0.5) + 
    geom_smooth(method="lm", formula=y~poly(x,2), se=F) +
    theme_classic() 
  
  mat <- left_join(veg_CSI_HF, clim2,by=c("NRNAME","WetlandType", "Protocol", "Site", "Year")) %>% 
    ggplot(aes(x=MAT, y=CSI)) +
    geom_point(alpha=0.5) + 
    geom_smooth(method="lm", formula=y~poly(x,2), se=F) +
    theme_classic() 
  
  summer_precip <- left_join(veg_CSI_HF, clim2, by=c("NRNAME","WetlandType", "Protocol", "Site", "Year")) %>% 
    ggplot(aes(x=summer_precip, y=CSI)) +
    geom_point(alpha=0.5) + 
    geom_smooth(method="lm", formula=y~poly(x,2), se=F) +
    theme_classic() 
  
  plot_grid(cmd, ffp, map, mat, summer_precip, nrow=3, ncol=2)

}

# what are the most predictive variables of CSI_hf? use regression trees and random forest
{
  
  library(rpart); library(rpart.plot)
  
  # add climate data
  predvars <- left_join(veg_CSI_HF, clim2, by=c("Protocol", "NRNAME", "WetlandType", "Site", "Year")) 
  head(predvars)
  
  # add species richness data
  head(predvars)
  head(spR)
  predvars <- inner_join(predvars, spR, by=c("Protocol", "NRNAME", "WetlandType", "Site", "Year", "totdist_km2")) %>% 
    distinct()
  
  # convert factors to factor (from character)
  predvars$Protocol <- as.factor(predvars$Protocol)
  predvars$NRNAME <- as.factor(predvars$NRNAME)
  predvars$WetlandType <- as.factor(predvars$WetlandType)
  
  # grow tree
  predvar_fit <- rpart(CSI ~ Protocol + NRNAME + WetlandType + rich + climatic_moisture_defecit + MAT + MAP + FFP + summer_precip, 
                       data=predvars, method="anova")
  
  # pick tree that minimizes miss-classification rate (prediction error); 
  # we want cp values that minimizes xerror
  bestcp <- predvar_fit$cptable[which.min(predvar_fit$cptable[,"xerror"]),"CP"]
  # prune tree usinging bestcp
  predvar_fit.pruned <- prune(predvar_fit, cp = bestcp)
  
  # plot tree 
  prp(predvar_fit.pruned, faclen = 0, cex = 0.8, extra = 1)
  
  # now generate random forest to compare the importance of each predictor
  library(randomForest)
  fit <- randomForest(CSI ~ Protocol + NRNAME + WetlandType + rich + climatic_moisture_defecit + MAT + MAP + FFP + summer_precip, 
                      data=predvars,
                      ntree=1000, replace=T)
  importance(fit) # importance of each predictor
  mean(fit$rsq)
  
}

# ordinations of the highest and lowest disturbed sites
veg_hf2 <- veg_hf %>% 
  select(NRNAME, Protocol, WetlandType, Site, Year, Species, PA)
head(veg_hf2)
veg_hf2 <- veg_hf2 %>% 
  spread(key=Species, value=PA) %>% 
  gather(key=Species, value=PA, 6:ncol(.)) %>% 
  replace_na(list(PA=0)) %>% 
  spread(key=Species, value=PA) 

veg_hf2 <- left_join(veg_hf2, select(hf_tot, -totdist_percent), by=c("NRNAME", "Protocol", "WetlandType", "Site", "Year"))
veg_hf2 <- veg_hf2 %>% filter(HFbin==1 | HFbin==10) %>% select(NRNAME, Protocol, WetlandType, Site, Year, HFbin, everything())

veg_hf2[1:5,1:10]

# distance matrix
veg_d <- vegdist(veg_hf2[,7:ncol(veg_hf2)], method="jaccard", binary=T)

# check for sig diffs among groups
mrpp(veg_d, grouping=as.factor(veg_hf2$HFbin)) # sig diff between groups based on mean score

# run ordinations to compare stress across diff axis numbers
{
  veg.nmds2 <- metaMDS(veg_d, k=2,trymax=100)
  veg.nmds3 <- metaMDS(veg_d, k=3,trymax=100)
  veg.nmds4 <- metaMDS(veg_d, k=4,trymax=100)
  veg.nmds5 <- metaMDS(veg_d, k=5,trymax=100)
  veg.nmds6 <- metaMDS(veg_d, k=6,trymax=100)
  veg.nmds7 <- metaMDS(veg_d, k=7,trymax=100)
  veg.nmds8 <- metaMDS(veg_d, k=8,trymax=100)
  veg.nmds9 <- metaMDS(veg_d, k=9,trymax=100)
  veg.nmds10 <- metaMDS(veg_d, k=10,trymax=100)
  veg.nmds11 <- metaMDS(veg_d, k=11,trymax=100)
  veg.nmds12 <- metaMDS(veg_d, k=12,trymax=100)
  veg.nmds13 <- metaMDS(veg_d, k=13,trymax=100)
  veg.nmds14 <- metaMDS(veg_d, k=14,trymax=100)
  
  data.frame(Dim=c(veg.nmds2$ndim,
                   veg.nmds3$ndim,
                   veg.nmds4$ndim,
                   veg.nmds5$ndim,
                   veg.nmds6$ndim,
                   veg.nmds7$ndim,
                   veg.nmds8$ndim,
                   veg.nmds9$ndim,
                   veg.nmds10$ndim,
                   veg.nmds11$ndim,
                   veg.nmds12$ndim,
                   veg.nmds13$ndim,
                   veg.nmds14$ndim),
             Stress=c(veg.nmds2$stress,
                      veg.nmds3$stress,
                      veg.nmds4$stress,
                      veg.nmds5$stress,
                      veg.nmds6$stress,
                      veg.nmds7$stress,
                      veg.nmds8$stress,
                      veg.nmds9$stress,
                      veg.nmds10$stress,
                      veg.nmds11$stress,
                      veg.nmds12$stress,
                      veg.nmds13$stress,
                      veg.nmds14$stress) ) %>%
    ggplot(aes(x=Dim,y=Stress)) + geom_point() + geom_line() + scale_x_continuous(breaks=c(1:15)) +
    geom_hline(yintercept = c(0.1,0.05), color="red") +
    annotate("text", x=2.5, y=c(.102,.052), label=c("stress=0.1", "stress=0.05"), color="red")
}

veg.nmds4 <- metaMDS(veg_hf2[,7:ncol(veg_hf2)], distance="jaccard", binary=T, k=4,trymax=100)

# extract site scores and convert to df
veg.scores <- data.frame(scores(veg.nmds4, "sites"))
# add in protocol, site, year, 
veg.scores$Protocol <- as.factor(veg_hf2$Protocol)
veg.scores$Site <- veg_hf2$Site
veg.scores$Year <- veg_hf2$Year
veg.scores$NRNAME <- veg_hf2$NRNAME
veg.scores$HFbin <- veg_hf2$HFbin
head(veg.scores)

ggplot(veg.scores, aes(x=NMDS1, y=NMDS2, 
                       color=as.factor(HFbin), 
                       shape=as.factor(HFbin))) +
  geom_point(size=3) + 
  stat_ellipse() +
  scale_color_brewer(palette = "Dark2", name="HF Bin") +
  scale_shape_manual(values=c(15,16), name="HF Bin") +
  # geom_label(aes(label=Site)) +
  theme_bw() +
  guides(col=guide_legend(ncol=4)) +
  theme(legend.position = "top", 
        panel.grid=element_blank())

ggplot(veg.scores, aes(x=NMDS3, y=NMDS4, color=as.factor(HFbin), shape=as.factor(HFbin))) +
  geom_point(size=3) + 
  stat_ellipse() +
  scale_color_brewer(palette = "Dark2", name="HF Bin") +
  scale_shape_manual(values=c(15,16), name="HF Bin") +
  # geom_label(aes(label=Site)) +
  theme_bw() +
  guides(col=guide_legend(ncol=4)) +
  theme(legend.position = "top", 
        panel.grid=element_blank())

spscores <- data.frame(scores(veg.nmds4, display="species"))

spscores$Species <- rownames(spscores)

impsp_5 <- bind_rows(top_n(spscores, 5, wt=NMDS1),
                     top_n(spscores, -5, wt=NMDS1))

ggplot(data=impsp_5) +
  geom_point(data=veg.scores, 
             aes(x=NMDS1, y=NMDS2, 
                 color=as.factor(HFbin), 
                 shape=as.factor(HFbin)),
             size=3) +
  stat_ellipse(data=veg.scores, 
               aes(x=NMDS1, y=NMDS2, 
                   color=as.factor(HFbin))) +
  scale_color_brewer(palette = "Dark2", name="HF Bin") +
  scale_shape_manual(values=c(15,16), name="HF Bin") +
  geom_segment(aes(x=0,y=0,xend=NMDS1,yend=NMDS2), 
               color=1,
               arrow=arrow(length=unit(0.3, "cm"))) +
  geom_label_repel(aes(x=NMDS1,y=NMDS2,label=Species),
                   box.padding=1, size=3.5) +
  theme(legend.position="top")

spdriversp
