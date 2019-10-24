rm(list=ls())
library(tidyverse); library(cowplot); library(ggrepel)
library(lme4)

# load plant data ####
veg_pa <- read.csv("/users/carif/Dropbox/Desktop/Waterloo/AB plant and invert responses to HF/data/cleaned/ABMI veg cleaned_latlong.csv")
veg_pa %>% distinct(Protocol, Site, Year) %>% nrow()

# load HF data ####
hf <- read.csv("/users/carif/Dropbox/Desktop/Waterloo/AB plant and invert responses to HF/data/cleaned/Alb wetlands HF_latlong.csv") 
hf %>% distinct(Protocol, Site, Year) %>% nrow()
# calculate total disturbance ####
hf_tot <- hf %>% group_by(Latitude, Longitude, Protocol, NRNAME, WetlandType, Site, Year) %>% summarize(totdist_percent=sum(Area_percent))

# summary of site breakdown ####
veg_pa %>% distinct(Protocol,WetlandType,Site,Year, NRNAME) %>% group_by(Protocol) %>% tally()
veg_pa %>% distinct(Protocol,WetlandType,Site,Year, NRNAME) %>% group_by(WetlandType) %>% tally()
veg_pa %>% distinct(Protocol,WetlandType,Site, Year, NRNAME) %>% group_by(NRNAME) %>% tally()

# compute total dist of each HF Cat ####
# this doesn't make much sense b/c we are summing percents
{
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
}

# hist of total disturbance
ggplot(hf_tot, aes(x=totdist_percent)) + 
  geom_histogram(fill="grey80", color="black") + 
  labs(x="Total Developed Area (% of plot)", y="Num. Wetlands") +
  ggtitle ("Distribution of total human development in AB") +
  theme_classic() +
  theme(legend.position = "none")

# OLDER calculate SSI 1X - replaced with randomizations
{
# calculate SSI for each speceis ####
{
  # first create 10 bins
  hf_tot$HFbin <- ntile(hf_tot$totdist_percent, n=10) 
  
  ggplot(hf_tot, aes(x=totdist_percent)) + 
    geom_histogram(fill="grey70", color=1) +
    labs(x="HD (%)") +
    theme(axis.text = element_text(size=22),
          axis.title=element_text(size=22))
  
  hf_tot %>%
    ggplot(aes(x=HFbin)) +
    geom_histogram(bins=10, color=1, fill="grey70") + # approx 200 sites per bin
    labs(x="HD Bin") +
    expand_limits(x=10) +
    scale_x_continuous(breaks=1:10) +
    theme(axis.text = element_text(size=22),
          axis.title=element_text(size=22))
  
  veg_hf <- left_join(veg_pa, hf_tot, by=c("NRNAME", "WetlandType", "Protocol", "Site", "Year"))
  
  # plot the occurrence frequency of example species 
  veg_hf %>% 
    group_by(HFbin,Species) %>% summarize(occ_freq=sum(PA)) %>% 
    arrange(Species,desc(occ_freq)) %>% 
    filter(Species=="Typha latifolia") %>% 
    ggplot(aes(x=HFbin,y=occ_freq)) + 
    ggtitle("Typha latifolia") +
    geom_bar(stat="identity") +
    expand_limits(x=10) +
    scale_x_discrete(limits=1:10, breaks=1:10) +
    labs(x="HD Bin", y="Occurrence (# sites)") +
    theme(axis.text = element_text(size=22),
          axis.title=element_text(size=22))
  
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

# explore SSI of each species ####
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
}
  
# SSI from 1000 randomizations
{
  sp_SSI <- read.csv("/users/carif/Dropbox/Desktop/Waterloo/AB plant and invert responses to HF/data/cleaned/ssi_mean_all randomizations.csv", sep=";")
  head(sp_SSI)
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

# examine spatial & temporal correlations of HF ####
{

  # latitude - sig & strong cor
  ggplot(veg_CSI_HF, aes(x=Latitude, y=totdist_percent)) +
    geom_point(aes(color=NRNAME)) +
    # geom_smooth(method="lm") +
    labs(x="Latitude", y="Disturbance (%)") +
    theme(legend.position="top",
          legend.title = element_blank())
  summary(lm(totdist_percent ~ Latitude, data=veg_CSI_HF))
  cor(veg_CSI_HF$Latitude, veg_CSI_HF$totdist_percent)

  # longitude - sig and weak cor
  ggplot(veg_CSI_HF, aes(x=Longitude, y=totdist_percent)) +
    geom_point(aes(color=NRNAME)) +
    labs(x="Longitude", y="Disturbance (%)") +
    theme(legend.position="top",
          legend.title = element_blank())
  summary(lm(totdist_percent ~ Longitude, data=veg_CSI_HF)) # sig
  cor(veg_CSI_HF$Longitude, veg_CSI_HF$totdist_percent)
  
  # year; not sig
  ggplot(veg_CSI_HF, aes(x=Year, y=totdist_percent)) +
    geom_jitter(aes(color=NRNAME)) +
    labs(x="Year", y="Disturbance (%)") +
    theme(legend.position="top",
          legend.title = element_blank())
  summary(lm(totdist_percent ~ Year, data=veg_CSI_HF)) # ns
  cor(veg_CSI_HF$Year, veg_CSI_HF$totdist_percent)
  
}

# 1. compare sp richness across HF gradient (intermediate disturbance hypothesis) ####
{
  head(veg_pa)
  spR <- veg_pa %>% group_by(Latitude, Longitude, Protocol, NRNAME, WetlandType, Site, Year) %>% 
    summarize(rich=sum(PA))
  spR <- inner_join(spR, hf_tot, by=c("Latitude", "Longitude", "Protocol", "NRNAME", "WetlandType", "Site", "Year"))
  spR$UniqueID <- paste(spR$Protocol, spR$Site, sep="_")
  head(spR)
  
  # check for spatial & temporal correlations of this relationship too
  tmp <- lm(rich ~ totdist_percent, data=spR)
  summary(lm(tmp$residuals ~ spR$Latitude)) # ns
  summary(lm(tmp$residuals ~ spR$Longitude)) # ns
  summary(lm(tmp$residuals ~ spR$Year)) # ns
  
  
  # stats - richness
  {
    rich.linear <- lmer(rich ~ totdist_percent + Protocol + Latitude + Longitude +  Year +
                          (1|UniqueID), 
                        data=spR, REML=F)
    rich.poly <- lmer(rich ~ poly(totdist_percent,2) + Protocol + Latitude + Longitude +  Year +
                        (1|UniqueID), 
                      data=spR, REML=F)
    summary(rich.linear) # RE of unique site ID should be kept
    summary(rich.poly) # RE of unique site ID should be kept
    AIC(rich.linear, rich.poly)
    
    anova(lme(rich ~ poly(totdist_percent,2) + Protocol + Latitude + Longitude + Year,
              random=~1|UniqueID,
              data=spR), type="marginal")
    
    rich.poly1 <- lmer(rich ~ poly(totdist_percent,2) + Protocol + Latitude + Longitude +
                         (1|UniqueID), 
                       data=spR, REML=F)
    rich.poly2 <- lmer(rich ~ poly(totdist_percent,2) + Protocol +
                         (1|UniqueID), 
                       data=spR, REML=F)
    rich.poly3 <- lmer(rich ~ poly(totdist_percent,2) + 
                         (1|UniqueID), 
                       data=spR, REML=F)
    
    AIC(rich.poly, rich.poly1, rich.poly2, rich.poly3)  
    summary(rich.poly2) # latitudinal climate variables not important for richness
    piecewiseSEM::rsquared(rich.poly2)
  }
  
  ggplot(spR, aes(x=totdist_percent, y=rich)) +
    labs(x="Total Human Development (%)", y="Species Richness") +
    geom_point(alpha=0.5, color="grey70") + 
    geom_smooth(method="lm", formula=y~poly(x,2), se=F, color=1) +
    geom_smooth(data=spR, aes(x=totdist_percent, y=rich, linetype=Protocol), 
                method="lm", formula=y~poly(x,2), se=F, color=1, size=0.5) +
    scale_linetype_manual(values=c("dashed", "dotdash")) +
    theme_classic() +
    theme(legend.position = "top")
  
  # for sites sampled 2x how is richness changing
  {
    dblsamp <- spR %>% group_by(UniqueID) %>% tally() %>% filter(n==2)
    dblsamp <- spR %>% filter(UniqueID %in% dblsamp$UniqueID)
    dblsamp <- dblsamp %>% arrange(UniqueID, Year) 
    dblsamp$SamplingTime <- rep(c("Before", "After"), times=470)
    dblsamp %>% group_by(UniqueID) %>% mutate(spchange = After-Before) %>% head()
    
    tmp <- dblsamp %>% ungroup() %>% select(UniqueID, rich, SamplingTime) %>% group_by(UniqueID) %>% spread(key=SamplingTime, value=rich)
    
    tmp$richchange <- ifelse(tmp$After - tmp$Before > 0, "increasing", ifelse(tmp$After-tmp$Before<0, "decreasing", "no change"))
    dblsamp <- left_join(dblsamp, select(tmp, UniqueID, richchange), by=c("UniqueID"))
    
    ggplot(dblsamp, aes(x=totdist_percent, y=rich, group=UniqueID)) +
      labs(x="Total Human Development (%)", y="Species Richness") +
      geom_point(alpha=0.5, color="grey70") + 
      geom_line(aes(color=richchange)) +
      scale_color_manual()
      # geom_smooth(method="lm", formula=y~poly(x,2), se=F, color=1) +
      # geom_smooth(data=spR, aes(x=totdist_percent, y=rich, linetype=Protocol), 
      #             method="lm", formula=y~poly(x,2), se=F, color=1, size=0.5) +
      # scale_linetype_manual(values=c("dashed", "dotdash")) +
      theme_classic() +
      theme(legend.position = "top")
    }
  
  
}

# 2. compare CSI across HF gradient ####
{
  head(veg_CSI_HF)

  ggplot(veg_CSI_HF, aes(x=CSI)) +
    geom_histogram(bins=30) 

  # stats - CSI
  csi.linear <- lmer(CSI ~ totdist_percent + Protocol + Latitude + Longitude + Year +
                            (1|UniqueID), 
             data=veg_CSI_HF,
             REML = F)
  csi.poly <- lmer(CSI ~ poly(totdist_percent,2) + Protocol + Latitude + Longitude + Year +
                     (1|UniqueID), 
                   data=veg_CSI_HF,
                          REML = F)
  summary(csi.linear) # variance on group RE indistinguishable from zero
  summary(csi.poly) # variance on group RE indistinguishable from zero
  
  AIC(csi.linear, csi.poly) # csi poly
  library(nlme)
  anova(lme(CSI ~ poly(totdist_percent,2) + Protocol + Latitude + Longitude + Year,
              random=~1|UniqueID,
              data=veg_CSI_HF), type="marginal")
  csi.poly1 <- lmer(CSI ~ poly(totdist_percent,2) + Protocol + Latitude + Longitude +
                     (1|UniqueID), 
                   data=veg_CSI_HF,
                   REML = F)
  csi.poly2 <- lmer(CSI ~ poly(totdist_percent,2) + Protocol +
                      (1|UniqueID), 
                    data=veg_CSI_HF,
                    REML = F)
  csi.poly3 <- lmer(CSI ~ poly(totdist_percent,2) +
                      (1|UniqueID), 
                    data=veg_CSI_HF,
                    REML = F)
  
  AIC(csi.poly, csi.poly1, csi.poly2, csi.poly3)
  
  summary(csi.poly1)
  piecewiseSEM::rsquared(csi.poly1)

  ggplot(veg_CSI_HF, aes(x=totdist_percent, y=CSI)) +
    labs(x="Total Human Development (%)", y="CSI to human develpoment") +
    geom_point(alpha=0.5, color="grey70") + 
    geom_smooth(method="lm", formula=y~poly(x,2), se=F, color=1) +
    geom_smooth(data=veg_CSI_HF, aes(x=totdist_percent, y=CSI, linetype=Protocol), 
                method="lm", formula=y~poly(x,2), se=F, color=1, size=0.5) +
    scale_linetype_manual(values=c("dashed", "dotdash")) +
    theme_classic() +
    theme(legend.position = "top")
  
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

# 3. exotic species ####
# prep data
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

# plot exotics ~ HF
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

# plot RICHNESS ~ HF*exotics 
{
ggplot(veg_exot, aes(x=totdist_percent, y=rich)) +
  geom_point(alpha=0.8, aes(color=propexotic)) +
  labs(x="Total Human Development (%)", y="Species Richness") +
  geom_smooth(method="lm", formula=y~poly(x,2),  color=1, se=F) + 
  geom_smooth(data=veg_exot, aes(x=totdist_percent, y=rich, linetype=Protocol), 
              method="lm", formula=y~poly(x,2), se=F, color=1, size=0.5) +
  scale_linetype_manual(values=c("dashed", "dotdash")) +
  scale_color_gradient(low="yellow", high="red", name="% Exotics") +
  theme_classic() +
  theme(legend.position = "top")
}

# 3.1 stats richness ~ hf and exotics
{
  rich.poly2 # previous best
  # refit previous best w/ updated df
  rich.poly2a <- lmer(rich ~ poly(totdist_percent,2) + Protocol +
                       (1|UniqueID),
                     data=veg_exot,
                     REML=F)
  rich.exot.only <- lmer(rich ~ propexotic + Protocol +
                        (1|UniqueID),
                      data=veg_exot,
                      REML=F)
  rich.poly.exot <- lmer(rich ~ poly(totdist_percent,2) + propexotic + Protocol +
                     (1|UniqueID),
                   data=veg_exot,
                   REML=F)
  rich.poly.exot.interaction <- lmer(rich ~ poly(totdist_percent,2) * propexotic + Protocol +
                        (1|UniqueID),
                      data=veg_exot,
                      REML=F)
  
  AIC(rich.poly2a, rich.exot.only, rich.poly.exot, rich.poly.exot.interaction)
  anova(rich.poly2a, rich.exot.only, rich.poly.exot, rich.poly.exot.interaction)
  summary(rich.poly.exot.interaction)
  piecewiseSEM::rsquared(rich.poly.exot.interaction)
  piecewiseSEM::rsquared(rich.poly2a)
  anova(rich.poly.exot.interaction, type="chisq")
}

# plot CSI ~ HF*exotics 
{
ggplot(veg_exot, aes(x=totdist_percent, y=CSI)) +
  geom_point(alpha=0.8, aes(color=propexotic)) +
  labs(x="Total Human Development (%)", y="CSI") +
  geom_smooth(method="lm", formula=y~poly(x,2), se=F, color=1) +
  geom_smooth(data=veg_exot, aes(x=totdist_percent, y=CSI, linetype=Protocol), 
              method="lm",se=F, formula=y~poly(x,2), size=0.5, color=1) +  
  scale_linetype_manual(values=c("dashed", "dotdash")) +
  scale_color_gradient(low="yellow", high="red", name="% Exotics") +
  theme(legend.position = "top") 
}

# 3.2. stats CSI ~ hf and exotics
{
  csi.poly1 # previous best
  # refit previous best w/ updated df
  csi.poly1a <- lmer(CSI ~ poly(totdist_percent,2) + Protocol + Latitude + Longitude +
                       (1|UniqueID),
                     data=veg_exot,
                     REML=F)
  csi.exot.only <- lmer(CSI ~ propexotic + Protocol + Latitude + Longitude +
                           (1|UniqueID),
                         data=veg_exot,
                         REML=F)
  csi.poly.exot <- lmer(CSI ~ poly(totdist_percent,2) + propexotic + Protocol + Latitude + Longitude +
                           (1|UniqueID),
                         data=veg_exot,
                         REML=F)
  csi.poly.exot.interaction <- lmer(CSI ~ poly(totdist_percent,2) * propexotic + Protocol + Latitude + Longitude +
                                       (1|UniqueID),
                                     data=veg_exot,
                                     REML=F)
  
  AIC(csi.poly1a, csi.exot.only, csi.poly.exot, csi.poly.exot.interaction)
  anova(csi.poly1a, csi.exot.only, csi.poly.exot, csi.poly.exot.interaction)
  summary(csi.poly.exot.interaction)
  piecewiseSEM::rsquared(csi.poly.exot.interaction)
  piecewiseSEM::rsquared(csi.poly1a)
  anova(csi.poly.exot.interaction, type="chisq")  
}

# now compare how plant comm sensitivity to HF varies across clim gradients ####
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
      ggplot(aes(x=climatic_moisture_defecit, y=CSI, color=Protocol)) +
    geom_point(alpha=0.5, size=0.5) + 
    geom_smooth(method="lm", formula=y~poly(x,2), se=F) +
      theme_classic() + 
    theme(legend.position = "top")

  ffp <- left_join(veg_CSI_HF, clim2, by=c("NRNAME","WetlandType", "Protocol", "Site", "Year")) %>% 
    ggplot(aes(x=FFP, y=CSI, color=Protocol)) +
    geom_point(alpha=0.5, size=0.5) + 
    geom_smooth(method="lm", formula=y~poly(x,2), se=F) +
    theme_classic() +
    theme(legend.position = "none")
  
  map <- left_join(veg_CSI_HF, clim2, by=c("NRNAME","WetlandType", "Protocol", "Site", "Year"))%>% 
    ggplot(aes(x=MAP, y=CSI, color=Protocol)) +
    geom_point(alpha=0.5, size=0.5) + 
    geom_smooth(method="lm", formula=y~poly(x,2), se=F) +
    theme_classic() +
    theme(legend.position = "none")
  
  mat <- left_join(veg_CSI_HF, clim2,by=c("NRNAME","WetlandType", "Protocol", "Site", "Year")) %>% 
    ggplot(aes(x=MAT, y=CSI, color=Protocol)) +
    geom_point(alpha=0.5, size=0.5) + 
    geom_smooth(method="lm", formula=y~poly(x,2), se=F) +
    theme_classic() +
    theme(legend.position = "none")
  
  summer_precip <- left_join(veg_CSI_HF, clim2, by=c("NRNAME","WetlandType", "Protocol", "Site", "Year")) %>% 
    ggplot(aes(x=summer_precip, y=CSI, color=Protocol)) +
    geom_point(alpha=0.5, size=0.5) + 
    geom_smooth(method="lm", formula=y~poly(x,2), se=F) +
    theme_classic() +
    theme(legend.position = "none")
  
  myleg <- get_legend(cmd)
  
  climp <- plot_grid(cmd + theme(legend.position = "none"), 
            ffp, map, mat, summer_precip, nrow=3, ncol=2)
  plot_grid(myleg, climp,
            nrow=2, ncol=1,
            rel_heights = c(0.2,2))

}

# what are the most predictive variables of CSI_hf? use regression trees and random forest ####
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

# ordinations of the highest and lowest disturbed sites ####
veg_hf <- veg_pa %>% 
  select(Latitude, Longitude, NRNAME, Protocol, WetlandType, Site, Year, Species, PA)
head(veg_hf)
veg_hf <- veg_hf %>% 
  spread(key=Species, value=PA) %>% 
  gather(key=Species, value=PA, 8:ncol(.)) %>% 
  replace_na(list(PA=0)) %>% 
  spread(key=Species, value=PA) 
hf_bin <- hf_tot
hf_bin$HFbin <- ntile(hf_bin$totdist_percent, n=10) 
head(hf_bin)
hf_bin %>% group_by(as.factor(HFbin)) %>% summarize(meandist=mean(totdist_percent),
                                                    meddist=median(totdist_percent))

veg_hf <- left_join(veg_hf, 
                     select(hf_bin, -totdist_percent), by=c("Latitude","Longitude", "NRNAME", "Protocol", "WetlandType", "Site", "Year"))

# veg_hf2 has most & least dist communities
# veg_hf3 has most, least, and intermed dist communities
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
veg_hf2[1:5,1:10]

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
veg_hf3[1:5,1:10]

# distance matrix
library(vegan)
veg_d2 <- vegdist(veg_hf2[,9:ncol(veg_hf2)], method="jaccard", binary=T)
veg_d3 <- vegdist(veg_hf3[,9:ncol(veg_hf3)], method="jaccard", binary=T)

# check for sig diffs among groups
mrpp(veg_d2, grouping=as.factor(veg_hf2$HFbin)) # sig diff between groups based on mean score
adonis2(veg_d2 ~ as.factor(veg_hf2$HFbin))
library(RVAideMemoire)
library(ecodist)

pairwise.perm.manova(resp=veg_d3, fact=veg_hf3$HFbin, p.method="holm")
mrpp(veg_d3, grouping=as.factor(veg_hf3$HFbin)) # sig diff between groups based on mean score

# NMDS
{
# run ordinations to compare stress across diff axis numbers - VEG_D2
{
  veg.nmds2 <- metaMDS(veg_d2, k=2,trymax=100)
  veg.nmds3 <- metaMDS(veg_d2, k=3,trymax=100)
  veg.nmds4 <- metaMDS(veg_d2, k=4,trymax=100)
  veg.nmds5 <- metaMDS(veg_d2, k=5,trymax=100)
  veg.nmds6 <- metaMDS(veg_d2, k=6,trymax=100)
  veg.nmds7 <- metaMDS(veg_d2, k=7,trymax=100)
  veg.nmds8 <- metaMDS(veg_d2, k=8,trymax=100)
  veg.nmds9 <- metaMDS(veg_d2, k=9,trymax=100)
  veg.nmds10 <- metaMDS(veg_d2, k=10,trymax=100)
  veg.nmds11 <- metaMDS(veg_d2, k=11,trymax=100)
  veg.nmds12 <- metaMDS(veg_d2, k=12,trymax=100)
  veg.nmds13 <- metaMDS(veg_d2, k=13,trymax=100)
  veg.nmds14 <- metaMDS(veg_d2, k=14,trymax=100)
  
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

# final model plus plotting - veg_df2
{
  
  veg.nmds_final2 <- metaMDS(veg_hf2[,9:ncol(veg_hf2)], distance="jaccard", binary=T, k=5,trymax=100,
                             maxit=500, sratmax=0.999999)
  
  # extract site scores and convert to df
  veg.scores2 <- data.frame(scores(veg.nmds_final2, "sites"))
  # add in protocol, site, year, 
  veg.scores2$Protocol <- as.factor(veg_hf2$Protocol)
  veg.scores2$Site <- veg_hf2$Site
  veg.scores2$Year <- veg_hf2$Year
  veg.scores2$NRNAME <- veg_hf2$NRNAME
  veg.scores2$HFbin <- veg_hf2$HFbin
  head(veg.scores2)
  
  ggplot(veg.scores2, aes(x=NMDS1, y=NMDS2, 
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
  
  ggplot(veg.scores2, aes(x=NMDS1, y=NMDS4, 
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
  
  spscores2 <- data.frame(scores(veg.nmds_final2, display="species"))
  
  spscores2$Species <- rownames(spscores2)
  
  impsp_5 <- bind_rows(top_n(spscores2, 5, wt=NMDS1),
                       top_n(spscores2, -5, wt=NMDS1))
  
  spdriversp2 <- ggplot(data=impsp_5) +
    geom_point(data=veg.scores2, 
               aes(x=NMDS1, y=NMDS2, 
                   color=as.factor(HFbin), 
                   shape=as.factor(HFbin)),
               size=3) +
    stat_ellipse(data=veg.scores2, 
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
  spdriversp2
  
  # ggsave(plot=spdriversp, 
  #        filename="/Users/cari/Desktop/Waterloo/AB plant and invert responses to HF/results/figs/Ordination of HF bin 1 vs 10.jpeg",
  #        height=18,
  #        width=18,
  #        units="cm")
  
}

# run ordinations to compare stress across diff axis numbers - VEG_D3
{
  veg.nmds2 <- metaMDS(veg_d3, k=2,trymax=100)
  veg.nmds3 <- metaMDS(veg_d3, k=3,trymax=100)
  veg.nmds4 <- metaMDS(veg_d3, k=4,trymax=100)
  veg.nmds5 <- metaMDS(veg_d3, k=5,trymax=100)
  veg.nmds6 <- metaMDS(veg_d3, k=6,trymax=100)
  veg.nmds7 <- metaMDS(veg_d3, k=7,trymax=100)
  veg.nmds8 <- metaMDS(veg_d3, k=8,trymax=100)
  veg.nmds9 <- metaMDS(veg_d3, k=9,trymax=100)
  veg.nmds10 <- metaMDS(veg_d3, k=10,trymax=100)
  veg.nmds11 <- metaMDS(veg_d3, k=11,trymax=100)
  veg.nmds12 <- metaMDS(veg_d3, k=12,trymax=100)
  veg.nmds13 <- metaMDS(veg_d3, k=13,trymax=100)
  veg.nmds14 <- metaMDS(veg_d3, k=14,trymax=100)
  
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

# final model plus plotting - veg_df3
{
  # no convergence with 4 axes must try 5
  veg.nmds_final3 <- metaMDS(veg_hf3[,9:ncol(veg_hf3)], distance="jaccard", binary=T, k=4,trymax=100,
                             maxit=600, sratmax=0.9999999, 
                             sfgrmin = 1e-8)
  veg.nmds_final3 <- metaMDS(veg_hf3[,9:ncol(veg_hf3)], distance="jaccard", binary=T, k=5,trymax=100,
                             maxit=500, sratmax=0.999999)
  
  # extract site scores and convert to df
  veg.scores3 <- data.frame(scores(veg.nmds_final3, "sites"))
  # add in protocol, site, year, 
  veg.scores3$Protocol <- as.factor(veg_hf3$Protocol)
  veg.scores3$Site <- veg_hf3$Site
  veg.scores3$Year <- veg_hf3$Year
  veg.scores3$NRNAME <- veg_hf3$NRNAME
  veg.scores3$HFbin <- veg_hf3$HFbin
  head(veg.scores3)
  
  ggplot(veg.scores3, aes(x=NMDS1, y=NMDS2, 
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
  
  ggplot(veg.scores3, aes(x=NMDS1, y=NMDS4, 
                          color=as.factor(HFbin), 
                          shape=as.factor(HFbin))) +
    geom_point(size=3) + 
    stat_ellipse() +
    scale_color_brewer(palette = "Dark2", name="HF Bin") +
    scale_shape_manual(values=c(15,16), name="HF Bin") +
    theme_bw() +
    guides(col=guide_legend(ncol=4)) +
    theme(legend.position = "top", 
          panel.grid=element_blank())
  
  spscores3 <- data.frame(scores(veg.nmds_final3, display="species"))
  
  spscores3$Species <- rownames(spscores3)
  
  impsp_5b <- bind_rows(top_n(spscores3, 5, wt=NMDS1),
                       top_n(spscores3, -5, wt=NMDS1))
  
  spdriversp3 <- ggplot(data=impsp_5b) +
    geom_point(data=veg.scores3, 
               aes(x=NMDS1, y=NMDS2, 
                   color=as.factor(HFbin), 
                   shape=as.factor(HFbin)),
               size=3) +
    stat_ellipse(data=veg.scores3, 
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
  spdriversp3
  
  # ggsave(plot=spdriversp, 
  #        filename="/Users/cari/Desktop/Waterloo/AB plant and invert responses to HF/results/figs/Ordination of HF bin 1 vs 10.jpeg",
  #        height=18,
  #        width=18,
  #        units="cm")
  
}
}

# PCA
{
veg_hf2_pca <- rda(veg_hf2[,9:ncol(veg_hf2)])
data.frame(summary(veg_hf2_pca)$cont)[,1:3] # first 3 PC axes explain 25% of var with UNSCALED data
pca2 <- data.frame(veg_hf2[,1:8],
                   summary(veg_hf2_pca)$sites[,1:3])
pc12 <- ggplot(pca2, aes(x=PC1, y=PC2, color=as.factor(HFbin))) +
  geom_point() +
  stat_ellipse() +
  theme(legend.position = "top")
pc13 <- ggplot(pca2, aes(x=PC1, y=PC3, color=as.factor(HFbin))) +
  geom_point() +
  stat_ellipse() +
  theme(legend.position = "top")
plot_grid(pc12, pc13, ncol=2, nrow=1)
plotly::plot_ly(data=pca2, 
                x=~PC1, y=~PC2, z=~PC3, color=~as.factor(HFbin), alpha=0.5,
                colors=c("red", "darkorchid2", "blue"),
                stroke=~as.factor(HFbin),
                type="scatter3d", mode="markers")



veg_hf3_pca <- rda(veg_hf3[,9:ncol(veg_hf3)], scale=T)
data.frame(summary(veg_hf3_pca)$cont)[,1:3] # first 3 PC axes explain about 25% of var with UNSCALED data
pca3 <- data.frame(veg_hf3[,1:8],
                   summary(veg_hf3_pca)$sites[,1:3])
pc12 <- ggplot(pca3, aes(x=PC1, y=PC2, color=as.factor(HFbin))) +
  geom_point() +
  stat_ellipse() +
  theme(legend.position = "top")
pc13 <- ggplot(pca3, aes(x=PC1, y=PC3, color=as.factor(HFbin))) +
  geom_point() +
  stat_ellipse() +
  theme(legend.position = "top")
plot_grid(pc12, pc13, ncol=2, nrow=1)

plotly::plot_ly(data=filter(pca3), 
                x=~PC3, y=~PC1, z=~PC2, color=~as.factor(HFbin), alpha=0.7,
                colors=c("firebrick3", "purple3", "blue"),
                stroke=~as.factor(HFbin),
                type="scatter3d", mode="markers")

pca3_sp <- data.frame(summary(veg_hf3_pca)$species[,1:3])
pca3_sp$Species <- rownames(pca3_sp)
head(pca3_sp)

impsp3 <- bind_rows(top_n(pca3_sp, n=3, wt=PC1),
          top_n(pca3_sp, n=-3, wt=PC1),
          top_n(pca3_sp, n=3, wt=PC2),
          top_n(pca3_sp, n=-3, wt=PC2),
          top_n(pca3_sp, n=3, wt=PC3),
          top_n(pca3_sp, n=-3, wt=PC3)) %>% distinct()
dim(impsp3)
library(ggrepel)
ggplot(impsp3) +
  geom_point(data=pca3, aes(x=PC1, y=PC2, color=as.factor(pca3$HFbin))) +
  stat_ellipse(data=pca3, aes(x=PC1, y=PC2, color=as.factor(pca3$HFbin)), level = 0.9) +
  geom_segment(aes(x=0,y=0,xend=PC1,yend=PC2), 
               color=1,
               arrow=arrow(length=unit(0.3, "cm"))) +
  geom_label_repel(aes(x=PC1,y=PC2,label=Species),
                   box.padding=1, size=3.5) +
  theme(legend.position = "top")
}

# dendrogram and clustering
{
library(factoextra)
library(fpc)
# k means clustering
km.veg3 <- eclust(veg_hf3[,9:ncol(veg_hf3)], "kmeans", k = 3, graph = FALSE)
km.veg3$cluster %>% head()
veg_hf3[1:5, 1:8]

# Visualize
fviz_cluster(km.veg3, data = veg_d3,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())
# external cluster validation
km3_stats <- fpc::cluster.stats(d=dist(veg_hf3[,9:ncol(veg_hf3)]),
                                        cluster=as.numeric(as.factor(veg_hf3$HFbin)),
                                        alt.clustering=km.veg3$cluster)
# Corrected Rand index
km3_stats$corrected.rand # ranges from -1 to 1 w/ -1 being bad clustering and 1 being perfect
# Meila's VI
km3_stats$vi

# Compute hierarchical clustering and cut into 3 clusters
hclust.veg3 <- eclust(veg_hf3[,9:ncol(veg_hf3)], "hclust", k = 3, graph = FALSE)
# Visualize
fviz_dend(hclust.veg3, rect = TRUE, cex = 0.5,
          k_colors = c("red", "purple", "blue"))

hclust3_stats <- fpc::cluster.stats(d=dist(veg_hf3[,9:ncol(veg_hf3)]),
                                cluster=as.numeric(as.factor(veg_hf3$HFbin)),
                                alt.clustering=hclust.veg3$cluster)
# Corrected Rand index
hclust3_stats$corrected.rand # ranges from -1 to 1 w/ -1 being bad clustering and 1 being perfect
# Meila's VI
hclust3_stats$vi

# PAM clustering and cut into 3 clusters
pam.veg3 <- eclust(veg_hf3[,9:ncol(veg_hf3)], "pam", k = 3, graph = FALSE)
pam3_stats <- fpc::cluster.stats(d=dist(veg_hf3[,9:ncol(veg_hf3)]),
                                    cluster=as.numeric(as.factor(veg_hf3$HFbin)),
                                    alt.clustering=pam.veg3$cluster)
# Corrected Rand index
pam3_stats$corrected.rand # ranges from -1 to 1 w/ -1 being bad clustering and 1 being perfect
# Meila's VI
pam3_stats$vi
}
