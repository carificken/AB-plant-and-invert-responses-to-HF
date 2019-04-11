rm(list=ls())
library(tidyverse); library(cowplot)

# load plant data
veg_pa <- read.csv("/Users/cari/Desktop/Waterloo/AB plant and invert responses to HF/data/cleaned/ABMI veg cleaned.csv")
veg_pa %>% distinct(Protocol, Site, Year) %>% nrow()

# load HF data 
hf <- read.csv("/Users/cari/Desktop/Waterloo/AB plant and invert responses to HF/data/cleaned/Alb wetlands HF.csv") %>% 
  select(Protocol, NRNAME, WetlandType, Site, Year, HFCategory, FEATURE_TY, Area_km2)
hf %>% distinct(Protocol, Site, Year) %>% nrow()

# keep only data in each df for which we have BOTH data types
keepsites <- inner_join(select(ungroup(veg_pa), NRNAME, Protocol, WetlandType, Site, Year),
                        select(ungroup(hf), NRNAME, Protocol, WetlandType, Site, Year)) %>% distinct()
head(keepsites)
dim(keepsites) # 1711
keepsites %>% distinct(Protocol, Site, Year) %>% nrow()

veg_pa <- left_join(keepsites, veg_pa, by=c("NRNAME", "Protocol", "WetlandType", "Site", "Year"))
veg_pa %>% distinct(NRNAME, Protocol, WetlandType, Site, Year) %>% dim() # 1711

hf <- left_join(keepsites, hf, by=c("NRNAME", "Protocol", "WetlandType", "Site", "Year"))
hf %>% distinct(NRNAME, Protocol, WetlandType, Site, Year) %>% dim() # 1711

# summary of site breakdown
veg_pa %>% distinct(Protocol,WetlandType,Site,Year, NRNAME) %>% group_by(Protocol) %>% tally()
veg_pa %>% distinct(Protocol,WetlandType,Site,Year, NRNAME) %>% group_by(WetlandType) %>% tally()
veg_pa %>% distinct(Protocol,WetlandType,Site, Year, NRNAME) %>% group_by(NRNAME) %>% tally()


# compute total dist of each HF Cat
hf <- hf %>% group_by(Protocol, NRNAME, WetlandType, Site, Year, HFCategory) %>% summarize(totdist_cat_km2 = sum(Area_km2))
head(hf)
ptotdist <- hf %>% group_by(NRNAME, HFCategory) %>% summarize(totdist=sum(totdist_cat_km2)) %>% 
  ggplot(aes(x=NRNAME, y=totdist, fill=HFCategory)) +
  geom_bar(stat="identity", position = position_stack()) +
  labs(x=NULL, y="Total Area (km2)") +
  theme_classic() +
  theme(legend.position = "top",
        legend.title=element_blank())

ppropdist <- hf %>% 
  group_by(NRNAME, HFCategory) %>% 
  summarize(totdist=sum(totdist_cat_km2)) %>% 
  group_by(NRNAME) %>%  
  mutate(totdist_all = sum(totdist),
         propdist=100*totdist/totdist_all) %>% 
  ggplot(aes(x=NRNAME, y=propdist, fill=HFCategory)) +
  geom_bar(stat="identity", position = position_stack()) +
  labs(x=NULL, y="Proportional Developed Area (%)") +
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
hf_tot <- hf %>% group_by(Protocol, NRNAME, WetlandType, Site, Year) %>% summarize(totdist_km2=sum(totdist_cat_km2))
ggplot(hf_tot, aes(x=totdist_km2)) + 
  geom_histogram(fill="grey80", color="black") + 
  labs(x="Total Developed Area (km2)", y="Num. Wetlands") +
  ggtitle ("Distribution of total human development in AB") +
  theme_classic() +
  theme(legend.position = "none")


# calculate SSI for each speceis
{
  # first create 10 bins
  hf_tot$HFbin <- ntile(hf_tot$totdist_km2, n=10) 
  
  hf_tot %>%
    ggplot(aes(x=HFbin)) +
    geom_histogram(bins=10, color=1, fill="white") # approx 140 sites per bin
  
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
  unique(occfreq_HFbin$Species) %>% length() # 883 species total; 34 sp occur 1x 
  
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
ggplot(veg_CSI_HF,aes(x=totdist_km2,y=CSI, color=Protocol)) +
  ggtitle("Protocol") +
  labs(x="Total Human Development (km2)", y="CSI to human develpoment") +
  geom_point(alpha=0.5) + 
  geom_smooth(method="lm", formula=y~poly(x,2), se=F) +
  facet_wrap(~Protocol) +
  theme_classic() +
  theme(legend.position = "none")

ggplot(veg_CSI_HF,aes(x=totdist_km2,y=CSI, color=WetlandType)) +
  ggtitle("Wetland Class") +
  labs(x="Total Human Development (km2)", y="CSI to human develpoment") +
  geom_point(alpha=0.5) + 
  geom_smooth(method="lm", formula=y~poly(x,2), se=F) +
  theme_classic() +
  facet_wrap(~WetlandType) +
  theme(legend.position = "none")

ggplot(veg_CSI_HF,aes(x=totdist_km2,y=CSI, color=NRNAME)) +
  ggtitle("Natural Region") +
  labs(x="Total Human Development (km2)", y="CSI to human develpoment") +
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
  
  ggplot(spR, aes(x=totdist_km2, y=rich)) +
    labs(x="Total Human Development (km2)", y="Species Richness") +
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
    ggplot(aes(x=totdist_km2, y=Value, color=ClimVar)) +
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