# This file runs the stats

library(tidyverse); library(cowplot); library(ggrepel)
library(lme4)
library(ape)
library(lmerTest)

rm(list=ls())

# prep CSI, species richness, and exotic species data frames ####
{ 
  # plant data ####
  veg_pa <- read.csv("data/cleaned/ABMI veg cleaned_latlong.csv")
  
  # load HF data ####
  hf <- read.csv("data/cleaned/Alb wetlands HF_latlong.csv") 
  # calculate total disturbance ####
  hf_tot <- hf %>% group_by(Latitude, Longitude, Protocol, NRNAME, WetlandType, Site, Year) %>% summarize(totdist_percent=sum(Area_percent))
  
  # SSI from 1000 randomizations exclude 127 rare sp (<= 3 occurrences)
  {
    sp_SSI <- read.csv("data/cleaned/ssi_mean.csv", sep=",")
    colnames(sp_SSI) <- c("Species", "CV")
    
    # 39 species have poor correlation among randomization runs
    outliers <- read.csv("data/cleaned/species_high_range_SSI.csv")
    sp_SSI_no_outliers <- sp_SSI %>% filter(Species %in% outliers$SCIENTIFIC_NAME == F)
    # sp_SSI <- sp_SSI_no_outliers
    }
  
  # calculate CSI : mean CV of each community (also compare the summed CV of each community) ####
  {
    veg_CSI_HF <- left_join(veg_pa, sp_SSI)
    veg_CSI_HF <- veg_CSI_HF %>% 
      group_by(Protocol,NRNAME, WetlandType,Site,Year) %>% 
      summarize(CSI=mean(CV, na.rm = T)) # 211 sites have na value for at least 1 sp
    
    veg_CSI_HF <- left_join(veg_CSI_HF,hf_tot, by=c("NRNAME", "Protocol", "WetlandType", "Site", "Year")) 
    veg_CSI_HF$UniqueID <- paste(veg_CSI_HF$Protocol, veg_CSI_HF$Site, sep="_")
    
  }
  
  # make sp richness df
  {
    spR <- veg_pa %>% 
      filter(Species %in% sp_SSI$Species) %>% # keep only non-rare species
      group_by(Latitude, Longitude, Protocol, NRNAME, WetlandType, Site, Year) %>% 
      summarize(rich=sum(PA))
    spR <- inner_join(spR, hf_tot, by=c("Latitude", "Longitude", "Protocol", "NRNAME", "WetlandType", "Site", "Year"))
    spR$UniqueID <- paste(spR$Protocol, spR$Site, sep="_")
    
  }
  
  # exotic species
  {
    exotics <- read.csv("data/cleaned/exotic_plants_ab.csv", sep=";")
    veg_exot <- left_join(veg_pa, exotics, by=c("Species"="SPECIES")) %>% select(-TYPE)
    veg_exot <- veg_exot %>% 
      filter(Species %in% sp_SSI$Species) %>% # keep only non-rare species
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

# prep data frames with 2 or 3 bins; calc distance matrices ####
{
  library(vegan)
  veg_hf <- veg_pa %>% 
    filter(Species %in% sp_SSI$Species) %>% 
    select(Latitude, Longitude, NRNAME, Protocol, WetlandType, Site, Year, Species, PA)
  veg_hf <- veg_hf %>% 
    spread(key=Species, value=PA) %>% 
    gather(key=Species, value=PA, 8:ncol(.)) %>% 
    replace_na(list(PA=0)) %>% 
    spread(key=Species, value=PA) 
  hf_bin <- hf_tot
  hf_bin$HFbin <- ntile(hf_bin$totdist_percent, n=10) 
  
  # disturbance levels of each bin
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
  
  veg_d2 <- vegdist(veg_hf2[,9:ncol(veg_hf2)], method="jaccard", binary=T)
  veg_d3 <- vegdist(veg_hf3[,9:ncol(veg_hf3)], method="jaccard", binary=T)
  
  # test 3 bins with different low HD bin
  {
    # veg_hf3 has most, least, and intermed dist communities
    veg_hf3a <- veg_hf %>% filter(HFbin==2 | HFbin==8 | HFbin==10) %>% 
      select(Latitude, Longitude, NRNAME, Protocol, WetlandType, Site, Year, HFbin, everything())
    veg_hf3b <- veg_hf %>% filter(HFbin==3 | HFbin==8 | HFbin==10) %>% 
      select(Latitude, Longitude, NRNAME, Protocol, WetlandType, Site, Year, HFbin, everything())
    
    sptokeep3a <- colSums(veg_hf3a[,9:ncol(veg_hf3a)]) %>% data.frame() 
    sptokeep3a$Species <- rownames(sptokeep3a)
    colnames(sptokeep3a) <- c("Obs", "Species")
    sptokeep3a <-  filter(sptokeep3a, Obs > 1)
    
    sptokeep3b <- colSums(veg_hf3b[,9:ncol(veg_hf3b)]) %>% data.frame() 
    sptokeep3b$Species <- rownames(sptokeep3b)
    colnames(sptokeep3b) <- c("Obs", "Species")
    sptokeep3b <-  filter(sptokeep3b, Obs > 1)
    
    veg_hf3a <- veg_hf3a %>% 
      gather(Species, PA, 9:ncol(.)) %>% 
      filter(Species %in% sptokeep3a$Species) %>% 
      spread(Species, PA)
    veg_hf3b <- veg_hf3b %>% 
      gather(Species, PA, 9:ncol(.)) %>% 
      filter(Species %in% sptokeep3b$Species) %>% 
      spread(Species, PA)
    
    veg_d3a <- vegdist(veg_hf3a[,9:ncol(veg_hf3a)], method="jaccard", binary=T)
    veg_d3b <- vegdist(veg_hf3b[,9:ncol(veg_hf3b)], method="jaccard", binary=T)
  }  
  
}

# how many sites and species ####
{
  spR %>% distinct(Protocol, Site) %>% nrow() # 1585 unique wetlands (some sampled >1x)
  spR %>% group_by(Protocol, Site) %>% 
    tally() %>% 
    filter(n>1) %>% 
    arrange(desc(n)) %>% 
    nrow() # 471 sampled 2x or 3x
  nrow(spR) # unique sampling events
  
  # how much time and HD in between samplings
  {  
    spR %>% filter(Site=="1500") # sampled 3x
    
    doublesites <- spR %>% group_by(Protocol, Site) %>% 
    tally() %>% 
    filter(n>1) %>% 
    arrange(desc(n)) %>% 
    filter(Site!="1500")

  dubsamp <- left_join(select(doublesites, -n), spR, by=c("Protocol", "Site")) %>% select(-Latitude, -Longitude)
  dubsamp$Year2 <- rep(c("Early", "Late"), times=nrow(dubsamp)/2)

  # on average how many years between sampling periods
  tmp1 <- dubsamp %>% select(-rich, -totdist_percent, -WetlandType) %>% spread(key=Year2, value=Year) %>% mutate(yrchange=(Late-Early))
  mean(tmp1$yrchange)
  
  }
  
  # total disturbance (area_km2) in early and late sampling year
  tmp <- dubsamp %>% select(-Year, -rich, -WetlandType) %>% spread(key=Year2, value=totdist_percent)
  mean(tmp$Late-tmp$Early)
  head(tmp)
  head(dubsamp)
  
  shapiro.test(dubsamp$totdist_percent) # not normal
  t.test(x=tmp$Early, y=tmp$Late, paired=T, alternative = "less")
  wilcox.test(x=tmp$Early, y=tmp$Late, paired=T, alternative = "less")
}


# 1a. how does sp richness vary across HF gradient ####
{
  spR$Year <- as.factor(spR$Year)
  # should HF be linear or quad? and does ran ef variance differ from zero?
  rich.linear <- lmer(rich ~ totdist_percent + 
                        Protocol + 
                        (1|Year) + (1|UniqueID), 
                      data=spR, REML=F)
  rich.poly <- lmer(rich ~ poly(totdist_percent,2) + 
                      Protocol + 
                      (1|Year) + (1|UniqueID), 
                    data=spR, REML=F)
  anova(rich.linear, type=2) # RE of unique site ID should be kept
  anova(rich.poly, type=2) # RE of unique site ID should be kept
  anova(rich.linear, rich.poly) # poly is better
  
  piecewiseSEM::rsquared(rich.poly) # best model
  AIC(rich.poly) - AIC(rich.linear)
  
  # old analyses - ignore
  {
    # anova(lme(rich ~ poly(totdist_percent,2) + Protocol + Latitude + Longitude + Year,
    #           random=~1|UniqueID,
    #           data=spR), type="marginal")
    # 
    # rich.poly1 <- lmer(rich ~ poly(totdist_percent,2) + 
    #                      Protocol + 
    #                      Latitude + Longitude +
    #                      (1|Year) + (1|UniqueID), 
    #                    data=spR, REML=F)
    # rich.poly2 <- lmer(rich ~ poly(totdist_percent,2) + 
    #                      Protocol +
    #                      (1|Year) + (1|UniqueID), 
    #                    data=spR, REML=F)
    # rich.poly3 <- lmer(rich ~ poly(totdist_percent,2) + 
    #                      (1|Year) + (1|UniqueID), 
    #                    data=spR, REML=F)
    # 
    # AIC(rich.poly, rich.poly1, rich.poly2, rich.poly3)  
    # summary(rich.poly2) # latitudinal climate variables not important for richness
    # piecewiseSEM::rsquared(rich.poly2)
    # 
    # Moran.I(residuals(rich.poly2), rich.d.inv)
  }

}

# 1b. how does sp richness vary across HF gradient within each NR ####
{
  spR$Year <- as.factor(spR$Year)
  table(spR$NRNAME)
  
  # boreal
  {
    boreal.linear <- lmer(rich ~ totdist_percent + 
                           Protocol + 
                           (1|Year) + (1|UniqueID), 
                         data=filter(spR, NRNAME=="Boreal"), 
                         REML=F)
    boreal.poly <- lmer(rich ~ poly(totdist_percent,2) + 
                         Protocol + 
                         (1|Year) + (1|UniqueID), 
                       data=filter(spR, NRNAME=="Boreal"), 
                       REML=F)
    AIC(boreal.linear, boreal.poly) # poly is better
    piecewiseSEM::rsquared(boreal.poly) # best model
  }
  
  # foothills - no sites were double sampled so removed unique ID as ran ef
  {
    foothills.linear <- lmer(rich ~ totdist_percent + 
                            Protocol + 
                            (1|Year), 
                          data=filter(spR, NRNAME=="Foothills"), 
                          REML=F)
    
    foothills.poly <- lmer(rich ~ poly(totdist_percent,2) + 
                          Protocol + 
                          (1|Year), 
                        data=filter(spR, NRNAME=="Foothills"), 
                        REML=F)
    AIC(foothills.linear, foothills.poly) # poly is better
    piecewiseSEM::rsquared(foothills.poly) # best model
  }
  
  # grassland
  {
    grass.linear <- lmer(rich ~ totdist_percent + 
                           Protocol + 
                           (1|Year) + (1|UniqueID), 
                         data=filter(spR, NRNAME=="Grassland"), 
                         REML=F)
    grass.poly <- lmer(rich ~ poly(totdist_percent,2) + 
                         Protocol + 
                         (1|Year) + (1|UniqueID), 
                       data=filter(spR, NRNAME=="Grassland"), 
                       REML=F)
    AIC(grass.linear, grass.poly) # poly is better
    piecewiseSEM::rsquared(grass.poly) # best model
  }

  # parkland - some convergence issues
  {
    parkland.linear <- lmer(rich ~ totdist_percent + 
                           Protocol + 
                           (1|Year) + (1|UniqueID), 
                         data=filter(spR, NRNAME=="Parkland"), 
                         REML=F)
    parkland.poly <- lmer(rich ~ poly(totdist_percent,2) + 
                         Protocol + 
                         (1|Year) + (1|UniqueID), 
                       data=filter(spR, NRNAME=="Parkland"), 
                       REML=F)
    AIC(parkland.linear, parkland.poly) # poly is better
    piecewiseSEM::rsquared(parkland.poly) # best model
    
  }
  
  # rocky mountain - linear is better but note that HD doesn't extend to full gradient
  {
    mtn.linear <- lmer(rich ~ totdist_percent + 
                              Protocol + 
                              (1|Year) + (1|UniqueID), 
                            data=filter(spR, NRNAME=="Rocky Mountain"), 
                            REML=F)
    mtn.poly <- lmer(rich ~ poly(totdist_percent,2) + 
                            Protocol + 
                            (1|Year) + (1|UniqueID), 
                          data=filter(spR, NRNAME=="Rocky Mountain"), 
                          REML=F)
    AIC(mtn.linear, mtn.poly) # linear is better
  }
}

# 2. How does CSI vary with HF ####
{
  csi.linear <- lmer(CSI ~ totdist_percent + 
                       Protocol + 
                       (1|Year) + (1|UniqueID), 
                     data=veg_CSI_HF,
                     REML = F)
  csi.poly <- lmer(CSI ~ poly(totdist_percent,2) + 
                     Protocol + 
                     (1|Year) + (1|UniqueID), 
                   data=veg_CSI_HF,
                   REML = F)
  
  ss <- getME(csi.poly,c("theta","fixef"))
  m3 <- update(csi.poly,start=ss,control=lmerControl(optimizer="bobyqa",
                                                   optCtrl=list(maxfun=2e5)))
  getME(csi.poly, "fixef")
  getME(m3, "fixef")
  
  anova(csi.linear, type=2) # variance on group RE indistinguishable from zero
  anova(csi.poly, type=2) # variance on group RE indistinguishable from zero
  anova(csi.linear, csi.poly)
  AIC(csi.linear, csi.poly) # csi poly
  AIC(csi.poly) - AIC(csi.linear)
  
  piecewiseSEM::rsquared(csi.poly) # best model
  piecewiseSEM::rsquared(m3) # best model
  
  # old analyses - ignore
  {
    # library(nlme)
    # anova(lme(CSI ~ poly(totdist_percent,2) + Protocol + Latitude + Longitude + Year,
    #           random=~1|UniqueID,
    #           data=veg_CSI_HF), type="marginal")
    # 
    # csi.poly1 <- lmer(CSI ~ poly(totdist_percent,2) + 
    #                     Protocol + 
    #                     Latitude + Longitude +
    #                     (1|Year) + (1|UniqueID), 
    #                   data=veg_CSI_HF,
    #                   REML = F)
    # csi.poly2 <- lmer(CSI ~ poly(totdist_percent,2) + 
    #                     Protocol +
    #                     (1|Year) + (1|UniqueID), 
    #                   data=veg_CSI_HF,
    #                   REML = F)
    # csi.poly3 <- lmer(CSI ~ poly(totdist_percent,2) +
    #                     (1|Year) + (1|UniqueID), 
    #                   data=veg_CSI_HF,
    #                   REML = F)
    # 
    # AIC(csi.poly, csi.poly1, csi.poly2, csi.poly3)
    # 
    # summary(csi.poly1) # lat/long coords are important
    # piecewiseSEM::rsquared(csi.poly1)
    
  }
  

}

# 3. Does including % exotics improve fit of richness model (from 1 above) ####
{
  rich.poly # previous best
  # refit previous best w/ updated df that includes exotic
  rich.polya <- lmer(rich ~ poly(totdist_percent,2) + 
                        Protocol +
                        (1|Year) + (1|UniqueID), 
                      data=veg_exot,
                      REML=F)
  rich.exot.only <- lmer(rich ~ propexotic + 
                           Protocol +
                           (1|Year) + (1|UniqueID), 
                         data=veg_exot,
                         REML=F)
  rich.poly.exot <- lmer(rich ~ poly(totdist_percent,2) + 
                           propexotic + 
                           Protocol +
                           (1|Year) + (1|UniqueID), 
                         data=veg_exot,
                         REML=F)
  rich.poly.exot.interaction <- lmer(rich ~ poly(totdist_percent,2) * propexotic + 
                                       Protocol +
                                       (1|Year) + (1|UniqueID), 
                                     data=veg_exot,
                                     REML=F)
  
  AIC(rich.polya, rich.exot.only, rich.poly.exot, rich.poly.exot.interaction)
  anova(rich.polya, rich.exot.only, rich.poly.exot, rich.poly.exot.interaction)
  summary(rich.poly.exot.interaction)
  anova(rich.poly.exot.interaction, type=2)
  piecewiseSEM::rsquared(rich.polya)
  
  anova(rich.polya, rich.poly.exot.interaction)

  piecewiseSEM::rsquared(rich.poly.exot.interaction) # best model
  
}

# 4. does including % exotics improve fit of CSI model (from 2 above) ####
{
  csi.poly # previous best
  # refit previous best w/ updated df
  csi.polya <- lmer(CSI ~ poly(totdist_percent,2) + 
                       Protocol + 
                       # Latitude + Longitude +
                       (1|Year) + (1|UniqueID), 
                     data=veg_exot,
                     REML=F)
  csi.exot.only <- lmer(CSI ~ propexotic +
                          Protocol +
                          # Latitude + Longitude +
                          (1|Year) + (1|UniqueID), 
                        data=veg_exot,
                        REML=F)
  csi.poly.exot <- lmer(CSI ~ poly(totdist_percent,2) + 
                          propexotic + 
                          Protocol + 
                          # Latitude + Longitude +
                          (1|Year) + (1|UniqueID), 
                        data=veg_exot,
                        REML=F)
  csi.poly.exot.interaction <- lmer(CSI ~ poly(totdist_percent,2) * propexotic + 
                                      Protocol + 
                                      # Latitude + Longitude +
                                      (1|Year) + (1|UniqueID), 
                                    data=veg_exot,
                                    REML=F)
  
  AIC(csi.polya, csi.exot.only, csi.poly.exot, csi.poly.exot.interaction)
  anova(csi.polya, csi.exot.only, csi.poly.exot, csi.poly.exot.interaction)
  summary(csi.poly.exot.interaction)
  
  piecewiseSEM::rsquared(csi.poly1a)
  
  piecewiseSEM::rsquared(csi.poly.exot.interaction) # best model
  anova(csi.poly.exot.interaction, type="chisq")  
  
}

# 0. test for spatial autocorrelation - Moran's I ####
{
  
  # save residuals from best models
  {
    
    
    rich.poly.resid <- data.frame(Protocol = spR$Protocol,
                                  Site = spR$Site,
                                  Year = spR$Year,
                                  Lat = spR$Latitude,
                                  Long = spR$Longitude,
                                  UniqueID = spR$UniqueID,
                                  Residuals=residuals(rich.poly) )
    csi.poly.resid <- data.frame(Protocol = spR$Protocol,
                                 Site = spR$Site,
                                 Year = spR$Year,
                                 Lat = spR$Latitude,
                                 Long = spR$Longitude,
                                 UniqueID = spR$UniqueID,
                                 Residuals=residuals(csi.poly) )
    rich.poly.exot.interaction.resid <- data.frame(Protocol = spR$Protocol,
                                                   Site = spR$Site,
                                                   Year = spR$Year,
                                                   Lat = spR$Latitude,
                                                   Long = spR$Longitude,
                                                   UniqueID = spR$UniqueID,
                                                   Residuals=residuals(rich.poly.exot.interaction) )
    csi.poly.exot.interaction.resid <- data.frame(Protocol = spR$Protocol,
                                                  Site = spR$Site,
                                                  Year = spR$Year,
                                                  Lat = spR$Latitude,
                                                  Long = spR$Longitude,
                                                  UniqueID = spR$UniqueID,
                                                  Residuals=residuals(csi.poly.exot.interaction) )
    save(rich.poly.resid, csi.poly.resid, rich.poly.exot.interaction.resid, csi.poly.exot.interaction.resid,
         file="results/Model residuals.Rdata")
    
    
  }
  
  # richness w/ ape::Moran.I
  {
    
    head(spR)
    # create inverse distance matrix
    rich.d <- as.matrix(dist(cbind(spR$Longitude, spR$Latitude)))
    rich.d.inv <- 1/rich.d
    diag(rich.d.inv) <- 0
    rich.d.inv[1:5, 1:5] # inf values have same coords
    rich.d.inv[is.infinite(rich.d.inv)] <- 0
    
    Moran.I(spR$rich, rich.d.inv) # yes spatial autocorr
  }
  
  # CSI
  {
    head(veg_CSI_HF)
    csi.d <- as.matrix(dist(cbind(veg_CSI_HF$Longitude, veg_CSI_HF$Latitude)))
    csi.d.inv <- 1/csi.d
    diag(csi.d.inv) <- 0
    csi.d.inv[1:5, 1:5] # inf values have same coords
    csi.d.inv[is.infinite(csi.d.inv)] <- 0
    
    Moran.I(veg_CSI_HF$CSI, csi.d.inv) # yes spatial autocorr
    
  }
  
  
  Moran.I(residuals(rich.poly), rich.d.inv) # I = 0.011
  Moran.I(residuals(csi.poly), csi.d.inv) # I = 0.012
  
  Moran.I(residuals(rich.poly.exot.interaction), rich.d.inv) # I=0.003
  Moran.I(residuals(csi.poly.exot.interaction), csi.d.inv) # I=0.006 
  
}



# 5. permanovas of multivariate NMDS's ####
{
  # permanova
  {
    # check for sig diffs among 2 groups
    mrpp(veg_d2, grouping=as.factor(veg_hf2$HFbin)) # sig diff between groups based on mean score
    
    adonis2(veg_d2 ~ as.factor(veg_hf2$HFbin) + as.factor(veg_hf2$Protocol),
            by="margin") # both significant
    
    # check for sig diffs among 3 groups
    library(RVAideMemoire)
    library(ecodist)
    
    adonis2(veg_d3 ~ as.factor(veg_hf3$HFbin) + as.factor(veg_hf3$Protocol),
            by="margin") # both significant
    adonis2(veg_d3a ~ as.factor(veg_hf3a$HFbin) + as.factor(veg_hf3a$Protocol),
            by="margin") # both significant
    adonis2(veg_d3b ~ as.factor(veg_hf3b$HFbin) + as.factor(veg_hf3b$Protocol),
            by="margin") # both significant
    
    pairwise.perm.manova(resp=veg_d3, fact=veg_hf3$HFbin, p.method="holm")
    mrpp(veg_d3, grouping=as.factor(veg_hf3$HFbin)) # sig diff between groups based on mean score

    pairwise.perm.manova(resp=veg_d3a, fact=veg_hf3a$HFbin, p.method="holm")
    mrpp(veg_d3a, grouping=as.factor(veg_hf3a$HFbin)) # sig diff between groups based on mean score

    pairwise.perm.manova(resp=veg_d3b, fact=veg_hf3b$HFbin, p.method="holm")
    mrpp(veg_d3b, grouping=as.factor(veg_hf3b$HFbin)) # sig diff between groups based on mean score
    
  }
}

# 6. comparison of median disturbance across low, med, high bins ####
{
  
  # disturbance levels of each bin
  tmp <- hf_bin %>% 
    mutate(HFbin = factor(HFbin, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))) %>% 
    group_by(HFbin) %>% 
    summarize(N=length(totdist_percent),
              meddist=median(totdist_percent),
              IQR=IQR(totdist_percent))
  tmp
  # write.csv(x=tmp, file="/Users/carif/Dropbox/Desktop/Waterloo/AB plant and invert responses to HF/results/HD across bins.csv")
  
  hf_bin2 <- hf_bin %>% filter(HFbin==1 | HFbin==10)
  hf_bin2$UniqueID <- paste(hf_bin2$Protocol, hf_bin2$Site, sep="_")
  summary(lmer(totdist_percent ~ as.factor(HFbin) + 
         Protocol + (1|Year) + (1|UniqueID), 
       data=hf_bin2, REML=F)) # sig effect of HF bin
  
  hf_bin3 <- hf_bin %>% filter(HFbin==1 | HFbin==8 | HFbin==10)
  hf_bin3$UniqueID <- paste(hf_bin3$Protocol, hf_bin3$Site, sep="_")
  hf_bin3$HFbin <- recode(hf_bin3$HFbin, "1"="Low", "8"="Int.", "10"="High")
  anova(lmer(totdist_percent ~ HFbin + 
                 Protocol + (1|Year) + (1|UniqueID), 
               data=hf_bin3, REML=F), type=2) # both bins 8 and 1 differ from 1
  
ggplot(hf_bin3, aes(x=HFbin, y=totdist_percent)) +
    geom_boxplot(fill="grey80") +
    labs(x="Disturbance Level", y="Human Development (%)")
  
  
}

# 7. comparison of median prop exotic across low/med/high bins ####
{
  exot_bin <- left_join(select(ungroup(hf_bin),Protocol, WetlandType, Year, Site, HFbin ), 
                        veg_exot, 
                        by=c("Protocol", "WetlandType", "Year", "Site"))
  
  exot_bin %>% 
    group_by(HFbin) %>% 
    summarize(medexot=median(propexotic),
              IQRexot=IQR(propexotic))
  
  
  # exot_bin2 <- exot_bin %>% filter(HFbin==1 | HFbin==10)
  # exot_bin2$UniqueID <- paste(exot_bin2$Protocol, exot_bin2$Site, sep="_")
  
  
  exot_bin3 <- exot_bin %>% filter(HFbin==1 | HFbin==8 | HFbin==10)
  exot_bin3$UniqueID <- paste(exot_bin3$Protocol, exot_bin3$Site, sep="_")
  exot_bin3$HFbin <- recode(exot_bin3$HFbin, "1"="Low", "8"="Int.", "10"="High")
  exot_bin3$HFbin <- factor(exot_bin3$HFbin, ordered=T, levels=c("Low", "Int.", "High"))
  
  anova(lmer(propexotic ~ as.factor(HFbin) + 
                 Protocol + (1|Year) + (1|UniqueID), 
               data=exot_bin3, REML=F), type = 2) # sig effect of HF bin
  exot_bin3 %>% group_by(HFbin) %>% tally()
  
}

# 8. linear vs poly relationship between prop exotics and HF ####
{

  exotic_m1 <- lmer(propexotic ~ totdist_percent + 
               Protocol + (1|Year) + (1|UniqueID), 
             data=veg_exot, REML=F) # sig effect of HF bin
  
  exotic_m2 <- lmer(propexotic ~ poly(totdist_percent, 2) + 
               Protocol + (1|Year) + (1|UniqueID), 
             data=veg_exot, REML=F) # sig effect of HF bin
  
  anova(exotic_m1, exotic_m2)
  summary(exotic_m2)
  anova(exotic_m2, type=2)
  piecewiseSEM::rsquared(exotic_m2)
  AIC(exotic_m2) - AIC(exotic_m1)
}

# 9. comparison of median CSI across low/med/high bins ####
{
  csi_bin <- left_join(select(ungroup(hf_bin),Protocol, WetlandType, Year, Site, HFbin ), 
                       veg_CSI_HF, 
                        by=c("Protocol", "WetlandType", "Year", "Site"))
  
  csi_bin %>% 
    group_by(HFbin) %>% 
    summarize(medCSI=median(CSI),
              IQRCSI=IQR(CSI))
  
  ggplot(filter(csi_bin, HFbin=="1" |
                  HFbin=="8" |
                  HFbin == "10"), aes(x=factor(HFbin, 
                                               levels=c("1", "8", "10"), 
                                               labels=c("Low", "Int.", "High")), y=CSI)) +
    labs(x="Development Level", y="Niche Specialization") +
    geom_boxplot(fill="grey90")

  csi_bin3 <- csi_bin %>% filter(HFbin==1 | HFbin==8 | HFbin==10)
  csi_bin3$UniqueID <- paste(csi_bin3$Protocol, csi_bin3$Site, sep="_")
  csi_bin3$HFbin <- recode(csi_bin3$HFbin, "1"="Low", "8"="Int.", "10"="High")
  csi_bin3$HFbin <- factor(csi_bin3$HFbin, ordered=T, levels=c("Low", "Int.", "High"))
  
  anova(lmer(CSI ~ as.factor(HFbin) + 
               Protocol + (1|Year) + (1|UniqueID), 
             data=csi_bin3, REML=F), type = 2) # sig effect of HF bin
  
  tmp <- lmer(CSI ~ HFbin + 
               Protocol + (1|Year) + (1|UniqueID), 
             data=csi_bin3, REML=F)
  
  emmeans::emmeans(tmp, list(pairwise ~ HFbin), adjust = "tukey")
  
  csi_bin3 %>% group_by(HFbin) %>% tally()
  
}
