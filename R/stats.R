# This file runs the stats to analyze relationships between HD vs richness and niche specialization
# we are updating the file to 

library(tidyverse); library(cowplot); library(ggrepel)
library(lme4)
library(ape)
library(lmerTest)
library(vegan)

rm(list=ls())

# load df with richness, CSI, and prop exotic sp
{
  veg_df <- read.csv("data/cleaned/veg_rich_CSI_exot.csv")
}



# how many sites and species ####
{
  veg_df %>% distinct(Protocol, Site) %>% nrow() # 1582 unique wetlands (some sampled >1x)
  veg_df %>% group_by(Protocol, WetlandType, Site) %>% 
    tally() %>% 
    filter(n>1) %>% 
    arrange(desc(n)) %>% 
    nrow() # 471 sampled 2x or 3x
  nrow(veg_df) # unique sampling events
  
  # how much time and HD in between samplings
  {  
    veg_df %>% filter(Site=="1500") # sampled 3x
    
    doublesites <- veg_df %>% group_by(Protocol, Site) %>% 
    tally() %>% 
    filter(n>1) %>% 
    arrange(desc(n)) %>% 
    filter(Site!="1500")

  dubsamp <- left_join(select(doublesites, -n), veg_df, by=c("Protocol", "Site")) %>% select(-Latitude, -Longitude)
  dubsamp$Year2 <- rep(c("Early", "Late"), times=nrow(dubsamp)/2)

  # on average how many years between sampling periods
  tmp1 <- dubsamp %>% 
    select(-rich, -totdist_percent, -WetlandType, -CSI, -propexotic) %>% 
    spread(key=Year2, value=Year) %>% mutate(yrchange=(Late-Early))
  mean(tmp1$yrchange)
  
  }
  
  # total disturbance (area_km2) in early and late sampling year
  tmp <- dubsamp %>% select(-Year, -rich, -WetlandType, -CSI, -propexotic) %>% 
    spread(key=Year2, value=totdist_percent)
  mean(tmp$Late-tmp$Early)
  head(tmp)
  head(dubsamp)
  
  shapiro.test(dubsamp$totdist_percent) # not normal
  t.test(x=tmp$Early, y=tmp$Late, paired=T, alternative = "less")
  wilcox.test(x=tmp$Early, y=tmp$Late, paired=T, alternative = "less")
}

# checking assumptions of normality
{
  ###### TESTING #######
  # This changes the "UniqueID" in veg_df (from Protocol_Site to Site_Year) and converts the "rich" variable from observed richness to chao-estimated true richness
  {
    # note that the "UniqueID" vars aren't the same
    head(truerich) # UniqueID = Site_Year
    head(veg_df) # UniqueID = Protocol_Site
    
    veg_df <- veg_df %>% mutate(UniqueID = paste(Site, Year, sep="_")) # replace with Site_Year UniqueID
    
    veg_df <- left_join(veg_df, 
                        select(truerich, chao, UniqueID, Protocol), 
                        by=c("Protocol", "UniqueID")) 
    head(veg_df)
    
    # doing this just so i dont have to change the text in all the models... may delete later
    veg_df <- veg_df %>% 
      mutate(rich = chao) %>% 
      select(-chao)
  }

  mod <- lm(rich ~ totdist_percent, data=filter(veg_df, Protocol=="Terrestrial"))
  
  # residuals are normally distributed? good
  hist(resid(mod)) 
  plot(mod, 2)
  
  # linear relationships - so so, but we use poly? transform?
  plot(mod, 1) 
  
  # equal variance of residuals (homoscedasticity)
  plot(mod,3) # pretty ok? so so?
  plot(lm(log(rich) ~ totdist_percent, data=filter(veg_df, Protocol=="Terrestrial")), 3) # doesn't really help
  ter.rich.linear <- lmer(rich ~ totdist_percent + 
                            (1|Year) + (1|UniqueID), 
                          data=filter(veg_df, Protocol=="Terrestrial"), REML=F)
  lmtest::bptest(mod) # Breusch-Pagan test
  car::ncvTest(mod)  # Breusch-Pagan test
  # these are significant but we don't end up using the linear fixed effects model?
  
  
  # extreme cases
  plot(mod,5) # 479?
}

# 1a. how does sp richness vary across HF gradient ####
{
  veg_df$Year <- as.factor(veg_df$Year)
  
  # terrestrial protocol
  {
    # should HF be linear or quad? and does ran ef variance differ from zero?
    ter.rich.linear <- lmer(rich ~ totdist_percent + 
                          (1|Year) + (1|UniqueID), 
                        data=filter(veg_df, Protocol=="Terrestrial"), REML=F)
    ter.rich.poly <- lmer(rich ~ poly(totdist_percent,2, raw=T) + 
                        (1|Year) + (1|UniqueID), 
                        data=filter(veg_df, Protocol=="Terrestrial"), REML=F)
    
    anova(ter.rich.linear, type=2) # RE of unique site ID should be kept
    anova(ter.rich.poly, type=2) # RE of unique site ID should be kept
    anova(ter.rich.linear, ter.rich.poly) # poly is better
    
    piecewiseSEM::rsquared(ter.rich.poly) # best model
    AIC(ter.rich.poly) - AIC(ter.rich.linear)
  }
  
  # wetland protocol
  {
    # should HF be linear or quad? and does ran ef variance differ from zero?
    wet.rich.linear <- lmer(rich ~ totdist_percent + 
                              (1|Year) + (1|UniqueID), 
                            data=filter(veg_df, Protocol=="Wetland"), REML=F)
    wet.rich.poly <- lmer(rich ~ poly(totdist_percent,2, raw=T) + 
                            (1|Year) + (1|UniqueID), 
                          data=filter(veg_df, Protocol=="Wetland"), REML=F)
    
    anova(wet.rich.linear, type=2) # RE of unique site ID should be kept
    anova(wet.rich.poly, type=2) # RE of unique site ID should be kept
    anova(wet.rich.linear, wet.rich.poly) # poly is better
    
    piecewiseSEM::rsquared(wet.rich.poly) # best model
    AIC(wet.rich.poly) - AIC(wet.rich.linear)
  }


}

# 1b. how does sp richness vary across HF gradient within each NR ####
# *****    not updated to separate wetland vs ter protocols ****
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
    boreal.poly <- lmer(rich ~ poly(totdist_percent,2, raw=T) + 
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
    
    foothills.poly <- lmer(rich ~ poly(totdist_percent,2, raw=T) + 
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
    grass.poly <- lmer(rich ~ poly(totdist_percent,2, raw=T) + 
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
    parkland.poly <- lmer(rich ~ poly(totdist_percent,2, raw=T) + 
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
    mtn.poly <- lmer(rich ~ poly(totdist_percent,2, raw=T) + 
                            Protocol + 
                            (1|Year) + (1|UniqueID), 
                          data=filter(spR, NRNAME=="Rocky Mountain"), 
                          REML=F)
    AIC(mtn.linear, mtn.poly) # linear is better
  }
}

# 2. How does CSI vary with HF ####
{
  # terrestrial protocol
  {
    ter.csi.linear <- lmer(CSI ~ totdist_percent + 
                         (1|Year) + (1|UniqueID), 
                       data=filter(veg_df, Protocol=="Terrestrial"), REML=F)
    
    ter.csi.poly <- lmer(CSI ~ poly(totdist_percent,2, raw=T) + 
                       (1|Year) + (1|UniqueID), 
                     data=filter(veg_df, Protocol=="Terrestrial"), REML=F)
    
    
    ss <- getME(ter.csi.poly,c("theta","fixef"))
    m3 <- update(ter.csi.poly,start=ss,control=lmerControl(optimizer="bobyqa",
                                                       optCtrl=list(maxfun=2e5)))
    getME(ter.csi.poly, "fixef")
    getME(m3, "fixef")
    
    anova(ter.csi.linear, type=2) # variance on group RE indistinguishable from zero
    anova(ter.csi.poly, type=2) # variance on group RE different from zero
    anova(ter.csi.linear, ter.csi.poly)
    AIC(ter.csi.linear, ter.csi.poly) # csi poly
    AIC(ter.csi.poly) - AIC(ter.csi.linear)
    
    piecewiseSEM::rsquared(ter.csi.poly) # best model
    piecewiseSEM::rsquared(m3) # best model
    
  }
  
  # wetland protocol
  {
    wet.csi.linear <- lmer(CSI ~ totdist_percent + 
                             (1|Year) + (1|UniqueID), 
                           data=filter(veg_df, Protocol=="Wetland"), REML=F)
    
    wet.csi.poly <- lmer(CSI ~ poly(totdist_percent,2, raw=T) + 
                           (1|Year) + (1|UniqueID), 
                         data=filter(veg_df, Protocol=="Wetland"), REML=F)
    
    anova(wet.csi.linear, type=2) # variance on group RE different from zero
    anova(wet.csi.poly, type=2) # variance on group RE different from zero
    anova(wet.csi.linear, wet.csi.poly)
    AIC(wet.csi.linear, wet.csi.poly) # csi poly
    AIC(wet.csi.poly) - AIC(wet.csi.linear)
    
    piecewiseSEM::rsquared(wet.csi.poly) # best model

  }

}

# 3. Does including % exotics improve fit of richness model (from 1 above) ####
{
  # terrestrial protocol
  {
    ter.rich.poly # previous best

    ter.rich.exot.only <- lmer(rich ~ propexotic + 
                             (1|Year) + (1|UniqueID), 
                             data=filter(veg_df, Protocol=="Terrestrial"), REML=F)
    
    ter.rich.poly.exot <- lmer(rich ~ poly(totdist_percent,2, raw=T) + 
                             propexotic + 
                             (1|Year) + (1|UniqueID), 
                             data=filter(veg_df, Protocol=="Terrestrial"), REML=F)
    
    ter.rich.poly.exot.interaction <- lmer(rich ~ poly(totdist_percent,2, raw=T) * propexotic + 
                                         (1|Year) + (1|UniqueID), 
                                       data=filter(veg_df, Protocol=="Terrestrial"), REML=F)
    
    
    AIC(ter.rich.poly, ter.rich.exot.only, ter.rich.poly.exot, ter.rich.poly.exot.interaction)
    anova(ter.rich.poly, ter.rich.exot.only, ter.rich.poly.exot, ter.rich.poly.exot.interaction)
    summary(ter.rich.poly.exot.interaction)
    anova(ter.rich.poly.exot.interaction, type=2)
    piecewiseSEM::rsquared(ter.rich.poly)
    
    anova(ter.rich.poly, ter.rich.poly.exot.interaction)
    
    piecewiseSEM::rsquared(ter.rich.poly.exot.interaction) # best model
    
  }
  
  # Wetland protocol
  {
    wet.rich.poly # previous best
    wet.rich.exot.only <- lmer(rich ~ propexotic + 
                                 (1|Year) + (1|UniqueID), 
                               data=filter(veg_df, Protocol=="Wetland"), REML=F)
    
    wet.rich.poly.exot <- lmer(rich ~ poly(totdist_percent,2, raw=T) + 
                                 propexotic + 
                                 (1|Year) + (1|UniqueID), 
                               data=filter(veg_df, Protocol=="Wetland"), REML=F)
    
    wet.rich.poly.exot.interaction <- lmer(rich ~ poly(totdist_percent,2, raw=T) * propexotic + 
                                             (1|Year) + (1|UniqueID), 
                                           data=filter(veg_df, Protocol=="Wetland"), REML=F)
    
    
    AIC(wet.rich.poly, wet.rich.exot.only, wet.rich.poly.exot, wet.rich.poly.exot.interaction)
    anova(wet.rich.poly, wet.rich.exot.only, wet.rich.poly.exot, wet.rich.poly.exot.interaction)
    summary(wet.rich.poly.exot)
    anova(wet.rich.poly.exot, type=2)
    piecewiseSEM::rsquared(wet.rich.poly)
    
    anova(wet.rich.poly, wet.rich.poly.exot)
    
    piecewiseSEM::rsquared(wet.rich.poly.exot) # best model - new result
    
  }
}

# 4. does including % exotics improve fit of CSI model (from 2 above) ####
{
  # Terrestrial Protocol
  {
    ter.csi.poly # previous best
    
    ter.csi.exot.only <- lmer(CSI ~ propexotic +
                            # Latitude + Longitude +
                            (1|Year) + (1|UniqueID), 
                          data=filter(veg_df, Protocol=="Terrestrial"), REML=F)
    
    ter.csi.poly.exot <- lmer(CSI ~ poly(totdist_percent,2) + 
                            propexotic + 
                            # Latitude + Longitude +
                            (1|Year) + (1|UniqueID), 
                            data=filter(veg_df, Protocol=="Terrestrial"), REML=F)
    
    ter.csi.poly.exot.interaction <- lmer(CSI ~ poly(totdist_percent,2) * propexotic + 
                                        # Latitude + Longitude +
                                        (1|Year) + (1|UniqueID), 
                                        data=filter(veg_df, Protocol=="Terrestrial"), REML=F)
    
    
    AIC(ter.csi.poly, ter.csi.exot.only, ter.csi.poly.exot, ter.csi.poly.exot.interaction)
    anova(ter.csi.poly, ter.csi.exot.only, ter.csi.poly.exot, ter.csi.poly.exot.interaction)
    summary(ter.csi.poly.exot.interaction)
    
    piecewiseSEM::rsquared(ter.csi.poly)
    
    piecewiseSEM::rsquared(ter.csi.poly.exot.interaction) # best model
    anova(ter.csi.poly.exot.interaction, type="2")  
  }
  
  # Wetland Protocol
  {
    wet.csi.poly # previous best
    
    wet.csi.exot.only <- lmer(CSI ~ propexotic +
                                # Latitude + Longitude +
                                (1|Year) + (1|UniqueID), 
                              data=filter(veg_df, Protocol=="Wetland"), REML=F)
    
    wet.csi.poly.exot <- lmer(CSI ~ poly(totdist_percent,2) + 
                                propexotic + 
                                # Latitude + Longitude +
                                (1|Year) + (1|UniqueID), 
                              data=filter(veg_df, Protocol=="Wetland"), REML=F)
    
    wet.csi.poly.exot.interaction <- lmer(CSI ~ poly(totdist_percent,2) * propexotic + 
                                            # Latitude + Longitude +
                                            (1|Year) + (1|UniqueID), 
                                          data=filter(veg_df, Protocol=="Wetland"), REML=F)
    
    
    AIC(wet.csi.poly, wet.csi.exot.only, wet.csi.poly.exot, wet.csi.poly.exot.interaction)
    anova(wet.csi.poly, wet.csi.exot.only, wet.csi.poly.exot, wet.csi.poly.exot.interaction)
    summary(wet.csi.poly.exot.interaction)
    
    piecewiseSEM::rsquared(wet.csi.poly)
    
    piecewiseSEM::rsquared(wet.csi.poly.exot.interaction) # best model
    anova(wet.csi.poly.exot.interaction, type="2")  
  }
  
}

# 0. test for spatial autocorrelation - Moran's I ####
{
  
  # save residuals from best models - terrestrial protocol
  {
    ter.rich.poly.resid <- data.frame( Site = filter(veg_df, Protocol=="Terrestrial")$Site,
                                  Year = filter(veg_df, Protocol=="Terrestrial")$Year,
                                  Lat = filter(veg_df, Protocol=="Terrestrial")$Latitude,
                                  Long = filter(veg_df, Protocol=="Terrestrial")$Longitude,
                                  UniqueID = filter(veg_df, Protocol=="Terrestrial")$UniqueID,
                                  Residuals=residuals(ter.rich.poly) )
    ter.csi.poly.resid <- data.frame(Site = filter(veg_df, Protocol=="Terrestrial")$Site,
                                 Year = filter(veg_df, Protocol=="Terrestrial")$Year,
                                 Lat = filter(veg_df, Protocol=="Terrestrial")$Latitude,
                                 Long = filter(veg_df, Protocol=="Terrestrial")$Longitude,
                                 UniqueID = filter(veg_df, Protocol=="Terrestrial")$UniqueID,
                                 Residuals=residuals(ter.csi.poly) )
    
    ter.rich.poly.exot.interaction.resid <- data.frame(Site = filter(veg_df, Protocol=="Terrestrial")$Site,
                                                   Year = filter(veg_df, Protocol=="Terrestrial")$Year,
                                                   Lat = filter(veg_df, Protocol=="Terrestrial")$Latitude,
                                                   Long = filter(veg_df, Protocol=="Terrestrial")$Longitude,
                                                   UniqueID = filter(veg_df, Protocol=="Terrestrial")$UniqueID,
                                                   Residuals=residuals(ter.rich.poly.exot.interaction) )
    ter.csi.poly.exot.interaction.resid <- data.frame(Site = filter(veg_df, Protocol=="Terrestrial")$Site,
                                                  Year = filter(veg_df, Protocol=="Terrestrial")$Year,
                                                  Lat = filter(veg_df, Protocol=="Terrestrial")$Latitude,
                                                  Long = filter(veg_df, Protocol=="Terrestrial")$Longitude,
                                                  UniqueID = filter(veg_df, Protocol=="Terrestrial")$UniqueID,
                                                  Residuals=residuals(ter.csi.poly.exot.interaction) )
    save(ter.rich.poly.resid, ter.csi.poly.resid, ter.rich.poly.exot.interaction.resid, ter.csi.poly.exot.interaction.resid,
         file="results/Model residuals - terrestrial protocol - 02092021.Rdata")
    
    
  }
  
  # save residuals from best models - Wetland protocol
  {
    wet.rich.poly.resid <- data.frame( Site = filter(veg_df, Protocol=="Wetland")$Site,
                                       Year = filter(veg_df, Protocol=="Wetland")$Year,
                                       Lat = filter(veg_df, Protocol=="Wetland")$Latitude,
                                       Long = filter(veg_df, Protocol=="Wetland")$Longitude,
                                       UniqueID = filter(veg_df, Protocol=="Wetland")$UniqueID,
                                       Residuals=residuals(wet.rich.poly) )
    wet.csi.poly.resid <- data.frame(Site = filter(veg_df, Protocol=="Wetland")$Site,
                                     Year = filter(veg_df, Protocol=="Wetland")$Year,
                                     Lat = filter(veg_df, Protocol=="Wetland")$Latitude,
                                     Long = filter(veg_df, Protocol=="Wetland")$Longitude,
                                     UniqueID = filter(veg_df, Protocol=="Wetland")$UniqueID,
                                     Residuals=residuals(wet.csi.poly) )
    
    wet.rich.poly.exot.interaction.resid <- data.frame(Site = filter(veg_df, Protocol=="Wetland")$Site,
                                                       Year = filter(veg_df, Protocol=="Wetland")$Year,
                                                       Lat = filter(veg_df, Protocol=="Wetland")$Latitude,
                                                       Long = filter(veg_df, Protocol=="Wetland")$Longitude,
                                                       UniqueID = filter(veg_df, Protocol=="Wetland")$UniqueID,
                                                       Residuals=residuals(wet.rich.poly.exot.interaction) )
    wet.csi.poly.exot.interaction.resid <- data.frame(Site = filter(veg_df, Protocol=="Wetland")$Site,
                                                      Year = filter(veg_df, Protocol=="Wetland")$Year,
                                                      Lat = filter(veg_df, Protocol=="Wetland")$Latitude,
                                                      Long = filter(veg_df, Protocol=="Wetland")$Longitude,
                                                      UniqueID = filter(veg_df, Protocol=="Wetland")$UniqueID,
                                                      Residuals=residuals(wet.csi.poly.exot.interaction) )
    save(wet.rich.poly.resid, wet.csi.poly.resid, wet.rich.poly.exot.interaction.resid, wet.csi.poly.exot.interaction.resid,
         file="results/Model residuals - wetland protocol - 02092021.Rdata")
    
    
  }
  
  # richness w/ ape::Moran.I
  {
    head(veg_df)
    
    # terrestrial
    {
      # create inverse distance matrix
      ter.rich.d <- as.matrix(dist(cbind(filter(veg_df, Protocol=="Terrestrial")$Longitude, 
                                         filter(veg_df, Protocol=="Terrestrial")$Latitude)))
      ter.rich.d.inv <- 1/ter.rich.d
      diag(ter.rich.d.inv) <- 0
      ter.rich.d.inv[1:5, 1:5] # inf values have same coords
      ter.rich.d.inv[is.infinite(ter.rich.d.inv)] <- 0
      
      Moran.I(filter(veg_df, Protocol=="Terrestrial")$rich, 
              ter.rich.d.inv) # yes spatial autocorr
    }
    
    # Wetland
    {
      # create inverse distance matrix
      wet.rich.d <- as.matrix(dist(cbind(filter(veg_df, Protocol=="Wetland")$Longitude, 
                                         filter(veg_df, Protocol=="Wetland")$Latitude)))
      wet.rich.d.inv <- 1/wet.rich.d
      diag(wet.rich.d.inv) <- 0
      wet.rich.d.inv[1:5, 1:5] # inf values have same coords
      wet.rich.d.inv[is.infinite(wet.rich.d.inv)] <- 0
      
      Moran.I(filter(veg_df, Protocol=="Wetland")$rich, 
              wet.rich.d.inv) # yes spatial autocorr
    }

  }
  
  # CSI
  {
    # terrestrial
    {
      ter.csi.d <- as.matrix(dist(cbind(filter(veg_df, Protocol=="Terrestrial")$Longitude, 
                                    filter(veg_df, Protocol=="Terrestrial")$Latitude)))
      ter.csi.d.inv <- 1/ter.csi.d
      diag(ter.csi.d.inv) <- 0
      ter.csi.d.inv[1:5, 1:5] # inf values have same coords
      ter.csi.d.inv[is.infinite(ter.csi.d.inv)] <- 0
      
      Moran.I(filter(veg_df, Protocol=="Terrestrial")$CSI, ter.csi.d.inv) # yes spatial autocorr
    }
    # wetland
    {
      wet.csi.d <- as.matrix(dist(cbind(filter(veg_df, Protocol=="Wetland")$Longitude, 
                                        filter(veg_df, Protocol=="Wetland")$Latitude)))
      wet.csi.d.inv <- 1/wet.csi.d
      diag(wet.csi.d.inv) <- 0
      wet.csi.d.inv[1:5, 1:5] # inf values have same coords
      wet.csi.d.inv[is.infinite(wet.csi.d.inv)] <- 0
      
      Moran.I(filter(veg_df, Protocol=="Wetland")$CSI, wet.csi.d.inv) # yes spatial autocorr
    }
    
  }
  
  Moran.I(residuals(ter.rich.poly), ter.rich.d.inv) # observed I = 0.0009
  Moran.I(residuals(ter.csi.poly), ter.csi.d.inv) # I = 0.018
  Moran.I(residuals(ter.rich.poly.exot.interaction), ter.rich.d.inv) # I=0.011
  Moran.I(residuals(ter.csi.poly.exot.interaction), ter.csi.d.inv) # I=0.018 
  
  Moran.I(residuals(wet.rich.poly), wet.rich.d.inv) # observed I = 0.026
  Moran.I(residuals(wet.csi.poly), wet.csi.d.inv) # I = 0.019
  Moran.I(residuals(wet.rich.poly.exot.interaction), wet.rich.d.inv) # I=0.018
  Moran.I(residuals(wet.csi.poly.exot.interaction), wet.csi.d.inv) # I=0.0185
}

# 5. permanovas of multivariate NMDS's ####
{
  # load veg site x sp matrices with only sites in 2 or 3 HF groups; calc chao dissimilarity
  {
    veg_2groups <- read.csv("data/cleaned/ordination data/veg site x sp mat - 2 HF groups.csv")
    veg_3groups <- read.csv("data/cleaned/ordination data/veg site x sp mat - 3 HF groups.csv")
    
    veg_distance_2groups <- vegdist(veg_hf_2groups[,9:ncol(veg_hf_2groups)], method="chao", binary=T)
    veg_distance_3groups <- vegdist(veg_hf_3groups[,9:ncol(veg_hf_3groups)], method="chao", binary=T)
  }
  
  # permanovas using 3 HF bin groups: 0%, 45-55%, and >95% HD
  {
    # check for sig diffs among 2 groups
    mrpp(veg_distance_2groups, grouping=as.factor(veg_2groups$HFbin)) # sig diff between groups based on mean score
    
    adonis2(veg_distance_2groups ~ as.factor(veg_2groups$HFbin) + as.factor(veg_2groups$Protocol),
            by="margin") # both significant
    # differences between groups could be because of dispersion (variance) or mean; adonis2 is less sensitive to dispersion than mrpp
    adonis2(veg_distance_2groups ~ as.factor(veg_2groups$HFbin)) # both significant
    anova(betadisper(d= veg_distance_2groups, group=as.factor(veg_2groups$HFbin)))
    
    # check for sig diffs among 3 groups
    mrpp(veg_distance_3groups, grouping=as.factor(veg_3groups$HFbin)) # sig diff between groups based on mean score
    
    adonis2(veg_distance_3groups ~ as.factor(veg_3groups$HFbin) + 
              as.factor(veg_3groups$Protocol),
            by="margin") # both significant
    # differences between groups could be because of dispersion (variance) or mean; adonis2 is less sensitive to dispersion than mrpp
    adonis2(veg_distance_3groups ~ as.factor(veg_3groups$HFbin)) # both significant
    anova(betadisper(d= veg_distance_3groups, group=as.factor(veg_3groups$HFbin)))
    # dispersion differences among 3 groups
    
    RVAideMemoire::pairwise.perm.manova(resp=veg_distance_3groups, 
                                        fact=veg_3groups$HFbin, p.method="holm")
    
  }
}

# 6. comparison of HD, rich, nonnatives, and CSI across low, med, high hd levels ####
{
  
  # HD
  left_join(veg_df, 
            select(veg_3groups, Protocol, Site, Year, HFbin),
            by=c("Protocol", "Site", "Year")) %>% # add bin number to HF df
    filter(!is.na(HFbin)) %>% 
    group_by(HFbin) %>% 
    summarize(N=length(totdist_percent),
              meddist=median(totdist_percent),
              IQR=IQR(totdist_percent))
  
  left_join(veg_df, 
            select(veg_3groups, Protocol, Site, Year, HFbin),
            by=c("Protocol", "Site", "Year")) %>% # add bin number to HF df
    filter(!is.na(HFbin)) %>% 
    ggplot(., aes(x=HFbin, y=totdist_percent)) +
        geom_boxplot(fill="grey80") +
        labs(x="Disturbance Level", y="Human Development (%)")
  
  # richness
  left_join(veg_df, 
            select(veg_3groups, Protocol, Site, Year, HFbin),
            by=c("Protocol", "Site", "Year")) %>% # add bin number to HF df
    filter(!is.na(HFbin)) %>% 
    group_by(HFbin) %>% 
    summarize(N=length(rich_chao ),
              medrich=median(rich_chao ),
              IQR=IQR(rich_chao ))
  
  left_join(veg_df, 
            select(veg_3groups, Protocol, Site, Year, HFbin),
            by=c("Protocol", "Site", "Year")) %>% # add bin number to HF df
    filter(!is.na(HFbin)) %>% 
    ggplot(., aes(x=HFbin, y=rich_chao )) +
    geom_boxplot(fill="grey80") +
    labs(x="Disturbance Level", y="True Species Richness (Chao-estimated)")
  
  # nonnatives
  left_join(veg_df, 
            select(veg_3groups, Protocol, Site, Year, HFbin),
            by=c("Protocol", "Site", "Year")) %>% # add bin number to HF df
    filter(!is.na(HFbin)) %>% 
    group_by(HFbin) %>% 
    summarize(N=length(propexotic  ),
              medrich=median(propexotic),
              IQR=IQR(propexotic ))
  
  left_join(veg_df, 
            select(veg_3groups, Protocol, Site, Year, HFbin),
            by=c("Protocol", "Site", "Year")) %>% # add bin number to HF df
    filter(!is.na(HFbin)) %>% 
    ggplot(., aes(x=HFbin, y=propexotic)) +
    geom_boxplot(fill="grey80") +
    labs(x="Disturbance Level", y="Proportion of Exotics (% of observed richness)")
  
  # csi
  # nonnatives
  left_join(veg_df, 
            select(veg_3groups, Protocol, Site, Year, HFbin),
            by=c("Protocol", "Site", "Year")) %>% # add bin number to HF df
    filter(!is.na(HFbin)) %>% 
    group_by(HFbin) %>% 
    summarize(N=length(CSI),
              medrich=median(CSI),
              IQR=IQR(CSI))
  
  left_join(veg_df, 
            select(veg_3groups, Protocol, Site, Year, HFbin),
            by=c("Protocol", "Site", "Year")) %>% # add bin number to HF df
    filter(!is.na(HFbin)) %>% 
    ggplot(., aes(x=HFbin, y=CSI)) +
    geom_boxplot(fill="grey80") +
    labs(x="Disturbance Level", y="Niche Specialization Index")
  
  
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
  hf_bin3 <- hf_tot %>% filter(totdist_percent==0 | 
                                 totdist_percent>=90 | 
                                 totdist_percent>=45 & totdist_percent<=55)  %>% 
    mutate(HFbin = ifelse(totdist_percent==0, "Low", 
                          ifelse(totdist_percent>=90, "High", "Int."))) %>% 
    select(Latitude, Longitude, NRNAME, Protocol, WetlandType, Site, Year, HFbin, everything())
  
  csi_bin <- left_join(select(ungroup(hf_bin3),Protocol, WetlandType, Year, Site, HFbin ), 
                       veg_CSI_HF, 
                        by=c("Protocol", "WetlandType", "Year", "Site"))
  csi_bin$HFbin <- factor(csi_bin$HFbin, ordered=T, levels=c("Low", "Int.", "High"))
  
  csi_bin %>% 
    group_by(HFbin) %>% 
    summarize(medCSI=median(CSI),
              IQRCSI=IQR(CSI))
  
  ggplot(csi_bin, aes(x=HFbin, y=CSI)) +
    labs(x="Human Development Level", y="Niche Specialization") +
    geom_boxplot(fill="grey90")

  tmp <- lmer(CSI ~ HFbin + 
               Protocol + (1|Year) + (1|UniqueID), 
             data=csi_bin, REML=F) # no convergence
  
  ss <- getME(tmp,c("theta","fixef"))
  m2 <- update(tmp,start=ss,control=lmerControl(optCtrl=list(maxfun=2e4)))
  
  anova(m2, type=2)
  
  emmeans::emmeans(m2, list(pairwise ~ HFbin), adjust = "tukey")
  
  csi_bin %>% group_by(HFbin) %>% tally()
  
}

# 10. trends in native vs nonnative richness over HD gradient ####
{
  veg_exot2 <- veg_exot %>% mutate(richexot=round(rich*(propexotic/100),0)) %>% 
    select(NRNAME, Protocol, WetlandType, Site, Year, UniqueID, totdist_percent, 
           "Total"=rich, "Nonnative"=richexot) %>% 
    mutate(Native=Total-Nonnative) 
  veg_exot2$Year <- as.factor(veg_exot$Year)
  
  head(veg_exot2)
  
  # nonnative species
  nnr.linear <- lmer(Nonnative ~ totdist_percent + 
                        Protocol + 
                        (1|Year) + (1|UniqueID), 
                      data=veg_exot2, REML=F)
  nnr.poly <- lmer(Nonnative ~ poly(totdist_percent,2, raw=T) + 
                      Protocol + 
                      (1|Year) + (1|UniqueID), 
                    data=veg_exot2, REML=F)
  anova(nnr.linear, type=2) # RE of unique site ID should be kept
  anova(nnr.poly, type=2) # RE of unique site ID should be kept
  anova(nnr.linear, nnr.poly) # poly is better
  summary(nnr.poly)
  piecewiseSEM::rsquared(nnr.poly) RAIC(nnr.poly) - AIC(nnr.linear)
  
  # native species
  nr.linear <- lmer(Native ~ totdist_percent + 
                       Protocol + 
                       (1|Year) + (1|UniqueID), 
                     data=veg_exot2, REML=F)
  nr.poly <- lmer(Native ~ poly(totdist_percent,2, raw=T) + 
                     Protocol + 
                     (1|Year) + (1|UniqueID), 
                   data=veg_exot2, REML=F)
  anova(nr.linear, type=2) # RE of unique site ID should be kept
  anova(nr.poly, type=2) # RE of unique site ID should be kept
  anova(nr.linear, nr.poly) # poly is better
  summary(nr.poly)
  piecewiseSEM::rsquared(nr.poly) 
  AIC(nr.poly) - AIC(nr.linear)
}
