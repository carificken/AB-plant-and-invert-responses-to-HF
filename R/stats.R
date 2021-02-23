# This file runs the stats to analyze relationships between HD vs rich_observedness and niche specialization
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
  veg_df$Year <- as.factor(veg_df$Year) # convert to factor so it can be modeled with random intercept
}

# how many sites and species ####
{
  # we have selected only one sampling event per site
  veg_df %>% distinct(Protocol, Site) %>% nrow() # 1582 unique wetlands (some sampled >1x)
  nrow(veg_df) # unique sampling events; also 1582 good
  
  veg_df %>% group_by(Protocol) %>% tally()
}

# checking assumptions of normality
{

  mod <- lm(rich_observed ~ totdist_percent, data=filter(veg_df, Protocol=="Terrestrial"))
  
  # residuals are normally distributed? good
  hist(resid(mod)) 
  plot(mod, 2)
  
  # linear relationships - so so, but we use poly? transform?
  plot(mod, 1) 
  
  # equal variance of residuals (homoscedasticity)
  plot(mod,3) # pretty ok? so so?
  plot(lm(log(rich_observed) ~ totdist_percent, data=filter(veg_df, Protocol=="Terrestrial")), 3) # doesn't really help
  ter.rich.linear <- lmer(rich_observed ~ totdist_percent + 
                            (1|Year),
                          data=filter(veg_df, Protocol=="Terrestrial"), REML=F)
  plot(ter.rich.linear) # looks ok
  
  # extreme cases
  plot(mod,5) # 479?
}

# 1a. how does sp rich_observed vary across HF gradient ####
{
  # both protocols together
  {
    # should HF be linear or quad? and does ran ef variance differ from zero?
    rich.linear <- lmer(rich_observed ~ totdist_percent + Protocol +
                              (1|Year),
                            data=veg_df, REML=F)
    rich.poly <- lmer(rich_observed ~ poly(totdist_percent,2, raw=T) + Protocol + 
                            (1|Year),
                          data=veg_df, REML=F)
    
    anova(rich.linear) # RE of unique site ID should be kept
    anova(rich.poly, type=2) # RE of unique site ID should be kept
    anova(rich.linear, rich.poly) # poly is better
    
    piecewiseSEM::rsquared(rich.poly) # best model
    AIC(rich.poly) - AIC(rich.linear)
  }
  
  # terrestrial protocol
  {
    # should HF be linear or quad? and does ran ef variance differ from zero?
    ter.rich.linear <- lmer(rich_observed ~ totdist_percent + 
                          (1|Year),
                        data=filter(veg_df, Protocol=="Terrestrial"), REML=F)
    ter.rich.poly <- lmer(rich_observed ~ poly(totdist_percent,2, raw=T) + 
                        (1|Year),
                        data=filter(veg_df, Protocol=="Terrestrial"), REML=F)
    
    anova(ter.rich.linear) # RE of unique site ID should be kept
    anova(ter.rich.poly, type=2) # RE of unique site ID should be kept
    anova(ter.rich.linear, ter.rich.poly) # poly is better
    
    piecewiseSEM::rsquared(ter.rich.poly) # best model
    AIC(ter.rich.poly) - AIC(ter.rich.linear)
  }
  
  # wetland protocol
  {
    # should HF be linear or quad? and does ran ef variance differ from zero?
    wet.rich.linear <- lmer(rich_observed ~ totdist_percent + 
                              (1|Year),
                            data=filter(veg_df, Protocol=="Wetland"), REML=F)
    wet.rich.poly <- lmer(rich_observed ~ poly(totdist_percent,2, raw=T) + 
                            (1|Year),
                          data=filter(veg_df, Protocol=="Wetland"), REML=F)
    
    anova(wet.rich.linear, type=2) # RE of unique site ID should be kept
    anova(wet.rich.poly, type=2) # RE of unique site ID should be kept
    anova(wet.rich.linear, wet.rich.poly) # poly is better
    
    piecewiseSEM::rsquared(wet.rich.poly) # best model
    AIC(wet.rich.poly) - AIC(wet.rich.linear)
  }


}

# 1b. how does sp richness vary across HF gradient within each NR ####
# *****    not yet updated to separate wetland vs ter protocols ****
{
  spR$Year <- as.factor(spR$Year)
  table(spR$NRNAME)
  
  # boreal
  {
    boreal.linear <- lmer(rich_observed ~ totdist_percent + 
                           Protocol + 
                           (1|Year),
                         data=filter(spR, NRNAME=="Boreal"), 
                         REML=F)
    boreal.poly <- lmer(rich_observed ~ poly(totdist_percent,2, raw=T) + 
                         Protocol + 
                         (1|Year),
                       data=filter(spR, NRNAME=="Boreal"), 
                       REML=F)
    AIC(boreal.linear, boreal.poly) # poly is better
    piecewiseSEM::rsquared(boreal.poly) # best model
  }
  
  # foothills - no sites were double sampled so removed unique ID as ran ef
  {
    foothills.linear <- lmer(rich_observed ~ totdist_percent + 
                            Protocol + 
                            (1|Year), 
                          data=filter(spR, NRNAME=="Foothills"), 
                          REML=F)
    
    foothills.poly <- lmer(rich_observed ~ poly(totdist_percent,2, raw=T) + 
                          Protocol + 
                          (1|Year), 
                        data=filter(spR, NRNAME=="Foothills"), 
                        REML=F)
    AIC(foothills.linear, foothills.poly) # poly is better
    piecewiseSEM::rsquared(foothills.poly) # best model
  }
  
  # grassland
  {
    grass.linear <- lmer(rich_observed ~ totdist_percent + 
                           Protocol + 
                           (1|Year),
                         data=filter(spR, NRNAME=="Grassland"), 
                         REML=F)
    grass.poly <- lmer(rich_observed ~ poly(totdist_percent,2, raw=T) + 
                         Protocol + 
                         (1|Year),
                       data=filter(spR, NRNAME=="Grassland"), 
                       REML=F)
    AIC(grass.linear, grass.poly) # poly is better
    piecewiseSEM::rsquared(grass.poly) # best model
  }

  # parkland - some convergence issues
  {
    parkland.linear <- lmer(rich_observed ~ totdist_percent + 
                           Protocol + 
                           (1|Year),
                         data=filter(spR, NRNAME=="Parkland"), 
                         REML=F)
    parkland.poly <- lmer(rich_observed ~ poly(totdist_percent,2, raw=T) + 
                         Protocol + 
                         (1|Year),
                       data=filter(spR, NRNAME=="Parkland"), 
                       REML=F)
    AIC(parkland.linear, parkland.poly) # poly is better
    piecewiseSEM::rsquared(parkland.poly) # best model
    
  }
  
  # rocky mountain - linear is better but note that HD doesn't extend to full gradient
  {
    mtn.linear <- lmer(rich_observed ~ totdist_percent + 
                              Protocol + 
                              (1|Year),
                            data=filter(spR, NRNAME=="Rocky Mountain"), 
                            REML=F)
    mtn.poly <- lmer(rich_observed ~ poly(totdist_percent,2, raw=T) + 
                            Protocol + 
                            (1|Year),
                          data=filter(spR, NRNAME=="Rocky Mountain"), 
                          REML=F)
    AIC(mtn.linear, mtn.poly) # linear is better
  }
}

# 2. How does CSI vary with HF ####
{
  # both protocols
  {
    csi.linear <- lmer(CSI ~ totdist_percent + Protocol + 
                             (1|Year),
                           data=veg_df, REML=F)
    csi.poly <- lmer(CSI ~ poly(totdist_percent,2, raw=T) + Protocol +
                           (1|Year),
                         data=veg_df, REML=F)
    
    summary(csi.linear) # variance on group RE indistinguishable from zero
    summary(csi.poly) # variance on group RE different from zero
    anova(csi.poly, type=2)
    anova(csi.linear, csi.poly)
    AIC(csi.linear, csi.poly) # csi poly
    AIC(csi.poly) - AIC(csi.linear)
    
    piecewiseSEM::rsquared(csi.poly) # best model
  }
  
  
  # terrestrial protocol
  {
    ter.csi.linear <- lmer(CSI ~ totdist_percent + 
                         (1|Year),
                       data=filter(veg_df, Protocol=="Terrestrial"), REML=F)
    ter.csi.poly <- lmer(CSI ~ poly(totdist_percent,2, raw=T) + 
                       (1|Year),
                     data=filter(veg_df, Protocol=="Terrestrial"), REML=F)
    
    # no longer needed; model fits fine w/o the extra ran effect
    # ss <- getME(ter.csi.poly,c("theta","fixef"))
    # m3 <- update(ter.csi.poly,start=ss,control=lmerControl(optimizer="bobyqa",
    #                                                    optCtrl=list(maxfun=2e5)))
    # getME(ter.csi.poly, "fixef")
    # getME(m3, "fixef")
    
    anova(ter.csi.linear, type=2) # variance on group RE indistinguishable from zero
    anova(ter.csi.poly, type=2) # variance on group RE different from zero
    anova(ter.csi.linear, ter.csi.poly)
    AIC(ter.csi.linear, ter.csi.poly) # csi poly
    AIC(ter.csi.poly) - AIC(ter.csi.linear)
    
    piecewiseSEM::rsquared(ter.csi.poly) # best model
    # piecewiseSEM::rsquared(m3) # best model
    
  }
  
  # wetland protocol
  {
    wet.csi.linear <- lmer(CSI ~ totdist_percent + 
                             (1|Year),
                           data=filter(veg_df, Protocol=="Wetland"), REML=F)
    wet.csi.poly <- lmer(CSI ~ poly(totdist_percent,2, raw=T) + 
                           (1|Year),
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
  # both protocols
  {
    rich.poly # previous best
    rich.exot.only <- lmer(rich_observed ~ propexotic + Protocol +
                                 (1|Year),
                               data=veg_df, REML=F)
    rich.poly.exot <- lmer(rich_observed ~ poly(totdist_percent,2, raw=T) + Protocol + 
                                 propexotic + 
                                 (1|Year),
                           data=veg_df, REML=F)
    rich.poly.exot.interaction <- lmer(rich_observed ~ poly(totdist_percent,2, raw=T) * propexotic + Protocol + 
                                             (1|Year),
                                           data=veg_df, REML=F)
    
    AIC(rich.poly, rich.exot.only, rich.poly.exot, rich.poly.exot.interaction)
    anova(rich.poly, rich.exot.only, rich.poly.exot, rich.poly.exot.interaction)
    anova(rich.poly.exot, rich.poly.exot.interaction) # interaction is best model
    summary(rich.poly.exot.interaction)
    anova(rich.poly.exot.interaction, type=2)
    
    piecewiseSEM::rsquared(rich.poly.exot.interaction) # best model
    
  }
  
  # terrestrial protocol
  {
    ter.rich.poly # previous best
    ter.rich.exot.only <- lmer(rich_observed ~ propexotic + 
                             (1|Year),
                             data=filter(veg_df, Protocol=="Terrestrial"), REML=F)
    ter.rich.poly.exot <- lmer(rich_observed ~ poly(totdist_percent,2, raw=T) + 
                             propexotic + 
                             (1|Year),
                             data=filter(veg_df, Protocol=="Terrestrial"), REML=F)
    ter.rich.poly.exot.interaction <- lmer(rich_observed ~ poly(totdist_percent,2, raw=T) * propexotic + 
                                         (1|Year),
                                       data=filter(veg_df, Protocol=="Terrestrial"), REML=F)
  
    AIC(ter.rich.poly, ter.rich.exot.only, ter.rich.poly.exot, ter.rich.poly.exot.interaction)
    anova(ter.rich.poly, ter.rich.exot.only, ter.rich.poly.exot, ter.rich.poly.exot.interaction)
    anova(ter.rich.poly.exot, ter.rich.poly.exot.interaction) # interaction is best model
    summary(ter.rich.poly.exot.interaction)
    anova(ter.rich.poly.exot.interaction, type=2)
    
    piecewiseSEM::rsquared(ter.rich.poly.exot.interaction) # best model
    
  }
  
  # Wetland protocol
  {
    wet.rich.poly # previous best
    wet.rich.exot.only <- lmer(rich_observed ~ propexotic + 
                                 (1|Year),
                               data=filter(veg_df, Protocol=="Wetland"), REML=F)
    wet.rich.poly.exot <- lmer(rich_observed ~ poly(totdist_percent,2, raw=T) + 
                                 propexotic + 
                                 (1|Year),
                               data=filter(veg_df, Protocol=="Wetland"), REML=F)
    wet.rich.poly.exot.interaction <- lmer(rich_observed ~ poly(totdist_percent,2, raw=T) * propexotic + 
                                             (1|Year),
                                           data=filter(veg_df, Protocol=="Wetland"), REML=F)
    AIC(wet.rich.poly, wet.rich.exot.only, wet.rich.poly.exot, wet.rich.poly.exot.interaction)
    anova(wet.rich.poly, wet.rich.exot.only, wet.rich.poly.exot, wet.rich.poly.exot.interaction)
    anova(wet.rich.exot.only, wet.rich.poly.exot) # additive model is best
    summary(wet.rich.poly.exot)
    anova(wet.rich.poly.exot, type=2)
    
    piecewiseSEM::rsquared(wet.rich.poly.exot) # best model - new result
    # interesting that the ran eff Year here more explanatory power
  }
}

# 4. does including % exotics improve fit of CSI model (from 2 above) ####
{
  # BOth Protocols
  {
    csi.poly # previous best
    csi.exot.only <- lmer(CSI ~ propexotic + Protocol +
                                # Latitude + Longitude +
                                (1|Year),
                              data=veg_df, REML=F)
    csi.poly.exot <- lmer(CSI ~ poly(totdist_percent,2) + 
                                propexotic + Protocol +
                                # Latitude + Longitude +
                                (1|Year),
                          data=veg_df, REML=F)
    csi.poly.exot.interaction <- lmer(CSI ~ poly(totdist_percent,2) * propexotic + 
                                        Protocol +
                                            # Latitude + Longitude +
                                            (1|Year),
                                      data=veg_df, REML=F)
    AIC(csi.poly, csi.exot.only, csi.poly.exot, csi.poly.exot.interaction)
    anova(csi.poly, csi.exot.only, csi.poly.exot, csi.poly.exot.interaction)
    summary(csi.poly.exot.interaction)
    piecewiseSEM::rsquared(csi.poly.exot.interaction) # best model
    anova(csi.poly.exot.interaction, type="2")  
  }
  
  # Terrestrial Protocol
  {
    ter.csi.poly # previous best
    ter.csi.exot.only <- lmer(CSI ~ propexotic +
                            # Latitude + Longitude +
                            (1|Year),
                          data=filter(veg_df, Protocol=="Terrestrial"), REML=F)
    ter.csi.poly.exot <- lmer(CSI ~ poly(totdist_percent,2) + 
                            propexotic + 
                            # Latitude + Longitude +
                            (1|Year),
                            data=filter(veg_df, Protocol=="Terrestrial"), REML=F)
    ter.csi.poly.exot.interaction <- lmer(CSI ~ poly(totdist_percent,2) * propexotic + 
                                        # Latitude + Longitude +
                                        (1|Year),
                                        data=filter(veg_df, Protocol=="Terrestrial"), REML=F)
    AIC(ter.csi.poly, ter.csi.exot.only, ter.csi.poly.exot, ter.csi.poly.exot.interaction)
    anova(ter.csi.poly, ter.csi.exot.only, ter.csi.poly.exot, ter.csi.poly.exot.interaction)
    summary(ter.csi.poly.exot.interaction)
    piecewiseSEM::rsquared(ter.csi.poly.exot.interaction) # best model
    anova(ter.csi.poly.exot.interaction, type="2")  
  }
  
  # Wetland Protocol
  {
    wet.csi.poly # previous best
    wet.csi.exot.only <- lmer(CSI ~ propexotic +
                                # Latitude + Longitude +
                                (1|Year),
                              data=filter(veg_df, Protocol=="Wetland"), REML=F)
    wet.csi.poly.exot <- lmer(CSI ~ poly(totdist_percent,2) + 
                                propexotic + 
                                # Latitude + Longitude +
                                (1|Year),
                              data=filter(veg_df, Protocol=="Wetland"), REML=F)
    wet.csi.poly.exot.interaction <- lmer(CSI ~ poly(totdist_percent,2) * propexotic + 
                                            # Latitude + Longitude +
                                            (1|Year),
                                          data=filter(veg_df, Protocol=="Wetland"), REML=F)
    AIC(wet.csi.poly, wet.csi.exot.only, wet.csi.poly.exot, wet.csi.poly.exot.interaction)
    anova(wet.csi.poly, wet.csi.exot.only, wet.csi.poly.exot, wet.csi.poly.exot.interaction)
    summary(wet.csi.poly.exot.interaction)
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
                                  Residuals=residuals(ter.rich.poly) )
    ter.csi.poly.resid <- data.frame(Site = filter(veg_df, Protocol=="Terrestrial")$Site,
                                 Year = filter(veg_df, Protocol=="Terrestrial")$Year,
                                 Lat = filter(veg_df, Protocol=="Terrestrial")$Latitude,
                                 Long = filter(veg_df, Protocol=="Terrestrial")$Longitude,
                                 Residuals=residuals(ter.csi.poly) )
    
    ter.rich.poly.exot.interaction.resid <- data.frame(Site = filter(veg_df, Protocol=="Terrestrial")$Site,
                                                   Year = filter(veg_df, Protocol=="Terrestrial")$Year,
                                                   Lat = filter(veg_df, Protocol=="Terrestrial")$Latitude,
                                                   Long = filter(veg_df, Protocol=="Terrestrial")$Longitude,
                                                   Residuals=residuals(ter.rich.poly.exot.interaction) )
    ter.csi.poly.exot.interaction.resid <- data.frame(Site = filter(veg_df, Protocol=="Terrestrial")$Site,
                                                  Year = filter(veg_df, Protocol=="Terrestrial")$Year,
                                                  Lat = filter(veg_df, Protocol=="Terrestrial")$Latitude,
                                                  Long = filter(veg_df, Protocol=="Terrestrial")$Longitude,
                                                  Residuals=residuals(ter.csi.poly.exot.interaction) )
    # save(ter.rich.poly.resid, ter.csi.poly.resid, ter.rich.poly.exot.interaction.resid, ter.csi.poly.exot.interaction.resid,
         # file="results/Model residuals - terrestrial protocol - 02092021.Rdata")
    
    
  }
  
  # save residuals from best models - Wetland protocol
  {
    wet.rich.poly.resid <- data.frame( Site = filter(veg_df, Protocol=="Wetland")$Site,
                                       Year = filter(veg_df, Protocol=="Wetland")$Year,
                                       Lat = filter(veg_df, Protocol=="Wetland")$Latitude,
                                       Long = filter(veg_df, Protocol=="Wetland")$Longitude,
                                       Residuals=residuals(wet.rich.poly) )
    wet.csi.poly.resid <- data.frame(Site = filter(veg_df, Protocol=="Wetland")$Site,
                                     Year = filter(veg_df, Protocol=="Wetland")$Year,
                                     Lat = filter(veg_df, Protocol=="Wetland")$Latitude,
                                     Long = filter(veg_df, Protocol=="Wetland")$Longitude,
                                     Residuals=residuals(wet.csi.poly) )
    
    wet.rich.poly.exot.resid <- data.frame(Site = filter(veg_df, Protocol=="Wetland")$Site,
                                                       Year = filter(veg_df, Protocol=="Wetland")$Year,
                                                       Lat = filter(veg_df, Protocol=="Wetland")$Latitude,
                                                       Long = filter(veg_df, Protocol=="Wetland")$Longitude,
                                                       Residuals=residuals(wet.rich.poly.exot) )
    wet.csi.poly.exot.interaction.resid <- data.frame(Site = filter(veg_df, Protocol=="Wetland")$Site,
                                                      Year = filter(veg_df, Protocol=="Wetland")$Year,
                                                      Lat = filter(veg_df, Protocol=="Wetland")$Latitude,
                                                      Long = filter(veg_df, Protocol=="Wetland")$Longitude,
                                                      Residuals=residuals(wet.csi.poly.exot.interaction) )
    # save(wet.rich.poly.resid, wet.csi.poly.resid, wet.rich.poly.exot.interaction.resid, wet.csi.poly.exot.interaction.resid,
    #      file="results/Model residuals - wetland protocol - 02092021.Rdata")
    
    
  }
  
  # rich_observedness w/ ape::Moran.I
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
      
      Moran.I(filter(veg_df, Protocol=="Terrestrial")$rich_observed, 
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
      
      Moran.I(filter(veg_df, Protocol=="Wetland")$rich_observed, 
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
  
  Moran.I(residuals(ter.rich.poly), ter.rich.d.inv) # observed I = 0.028
  Moran.I(residuals(ter.csi.poly), ter.csi.d.inv) # I = 0.056
  Moran.I(residuals(ter.rich.poly.exot.interaction), ter.rich.d.inv) # I=0.034
  Moran.I(residuals(ter.csi.poly.exot.interaction), ter.csi.d.inv) # I=0.045 
  
  Moran.I(residuals(wet.rich.poly), wet.rich.d.inv) # observed I = 0.040
  Moran.I(residuals(wet.csi.poly), wet.csi.d.inv) # I = 0.059
  Moran.I(residuals(wet.rich.poly.exot), wet.rich.d.inv) # I=0.025
  Moran.I(residuals(wet.csi.poly.exot.interaction), wet.csi.d.inv) # I=0.043
}

# 5. permanovas of multivariate NMDS's ####
{
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
  
  # permanovas using 2 HF bin groups: 0% and >95% HD
  {
    # both protocols
    adonis2(veg_distance_2groups ~ veg_2groups$HFbin) #  significant
    anova(betadisper(d= veg_distance_2groups, 
                     group=veg_2groups$HFbin)) # no significant dispersion
    veg_2groups %>% 
      group_by(HFbin) %>% 
      tally() # very different sample sizes...
    
    # terrestrial protocol
    # differences between groups could be because of dispersion (variance) or mean; adonis2 is less sensitive to dispersion than mrpp
    adonis2(veg_distance_2groups_ter ~ veg_2groups_ter$HFbin) #  significant
    anova(betadisper(d= veg_distance_2groups_ter, 
                     group=veg_2groups_ter$HFbin)) # significant dispersion too... sample sizes are diff?
    veg_2groups_ter %>% 
      group_by(HFbin) %>% 
      tally() # very different sample sizes...
    
    # wetland protocol
    # differences between groups could be because of dispersion (variance) or mean; adonis2 is less sensitive to dispersion than mrpp
    adonis2(veg_distance_2groups_wet ~ veg_2groups_wet$HFbin) #  significant
    anova(betadisper(d= veg_distance_2groups_wet, 
                     group=veg_2groups_wet$HFbin)) # no significant dispersion
    veg_2groups_wet %>% 
      group_by(HFbin) %>% 
      tally() # very different sample sizes...
  }
  
  # permanovas using 3 HF bin groups: 0%, 45-55%, and >95% HD
  {
    # both protocols
    adonis2(veg_distance_3groups ~ veg_3groups$HFbin) #  significant
    anova(betadisper(d= veg_distance_3groups, 
                     group=veg_3groups$HFbin)) # significant dispersion
    veg_3groups %>% 
      group_by(HFbin) %>% 
      tally() # very different sample sizes...
    
    # terrestrial protocol
    # differences between groups could be because of dispersion (variance) or mean; adonis2 is less sensitive to dispersion than mrpp
    adonis2(veg_distance_3groups_ter ~ veg_3groups_ter$HFbin) #  significant
    anova(betadisper(d= veg_distance_3groups_ter, 
                     group=veg_3groups_ter$HFbin)) # significant dispersion too... sample sizes are diff?
    veg_3groups_ter %>% 
      group_by(HFbin) %>% 
      tally() # very different sample sizes...
    # this is supposed to do pairwise comparisons, but i'm suspicious
    RVAideMemoire::pairwise.perm.manova(resp=veg_distance_3groups_ter, 
                                        fact=veg_3groups_ter$HFbin, p.method="holm")
    # wetland protocol
    # differences between groups could be because of dispersion (variance) or mean; adonis2 is less sensitive to dispersion than mrpp
    adonis2(veg_distance_3groups_wet ~ veg_3groups_wet$HFbin) #  significant
    anova(betadisper(d= veg_distance_3groups_wet, 
                     group=veg_3groups_wet$HFbin)) # significant dispersion
    veg_3groups_wet %>% 
      group_by(HFbin) %>% 
      tally() # very different sample sizes...
    RVAideMemoire::pairwise.perm.manova(resp=veg_distance_3groups_wet, 
                                        fact=veg_3groups_wet$HFbin, p.method="holm")
    
  }
}

# 6. comparison of HD, rich_observed, nonnatives, and CSI across low, med, high hd levels ####
{
  # make new df only with sites in 3 hf bins
  veg_df_3groups <- left_join(veg_df, 
            select(veg_3groups, Protocol, Site, Year, HFbin),
            by=c("Protocol", "Site", "Year")) %>% # add bin number to HF df
    filter(!is.na(HFbin))
  # make HF bin and ordered factor
  veg_df_3groups$HFbin <- factor(veg_df_3groups$HFbin, ordered=T, levels=c("Low", "Int.", "High"))
  
  ss <- getME(tmp,c("theta","fixef"))
  m2 <- update(tmp,start=ss,control=lmerControl(optCtrl=list(maxfun=2e4)))
  
  anova(m2, type=2)
  
  emmeans::emmeans(m2, list(pairwise ~ HFbin), adjust = "tukey")
  
  # HD
  veg_df_3groups %>% 
    group_by(HFbin) %>% 
    summarize(N=length(totdist_percent),
              meddist=median(totdist_percent),
              IQR=IQR(totdist_percent))
  
  anova(lmer(totdist_percent ~ HFbin + 
               Protocol + (1|Year),
             data=veg_df_3groups, REML=F), type=2)
  
  ggplot(veg_df_3groups, aes(x=HFbin, y=totdist_percent)) +
        geom_boxplot(fill="grey80") +
        labs(x="Disturbance Level", y="Human Development (%)")
  
  # richness
  veg_df_3groups %>% 
    group_by(HFbin) %>% 
    summarize(N=length(rich_observed),
              medrich_observed=median(rich_observed),
              IQR=IQR(rich_observed))
  
  ggplot(veg_df_3groups, aes(x=HFbin, y=rich_observed)) +
    geom_boxplot(fill="grey80") +
    labs(x="Disturbance Level", y="Species Richness (Observed)")
  
  # nonnatives
  veg_df_3groups %>% 
    group_by(HFbin) %>% 
    summarize(N=length(propexotic  ),
              medrich_observed=median(propexotic),
              IQR=IQR(propexotic ))
  
  ggplot(veg_df_3groups, aes(x=HFbin, y=propexotic)) +
    geom_boxplot(fill="grey80") +
    labs(x="Disturbance Level", y="Proportion of Exotics (% of observed richness)")
  
  # csi
  veg_df_3groups %>% 
    group_by(HFbin) %>% 
    summarize(N=length(CSI),
              medrich_observed=median(CSI),
              IQR=IQR(CSI))
  
  ggplot(veg_df_3groups, aes(x=HFbin, y=CSI)) +
    geom_boxplot(fill="grey80") +
    labs(x="Disturbance Level", y="Niche Specialization Index")
  
  
}

# 7. linear vs poly relationship between prop exotics and HF 
{

  exotic_m1 <- lmer(propexotic ~ totdist_percent + 
               Protocol + (1|Year),
             data=veg_df, REML=F) # sig effect of HF bin
  
  exotic_m2 <- lmer(propexotic ~ poly(totdist_percent, 2) + 
               Protocol + (1|Year),
             data=veg_df, REML=F) # sig effect of HF bin
  
  anova(exotic_m1, exotic_m2)
  summary(exotic_m2)
  anova(exotic_m2, type=2)
  piecewiseSEM::rsquared(exotic_m2)
  AIC(exotic_m2) - AIC(exotic_m1)
}

# 8. comparison of median CSI across low/med/high bins #### NEEDS UPDATING
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
               Protocol + (1|Year),
             data=csi_bin, REML=F) # no convergence
  
  ss <- getME(tmp,c("theta","fixef"))
  m2 <- update(tmp,start=ss,control=lmerControl(optCtrl=list(maxfun=2e4)))
  
  anova(m2, type=2)
  
  emmeans::emmeans(m2, list(pairwise ~ HFbin), adjust = "tukey")
  
  csi_bin %>% group_by(HFbin) %>% tally()
  
}

# 9. trends in native vs nonnative richness over HD gradient#### NEEDS UPDATING
{
  veg_exot2 <- veg_exot %>% mutate(rich_observedexot=round(rich_observed*(propexotic/100),0)) %>% 
    select(NRNAME, Protocol, WetlandType, Site, Year, UniqueID, totdist_percent, 
           "Total"=rich_observed, "Nonnative"=rich_observedexot) %>% 
    mutate(Native=Total-Nonnative) 
  veg_exot2$Year <- as.factor(veg_exot$Year)
  
  head(veg_exot2)
  
  # nonnative species
  nnr.linear <- lmer(Nonnative ~ totdist_percent + 
                        Protocol + 
                        (1|Year),
                      data=veg_exot2, REML=F)
  nnr.poly <- lmer(Nonnative ~ poly(totdist_percent,2, raw=T) + 
                      Protocol + 
                      (1|Year),
                    data=veg_exot2, REML=F)
  anova(nnr.linear, type=2) # RE of unique site ID should be kept
  anova(nnr.poly, type=2) # RE of unique site ID should be kept
  anova(nnr.linear, nnr.poly) # poly is better
  summary(nnr.poly)
  piecewiseSEM::rsquared(nnr.poly) RAIC(nnr.poly) - AIC(nnr.linear)
  
  # native species
  nr.linear <- lmer(Native ~ totdist_percent + 
                       Protocol + 
                       (1|Year),
                     data=veg_exot2, REML=F)
  nr.poly <- lmer(Native ~ poly(totdist_percent,2, raw=T) + 
                     Protocol + 
                     (1|Year),
                   data=veg_exot2, REML=F)
  anova(nr.linear, type=2) # RE of unique site ID should be kept
  anova(nr.poly, type=2) # RE of unique site ID should be kept
  anova(nr.linear, nr.poly) # poly is better
  summary(nr.poly)
  piecewiseSEM::rsquared(nr.poly) 
  AIC(nr.poly) - AIC(nr.linear)
}

# 10. species typical of binned sites