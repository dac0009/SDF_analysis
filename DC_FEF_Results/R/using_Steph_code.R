library(tidyverse) # year
library(dplyr) # select, pipes
library(plyr) # mutate
library(readxl)
library(nlme) # lme
library(piecewiseSEM) # rsquared
library(car) # Anova
library(predictmeans) # CookD

#####
# IV - Custom theme to see everything better
my_theme <- theme(axis.line.x = element_line(linewidth = 0.5, colour = "black"), 
                  axis.line.y = element_line(linewidth = 0.5, colour = "black"),
                  axis.line = element_line(linewidth = 1, colour = "black"), panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(), panel.border = element_blank(),  
                  panel.background = element_blank(), text=element_text(size = 14),
                  axis.text.x=element_text(colour="black", size = 14),
                  axis.text.y=element_text(colour="black", size = 14), 
                  legend.title = element_blank(),legend.key=element_blank())
#####
# read in the data
field_dat <- read.csv("spreadsheets/SDFresults.csv")

# Okay so in stephs data they are grouped by data so however many samples are set to a specific date code.
# This sounds like how the watersheds are in my data set so i am going to use the watershed the same way she
# uses the date codes

# I am also not splitting the data by depth right now because I am trying to figure out what matches with
# what between the two data sets... This is what i came up with...
# Her block is our plot // Her crop is our watershed // I guess we can use depth instead of Date Code?

statdat <- field_dat %>%
  select(SampleID, watershed, plot, depth,
         BulkC_gm2, LFC_gm2, hPOMC_gm2, MAOMC_gm2) %>%
  filter(!SampleID == c(47:48)) # WS5-6 - Cut on a different rotation

hist(statdat$BulkC_gm2) # possibe outliers

BC.m1 <- lme(BulkC_gm2 ~ watershed, random = ~1|plot/depth, data = statdat, na.action = na.exclude)
# Plot cook's distance to check for outliers
# If Cook's distance > 1, that point may be considered an outlier !!
# NOTE: I read that above 0.5 could also fuck with the model? Just something to consider
CookD(BC.m1) # Nothing is above 1 or 0.5 the highest is ~0.25... I have no idea what that means in regards
# to this analysis but it does identify Sample IDs that look wild: **11,13,17**

# I am not going to remove anything yet. 

# Save the residuals to the datatable
statdat$resid_BC.m1<-residuals(BC.m1, type = "normalized")
# do the ANOVA on the model
Anova(BC.m1)
# R2 value
rsquared(BC.m1) 

# check the assumptions of the model - examine distribution of residuals
# residuals vs fitted values
plot(BC.m1)
# Check normality of residuals
par(mfrow=c(1,2))
qqPlot(BC.m1$residuals) # Car package qqPlot() that will just get the residuals for you
qqnorm(statdat$resid_BC.m1)
qqline(statdat$resid_BC.m1)
hist(statdat$resid_BC.m1)

# Boxplot of residuals by fixed effect
ggplot(statdat, aes(x = watershed, y = resid_BC.m1))+
  geom_boxplot() +
  ggtitle("Residuals by Watershed") +
  my_theme

# There are five identifiable outliers according to the box plot - Remember this is NOT split by depth! 
# I guess we can split it by depth and see if there's a difference // This was only on Bulk Carbon ! 

############
# 180 data #
############
field_dat <- read.csv("spreadsheets/bulk180.csv")

statdat <- field_dat %>%
  filter(!watershed == "F381")
statdat <- statdat %>%
  filter(!SampleID == c(139:144))

hist(statdat$BulkC_gm2) # possible outliers?

# Fit the model 
model <- lme(BulkC_gm2 ~ watershed, random = ~1|plot/subplot, data = statdat, na.action = na.exclude)

# Print summary stats
CookD(model)
summary(model)
Anova(model)
rsquared(model)
