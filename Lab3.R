#   WILD562 - Lab 3                                                         ####

# Author: Jordan Heiman

# Date:2023-02-02 

# Purpose: Lab 3 for Wildlife Habitat Modeling with Dr. Mark Hebblewhite. This 
#          lab focuses on logistic regression for univariate analysis using data 
#          from two wolf packs in the Ya Ha Tinda area. This is a continuation 
#          of code from lab 2.  

################################################################################
# Setup                                                                     ####
# Line to set working directory/install and load here package
if (!require("here", character.only = TRUE)) {
  install.packages("here", dependencies = TRUE)
  library("here", character.only = TRUE)
}

################################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
# Using ipak function, install and load packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# List packages needed
packages <- c("ggplot2","lattice", "tidyverse", "effects", "data.table", "tools")

# Run function
ipak(packages)

#      Functions                                                            ####

#      Data                                                                 ####
wolf_used <- fread(here("Data", "wolfused.csv"))
wolf_avail <- fread(here("Data", "wolfavail.csv"))
################################################################################

# Bring together the used and available points from the wolf data. Because the 
# columns in each table are the same they can be brought together with rbind()
wolf_all <- rbind(wolf_used, wolf_avail)

# Take a quick look at the data to make sure things look right
str(wolf_all)

# Check how many used vs available points there are for each pack
table(wolf_all$used, wolf_all$pack)

# And how many used vs available points there are for each deer  winter HSI level
table(wolf_all$used, wolf_all$deer_w2)

# Convert the used/available column to a factor. I labeled them as Available and 
# Used but could label as anything
wolf_all[, used := factor(used, 
                          labels = c("Available", "Used"))]

# I'm also going to clean up the column names a little here
setnames(wolf_all, 
         names(wolf_all),
         gsub("_w2", "_wHSI", names(wolf_all)))
setnames(wolf_all, 
         c("Elevation2", "DistFromHumanAccess2", "DistFromHighHumanAccess2", "used"),
         c("elevation", "dist_human_acc", "dist_high_acc", "used_avail"))

# Confirm that that got the desired results
str(wolf_all)

# While we are here, break up the data into each pack 
bv <- wolf_all[pack == "Bow Valley", ]
rd <- wolf_all[pack == "Red Deer", ]

################################################################################
#   Box plots                                                               ####
# This is a function that will make box plots that compare used vs available 
# points for all the different covariates and print them in a list for viewing
boxplots <- function(dt, by_pack = FALSE){
  
  cols_HSI <- grep("wolf", names(dt), value = TRUE, invert = TRUE) %>% 
    grep("HSI", ., value = TRUE)
  cols_other <- grep("HSI|pack|used_avail", names(dt), value = TRUE, invert = TRUE)
  
  par(mfrow = c(2, ceiling(length(cols_HSI)/2)))
 
  for (i in 1:length(cols_HSI)){
    
    if (by_pack == FALSE){
      
      boxplot(eval(parse(text = cols_HSI[[i]])) ~ used_avail,
              main = toTitleCase(gsub("_wHSI", "", cols_HSI[i])),
              ylab = "Winter HSI",
              xlab = "Category", 
              data = dt)
      
    } else if (by_pack == TRUE){
      
      boxplot(eval(parse(text = cols_HSI[[i]])) ~ used_avail + pack,
              main = toTitleCase(gsub("_wHSI", "", cols_HSI[i])),
              ylab = "Winter HSI",
              xlab = "Category", 
              data = dt)
    }
    
  }
  
  par(mfrow = c(2, ceiling(length(cols_other)/2)))
  
  for (j in 1:length(cols_other)){
    
    if (by_pack == FALSE){
      
      boxplot(eval(parse(text = cols_other[[j]])) ~ used_avail,
              main = case_when(cols_other[j] == "elevation" ~ "Elevation",
                               cols_other[j] == "dist_human_acc" ~ "Distance to Human Access",
                               cols_other[j] == "dist_high_acc" ~ "Distance to High Access",),
              ylab = "Meters",
              xlab = "Category", 
              data = dt)
      
    } else if (by_pack == TRUE){
      
      boxplot(eval(parse(text = cols_other[[j]])) ~ used_avail + pack,
              main = case_when(cols_other[j] == "elevation" ~ "Elevation",
                               cols_other[j] == "dist_human_acc" ~ "Distance to Human Access",
                               cols_other[j] == "dist_high_acc" ~ "Distance to High Access",),
              ylab = "Meters",
              xlab = "Category", 
              data = dt)
      
    }
    
  }
  
}

# Just to remind ourselves what the data looked like last week, make some box 
# plots to see the different data using the function we just created. (Scroll 
# back in the plots to switch between prey covariates and other covariates)
boxplots(wolf_all)
boxplots(bv)
boxplots(rd)

# We can make the box plots more complex to compare between packs
boxplots(wolf_all, by_pack = TRUE)

## using lattice package (these are ugly)
bwplot(sheep_wHSI + goat_wHSI + elk_wHSI + moose_wHSI + deer_wHSI ~ as.factor(used_avail) | pack, 
       data = wolf_all, 
       layout = c(2, 5), 
       pch = "|",
       outer = TRUE)

# Try to get some summary statistics of the data, commented out, throws error 
# (because of NA values?)
# aggregate(wolf_all[1:9], 
#           by = list(wolf_all$pack, wolf_all$used_avail),
#           FUN = mean, 
#           na.rm = TRUE)

# Take a look at the averages of different coavriates for each pack and used and 
# available
wolf_all[, .(mean_elev = mean(elevation), 
             mean_dha = mean(dist_human_acc, na.rm = TRUE), 
             mean_dhha = mean(dist_high_acc, na.rm = TRUE),
             mean_moose = mean(moose_wHSI, na.rm = TRUE),
             mean_elk = mean(elk_wHSI, na.rm = TRUE),
             mean_sheep = mean(sheep_wHSI, na.rm = TRUE),
             mean_deer = mean(deer_wHSI, na.rm = TRUE),
             mean_goat = mean(goat_wHSI, na.rm = TRUE)), by = c("pack", "used_avail")] %>% 
  setorder(., pack, used_avail) %>% 
  print()

par(mfrow = c(1, 1))
################################################################################
#   Logistic Regression                                                     ####

# Just to learn about logistic regression let's look at if we were flipping a 
# fair coin. In practice we are actually trying to estimate the 0.5 based on the 
# trial results
coin_trial <- rbinom(100, 1, 0.5)
sum(coin_trial)

# Now what if this was survival data, where the survival probability is 0.9
surv_trial <- rbinom(100, 1, 0.9)
sum(surv_trial)

# Now let's play with plogis(), this is the distribution function for converting 
# a value from the logistic scale to a real probability
# If p = 0, then the probability of an outcome is 0.5, either heads or tails (a fair coin)
plogis_dt <- data.table(p = -100:100,
                        plogis = 0)
plogis_dt[, plogis := plogis(p)]
ggplot(plogis_dt) +
  aes(x = p, y = plogis) +
  geom_line(colour = "#112446") +
  theme_minimal()

# No matter what p is, plogis(p) will always be between 0 and 1

# Let's play with it some more. Next we will simulate 101 numbers from -50 to 50, 
# and then imagine we are conducting a single binomial trial at each value from
# -50 to 50. The probability of, say, selecting a discrete habitat value increases 
# with each value of x according to the Logistic beta coefficient of 0.07.

# Start by creating a uniform vector from -50 to 50
x <- c(-50:50) 
y <- rbinom(length(x), 1, plogis(0 + 0.07 * x)) 
plot(y ~ x)
abline(lm(y ~ x)) 

# This linear model is wrong though because it can be more than 1 and less than 0
linear_mod <- lm(y~x)
summary(linear_mod)
coef(linear_mod)

# What's needed is a GLM
glm_mod <- glm(y ~ x, family = binomial(link = "logit"))
summary(glm_mod)
yLogit <- predict(glm_mod) # linear predictor part of equation

plot(yLogit ~ x )
yhat <- predict(glm_mod, type = "response")
plot(y ~ x)
lines(yhat ~ x)
# Slope at inflection point is the same as the slope of the linear predictor function

################################################################################
#   Wolf Logistic Regression                                                ####

# Starting with the elevation covariate, first creating a GLM
elev_glm <- glm(used_avail ~ elevation, 
                family = binomial(logit), 
                data = wolf_all)
summary(elev_glm)

# Looking at summary, intercept is the baseline probability of wolf selecting
# elevation 0 feet. The negative slope means as elevation increases, chance of 
# wolf selecting it goes down

# Exploring univarite logistic regression:
# How to obtain 95% confidence intervals? 
# Where are they in the output?
# CI's using profile log-likelihood's
confint(elev_glm)
## CI's using standard errors
confint.default(elev_glm)

# To estimate the odds ratio:
exp(coefficients(elev_glm))
# To obtain 95% CI's on odds ratio's
exp(cbind(OR = coef(elev_glm), confint(elev_glm)))
# Thus, for every increase in elevation by 1 meter, the odds of wolf use decrease 
# by 0.9936, a really small amount. Note this is not really compelling. 

# Next we will consider rescaling to make a more compelling example. Let's 
# rescale to every kilometer instead of every meter
wolf_all[, elevation_km := wolf_all$elevation / 100]
elev_km_glm <- glm(used_avail ~ elevation_km, 
                   family = binomial(logit), 
                   data = wolf_all)
summary(elev_km_glm)
exp(coef(elev_km_glm))
# We would conclude that for every increase in 100 meters of elevation, the odds 
# of wolf use decreases by 0.528. Therefore the interpretation of the odds ratio 
# is scale dependent. 

# Next, let's continue by extracting predictions from our Elevation model. 
# First, we create a new vector of elevations from 0 to 3000 meters, the 
# approximate range of elevations in BNP where wolves were observed or could
# have been (available elevations). 
bnp_elev <- 0:3000 

# Then use the predict function to predict Y values, given the elevation glm 
elev_predict <- predict(elev_glm,
                        newdata = data.frame(elevation = bnp_elev), 
                        type = "response") 
hist(elev_predict)
plot(bnp_elev, 
     elev_predict, 
     type = "l", 
     ylim = c(0, 1.0), 
     ylab = "Pr(Used)") # Y axis, predicted probability of use

# If we look at just the elevations that were actually available in the wolf  
# home ranges, we can now tell that we are only seeing a portion of the logistic
# regression. Note that because the wolf_all$used_avail column is a factor type, 
# the axis is from 1 to 2 instead of from 0 to 1 so the elev_predict object 
# needs 1 added to every value to make the values between 1 and 2 as well.
plot(wolf_all$elevation, wolf_all$used_avail)
lines(bnp_elev, 
      elev_predict + 1, 
      type = "l", 
      ylab = "Pr(Used)")

# Now we work through the rest of the covariates, next is human use
human_glm <- glm(used_avail ~ dist_human_acc, 
                 family = binomial(logit), 
                 data = wolf_all)
summary(human_glm)
hist(wolf_all$dist_human_acc)
bnp_human <- 0:7000
human_predict <- predict(human_glm,
                         newdata = data.frame(dist_human_acc = bnp_human), 
                         type = "response")
hist(human_predict)
plot(bnp_human, 
     human_predict, 
     type = "l",
     ylab = "Pr(Used)")
plot(wolf_all$dist_human_acc, wolf_all$used_avail)
lines(bnp_human, 
      human_predict + 1, 
      type = "l",
      ylab = "Pr(Used)")

# Then high human use
high_glm <- glm(used_avail ~ dist_high_acc, 
                family = binomial(logit), 
                data = wolf_all)
summary(high_glm)
hist(wolf_all$dist_high_acc)
bnp_high <- 0:10000
high_predict <- predict(high_glm, 
                        newdata = data.frame(dist_high_acc = bnp_high), 
                        type = "response")
hist(high_predict)
plot(bnp_high, 
     high_predict, 
     type = "l", 
     ylab = "Pr(Used)")
plot(wolf_all$dist_high_acc, wolf_all$used_avail)
lines(bnp_high, 
      high_predict + 1, 
      type = "l", 
      ylab = "Pr(Used)")

# Now the ungulate HSI models
sheep_glm <- glm(used_avail ~ sheep_wHSI,
                 family = binomial(logit), 
                 data = wolf_all)
summary(sheep_glm)
deer_glm <- glm(used_avail ~ deer_wHSI, 
                family = binomial(logit),
                data = wolf_all)
summary(deer_glm)
elk_glm <- glm(used_avail ~ elk_wHSI, 
               family = binomial(logit), 
               data = wolf_all)
summary(elk_glm)
moose_glm <- glm(used_avail ~ moose_wHSI, 
                 family = binomial(logit),
                 data = wolf_all)
summary(moose_glm)
exp(coef(moose_glm)) # odds ratio
goat_glm <- glm(used_avail ~ goat_wHSI, 
                family = binomial(logit),
                data = wolf_all)
summary(goat_glm)

# And the predicted values for those models using a vector of the possible HSI 
# values (1 to 7)
HSI_values <- 0:7 
sheep_predict <- predict(sheep_glm,
                         newdata = data.frame(sheep_wHSI = HSI_values), 
                         type = "response")
goat_predict <- predict(goat_glm, 
                        newdata = data.frame(goat_wHSI= HSI_values),
                        type = "response")
moose_predict <- predict(moose_glm, 
                         newdata = data.frame(moose_wHSI= HSI_values), 
                         type = "response")
elk_predict <- predict(elk_glm, 
                       newdata = data.frame(elk_wHSI= HSI_values), 
                       type = "response")
deer_predict <- predict(deer_glm, 
                        newdata = data.frame(deer_wHSI= HSI_values), 
                        type = "response")
sheep_predict <- predict(sheep_glm, 
                         newdata = data.frame(sheep_wHSI= HSI_values), 
                         type = "response")

# Now using the elevation glm, we will save the predicted values as a new column.
# Just a reminder of the elevation glm
summary(elev_glm)
# Then add a column that is the predicted probability of wolf use at that elevation
wolf_all[, elev_fitted := fitted(elev_glm)]
# Let's look at that data a little
head(wolf_all)
hist(wolf_all$elev_fitted)
plot(wolf_all$elevation, wolf_all$elev_fitted)

################################################################################
#   Graphical Predictions with ggplot                                       ####


## ------------------------------------------------------------------------------------------------------------------------
# ggplot 2 explore basic histogram functio
ggplot(wolf_all, aes(x=fitted.Elev)) + 
  geom_histogram()
# lets explore faceting
ggplot(wolf_all, aes(x=fitted.Elev)) + 
  geom_histogram(binwidth=0.05, fill="gray70", colour="black") + 
  facet_grid(used ~ .)
ggplot(wolf_all, aes(x=fitted.Elev)) + 
  geom_histogram(binwidth=0.05, fill="gray70", colour="black") + 
  facet_grid(used ~ ., scales = "free")
ggplot(wolf_all, aes(x=fitted.Elev, fill=usedFactor)) +
  geom_histogram(binwidth=0.05, position="identity", alpha=0.7) + 
  xlab("Predicted Probability of Wolf Use") + 
  theme(axis.title.x=element_text(size=16))
# lets redo this graph using faceting by pack - stacked denstiy plot, use b/c sample sizes are different
ggplot(wolf_all, aes(x = fitted.Elev, y=after_stat(density), fill=usedFactor)) +
  geom_histogram(binwidth=0.05, position="identity", alpha=0.7) + 
  xlab("Predicted Probability of Wolf Use") + 
  theme(axis.title.x=element_text(size=16)) + 
  facet_grid(pack ~ ., scales="free")


## ------------------------------------------------------------------------------------------------------------------------
# Now lets explore fitting functions to the distributions
ggplot(wolf_all, aes(x=fitted.Elev)) + geom_density()
ggplot(wolf_all, aes(x=fitted.Elev), fill=usedFactor) + geom_density(alpha=0.5) + xlim(0,1)+xlab("Predicted Probability of Wolf Use") + theme(axis.title.x=element_text(size=16)) 
# kernel lines
ggplot(wolf_all, aes(x=fitted.Elev, y=..density.., fill=usedFactor)) + geom_histogram(binwidth=0.05) + geom_density(alpha = 0.5) + facet_grid(pack ~ .)



## ------------------------------------------------------------------------------------------------------------------------
# Exploring Predictions as a function of covariates
# this fits a univariate glm as a function of elevation and predicts
ggplot(wolf_all, aes(x=Elevation2, y=used)) + geom_point() + stat_smooth(method="glm", method.args = list(family="binomial"))


## ------------------------------------------------------------------------------------------------------------------------
ggplot(wolf_all, aes(x=DistFromHumanAccess2, y=used)) + geom_point() + stat_smooth(method="glm", method.args = list(family="binomial"))
ggplot(wolf_all, aes(x=elk_w2, y=used)) + geom_point() + stat_smooth(method="glm", method.args = list(family="binomial"))


## ------------------------------------------------------------------------------------------------------------------------
ggplot(wolf_all, aes(x=elk_w2, y=used)) + geom_point() +geom_jitter(aes(colour = used), width=0.25, height = 0.05) + stat_smooth(method="glm", method.args = list(family="binomial"))


## ------------------------------------------------------------------------------------------------------------------------
## lets redo elevation jittered by used
ggplot(wolf_all, aes(x=Elevation2, y=used)) + geom_point() +geom_jitter(aes(colour = used), width=0.25, height = 0.05)+ stat_smooth(method="glm", method.args = list(family="binomial"))


## ------------------------------------------------------------------------------------------------------------------------
ggplot(wolf_all, aes(x=Elevation2, y=used, colour=pack)) + geom_point() + geom_jitter(width=0.25, height = 0.05) +stat_smooth(method="glm", method.args = list(family="binomial"), level=0.90)
# Showing much higher avoidance of high elevation by bow valley pack


## ------------------------------------------------------------------------------------------------------------------------
ggplot(wolf_all, aes(x=moose_w2, y=used, colour=pack)) + stat_smooth(method="glm", method.args = list(family="binomial")) # pretty parallel here
# don't really pay attention to slope as much
ggplot(wolf_all, aes(x=sheep_w2, y=used, colour=pack)) + stat_smooth(method="glm", method.args = list(family="binomial"))
# big difference between packs


## ------------------------------------------------------------------------------------------------------------------------
# versus faceting by wolf pack
ggplot(wolf_all, aes(x=Elevation2, y=used)) + geom_point() + stat_smooth(method="glm", method.args = list(family="binomial"), level=0.90) + facet_grid(pack~.)


## ------------------------------------------------------------------------------------------------------------------------
# this function plots predictions from the previously fitted best model
ggplot(wolf_all, aes(x=Elevation2, y=fitted.Elev)) + geom_point() + stat_smooth(method=lm) + ylim(0, 0.8)


## ------------------------------------------------------------------------------------------------------------------------
plot(effects::effect("Elevation2", elev), grid=TRUE, rescale.axis = FALSE, ylab = "Probability(Used)") 


## ------------------------------------------------------------------------------------------------------------------------
plot(effects::effect("deer_w2", deer), grid=TRUE, rescale.axis = FALSE, ylab = "Probability(Used)")


## ------------------------------------------------------------------------------------------------------------------------
#Printing PDFs from R
pdf("Output/wolf_elev.pdf", width=4, height=4)
print(ggplot(wolf_all, aes(x=Elevation2, y=used)) + 
        geom_point(colour="gray") + 
        stat_smooth(method="glm", method.args = list(family="binomial")) + 
        xlab("Elevation (m)") + 
        ylab("Predicted Probability of Wolf Use"))
dev.off()
# then go and look in the active directory for wolf_elev.pdf
#or
ggsave("Output/elev_wolf2.pdf", width=4, height=4)
#


## ------------------------------------------------------------------------------------------------------------------------
figPrey <- ggplot(wolf_all, aes(x=elk_w2, y=used)) + 
  geom_smooth(data = wolf_all, 
              aes(x=elk_w2, y=used, col="Elk"),
              method="glm", 
              method.args = list(family="binomial")) + 
  geom_smooth(data = wolf_all, 
              aes(x=deer_w2, y=used, col="Deer"),
              method="glm",
              method.args = list(family="binomial")) +
  geom_smooth(data = wolf_all,
              aes(x=moose_w2, y=used, col="Moose"),
              method="glm",
              method.args = list(family="binomial")) +
  geom_smooth(data = wolf_all, 
              aes(x=sheep_w2, y=used, col="Sheep"),
              method="glm", 
              method.args = list(family="binomial")) +
  geom_smooth(data = wolf_all, 
              aes(x=goat_w2, y=used, col="Goat"),
              method="glm",
              method.args = list(family="binomial")) +
  xlab("Relative prey habitat suitability") +
  ylab("Relative probability of wolf use") +
  theme(axis.title.y=element_text(size=12), axis.text.y=element_text(size=12)) +
  theme(axis.title.x=element_text(size=12), axis.text.x=element_text(size=12)) +
  labs(fill="Prey Species")

figPrey

## lets save this
pdf("Output/figPrey.pdf", width=6, height=6)
figPrey
dev.off()

# These are all univariate though! but the graph makes it look like all are taken into account


