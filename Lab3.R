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
#   Boxplots                                                                ####
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
plogis(0)
plogis(1)
plogis(-5)
plogis(-100)
plogis(5)
plogis(100)

# No matter what p is, plogis(p) will always be between 0 and 1

# Let's play with it some more. Next we will simulate 101 numbers from -50 to 50, 
# and then imagine we are conducting a single binomial trial at each value from
# -50 to 50. The probability, say, selecting a discrete habitat value increases 
# with each value of x according to the Logistic beta coefficient of 0.07.

# Start by creating a uniform vector from -50 to 50
x <- c(-50:50) 
y <- rbinom(length(x), 1, plogis(0+0.07*x)) 

# The 0.07 here is the coefficient for a habitat covariate 

plot( y ~ x)
abline(lm(y~x)) # Linear model is wrong though cus it can be more than 1 and less than 0 but the intercept is right (see next chunk)


## ------------------------------------------------------------------------------------------------------------------------
wrong = lm(y~x)
summary(wrong)
coef(wrong)


## ------------------------------------------------------------------------------------------------------------------------
res <- glm( y~x, family=binomial(link="logit"))
summary(res)
yLogit <- predict(res) # linear predictor part of equation

plot( yLogit ~ x )
yhat <- predict(res, type="response")
plot( y ~ x)
lines(yhat~x)
# Slope at inflection point is the same as the slope of the linear predictor function


## ------------------------------------------------------------------------------------------------------------------------
elev <- glm(used ~ Elevation2, family=binomial(logit), data=wolf_all)
summary(elev)
# looking at summary, intercept is the baseline probability of wolf selecting elevation 0
# negative slope means as elevation increases, chance of wolf selecting it goes down
## exploring univarite logistic regression
## how to obtain 95% confidence intervals? Where are they in the output?
## CI's using profile log-likelihood's
confint(elev)
## CI's using standard errors
confint.default(elev)


## ------------------------------------------------------------------------------------------------------------------------
exp(coefficients(elev))
## how to obtain 95% CI's on odds ratio's
exp(cbind(OR=coef(elev), confint(elev)))



## ------------------------------------------------------------------------------------------------------------------------
wolf_all$elev100 <- wolf_all$Elevation2 / 100
elev100 <- glm(used ~ elev100, family=binomial(logit), data=wolf_all)
summary(elev100)
exp(coef(elev100))


## ------------------------------------------------------------------------------------------------------------------------
elevBnp <- 0:3000 ## creates a new vector elevBnp with ranges from 0 - 3000 in it.
elevPred <- predict(elev, newdata=data.frame(Elevation2=elevBnp), type = "response") ## uses the predict function to predict Y values given the model object elev
hist(elevPred)
plot(elevBnp, elevPred, type="l", ylim = c(0,1.0), ylab= "Pr(Used)") # y axis, predicted probability of use


## ------------------------------------------------------------------------------------------------------------------------
plot(wolf_all$Elevation2, wolf_all$used)
lines(elevBnp, elevPred, type="l", ylab= "Pr(Used)")


## ------------------------------------------------------------------------------------------------------------------------
## next human use
distHuman <- glm(used ~ DistFromHumanAccess2, family=binomial(logit), data=wolf_all)
summary(distHuman)
hist(wolf_all$DistFromHumanAccess2)
disthumanBnp = 0:7000
disthumanPred = predict(distHuman, newdata=data.frame(DistFromHumanAccess2=disthumanBnp), type="response")
hist(disthumanPred)
plot(disthumanBnp, disthumanPred, type="l", ylab= "Pr(Used)")
plot(wolf_all$DistFromHumanAccess2, wolf_all$used)
lines(disthumanBnp, disthumanPred, type="l", ylab= "Pr(Used)")


## ------------------------------------------------------------------------------------------------------------------------
## next human use
distHHuman <- glm(used ~ DistFromHighHumanAccess2, family=binomial(logit), data=wolf_all)
summary(distHHuman)
hist(wolf_all$DistFromHighHumanAccess2)
disthumanBnp = 0:10000
disthumanPred = predict(distHHuman, newdata=data.frame(DistFromHighHumanAccess2=disthumanBnp), type="response")
hist(disthumanPred)
plot(disthumanBnp, disthumanPred, type="l", ylab= "Pr(Used)")
plot(wolf_all$DistFromHighHumanAccess2, wolf_all$used)
lines(disthumanBnp, disthumanPred, type="l", ylab= "Pr(Used)")


## ------------------------------------------------------------------------------------------------------------------------
# now lets do all at once for ungulate HSI models
sheep <- glm(used ~ sheep_w2, family=binomial(logit), data=wolf_all)
summary(sheep)
deer <- glm(used ~ deer_w2, family=binomial(logit), data=wolf_all)
summary(deer)
elk <- glm(used ~ elk_w2, family=binomial(logit), data=wolf_all)
summary(elk)
moose <- glm(used ~ moose_w2, family=binomial(logit), data=wolf_all)
summary(moose)
exp(coef(moose)) # odds ratio
goat <- glm(used ~ goat_w2, family=binomial(logit), data=wolf_all)
summary(goat)


## ------------------------------------------------------------------------------------------------------------------------
habvalues <- 0:7 ## making a vector of hsi values
sheeppred <- predict(sheep, newdata = data.frame(sheep_w2 = habvalues), type = "response")
goatpred <- predict(goat, newdata = data.frame(goat_w2 = habvalues), type = "response")
moosepred <- predict(moose, newdata = data.frame(moose_w2 = habvalues), type = "response")
elkpred <- predict(elk, newdata = data.frame(elk_w2 = habvalues), type = "response")
deerpred <- predict(deer, newdata = data.frame(deer_w2 = habvalues), type = "response")
sheeppred <- predict(sheep, newdata = data.frame(sheep_w2 = habvalues), type = "response")


## ------------------------------------------------------------------------------------------------------------------------
## back to elevation
elev <- glm(used ~ Elevation2, family=binomial(logit), data=wolf_all)
summary(elev)
wolf_all$fitted.Elev <- fitted(elev) # predicted probability of wolf use at that elevation
head(wolf_all)
hist(wolf_all$fitted.Elev)
plot( wolf_all$Elevation2, wolf_all$fitted.Elev)


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


