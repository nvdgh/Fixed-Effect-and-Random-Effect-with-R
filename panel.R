library(foreign)
library(car)
library(plm)
library(gplots)

Panel <- read.dta("http://dss.princeton.edu/training/Panel101.dta")

########################
# Exploring Panel Data #
########################

coplot(y~year|country, type='l', data=Panel)
coplot(y~year|country, type='b', data=Panel)

# Heterogenity across countries
scatterplot(y~year|country, boxplots=F, smooth=T, reg.line=F, data=Panel)
scatterplot(y~country, boxplots=F, smooth=T, reg.line=F, data=Panel) # = scatterplot(y~country, data=Panel)

# Heterogenity across years
scatterplot(y~year|country, boxplots=F, smooth=T, reg.line=T, data=Panel)

######################
# Fixed Effect Model #
######################

# Fixed effects: Heterogeneity across countries (or entities)
plotmeans(y~country, main='Heterogenity across countries', data=Panel)

# Fixed effects: Heterogeneity across years 
plotmeans(y~year, main='Heterogeneity across years', data=Panel)

##################
# OLS regression #
##################

# Regular OLS regression does not consider heterogeneity across groups or time (Simpson Paradox) see Ben p2 v18
ols <- lm(y~x1, data=Panel)
warnings(ols)
summary(ols)

yhat <- ols$fitted

plot(Panel$x1, Panel$y, pch=19, xlab = 'x1', ylab = 'y')
abline(ols, lwd=3, col='red')

#########################################################
# Fixed effects using Least squares dummy variable model #
##########################################################

fixed.dum <- lm(y~x1+factor(country)-1,data=Panel)
summary(fixed.dum)

# Comparison plot of LSDV and pooled OLS
yhat <- fixed.dum$fitted

scatterplot(yhat~Panel$x1|Panel$country, xlab='x1',ylab='yhat') # graph (11)
abline(ols, lwd=3, col='red')

###########################################################
# Fixed effects: n entity-specific intercepts (using plm) #
###########################################################

fixed <- plm(y~x1, data = Panel, index=c('country','year'), model='within')
summary(fixed)

#  # Display the fixed effects (constants for each country)
fixef(fixed) # see the graph (11) above, D is the highest and C is the lowest

# check whether the assumption of country unobserved heterogeineity is correct (fixed vs ols)
pFtest(fixed,ols)

##############################
# Random effects (using plm) #
##############################

random <- plm(y~x1, data=Panel, index=c('country','year'), model='random')
summary(random)

# Setting as panel data (an alternative way to run the above model
Panel.set <- plm.data(Panel, index=c('country', 'year'))

# Random effects using panel setting (same output as above)
random.set <- plm(y~x1, data=Panel.set, model='random')
summary(random.set)

#################################
# Fixed or Random: Hausman test #
#################################

phtest(fixed, random)
