library("zoo") # convert number to date

setwd("c:/Users/DIFENGY/Working/[COVID 19]")
df <- read.csv("time_series_covid19_confirmed_china.csv")
df$Date <- as.Date(df$Date, format = "%m/%d/%y")
df$Date <- as.numeric(df$Date)

df_emer <- read.csv("time_series_covid19_china_emergency.csv")
df_emer$DateL1 <- as.Date(df_emer$DateL1, format = "%m/%d/%y")
df_emer$Date95 <- as.Date(df_emer$Date95, format = "%m/%d/%y")

# Remove Outliers
df <- df[,!names(df) %in% c("Hong.Kong")]  #the ones that haven't saturated
df <- df[,!names(df) %in% c("Tibet")] #too little cases
df <- df[,!names(df) %in% c("Beijing", "Gansu", "Macau", "Shanghai")] #large number of overseas imported cases which makes a second increase
df <- df[,!names(df) %in% c("Shandong")]  #special case
df <- df[,!names(df) %in% c("Hubei")] #too severe

df_emer <- df_emer[!df_emer$Province %in% c("Hong Kong"),]
df_emer <- df_emer[!df_emer$Province %in% c("Tibet"),]
df_emer <- df_emer[!df_emer$Province %in% c("Beijing", "Gansu", "Macau", "Shanghai"),]
df_emer <- df_emer[!df_emer$Province %in% c("Shandong"),]
df_emer <- df_emer[!df_emer$Province %in% c("Hubei"),]

# Logistic Curve Fitting
for (curr in c(2:ncol(df))) {
  mL <- nls(as.formula(paste(names(df)[curr], "~ SSlogis(Date, Asym, xmid, scal)")), df)
  p <- summary(mL)
  df_emer$upperlimit[curr-1] <- coef(mL)[1]
  df_emer$inflection[curr-1] <- coef(mL)[2] 
  df_emer$scale[curr-1] <- coef(mL)[3]
  df_emer$growthrate[curr-1] <- 1/coef(mL)[3]
  df_emer$inflectionDate[curr-1] <- floor(coef(mL)[2])
  # Slope Calculation
  Asym <- coef(mL)[1]
  xmid <- coef(mL)[2]
  scal <- coef(mL)[3]
  dGomp <- deriv(y ~ Asym/(1+exp((xmid-x)/scal)), "x", func = TRUE)
  df_emer$slopeL1[curr-1] <- attr(dGomp(as.numeric(df_emer$DateL1[curr-1])), "gradient")
  df_emer$slope95[curr-1] <- attr(dGomp(as.numeric(df_emer$Date95[curr-1])), "gradient")
  df_emer$slopeHighest[curr-1] <- attr(dGomp(xmid), "gradient")
  #df_emer$slopeRandom[curr-1] <- attr(dGomp(df$Date[5]), "gradient") # See if random correlation exsits 
}
df_emer$inflectionDate <- as.Date(df_emer$inflectionDate)
D(expression(Asym/(1+exp((xmid-x)/scal))), "x") # Function Derivative

# Individual province drawing
curr <- 9
mL <- nls(as.formula(paste(names(df)[curr], "~ SSlogis(Date, Asym, xmid, scal)")), df)
summary(mL)
plot(as.Date(df$Date),df[[curr]])
lines(df$Date,predict(mL))
names(df)[curr]

# Count Days
df_emer$L1toInfl <- df_emer$inflection - as.numeric(df_emer$DateL1)
df_emer$D95toInfl <- df_emer$inflection - as.numeric(df_emer$Date95)

hist(df_emer$L1toInfl)
hist(df_emer$D95toInfl)

# Midpoint/Curve prediction
library(ggplot2)

### Single Predictor
##### Slope
#p <- ggplot(df_emer, aes(x=slopeL1, y=L1toInfl)) + geom_point() + geom_smooth(method = "lm")
#p + scale_fill_brewer(palette="Paired") + theme_minimal()
#cor(df_emer$slopeL1, df_emer$L1toInfl) #r = -0.04616008

#p <- ggplot(df_emer, aes(x=slopeL1, y=growthrate)) + geom_point() + geom_smooth(method = "lm")
#p + scale_fill_brewer(palette="Paired") + theme_minimal()
#cor(df_emer$slopeL1, df_emer$growthrate) #r = 0.09930863

#p <- ggplot(df_emer, aes(x=slope95, y=D95toInfl)) + geom_point() + geom_smooth(method = "lm")
#p + scale_fill_brewer(palette="Paired") + theme_minimal()
#cor(df_emer$slope95, df_emer$D95toInfl) #r = -0.3257696

#p <- ggplot(df_emer, aes(x=slope95, y=growthrate)) + geom_point() + geom_smooth(method = "lm")
#p + scale_fill_brewer(palette="Paired") + theme_minimal()
#cor(df_emer$slope95, df_emer$growthrate) #r = 0.2152935

##### Date
p <- ggplot(df_emer, aes(x=L1toInfl, y=growthrate)) + geom_point() + geom_smooth(method = "lm")
p + scale_fill_brewer(palette="Paired") + theme_minimal()
cor(df_emer$L1toInfl, df_emer$growthrate) #r = -0.4099569 -> Low negative correlation

#p <- ggplot(df_emer, aes(x=D95toInfl, y=growthrate)) + geom_point() + geom_smooth(method = "lm")
#p + scale_fill_brewer(palette="Paired") + theme_minimal()
#cor(df_emer$D95toInfl, df_emer$growthrate) #r = -0.14549

#p <- ggplot(df_emer, aes(x=L1toInfl, y=slopeHighest)) + geom_point() + geom_smooth(method = "lm")
#p + scale_fill_brewer(palette="Paired") + theme_minimal()
#cor(df_emer$L1toInfl, df_emer$slopeHighest) #r = 0.1185144

#p <- ggplot(df_emer, aes(x=D95toInfl, y=slopeHighest)) + geom_point() + geom_smooth(method = "lm")
#p + scale_fill_brewer(palette="Paired") + theme_minimal()
#cor(df_emer$D95toInfl, df_emer$slopeHighest) #r = -0.1282253

##### Useful
p <- ggplot(df_emer, aes(x=slopeL1, y=slopeHighest)) + geom_point() + geom_smooth(method = "lm")
p + scale_fill_brewer(palette="Paired") + theme_minimal()
lmFitL1 <- lm(slopeHighest ~ slopeL1, data = df_emer)
summary(lmFitL1)
cor(df_emer$slopeL1, df_emer$slopeHighest) #r = 0.9342886

p <- ggplot(df_emer, aes(x=slope95, y=slopeHighest)) + geom_point() + geom_smooth(method = "lm")
p + scale_fill_brewer(palette="Paired") + theme_minimal()
lmFit95 <- lm(slopeHighest ~ slope95, data = df_emer)
summary(lmFit95)
cor(df_emer$slope95, df_emer$slopeHighest) #r = 0.9436328

lmFitComb <- lm(slopeHighest ~ slopeL1 + slope95, data = df_emer)
summary(lmFitComb)

##### Leaving unsolved
#p <- ggplot(df_emer, aes(x=slopeRandom, y=slopeHighest)) + geom_point() + geom_smooth(method = "lm")
#p + scale_fill_brewer(palette="Paired") + theme_minimal()
#lmFit <- lm(slopeHighest ~ slopeRandom, data = df_emer)
#summary(lmFit)
#cor(df_emer$slopeRandom, df_emer$slopeHighest) 

pred_df <- data.frame("predL1" = predict(lmFitL1, newdata = df_emer),
                      "pred95" = predict(lmFit95, newdata = df_emer),
                      "predComb" = predict(lmFitComb, newdata = df_emer))

TRIMIN <- 1
TRIMAX <- 10
df_trim <- df[TRIMIN:TRIMAX,]
curr <- 23   #TRIMIN = 1 and TRIMAX = 10 and curr = 23 show okay fit, for example
c1 <- pred_df$predComb[curr -1]
x <- as.numeric(df$Date[TRIMIN:TRIMAX])
y <- as.matrix(df_trim[curr])
mL <- nls(y ~ (4*c1/grate)/(1+exp((xmid-x)*grate)),
          start = list(grate = 0.25, xmid = as.numeric(df_emer$DateL1[curr-1]) + 10),
          lower = list(grate = 0.20, xmid = as.numeric(df_emer$DateL1[curr-1]) + 5),
          upper = list(grate = 0.30, xmid = as.numeric(df_emer$DateL1[curr-1]) + 15),
          algorithm = "port") # Start, Lower, and Upper can be improved with the bound
summary(mL)
newy <- (4*c1/coef(mL)[1])/(1+exp((coef(mL)[2]-df$Date)*coef(mL)[1]))
plot(as.Date(df$Date),df[[curr]])
lines(df$Date, newy)
names(df)[curr]

#https://en.wikipedia.org/wiki/Logistic_function
#https://covid-19-au.com/

#####Let's see how VIC looks
df_vic <- read.csv("time_series_covid19_confirmed_vic.csv")
df_vic$Date <- as.Date(df_vic$Date, format = "%m/%d/%y")
# Slope approximation
slopeL1_vic <- (df_vic$VIC[21] - df_vic$VIC[19]) / 2   #Stage 1: 23 March 2020
slope95_vic <- (df_vic$VIC[28] - df_vic$VIC[27]) / 1   #Stage 3: 31 March 2020
pred_95_vic <- predict(lmFit95, 
                       newdata = data.frame("slopeL1" = c(slopeL1_vic), "slope95" = c(slope95_vic)))
c1 <- pred_95_vic
x <- as.numeric(df_vic$Date)
y <- as.matrix(df_vic$VIC)
mL <- nls(y ~ (4*c1/grate)/(1+exp((xmid-x)*grate)),
          start = list(grate = 0.25, xmid = as.numeric(df_vic$Date[28])),
          algorithm = "port") 
summary(mL)
lastDate <- as.numeric(df_vic$Date[nrow(df_vic)])
df_vic <- rbind(df_vic, data.frame("Date" = as.Date(c((lastDate+1):(lastDate+60))), "VIC" = NA))
df_vic$Pred <- (4*c1/coef(mL)[1])/(1+exp((coef(mL)[2]-as.numeric(df_vic$Date))*coef(mL)[1]))
plot(as.Date(df_vic$Date),df_vic$VIC, ylim=c(0, 3000), xlab="Date", ylab="Predicted Confirmed Cases")
lines(as.Date(df_vic$Date), newy, lty = 3)

#####Fancy Plots
library(ggplot2)
p <- ggplot(data = df_vic) +
  geom_line(aes(x=Date, y=Pred),linetype="dashed", color="blue", size=1.2) +
  geom_point(aes(x=Date, y=VIC), color="red", size=1.5) +
  ylab("Predicted Confirmed Cases")
p + theme_minimal()


