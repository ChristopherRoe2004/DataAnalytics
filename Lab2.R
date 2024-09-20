


###Setup###

epi2024results06022024 <- read.csv("C:/Users/roec3/R/epi2024results06022024.csv")
View(epi2024results06022024)
EPI_data <- epi2024results06022024
View(EPI_data)
attach(EPI_data)
EPI.new
tf <- is.na(EPI.new)
E <- EPI.new[!tf]

###Boxplot comparing PAR.new, SPI.new, TBN.new###
boxplot(PAR.new, SPI.new, TBN.new, main="PAR.new Boxplot vs. SPI.new Boxplot vs. TBN.new Boxplot")

###Q-Q Plots###

#PAR.new to Norm Dist#
x <- seq(20., 80., 1.0)
qqplot(qnorm(ppoints(200)), PAR.new, main="PAR.new Q-Q Plot vs. Norm Dist Q-Q Plot")
qqline(PAR.new)
qqplot(qnorm(ppoints(200)),x,main="PAR.new Q-Q Plot vs. Norm Dist Q-Q Plot")
qqline(x)

#SPI.new to PAR.new#
qqplot(qnorm(ppoints(200)), SPI.new,main="SPI.new Q-Q Plot vs. PAR.new Q-Q Plot")
qqline(SPI.new)
qqplot(qnorm(ppoints(200)),PAR.new,main="SPI.new Q-Q Plot vs. PAR.new Q-Q Plot")
qqline(PAR.new)

#TBN.new to SPI.new#
qqplot(qnorm(ppoints(200)), TBN.new,main="TBN.new Q-Q Plot vs. SPI.new Q-Q Plot")
qqline(TBN.new)
qqplot(qnorm(ppoints(200)),SPI.new,main="TBN.new Q-Q Plot vs. SPI.new Q-Q Plot")
qqline(SPI.new)

###ECDF Plots###

#PAR.new to TBN#
plot(ecdf(PAR.new), do.points=FALSE, main="PAR.new ECDF vs. TBN.new ECDF") 
plot(ecdf(TBN.new), do.points=FALSE, main="PAR.new ECDF vs. TBN.new ECDF")
lines(ecdf(PAR.new))

#SPI.new to PAR.new#
plot(ecdf(SPI.new), do.points=FALSE, main="SPI.new ECDF vs. PAR.new ECDF")
plot(ecdf(PAR.new), do.points=FALSE, main="SPI.new ECDF vs. PAR.new ECDF")
lines(ecdf(SPI.new))

#TBN.new to SPI.new#
plot(ecdf(TBN.new), do.points=FALSE, main="TBN.new ECDF vs. SPI.new ECDF")
plot(ecdf(SPI.new), do.points=FALSE, main="TBN.new ECDF vs. SPI.new ECDF")
lines(ecdf(TBN.new))

### Populations Dataset ###

## read data
populations_2023 <- read.csv("countries_populations_2023.csv")

## drop country populations that don't exist in epi results
populations <- populations_2023[-which(!populations_2023$Country %in% epi.results$country),]

## sort populations by country name
populations <- populations[order(populations$Country),]

## drop country results that don't exist in populations
epi.results.sub <- epi.results[-which(!epi.results$country %in% populations$Country),]

## sort results by country name
epi.results.sub <- epi.results.sub[order(epi.results.sub$country),]

## only keep relevant columns
epi.results.sub <- epi.results.sub[,c("country","PAR.new","PAR.old","SPI.new","SPI.old","TBN.new","TBN.old")]

## convert to mnumeric
epi.results.sub$population <- as.numeric(populations$Population)

## compute population log
epi.results.sub$population_log <- log10(epi.results.sub$population)

attach(epi.results.sub)

###Linear Plots###

#PAR.new#
lin.mod.parnew <- lm(PAR.new~population_log,epi.results.sub)

plot(PAR.new~population_log)
abline(lin.mod.parnew)

summary(lin.mod.parnew)

#Call:
#lm(formula = PAR.new ~ population_log, data = epi.results.sub)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-55.868 -18.302  -3.577  16.598  70.300 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     112.001     22.018   5.087 1.09e-06 ***
#  population_log   -9.887      3.095  -3.194  0.00171 ** 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 26.49 on 148 degrees of freedom
#(15 observations deleted due to missingness)
#Multiple R-squared:  0.0645,	Adjusted R-squared:  0.05818 
#F-statistic:  10.2 on 1 and 148 DF,  p-value: 0.001712

plot(lin.mod.parnew)


ggplot(epi.results.sub, aes(x = population_log, y = PAR.new)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod.parnew, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')

#SPI.new#
lin.mod.spinew <- lm(SPI.new~population_log,epi.results.sub)

plot(SPI.new~population_log)
abline(lin.mod.spinew)

summary(lin.mod.spinew)

#Call:
#lm(formula = SPI.new ~ population_log, data = epi.results.sub)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-61.124 -20.814   1.461  21.575  55.338 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)  
#(Intercept)      14.347     18.544   0.774   0.4403  
#population_log    5.206      2.649   1.965   0.0511 .
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 27.64 on 161 degrees of freedom
#(2 observations deleted due to missingness)
#Multiple R-squared:  0.02342,	Adjusted R-squared:  0.01736 
#F-statistic: 3.862 on 1 and 161 DF,  p-value: 0.05112

plot(lin.mod.spinew)


ggplot(epi.results.sub, aes(x = population_log, y = SPI.new)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod.spinew, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')

#TBN.new#
lin.mod.tbnnew <- lm(TBN.new~population_log,epi.results.sub)

plot(TBN.new~population_log)
abline(lin.mod.tbnnew)

summary(lin.mod.tbnnew)

#Call:
#lm(formula = TBN.new ~ population_log, data = epi.results.sub)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-51.992 -23.711   1.088  24.688  48.524 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)  
#(Intercept)     49.3540    19.9566   2.473   0.0144 *
#  population_log   0.3645     2.8580   0.128   0.8987  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 30.47 on 163 degrees of freedom
#Multiple R-squared:  9.978e-05,	Adjusted R-squared:  -0.006035 
#F-statistic: 0.01627 on 1 and 163 DF,  p-value: 0.8987

plot(lin.mod.tbnnew)


ggplot(epi.results.sub, aes(x = population_log, y = TBN.new)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod.tbnnew, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')

