library('tidycensus')
library('tidyverse')
require("ggpubr")
require("ggplot2")

##### WEEK 1 #####

df <- get_acs(
  geography = "tract",
  variables = c('DP05_0001E','DP05_0018E','DP03_0062E','DP02_0065PE','DP03_0096PE','DP03_0128PE','DP04_0047PE'),
  year=2019,
  state = "IL",
  county = "Cook",
  geometry = TRUE,
  output="wide",
  key = 'e61b56441ee4ab32492482feed5b4d49fd550cea'
)

df<- df[grep("M$",names(df),invert=TRUE)]
colnames(df) <- c('geoid','name','totpop','medage','medhhinc','propbac','propcov','proppov','proprent','geometry')
df <- na.omit(df)

#Step 4 heat map
ggplot() +
  geom_sf(data = df, aes(fill = propbac), show.legend = TRUE) +
  labs(title = 'Baccalaureate Attainment Rates', subtitle = 'Cook County, IL', caption = "From 2015 to 2019") + 
  scale_fill_viridis_c(name = 'Rate', option="B", direction = -1)

#Step 5 
linAlg <- lm('propbac ~ medhhinc',data=df)
summary(linAlg)


ggplot(data = df, 
       aes(x = medhhinc, y = propbac)) +
  geom_point(col = "black" , alpha = 0.5) +
  labs(title = 'Median Household Income vs Baccalaureate Attainment Rates',
       subtitle = 'Cook County, IL',
       caption = "From 2015 to 2019",
       x = 'Median Household Income ($)',
       y = 'Baccalaureate Attainment Rates (%)') + 
  xlim(0,250000) + 
  ylim(0,100) +
  stat_smooth(method = "lm",
              formula =  y ~ x,
              geom = "smooth")

##### Week 2 #####

# Finding Normality, Serial Correlation, heteroskedasticity
# Normality
hist(linAlg$residuals, xlab='Residual' , prob="TRUE", col = 'blue', main = "Histogram of Residuals")
  lines(density(linAlg$residuals), lwd = 2)
  grid()
  
plot(linAlg$residuals, xlab = "Index of Residual", ylab = "Residual", col = 'blue', main = "Scatter Plot of Residuals")
grid()

ggqqplot(linAlg$residuals, color = "Blue") # shows not normal

shapiro.test(linAlg$residuals) 
ks.test(linAlg$residuals, pnorm)#test and plots show that the data is not normal

#Serial Correlation, need to discuss during meeting
library(lmtest)
dwtest(linAlg) # There is serial correlation we will reject Null hypothesis, shows serial corr..
plot(acf(linAlg$residuals, type = "correlation"), main = "Auto Correlation Plot") # does not show serial corr
grid()
plot(linAlg$residuals[-1314], linAlg$residuals[-1], col = "blue", xlab = "Lag", ylab= "Current", main ="Relationship between consecutive values of Baccalaureate") #lag and current values plot - does not show serial corr
grid()
#Heteroskedasticity
plot(linAlg, 1, col = "Blue")#look for residual vs Fitted plot
grid()
plot(linAlg, 3, col = "Blue")
grid()
bptest(linAlg) #shows hetero..

#Step 7 Simulating Data
sim_medhhinc = replicate(10000,sample(df$medhhinc,nrow(df),replace=TRUE))
sim_medhhinc

#find the actual correlation between data
ActualCorr <- cor(df$propbac, df$medhhinc)
ActualCorr # actual samples corr

corfunction <- function(x){
  y <- cor(sim_medhhinc[,x], df$propbac)
}

samplesCorrMatrix <- lapply(X = 1:10000, corfunction)

samplesCorrMatrix <- as.numeric(samplesCorrMatrix)
proportionofsamples <- samplesCorrMatrix >= ActualCorr
proportionofsamples
sum(proportionofsamples) / 10000 # finding proportion which is 0 and that is good :D

# Step 8 - Distribution of correlation
hist(samplesCorrMatrix, breaks = 100, prob = TRUE, col= "blue", 
  main = "Bootstrapped Correlations between Median Household Incomes
     and actual Baccalaureate Attainment Rates", xlab = "Sample Correlations",
    xlim=range( c(min(samplesCorrMatrix),ActualCorr) )
  )
 lines(density(samplesCorrMatrix), lwd = 2)
 grid()
points(ActualCorr,0,pch=19,col='red')

#test for normality -- both test shows that data is normal
#install.packages("tseries")
require(tseries)
jarque.bera.test(samplesCorrMatrix)
#install.packages("nortest")
require(nortest)
lillie.test(samplesCorrMatrix)

ggqqplot(samplesCorrMatrix, color = "blue") # shows normal data

#------------------------------------ WEEK 3------------------------


actualPredictions <- fitted(linAlg)
sum((actualPredictions - df$propbac)^2)

yintercept <- linAlg$coefficients['(Intercept)']
yintercept

simulatedm <- seq(from = 0.0002, to= 0.00031, length = 1000)
simulatedm

SSEFunc <- function(x) {
  newYs <- (simulatedm[x]*df$medhhinc) + yintercept
  sum((newYs - df$propbac)^2)
}

actualm <- linAlg$coefficients[2]
actualm

SSEFinal <- lapply(1:length(simulatedm), SSEFunc)
SSEFinal

plot(simulatedm, SSEFinal, xlab = "Gradient Values", ylab="Corresponding SSE", col = "blue", main = "Response of SSE for different Values of Slope", sub = "Keeping Y-Intercept constant")
 abline(v = actualm, lwd = 2, lty = 2)
 mtext("This is Model based value of Gradient: 0.0002577586 ", side=3)
 
#Q11
 slope = linAlg$coefficients['medhhinc']
 inter_test = seq(0, 2*yintercept, 0.01*yintercept)
 data = data.frame(cbind(df$medhhinc, df$propbac)) %>%
   rename(X = X1, Y = X2) %>% drop_na()
 log_likelihood = function(yintercept, slope. = slope, x = data$X, y = data$Y){
   SSE = sum((y - slope.*x - yintercept)^2); n = length(y)
   sigma_sq = SSE/(n - 2)
   return(-(log(sigma_sq)/2 + log(2*pi)/2)*n - SSE/(2*sigma_sq))
 }
 log_likeli = sapply(inter_test, log_likelihood)
 plot(inter_test, log_likeli, type = 'l',  lwd = 2,
      xlab = "Intercept Values", ylab="Corresponding Log Likelihood Values", col = "blue", main = "Response of Log Likelihood for different Values of Intercept", sub = "Keeping Slope constant")
 points(x = yintercept, y = log_likelihood(yintercept), col = 'red', pch = 16)
 abline(v = yintercept, lty = 2, lwd = 2)
 mtext("This is Model based value of Gradient: 0.0002577586 ", side=3)
 
 
 
 
#Q12
 
 sum(df$totpop*df$propbac/100, na.rm = TRUE)
 df_new = df %>% drop_na(medhhinc, propbac)
 df_new[['medhhinc_policy']] = df_new[['medhhinc']]
 medhhinc_sort = sort(df_new[['medhhinc']], index.return = TRUE)$ix
 df_new[['medhhinc_policy']][medhhinc_sort[1:50]] = df_new[['medhhinc_policy']][medhhinc_sort[1:50]] + 10000
 df_new[['medhhinc_policy']][rev(medhhinc_sort)[1:50]] = df_new[['medhhinc_policy']][rev(medhhinc_sort)[1:50]] - 10000
 df_new[['propbac_pre']] = slope*df_new[['medhhinc_policy']] + yintercept
 sum(df_new$totpop*df_new$propbac_pre/100)
 
 ggplot() +
   geom_sf(data = df, aes(fill = propbac), show.legend = TRUE) +
   labs(title = 'Baccalaureate Attainment Rates', subtitle = 'Cook County, IL', caption = "From 2015 to 2019") + 
   scale_fill_viridis_c(name = 'Rate', option="B", direction = -1)
 

 #overlaid robinhood tax
 df_new[['propbac_old']] = slope*df_new[['medhhinc']] + yintercept
 
 ggplot(df_new, aes(x = df_new$medhhinc)) +
   geom_point(aes(y = df_new$propbac, color = "Actual Rates")) +
   geom_point(aes(y = df_new$propbac_pre, color = "Predicted Rates After Tax")) +
   geom_point(aes(y = df_new$propbac_old, color = "Predicted Rates Before Tax")) +
   labs(
     x='Original Median Household Income',
     y='Baccalaureate Attainment Rates'
    ) +
   ggtitle('Effect of Robinhood Tax')+theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = c(0.8,0.2))

 mean(df_new$propbac)
 mean(df_new$propbac_pre)
 
 
 
 
################################# GROUP ASSIGNMENT 2 ###########################
### WEEK 4
##Q1 and Q2
GA2Model <- lm(propbac ~ totpop + medage + medhhinc + propcov + proppov + proprent, data = df)
summary(GA2Model)
#newR^2 0.7149 and #preR^2 0.5351
#Explanations
  #2.a) The old model's R² is 0.5351, while the new one is 0.7149. 
    # This means our new model is able to explain ~18% more of the variance due to the additional predictors 
    # added. Since most of the coefficients of the new predictors are significant, it likely means that
    # the additional explainability is because such factors truely do affect propbac in real life. 

#Q2b
anova(linAlg,GA2Model)
  #anova result p-value < 2.2e-16 hence improvement is significant 

#Q2c
ggplot() + 
  geom_density(
    aes(x = GA2Model$residuals, y=..density.., fill="Single-variate"), 
    alpha=0.3 , colour="black")+ 
  geom_density(
    aes(x = linAlg$residuals, y =..density.., fill="Multivariate"), 
    alpha=0.3,  colour = "Black", position="identity") + 
  labs(x = "Residuals",
       y= "Emperical Densities", 
       title = "Density of Residuals from Single-Variate Model vs Multivariate Model",
       color='Legend') +
  scale_color_manual(values = c('old'='blue','new'='red')) +
  guides(fill=guide_legend('Model Residual'))

#another graph for densities
colors <- c("Multi_Feature" = "blue", "Single_Feature" = "red")
ggplot() + 
  labs(x = "Residuals", y= "Emperical Densities", title = "Distribution of Residuals from Single Predictor Model and Multi Predictor Model", color = "Legend") + 
  geom_density(aes(x = GA2Model$residuals, y=..density.., color = "Multi_Feature"), size = 2) + 
  geom_density(aes(x = linAlg$residuals, y =..density.., color = "Single_Feature"), alpha = 0.5, size = 2) +
  scale_color_manual(values = colors)

#Q3 we should use the multi predictor one..
# WE should use the multivariate model because its R² is higher and anova indicates improvement is significant

#Q4 
#A: Totpop, MedAge
#B: Proprent
#C: medhhinc, proppov, propcov


#q6a

df2 <- map_df(us, function(x) { 
  get_acs(geography = "tract", 
          year = 2019,
          variables = c('DP05_0001E','DP02_0065PE'), 
          state = x,
          output = 'wide')
})

#renaming
df2<- df2[grep("M$",names(df2),invert=TRUE)]
colnames(df2) <- c('geoid','name','totpop','propbac','geometry')
head(df2)

#created a column IsCook and added Flag to it
df2$IsCook <- 0
df2$IsCook[grep('Cook', df2$name)] = 1

prevDim <- dim(df2)
prevDim

df2 <- na.omit(df2)
newDim <- dim(df2)
newDim

#filtering population to atleast 100
df2 <- df2 %>% 
  filter(df2$totpop >=100)

df2

#Q6b

#equal weight avg
eq_weight <- mean(df2$propbac[df2$IsCook == 0])

#weighted by population
weighted.mean(df2$propbac[df2$IsCook == 0], df2$totpop[df2$IsCook==0])

#q6c t test to see the means
t.test(df2$propbac[df2$IsCook == 1] , mu = eq_weight)


#Q7_____________ 
# I dont know if  I am right or wrong, I got the track for NBC from this web
#https://geocoding.geo.census.gov/geocoder/geographies/address?street=455%20Cityfront%20Plaza%20Dr&city=Chicago&state=Illinois&zip=60611&benchmark=4&vintage=4
#I assume gleatcher is also in the same tract
# it also matches geoid so should be good
#17031081403 GEO ID for NBC and GLEATCHER


GA2Model$fitted.values[df2$geoid == "17031081403"] #point estimate
confint(GA2Model, level= 0.9) #no idea here, confused, Discuss

#Q7B
#adding weights
GA2Model2 <- lm(propbac ~ totpop + medage + medhhinc + propcov + proppov + proprent, data = df, weights = totpop)
confint(GA2Model2, level = 0.9) #interval are more tighter
GA2Model2$fitted.values[df2$geoid == 17031081403] #point estimate shows smaller value

#Q7C
 
