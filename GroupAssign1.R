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

# Propcov : proportion of insurance coverage
# Proppov : proportion below poverty level
# Proprent : proportion renting houses
#Q4 
#A: Totpop, MedAge
#B: Proprent
#C: medhhinc, proppov, propcov

#Q5
summary(GA2Model)
medhhincBoostReqNaive = 0.05/GA2Model$coefficients['medhhinc']
#Naive answer boost insurance coverage proportion (propcov) by ≈ 0.0887

simpleDf = st_drop_geometry(df[c('totpop','medage','medhhinc','propcov','proppov','proprent')])
corrMat = cor(simpleDf)

povHincLm = lm(proppov ~ medhhinc,data=df)
covHincLm = lm(propcov ~ medhhinc,data=df)
rentHincLm = lm(proprent ~ medhhinc,data=df)

endogMedhhincCoef = GA2Model$coefficients['medhhinc'] +
  povHincLm$coefficients['medhhinc']*GA2Model$coefficients['proppov'] + 
  covHincLm$coefficients['medhhinc']*GA2Model$coefficients['propcov'] +
  rentHincLm$coefficients['medhhinc']*GA2Model$coefficients['proprent']

medhhincBoostReqEndog = 0.05/endogMedhhincCoef


#q6a
Allstates <- unique(fips_codes$state)[1:51]
df2 <- map_df(Allstates, function(x) { 
  get_acs(geography = "tract", 
          year = 2019,
          variables = c('DP05_0001E','DP02_0065PE'),
          state = x,
          output = 'wide',
          key = 'e61b56441ee4ab32492482feed5b4d49fd550cea'
  )})

#renaming
df2<- df2[grep("M$",names(df2),invert=TRUE)]
colnames(df2) <- c('geoid','name','totpop','propbac','geometry')
head(df2)

#created a column IsCook and added Flag to it
df2$IsCook <- 0
df2$IsCook[grep('Cook County, Illinois', df2$name)] = 1

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
# 17031081403 GEO ID for NBC and GLEATCHER

#Q7A
predict(GA2Model,df[df$geoid=='17031081403',],interval='confidence',level=0.9) #point estimate
df[df$geoid=='17031081403',]['propbac'] #actual attainment falls below confidence interval

#Q7B
#adding weights
GA2Model2 <- lm(propbac ~ totpop + medage + medhhinc + propcov + proppov + proprent, data = df, weights = totpop)
predict(GA2Model2,df[df$geoid=='17031081403',],interval='confidence',level=0.9)

#Q7C
ga2summ = summary(GA2Model)
p7cIntercept = rnorm(
  10000,
  ga2summ$coefficients['(Intercept)','Estimate'],
  ga2summ$coefficients['(Intercept)','Std. Error']
)
p7ctotpop = rnorm(
  10000,
  ga2summ$coefficients['totpop','Estimate'],
  ga2summ$coefficients['totpop','Std. Error']
)
p7cmedage = rnorm(
  10000,
  ga2summ$coefficients['medage','Estimate'],
  ga2summ$coefficients['medage','Std. Error']
)
p7cmedhhinc = rnorm(
  10000,
  ga2summ$coefficients['medhhinc','Estimate'],
  ga2summ$coefficients['medhhinc','Std. Error']
)
p7cpropcov = rnorm(
  10000,
  ga2summ$coefficients['propcov','Estimate'],
  ga2summ$coefficients['propcov','Std. Error']
)
p7cproppov = rnorm(
  10000,
  ga2summ$coefficients['proppov','Estimate'],
  ga2summ$coefficients['proppov','Std. Error']
)
p7cproprent = rnorm(
  10000,
  ga2summ$coefficients['proprent','Estimate'],
  ga2summ$coefficients['proprent','Std. Error']
)
simBetaDf = data.frame(p7cIntercept,p7ctotpop,p7cmedage,p7cmedhhinc,p7cpropcov,p7cproppov,p7cproprent)

simBetaDf['prediction'] = simBetaDf$p7cIntercept+
  simBetaDf$p7ctotpop*df[df$geoid=='17031081403',]$totpop+
  simBetaDf$p7cmedage*df[df$geoid=='17031081403',]$medage+
  simBetaDf$p7cmedhhinc*df[df$geoid=='17031081403',]$medhhinc+
  simBetaDf$p7cpropcov*df[df$geoid=='17031081403',]$propcov+
  simBetaDf$p7cproppov*df[df$geoid=='17031081403',]$proppov+
  simBetaDf$p7cproprent*df[df$geoid=='17031081403',]$proprent

quantile(simBetaDf$prediction,c(.05,.95))

#8 

df<-within(
  df, 
  residQuartile <- as.integer(
    cut(
      GA2Model$residuals, 
      quantile(GA2Model$residuals, probs=0:4/4), 
      include.lowest=TRUE
    )
  )
)

df['residual'] = GA2Model$residuals

ggplot() +
  geom_sf(data = df, aes(fill = residQuartile), show.legend = TRUE) +
  labs(title = 'Tract Labeled By Residual Quartile', subtitle = 'Cook County, IL', caption = "From 2015 to 2019") + 
  scale_fill_viridis_c(name = 'Residual\nQuartile', option="B", direction = -1)+
  theme(legend.position = c(0.2,0.4))

ggplot() +
  geom_sf(data = df, aes(fill = residual), show.legend = TRUE) +
  labs(title = 'Tract Heatmap by Residual', subtitle = 'Cook County, IL', caption = "From 2015 to 2019") + 
  scale_fill_viridis_c(name = 'Residuals', option="B", direction = -1)+
  theme(legend.position = c(0.2,0.4))

df[df$geoid=='17031081403',]$residual
summary(df)

mean(df[df$residQuartile==1,]$propbac)

st_drop_geometry(df) %>% group_by(residQuartile)  %>%
  summarise(
    avg_totpop = median(totpop),
    avg_medage = median(medage),
    avg_medhhinc = median(medhhinc),
    avg_propbac = median(propbac),
    avg_propcov = median(propcov),
    avg_proppov = median(proppov),
    avg_proprent = median(proprent),
    
  )

residLm = lm(propbac ~ residual, data = df)
g1 = ggplot(df, mapping=aes(y = propbac, x=residual, color=as.factor(residQuartile))) +
  geom_point() +
  scale_fill_brewer() +
  guides(color = guide_legend(title = 'Residual\nQuartile'))+
  theme(legend.position = c(0.8,0.2))

# residLm = lm(propbac ~ residual, data = df)
g2 = ggplot(df, mapping=aes(y = medhhinc, x=residual, color=as.factor(residQuartile))) +
  geom_point() +
  scale_fill_brewer() +
  guides(color = guide_legend(title = 'Residual\nQuartile'))+
  theme(legend.position = c(0.8,0.8))

grid.arrange(g1,g2,nrow=1)


## Q9

# The 10 variables selected:
# 1. Median age: 'medage' ('DP05_0018E')
# 2. Proportion of bachelor's degree attainment: 'propbac' ('DP02_0065PE')
# 3. Proportion of health insurance coverage: 'propcov' ('DP03_0096PE')
# 4. Proportion of employment: 'propempl' ('DP03_0004PE')
# 5. Proportion of black people: 'propblack' ('DP05_0038PE')
# 6. Fertility rate (Number of women 15 to 50 years old who had a birth
#    in the past 12 months): 'fertrate' ('DP02PR_0037PE')
# 7. Proportion of school enrollment: 'propsch' ('DP02_0053PE')
# 8. Proportion of households with a computer: 'propcomp' ('DP02_0152PE')
# 9. Total housing units: 'tolhouse' ('DP04_0001E')
# 10. Proportion of civilians with disability: 'propdisa' ('DP02_0072PE')

Variables = c('DP03_0128PE', 'DP05_0018E', 'DP02_0065PE', 'DP03_0096PE',
              'DP03_0004PE', 'DP05_0038PE', 'DP02_0037PE', 'DP02_0053PE',
              'DP02_0152PE', 'DP04_0001E', 'DP02_0072PE')

df3 <- get_acs(
  geography = "tract", variables = Variables,
  year = 2019, state = Allstates, output="wide",
  key = 'e61b56441ee4ab32492482feed5b4d49fd550cea'
)

df3 <- df3[grep("M$", names(df3), invert = TRUE)]
col_names = c('geoid','name', 'proppov', 'medage', 'propbac',
              'propcov', 'propempl', 'propblack','fertrate',
              'propsch', 'propcomp', 'tolhouse', 'propdisa')
colnames(df3) <- col_names
head(df3)
summary(df3)

# Q9 (a)
# The pattern of missing values
require(VIM)
plot(aggr(df3, plot = FALSE), prop = TRUE, numbers = FALSE)

# Filter out the missing values and useless variables
df3 <- (df3 %>% filter(complete.cases(df3)))[col_names[3:length(col_names)]]

# Q9 (b)
require(leaps)
Bestlm <- regsubsets(
  x = proppov~., data = df3,
  nvmax = 11, method = "exhaustive")
summary(Bestlm)$which
summary(Bestlm)$adjr2

lm_empty = lm(proppov ~ 1, data = df3)
lm_full = lm(proppov ~ ., data = df3)
step(lm_full, direction = 'backward')
step(lm_empty, scope = formula(lm_full), direction = 'forward')

Best_lm =  lm(formula = proppov ~ medage + propbac + propcov + propempl + 
propblack + fertrate + propcomp + tolhouse + propdisa, data = df3)

dwtest(Best_lm, alternative = "two.sided")
bptest(Best_lm)
ks.test(Best_lm$residuals/summary(Best_lm)$sigma, pnorm)

# Q9 (C)

RMSE = summary(Best_lm)$sigma
AdjR2 = summary(Best_lm)$adj.r.squared

## Q10

# 1. Transformation on response

# Use root function to transform the response
# RMSE can be reduced from 6.92 to 6.81.
lm_full =  lm(formula = sqrt(proppov) ~ medage + propbac + propcov + propempl + 
  propblack + fertrate + propcomp + tolhouse + propdisa, data = df3)
step(lm_full, direction = 'backward')

lm_trans1 =  lm(formula = sqrt(proppov) ~ medage + propbac + propcov + propempl + 
  propblack + fertrate + propcomp + tolhouse + propdisa, data = df3)

RMSE = sqrt(sum(((lm_trans1$fitted.values)^2 - df3['proppov'])^2)/nrow(df3))
AdjR2_1 = summary(lm_trans1)$adj.r.squared

dwtest(lm_trans1, alternative = "two.sided")
bptest(lm_trans1)
ks.test(lm_trans1$residuals/summary(lm_trans1)$sigma, pnorm)

# 2. Transformation on explanatory variables

# - Try to improve the linear relationship between the response
# - covariates as much as possible

attach(df3)
plot(proppov)
plot(medage, sqrt(proppov), col = 'deepskyblue')
plot(propbac, sqrt(proppov), col = 'deepskyblue')
plot(propcov, sqrt(proppov), col = 'deepskyblue')
plot(propempl, sqrt(proppov), col = 'deepskyblue')
plot(propblack, sqrt(proppov), col = 'deepskyblue')
plot(fertrate, sqrt(proppov), col = 'deepskyblue')
plot(propcomp, sqrt(proppov), col = 'deepskyblue')
plot(tolhouse, sqrt(proppov), col = 'deepskyblue')
plot(propdisa, sqrt(proppov), col = 'deepskyblue')
plot(propsch, sqrt(proppov), col = 'deepskyblue')
detach(df3)

# (1) Step 1 - Transformation on 'tolhouse'

plot(log(tolhouse), sqrt(proppov), col = 'deepskyblue')

# Use log transformation on 'tolhouse'
# Adjusted R^2 is improved from 0.6542 to 0.6587
lm_full =  lm(formula = sqrt(proppov) ~ medage + propbac + propcov + propempl + 
  propblack + fertrate + propcomp + log(tolhouse) + propdisa + propsch, data = df3)
step(lm_full, direction = 'backward')

dwtest(lm_full, alternative = "two.sided")
bptest(lm_full)
ks.test(lm_full$residuals/summary(lm_full)$sigma, pnorm)

lm_trans2 = lm(formula = sqrt(proppov) ~ medage + propbac + propcov + propempl + 
  propblack + fertrate + propcomp + log(tolhouse) + propdisa + propsch, data = df3)
summary(lm_trans2)
AdjR2_2 = summary(lm_trans2)$adj.r.squared

dwtest(lm_trans2, alternative = "two.sided")
bptest(lm_trans2)
ks.test(lm_trans2$residuals/summary(lm_trans2)$sigma, pnorm)

# (2) Step 2 - Transformation on 'propsch'

plot(log(propsch), sqrt(proppov), col = 'deepskyblue')

# Still use log transformation on 'propsch'
# This operation will make the scale of all the explanatory variables comparable
# Adjusted R^2 is improved from 0.6587 to 0.6693
lm_full =  lm(formula = sqrt(proppov) ~ medage + propbac + propcov + propempl +
  propblack + fertrate + propcomp + log(tolhouse) + propdisa + log(propsch),
  data = df3, subset = (df3$propsch != 0))
step(lm_full, direction = 'backward')

lm_trans3 = lm(formula = sqrt(proppov) ~ medage + propbac + propcov + propempl +
  propblack + fertrate + propcomp + log(tolhouse) + propdisa + log(propsch),
  data = filter(df3, propsch != 0))
summary(lm_trans3)
AdjR2_3 = summary(lm_trans3)$adj.r.squared

dwtest(lm_trans3, alternative = "two.sided")
bptest(lm_trans3)
ks.test(lm_trans3$residuals/summary(lm_trans3)$sigma, pnorm)

# (3) Step 3 - Transformation on 'fertrate'

summary(df3$fertrate)

# The linear relationship between 'fertrate' and the response 'sqrt(proppov)' is bad
# Use step functions derived from continuous predictors this time
# Discretize the variable 'fertrate' into a series of levels as its distribution is typically pyramid-shaped
# The ajusted R^2 can be improved to 0.6698
df3$fertrate_grp = NA
floors = c(seq(0, 200, 10), Inf)
Levels = length(floors) - 1
for (k in c(1:Levels)){
  df3[(df3$fertrate >= floors[k])&(df3$fertrate < floors[k+1]), 'fertrate_grp'] = paste('LV', k)
}
df3$fertrate_grp = factor(df3$fertrate_grp, levels = paste('LV', 1:Levels))

lm_full = lm(formula = sqrt(proppov) ~ medage + propbac + propcov + propempl +
  propblack + fertrate_grp + propcomp + log(tolhouse) + propdisa + log(propsch),
  data = filter(df3, propsch != 0))
step(lm_full, direction = 'backward')

lm_trans4 = lm(formula = sqrt(proppov) ~ medage + propbac + propcov + propempl +
  propblack + fertrate_grp + propcomp + log(tolhouse) + propdisa + log(propsch),
  data = filter(df3, propsch != 0))

summary(lm_trans4)
AdjR2_4 = summary(lm_trans4)$adj.r.squared
dwtest(lm_trans4, alternative = "two.sided")
bptest(lm_trans4)
ks.test(lm_trans4$residuals/summary(lm_trans4)$sigma, pnorm)

# (4) Step 4 - Transformation on 'propcov'

plot((propcov/100)^4, sqrt(proppov), col = 'deepskyblue')

# The linear relationship between 'propcov' and the response is also bad and the distribution of 'propcov' is concentrated on [80, 100]
# To improve the linearity, use a quartic function to transform 'propcov' this time
# We should divide 'propcov' by 100 to standardize it firstly to control its scale after transformation
# The ajusted R^2 is improved to 0.6714 now
lm_full = lm(formula = sqrt(proppov) ~ medage + propbac + I((propcov/100)^4) +
  propempl +propblack + fertrate_grp + propcomp + log(tolhouse) + propdisa +
  log(propsch), data = filter(df3, propsch != 0))
step(lm_full, direction = 'backward')
lm_trans5 = lm(formula = sqrt(proppov) ~ medage + propbac + I((propcov/100)^4) +
  propempl +propblack + fertrate_grp + propcomp + log(tolhouse) + propdisa +
  log(propsch), data = filter(df3, propsch != 0))

summary(lm_trans5)
AdjR2_5 = summary(lm_trans5)$adj.r.squared
dwtest(lm_trans5, alternative = "two.sided")
bptest(lm_trans5)
ks.test(lm_trans5$residuals/summary(lm_trans5)$sigma, pnorm)

# Although the above statistical tests are still significant, from the plots below,
# we can see compared with the original model in Question 9, the model after reform ('lm_trans5')
# is much better on OLS residual assumptions
plot(lm_trans5)
