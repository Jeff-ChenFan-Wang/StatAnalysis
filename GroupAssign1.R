library('tidycensus')
library('tidyverse')

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

#heat map
ggplot() +
  geom_sf(data = df, aes(fill = propbac), show.legend = TRUE) +
  ggtitle(label = "Heatmap of Baccalaureate Attainment Rate in the Cook County") +
  scale_fill_viridis_c(name = 'Proportion', option="B", direction = -1)


linAlg <- lm('propbac ~ medhhinc',data=df)
summary(linAlg)

plot(df$medhhinc,
     df$propbac,
     main="Relationship Between Median Household Income and Baccalaureate Attainment Rates",
     xlab="Median Household Income",
     ylab="Baccalaureate Attainment Rates",
     pch=19, 
     col="blue",
     cex=1.2) 
abline(linAlg,
       lty=2, 
       lwd=2, 
       col="darkgray")
grid()

summary(linAlg)

##### Week 2 #####

# Finding Normality, Serial Correlation, heteroskedasticity
# Normality
hist(linAlg$residuals)
plot(linAlg$residuals)
ks.test(linAlg$residuals, pnorm) #test and plots show that the data is not normal

#Serial Correlation, need to discuss during meeting
dwtest(linAlg) # There is serial correlation we will reject Null hypothesis, shows serial corr..
plot(acf(linAlg$residuals, type = "correlation")) # does not show serial corr
plot(df$propbac[-1319], df$propbac[-1]) #lag and current values plot - does not show serial corr

#Heteroskedasticity
plot(linAlg) #look for residual vs Fitted plot
bptest(linAlg) #shows hetero..

#Step 7 Simulating Data
sim_medhhinc = replicate(10000,sample(df$medhhinc,nrow(df),replace=TRUE))

# Step 8 - Distribution of correlation
attach(df)
CorSample = apply(sim_medhhinc, MARGIN = 2, FUN = cor, propbac, 'complete.obs')
detach(df)
plot(density(CorSample), col = "blue",
     main = "The distribution of correlations",
     xlab = "Correlation",lty = 1, lwd = 1.5)


### Week 3 ###
# Step 10
inter = linAlg$coefficients['(Intercept)']
slope = linAlg$coefficients['medhhinc']
slope_test = seq(0, 10*slope, 0.01*slope)
SSE_Opt = sum(linAlg$residuals^2)
SSE = sapply(slope_test, function(x) sum((x*df$medhhinc + inter - df$propbac)^2, na.rm = TRUE))
plot(slope_test, SSE, type = 'l', col = 'blue',
     main = 'Changes in MSE for different slopes on median household income')
points(x = slope, y = SSE_Opt, col = 'red', pch = 16)
abline(v = slope, col = 'red', lty = 2)

# Step 11
inter_test = seq(0, 5*inter, 0.01*inter)
data = data.frame(cbind(df$medhhinc, df$propbac)) %>%
  rename(X = X1, Y = X2) %>% drop_na()
log_likelihood = function(inter, slope. = slope, x = data$X, y = data$Y){
  SSE = sum((y - slope.*x - inter)^2); n = length(y)
  sigma_sq = SSE/(n - 2)
  return(-(log(sigma_sq)/2 + log(2*pi)/2)*n - SSE/(2*sigma_sq))
}
log_likeli = sapply(inter_test, log_likelihood)
plot(inter_test, log_likeli, type = 'l', col = 'blue',
     main = 'Changes in log-likelihood for different intercepts')
points(x = inter, y = log_likelihood(inter), col = 'red', pch = 16)
abline(v = inter, col = 'red', lty = 2)

# Step 12
sum(df$totpop*df$propbac/100, na.rm = TRUE)
df_new = df %>% drop_na(medhhinc, propbac)
df_new[['medhhinc_policy']] = df_new[['medhhinc']]
medhhinc_sort = sort(df_new[['medhhinc']], index.return = TRUE)$ix
df_new[['medhhinc_policy']][medhhinc_sort[1:50]] = df_new[['medhhinc_policy']][medhhinc_sort[1:50]] + 10000
df_new[['medhhinc_policy']][rev(medhhinc_sort)[1:50]] = df_new[['medhhinc_policy']][rev(medhhinc_sort)[1:50]] - 10000
df_new[['propbac_pre']] = slope*df_new[['medhhinc_policy']] + inter
sum(df_new$totpop*df_new$propbac_pre/100)
ggplot() +
  geom_sf(data = df_new, aes(fill = propbac_pre), show.legend = TRUE) +
  ggtitle(label = "Heatmap of Predicted Baccalaureate Attainment Rate") +
  scale_fill_viridis_c(name = 'Proportion', option="B", direction = -1)

