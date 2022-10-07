library('tidycensus')
df <- get_acs(
  geography = "tract",
  variables = c('DP05_0001E','DP05_0018E','DP03_0062E','DP02_0065PE','DP03_0096PE','DP03_0128PE','DP04_0047PE'),
  year=2019,
  state = "IL",
  county = "Cook",
  geometry = TRUE,
  output="wide"
)

df<- df[grep("M$",names(df),invert=TRUE)]
colnames(df) <- c('geoid','name','totpop','medage','medhhinc','propbac','propcov','proppov','proprent','geometry')

#heat map
ggplot() + 
geom_sf(data = df, aes(fill = propbac), show.legend = TRUE)
+ ggtitle("Heatmap of Baccalaureate Attainment Rate in the Cook County")
+ scale_fill_viridis_c(option="B", direction = -1)


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


