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

#test line