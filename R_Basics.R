## New Project Title
#Import Rallfun-v35
source(file.choose()) 
install.packages("WRS2")
install.packages("ggplot2")
install.packages("psych")
newDataset <- read.csv(file.choose(), header = TRUE)

#Function to complete steps related to normality check
checkNormality <- function(x){
  z <- shapiro.test(x)
  multi.hist(x)
  boxplot(x)
  qqnorm(x, main <- ("QQ plot"), pch=19)
  qqline(x)
  list <- list(z , describe(x))
  list
}

#Organize variables of relevant groupings into list format 
newDataset_var1var2 <- fac2list(newDataset[,9], newDataset[c(2,3)])

#Function to check normality of residuals from a linear regression where arg1 is the object defined by the linear model function lm()
checkResiduals <- function(x){
  lm.x.res <- residuals(lm.x)  													#Compute residuals of regression model
  checkNormality (lm.x.res)														#Check normality of residuals
  lm.x.pred <- predict(lm.x)													#Compute predicted values
  plot(x=lm.x.pred, y=lm.x.res, main="Predicted values vs. Residuals", pch=3) 	#Plot residuals against predicted values
  lines(lowess(x=lm.x.pred, y=lm.x.res), col="red")
  abline(h=0, col="blue")
}

#Compare two groups using percentile bootstrap method with 20% trimming
trimpb2(newDataset$variable, newDataset$variable2, tr=0.2, alpha=0.05, nboot=2000) 


ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
    geom_point(alpha = 0.1, aes(color = species_id))
