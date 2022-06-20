#setupset.seed(20873644)obs <- sample(1:413, 300, replace = FALSE)dataset <- real_estate2[obs,]library("moments")library("MASS")#Q1a#set variablex.i <- dataset$MRT_distance#five number summaryfivenum(x.i) #find IQR1426.70800-289.32480#find Range6396.28300-23.38284#find meanmean<-mean(x.i) mean#find standard deviationsd(x.i) #find skewnessskewness(x.i)#Q1cr <- 1/mean# plot relative frequency histogramhist(x.i,prob=T, main="Relative Frequency Histogram of MRT Distance", ylab="Relative Frequency", xlab="MRT_distance") curve(dexp(x,r),col="red",add=TRUE,lwd=2) # superimpose Exponential pdf#Q1d#Observed Frequency#define bins distbin <- c(0,200,400,600,800,1000,1200,Inf)distgroups <- cut(dataset$MRT_distance, breaks = distbin, right = FALSE)#gives intervals closed on the leftf <- table(distgroups)f#Expected Frequencye <- 300*c(pexp(200, r), pexp(400, r)-pexp(200, r), pexp(600, r)-pexp(400, r), pexp(800, r)-pexp(600, r),pexp(1000, r)-pexp(800, r),pexp(1200, r)-pexp(1000, r),1-pexp(1200, r))e#Q1e#find lambdalambda <- 2*(50*log(50/50.47)+75*log(75/41.98)+50*log(50/34.92)+16*log(16/29.04)+13*log(13/24.16)+8*log(8/20.09)+88*log(88/99.35))lambda#find p-value1-pchisq(lambda,5)


#Q2a
plot(dataset$age, dataset$price,
     main="Scatterplot for House Age vs Price",
     xlab="House Age (year)",
     ylab="Price Per Unit Area",
     pch=19,
     col="darkblue",
     cex.axis=1.25,
     cex.lab=1
)


#Q2b
cor(dataset$age, dataset$price)


#Q2c
pricebin <- c(0,20,40,Inf)
pricegroups <- cut(dataset$price, breaks = pricebin, right = FALSE)
agebin <- c(0,15,30,Inf)
agegroups <- cut(dataset$age, breaks = agebin, right = FALSE)

#find the observed frequencies of the intersections
observed <- table(agegroups, pricegroups)
observed


#Q2d
#find the expected frequencies of the intersections
f<-matrix(c(7,51,79,11,47,36,8,34,27),ncol=3,byrow=TRUE)  # matrix of observed frequencies
row<-margin.table(f,1)     # row totals
col<-margin.table(f,2)     # column totals
e<-outer(row,col)/sum(f)   # matrix of expected frequencies
e


#Q2e
#find lambda
lambda <- 2*(7*log(7/11.873)+51*log(51/60.28)+79*log(79/64.847)+11*log(11/8.146)+47*log(47/41.36)+36*log(36/44.493)+8*log(8/5.98)+34*log(34/30.36)+27*log(27/32.66))
lambda

#find p-value
1-pchisq(lambda, 4)


#Q3a
plot(dataset$MRT_distance, dataset$price,
     main="Scatterplot for Distance to Nearest Metro Station vs Price",
     xlab="Distance to Nearest Metro Station (m)",
     ylab="Price Per Unit Area",
     pch=19,
     col="darkblue",
     cex.axis=1.25,
     cex.lab=1
)

#find correlation coefficient
cor(dataset$MRT_distance, dataset$price)


#Q3b
#naming variables
x <- dataset$MRT_distance
y <- dataset$price

#number of complete observations
n <- 300

# run regression y = alpha+beta*x
RegModel<-lm(y~x)

summary(RegModel)

#find the maximum likelihood estimate of the intercept
alphahat<-RegModel$coefficients[1] 
alphahat

#find the maximum likelihood estimate of the slope
betahat<-RegModel$coefficients[2]
betahat

#find the unbiased estimate of alpha
se<-summary(RegModel)$sigma
se


#Q3c
#find test statistic
d <- abs(-0.0070653-0)/0.0004132
d

#find p-value
2*(1-pt(d, 298))


#Q3d
# 90% Confidence interval for slope
confint(RegModel,level=0.90)


#Q3e
predict(lm(y~x),data.frame("x"=1000),interval="confidence",lev=0.90)


#Q3f
predict(lm(y~x),data.frame("x"=1000),interval="prediction",lev=0.90)


#Q3g
#plot scatterplot
plot(x, y,
	main = "Scatterplot for Distance to Nearest Metro Station vs Price of House",  
	pch=19,
	xlab = "Distance to Nearest Metro Station (m)", 
	ylab = "Price per Unit Area",
	col = "blue"
)

#add line of best fit
abline(lm(y~x), col="red")

#Plot of the standardized residuals versus the explanatory variate
se<-summary(RegModel)$sigma
r<- RegModel$residuals # residuals
rstar <- r/se # standardized residuals
plot(x,rstar,xlab="Distance to Nearest Metro Station (m)",ylab="Standardized Residual")
title(main="Residual vs Distance to Nearest Metro Station")
abline(0,0,col="red",lwd=1.5)

#Plot of the standardized residuals versus the fitted values
muhat<-RegModel$fitted.values # fitted responses
plot(muhat,rstar,xlab="Muhat",ylab="Standardized Residual")
abline(0,0,col="red",lwd=1.5)
title(main="Residual vs Fitted Model")

qqnorm(rstar,main="")
title(main="Qqplot of Residuals")
qqline(rstar,lwd="3", 
       col="darkred")


#Q4a
plot(dataset$stores, dataset$price,
     main="Scatterplot for Number of Nearby Stores vs Price",
     xlab="Number of Nearby Stores",
     ylab="Price Per Unit Area",
     pch=19,
     col="darkblue",
     cex.axis=1.25,
     cex.lab=1
)

#find correlation coefficient
cor(dataset$stores, dataset$price)

#naming variables
x <- dataset$stores
y <- dataset$price

#number of complete observations
n <- 300

# run regression y = alpha+beta*x
RegModel<-lm(y~x)

summary(RegModel)

#find the maximum likelihood estimate of the intercept
alphahat<-RegModel$coefficients[1] 
alphahat

#find the maximum likelihood estimate of the slope
betahat<-RegModel$coefficients[2]
betahat

#find the unbiased estimate of alpha
se<-summary(RegModel)$sigma
se


#Q4c
#find test statistic
d <- abs(2.656-0)/0.196
d

#find p-value
2*(1-pt(d, 298))


#Q4d
#plot scatterplot
plot(x, y,
	main = "Scatterplot for Number of Nearby Store vs Price",  
	pch=19,
	xlab = "Number of Nearby Store", 
	ylab = "Price per Unit Area",
	col = "blue"
)

#add line of best fit
abline(lm(y~x), col="red")

#Plot of the standardized residuals versus the explanatory variate
se<-summary(RegModel)$sigma
r<- RegModel$residuals # residuals
rstar <- r/se # standardized residuals
plot(x,rstar,xlab="Number of Nearby Store",ylab="Standardized Residual")
title(main="Residual vs Number of Nearby Store")
abline(0,0,col="red",lwd=1.5)

#Plot of the standardized residuals versus the fitted values
muhat<-RegModel$fitted.values # fitted responses
plot(muhat,rstar,xlab="Muhat",ylab="Standardized Residual")
abline(0,0,col="red",lwd=1.5)
title(main="Residual vs Fitted Model")

#qqplot of the standard residuals
qqnorm(rstar,main="")
title(main="Qqplot of Residuals")
qqline(rstar,lwd="3", 
       col="darkred")