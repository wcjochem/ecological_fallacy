#
# Ecological Bias Demonstration (2 forms)
#
# Chris Jochem
# Feb 2014
#

library(ggplot2)

set.seed(123) # change for different results

# create simulation data of 100 obs. in 5 groups
# based on the example in Thomas, D. (2009:207-208)
df <- data.frame("c"=rep(c(1,2,3,4,5), each=20))
df$xci <- runif(100, df$c, df$c+5)
df$ac <- rnorm(100, 10-2*df$c, 0.5^2)
df$bc <- rnorm(100, 1, 0.1^2)

# the outcome association is linear
df$yci <- df$ac + df$bc*df$xci
head(df)

# take a look at the data, good separation of the groups
indiv.plot <- qplot(xci, yci, data=df, shape=factor(c), size=2,
                    xlab="Exposure", ylab="Outcome")
indiv.plot

# aggregate the data to calculate the group means
df.agg <- aggregate(cbind(yci, xci)~c, df, mean)

# overlay those points
group.plot <- indiv.plot + geom_point(data=df.agg, aes(xci, yci), col="red", size=4)
group.plot

# check the regressions for each group
indiv.plot

lm1 <- lm(yci ~ xci, data=df, subset=df$c==1)
lm2 <- lm(yci ~ xci, data=df, subset=df$c==2)
lm3 <- lm(yci ~ xci, data=df, subset=df$c==3)
lm4 <- lm(yci ~ xci, data=df, subset=df$c==4)
lm5 <- lm(yci ~ xci, data=df, subset=df$c==5)

lm.coef <- data.frame(rbind(coef(lm1), coef(lm2), coef(lm3), coef(lm4), coef(lm5)))
names(lm.coef) <- c("intercept", "slope")
lm.coef$c <- c(1:5)
lm.coef

# plot over the individual scatterplot
indiv.plot + stat_smooth(method="lm", se=FALSE, col="black", linetype="dashed", size=1)
# indiv.plot + geom_abline(aes(intercept=intercept, slope=slope), data=lm.coef)

lm.grp <- lm(yci ~ xci, data=df.agg)
summary(lm.grp)
lm.grp.coef <- data.frame(rbind(coef(lm.grp)))
names(lm.grp.coef) <- c("intercept", "slope")

group.plot + geom_abline(aes(intercept=intercept, slope=slope), data=lm.grp.coef, col="red", size=1.2)

# all toegether now
indiv.plot + stat_smooth(method="lm", se=FALSE, col="black", linetype="dashed", size=1) + geom_point(data=df.agg, aes(xci, yci), col="red", size=4) + geom_abline(aes(intercept=intercept, slope=slop), data=lm.grp.coef, col="red", size=1.2) 

# what if we use all the data
# lm.all <- lm(yci ~ xci, data=df)
# summary(lm.all)
# lm.all.coef <- data.frame(rbind(coef(lm.all)))
# names(lm.all.coef) <- c("intercept", "slope")
# 
# indiv.plot + geom_abline(aes(intercept=intercept, slope=slope), data=lm.all.coef)



##### Example 2 #####
# http://www.r-bloggers.com/open-data-and-ecological-fallacy/
library(mnormt)

n=1000

# create some random data
r <- -.5
Z <- rmnorm(n,c(0,0),matrix(c(1,r,r,1),2,2))
X <- Z[,1]
E <- Z[,2]
Y <- 3+2*X+E

cor(X,Y) # inividual points correlation
plot(X,Y)

I <- cut(Z[,2],qnorm(seq(0,1,by=.05))) # create groups
# calculate group means
Yg <- tapply(Y,I,mean) # calculate the mean of X and Y for each group
Xg <- tapply(X,I,mean)

cor(Xg,Yg) # correlation of the group means
points(Xg, Yg, col="Red", pch=16) # plot the group means

# plot the group regression
lm.g <- lm(Yg ~ Xg)
summary(lm.g)
points(Xg, Yg, col="Red", pch=16)
abline(lm.g, col="Red")
