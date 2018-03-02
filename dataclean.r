library(LearnBayes)
### Observed Data: Average Yearly Temperatures in New Haven over 60 years

setwd("~/Documents/2017fall/bayesian/project")
alldata <- read.delim("competition_extratime_train.txt", header = T, sep = "\t")

alldata$orderdate <- as.Date(alldata$orderdate)
v <- c("uid",
       "user_avgadvanceddate",
       "user_avgstar",
       "user_activation",
       "user_avgprice",
       "user_citynum",
       "user_avgdealprice",
       "user_ordnum_1month",
       "user_ordnum_3month")
df <- alldata[alldata$orderdate=="2017-06-06", which(names(alldata) %in% v)]
df <- unique(df)

df$user_avgdealprice <- as.numeric(df$user_avgdealprice)
df$user_activation <- as.numeric(df$user_activation)
df$user_ordnum_1month <- as.numeric(df$user_ordnum_1month)
df$user_ordnum_3month <- as.numeric(df$user_ordnum_3month)

# Delete ID
df <- df[, -which(names(df) %in% c("uid"))]

# Delete outliers
df <- df[-which(df$user_ordnum_1month == 81),]
df <- df[-which(df$user_avgadvanceddate < 0),]
df$user_citynum <- df$user_citynum - 200

# Check the distribution
par(mfrow=c(1,2))
hist(df$user_ordnum_1month, col = "gray")
hist(df$user_ordnum_3month, col = "gray")

# write.csv(df, "df.csv", row.names=FALSE)
df <- read.csv("df.csv", header = T)


# Dummy variable
advday1 = advday2 = advday3 = star1 = star2 = star3 = act1 = act2 = act3 = act4 = price1 = price2 = price3 = city1 = city2 = city3 <- rep(0, dim(df)[1])

attach(df)
advday1[which(user_avgadvanceddate < 2)] <- 1
advday2[which(user_avgadvanceddate >= 2 & user_avgadvanceddate < 5)] <- 1
advday3[which(user_avgadvanceddate >= 5)] <- 1
star1[which(user_avgstar < 7)] <- 1
star2[which(user_avgstar >= 7 & user_avgstar < 9)] <- 1
star3[which(user_avgstar >= 9)] <- 1
price1[which(user_avgdealprice < 10000)] <- 1
price2[which(user_avgdealprice >= 10000 & user_avgdealprice < 15000)] <- 1
price3[which(user_avgdealprice >= 15000)] <- 1
act1[which(user_activation < 2000)] <- 1
act2[which(user_activation >= 2000 & user_activation < 4000)] <- 1
act3[which(user_activation >= 4000 & user_activation < 6000)] <- 1
act4[which(user_activation >= 6000)] <- 1
city1[which(user_citynum <= 6)] <- 1
city2[which(user_citynum > 6 & user_citynum - 200 <= 20)] <- 1
city3[which(user_citynum > 20)] <- 1
detach(df)

dat <- cbind(advday1 , advday2 , advday3 , star1 , star2 , star3 , price1, price2, price3, act1 , act2 , act3 , act4, df$user_ordnum_3month, df$user_ordnum_1month)
dat <- as.data.frame(dat)
names(dat)[c(14,15)] <- c("y3","y1")

# write.csv(dat, "dat.csv", row.names=FALSE)
dat <- read.csv("dat.csv", header = T)
