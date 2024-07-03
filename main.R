#-----------------------------------------------------------------
# IMPORT DATA AND SET UP
#-----------------------------------------------------------------
data <- read.csv("C:/Users/User/Desktop/Linear Regression E-commerce Customer Device Usage - R/data/Ecommerce Customers")
View(data)

str(data)
summary(data)


#-----------------------------------------------------------------
# CREATE PLOTS AND SEARCH FORM INSIGHTS
#-----------------------------------------------------------------

install.packages("ggplot2")

library(ggplot2)

# CORRELATION BETWEEN TIME ON WEBSITE AND YEARLY AMOUNT SPENT ?
ggplot(data, aes(x=Time.on.Website, y = Yearly.Amount.Spent)) +
  geom_point(color="orange") +
  ggtitle("Time on website against Yearly amount spent") +
  xlab("Time on website") +
  ylab("Yearly amount spent")

# AVG SESSION LENGTH VS YEARLY AMOUNT SPENT
ggplot(data, aes(x=Avg..Session.Length, y = Yearly.Amount.Spent)) +
  geom_point(color="orange") +
  ggtitle("Session Length against Yearly amount spent") +
  xlab("Session Length") +
  ylab("Yearly amount spent")

## PAIRPLOT OF ALL CONTINUOUS

pairs(data[c("Avg..Session.Length",
             "Time.on.App",
             "Time.on.Website",
             "Length.of.Membership",
             "Yearly.Amount.Spent"
             )],
      col="orange",
      pch=16,
      main = "Pairplot of all continuous variables"
      )


#-----------------------------------------------------------------
# EXPLORE THE SELECTED VARIABLES
#-----------------------------------------------------------------


# is the variable normally distributed

hist(data$Length.of.Membership)
ggplot(data, aes(x=Length.of.Membership)) +
  geom_histogram(
    color="white",
    fill="orange",
    binwidth = 0.5
  )

boxplot(data$Length.of.Membership)
ggplot(data, aes(x=Length.of.Membership)) +
  geom_boxplot(fill="orange")


#-----------------------------------------------------------------
# FITTING A LINEAR MODEL
#-----------------------------------------------------------------

attach(data)
lm.fit1 <- lm(Yearly.Amount.Spent~Length.of.Membership)

summary(lm.fit1)
plot(Yearly.Amount.Spent~Length.of.Membership)
abline(lm.fit1, col="red")

#-----------------------------------------------------------------
# RESIDUALS ANALYSIS
#-----------------------------------------------------------------

hist(residuals(lm.fit1))

qqnorm(residuals(lm.fit1))
qqline(residuals(lm.fit1), col="red")

shapiro.test(residuals(lm.fit1))


#-----------------------------------------------------------------
# EVALUATION OF THE MODEL
#-----------------------------------------------------------------

set.seed(1)
row.number <- sample(1:nrow(data), 0.8*nrow(data))


train <- data[row.number,]
test <- data[-row.number,]



# estimate the linear fit with the training set

lm.fit0.8 <- lm(Yearly.Amount.Spent~Length.of.Membership, data = train)
summary(lm.fit0.8)

# predict in the test dataset

prediction0.8 <- predict(lm.fit0.8, newdata = test)

err0.8 <- prediction0.8 - test$Yearly.Amount.Spent

# root mean square error
rmse <- sqrt(mean(err0.8^2))
# mean absolute percentage error
mape <- mean(abs(err0.8/test$Yearly.Amount.Spent))

c(RMSE= rmse, mape=mape, R2=summary(lm.fit0.8)$r.squared)



#-----------------------------------------------------------------
# MULTIPLE REGRESSION
#-----------------------------------------------------------------


attach(data)
lm.fit <- lm(Yearly.Amount.Spent~Avg..Session.Length + Time.on.App + Time.on.Website + Length.of.Membership)

summary(lm.fit)



#-----------------------------------------------------------------
# EVALUATION OF THE MULTIPLE REGRESSION
#-----------------------------------------------------------------

set.seed(1)
row.number <- sample(1:nrow(data), 0.8*nrow(data))


train <- data[row.number,]
test <- data[-row.number,]



# estimate the linear fit with the training set

multi.lm.fit0.8 <- lm(Yearly.Amount.Spent~Avg..Session.Length + Time.on.App + Time.on.Website + Length.of.Membership,
                data = train)
summary(multi.lm.fit0.8)

# predict in the test dataset

prediction0.8 <- predict(multi.lm.fit0.8, newdata = test)

err0.8 <- prediction0.8 - test$Yearly.Amount.Spent

# root mean square error
rmse <- sqrt(mean(err0.8^2))
# mean absolute percentage error
mape <- mean(abs(err0.8/test$Yearly.Amount.Spent))

c(RMSE= rmse, mape=mape, R2=summary(lm.fit0.8)$r.squared)




