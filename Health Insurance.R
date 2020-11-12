#install.packages("psych")
#install.packages("knitr")

setwd("input")

library(tidyverse)
library(corrplot)
require(rio)
library(ggplot2)
library(dplyr)
library(cowplot)
library(gridExtra)
library(psych)
library(knitr)
library(caret)
library(lattice)

insu <-import("insurance.csv")
attach(insu)
head(insu)
str(insu)
summary(insu)
any(is.na(insu))

# Correlation between expenses and Age / BMI
x <- ggplot(insu, aes(age, expenses)) +
  geom_jitter(color = "blue", alpha = 0.5) +
  theme_light()

y <- ggplot(insu, aes(bmi, expenses)) +
  geom_jitter(color = "green", alpha = 0.5) +
  theme_light()

p <- plot_grid(x, y) 
title <- ggdraw() + draw_label("1. Correlation between expenses and Age / BMI", 
                               fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

# Correlation between expenses and Sex / Children covered by insuurance
x <- ggplot(insu, aes(sex, expenses)) +
  geom_jitter(aes(color = sex), alpha = 0.7) +
  theme_light()

y <- ggplot(insu, aes(children, expenses)) +
  geom_jitter(aes(color = children), alpha = 0.7) +
  theme_light()

p <- plot_grid(x, y) 
title <- ggdraw() + draw_label("2. Correlation between expenses and Sex / Children covered by insuurance", 
                               fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

# Correlation between expenses and Smoker / Region
x <- ggplot(insu, aes(smoker, expenses)) +
  geom_jitter(aes(color = smoker), alpha = 0.7) +
  theme_light()

y <- ggplot(insu, aes(region, expenses)) +
  geom_jitter(aes(color = region), alpha = 0.7) +
  theme_light()

p <- plot_grid(x, y) 
title <- ggdraw() + draw_label("3. Correlation between expenses and Smoker / Region", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

cor <- cor(select(insu,age,bmi,children, expenses))
cor
corrplot(cor,method = "number", type = "upper")

dmy <- dummyVars("~.", data = insu)
insuurance <- data.frame  (predict(dmy, newdata=insu))
crr <- cor(insuurance)
corrplot(crr, method = "number", type = "upper")

pairs(insu[c("age","bmi","children","expenses")])

# Boxplot of insuurance expenses with 
boxplot(insu$expenses, col = "light blue",
        main = "insuurance Expenses",
        border = "brown")

# Histogram and density plot of expenses
d <- density(insu$expenses)
xlim <- range(d$x)
ylim <- range(d$y)
hist(insu$expenses, probability = TRUE, xlim = xlim, ylim = ylim, xlab="Expenses", 
     main="Histogram & density plot of Expenses", col = "light grey")
lines(d,col="red")

table(insu$region)

# Linear Regression Model
# Preparation and splitting the data
n_train <- round(0.8 * nrow(insu))
train <- sample(1:nrow(insu), n_train)
x_train <- insu[train, ]
x_test <- insu[-train, ]

model_0 <- lm(formula = expenses~., data = x_train)
summary(model_0)
#Saving R-squared
r_sq_0 <- summary(model_0)$r.squared
#predict data on test set
prediction_0 <- predict(model_0, newdata = x_test)
#calculating the residuals
residuals_0 <- x_test$expenses - prediction_0
#calculating Root Mean Squared Error
rmse_0 <- sqrt(mean(residuals_0^2))

# Train and Test New Model
model_1 <- lm(formula = expenses ~. - sex, data = x_train)
summary(model_1)

r_sq_1 <- summary(model_1)$r.squared
prediction_1 <- predict(model_1, newdata = x_test)
residuals_1 <- x_test$expenses - prediction_1
rmse_1 <- sqrt(mean(residuals_1^2))

# Compare the models
print(paste0("R-squared for first model:", round(r_sq_0, 4)))
print(paste0("R-squared for new model: ", round(r_sq_1, 4)))
print(paste0("RMSE for first model: ", round(rmse_0, 2)))
print(paste0("RMSE for new model: ", round(rmse_1, 2)))

# Prediction
predicted <- predict(object=model_1,newdata=x_test, type = "response")
result.df <- data.frame(cbind(actuals = x_test$expenses, predicted=predicted))
result.df <- result.df %>%
  mutate(error = result.df$actuals - result.df$predicted )%>%
  round(.,2)
result.df <- result.df %>%
  mutate(error_percent = paste0(round(result.df$error/result.df$actuals*100,2),"%"))
kable(head(result.df))
sprintf("The Average percent error is: %s%%", 
        round(mean(result.df$error/result.df$actuals*100),2))
# Plot the model
x_test$prediction <- predict(model_1, newdata = x_test)
ggplot(x_test, aes(x = prediction, y = expenses)) + 
  geom_point(color = "blue", alpha = 0.7) + 
  geom_abline(color = "red") +
  ggtitle("Prediction vs. Real values")

x_test$residuals <- x_test$expenses - x_test$prediction

ggplot(data = x_test, aes(x = prediction, y = residuals)) +
  geom_pointrange(aes(ymin = 0, ymax = residuals), color = "blue", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = 3, color = "red") +
  ggtitle("Residuals vs. Linear model prediction")

ggplot(x_test, aes(x = residuals)) + 
  geom_histogram(bins = 15, fill = "blue") +
  ggtitle("Histogram of residuals")

# Example apply on new data
Joe <- data.frame(age = 34,
                  sex= "male",
                  bmi = 26.5,
                  children = 0,
                  smoker = "yes",
                  region = "northwest")
print(paste0("Health care charges for Joe: ", round(predict(model_1, Joe), 2)))
Kay <- data.frame(age = 30,
                  sex = "female",
                  bmi = 31.2,
                  children = 0,
                  smoker = "no",
                  region = "northeast")
print(paste0("Health care charges for Kay: ", round(predict(model_1, Kay), 2)))  

#making copy of data frame to modify for data mining 

imining <- insurance
imining
attach(imining)
#changing expense from numerical to categorical variable "expense level"

#defining categorical levels based quartile distribution
# L1 <- (below 3000) L2 <- (3000 to 6000) L3 <- (6000 to 9000) L4<- (9000 to 12000)
# L5 <- (12000 to 15000) L6 <- (15000 to 18000) 
# disolving pattern since above 3rd quartile now
# L7 <- (18000 to 36000) L8 <- (36000 to 72000) above max L9 <- (72000 and above)
imining$expenses <- cut(imining$expenses, 
                        breaks = c(0, 3000, 6000, 9000, 12000, 15000, 18000, 36000,72000, 100000), 
                        labels = c("L1","L2","L3", "L4", "L5", "L6", "L7", "L8", "L9"), right = F)
str(imining)


t <- table(imining$expenses)

print(t)



library(party)
install.packages("caret")
library(caret)

set.seed(123)
ind <- sample(2, nrow(imining), replace=TRUE, prob = c(0.70,0.30))
train.data <- imining[ind==1,]
test.data <- imining[ind==2,]

pie(table(train.data$expenses), levels(train.data$expenses), main = "Distribution of class in Train Set")

expen.form <- expenses ~ .  
#expen.form <- age+sexmale+smokeryes+bmi+children+regionnortheast+regionnorthwest+regionsoutheast+regionsouthwest
expen_ctree <- ctree(expen.form, data = train.data)
plot(expen_ctree)

train.predict <- predict(expen_ctree)
confusionMatrix(train.predict, train.data$expenses)


#testing model with test set
test.predict <- predict(expen_ctree, newdata= test.data)
confusionMatrix(test.predict,test.data$expenses)

plot(test.predict,test.data$expenses, col=test.data$expenses, main = "Prediction vs Observed")

low.expen <- filter(test.data,test.data$expenses %in% c("L1","L2","L3","L4") | smokeryes == "1")
plot(low.expen[c("age","bmi")], col=low.expen$expenses)

high.expen <- filter(test.data,test.data$expenses %in% c("L5","L6","L7","L8","L9")| smokeryes == "1")
plot(high.expen[c("age","bmi")], col=high.expen$expenses, col.axis= 43)

x1<- ggplot(low.expen, aes(age, bmi)) +
  geom_jitter(color = "blue", alpha = 0.5) +
  theme_light()

y1 <- ggplot(high.expen, aes(age,bmi )) +
  geom_jitter(color = "green", alpha = 0.5) +
  theme_light()

p1 <- plot_grid(x1, y1) 
title <- ggdraw() + draw_label("Correlation between Age and bmi of smokers in different expense level", 
                               fontface='bold')
plot_grid(title, p1, ncol=1, rel_heights=c(0.1, 1))




