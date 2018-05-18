# https://www.svm-tutorial.com/2014/10/support-vector-regression-r/

X<-c(1:20)
Y<-c(3,4,8,4,6,9,8,12,15,26,35,40,45,54,49,59,60,62,63,68)
data<-data.frame(X,Y)

# Plot the data
plot(data, pch=16)

# Create a linear regression model
model <- lm(Y ~ X, data)

# Add the fitted line
abline(model)
summary(model)

plot(data, pch=16)
model <- lm(Y ~ X , data)

# make a prediction for each X
predictedY <- predict(model, data)

# display the predictions
points(data$X, predictedY, col = "blue", pch=4)

rmse <- function(error) sqrt(mean(error^2))

error <- model$residuals  # same as data$Y - predictedY
predictionRMSE <- rmse(error)   # 5.703778

library(e1071) #install.packages("e1071")

model <- svm(Y ~ X , data)
predictedY <- predict(model, data)
points(data$X, predictedY, col = "red", pch=4)

error <- data$Y - predictedY
svrPredictionRMSE <- rmse(error)

# perform a grid search
tuneResult <- tune(svm, Y ~ X,  data = data, ranges = list(epsilon = seq(0,0.2,0.01), cost = 2^(2:9)))
print(tuneResult)
# Draw the tuning graph
plot(tuneResult)

tunedModel <- tuneResult$best.model
tunedModelY <- predict(tunedModel, data) 

error <- data$Y - tunedModelY  

# this value can be different on your computer
# because the tune method  randomly shuffles the data
tunedModelRMSE <- rmse(error)  # 2.219642  
points(data$X, tunedModelY, col = "red", pch=4)
str(tunedModel)

# https://www.kdnuggets.com/2017/03/building-regression-models-support-vector-regression.html

plot(data, pch=16)
points(data$X, predictedY, col = "blue", pch=3)
points(data$X, predictedY, col = "blue", pch=3, type="l")
points(data$X, tunedModelY, col = "red", pch=4)
points(data$X, tunedModelY, col = "red", pch=4, type="l")

plot(data$Y,predictedY, col = "blue", pch=3)
point(data$Y,tunedModelY,col = "red", pch=4)
