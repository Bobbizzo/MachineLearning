#Robert Agueros

library(mlbench)
library(caret)
library(corrplot)
library(AppliedPredictiveModeling)
library(psych)
library(earth)
library(tidyverse)

# The following is built-in data set that comes along with the package.
data("BostonHousing")
View(BostonHousing)


############################################################Test!
# Lets learn a trick about extracting any built-in data set.
install.packages("xlsx")
library(xlsx)
write.xlsx(BostonHousing, file = "Group1.xlsx")

#Check for missing values (N/A's)
sum(is.na(BostonHousing))

#Check the structure of BostonHousing
str(BostonHousing)

#summarize data
summary(BostonHousing)

#look at first six rows of BostonHousing
head(BostonHousing)

#set the seed
set.seed (100)

#perform stratified random split of the data
TrainingIndex<- createDataPartition(BostonHousing$medv, p=.75, list = FALSE)
TrainingSet <- BostonHousing[TrainingIndex,] #Training Set
TestingSet <- BostonHousing[-TrainingIndex,] #Testing Set

#Training Model
Model<-train(medv ~., data = TrainingSet,
             method = 'lm',
             na.action = na.omit,
             preProcess=c("scale", "center"),
             tuneLength = 10,
             trControl = trainControl(method= "cv"))

#Apply model for prediction on Training Set
Model.training <-predict(Model, TrainingSet)

#Apply Model for prediction on Testing Set
Model.testing <- predict(Model, TestingSet)


#Scatter Plot for Training & Testing Sets
plot(TrainingSet$medv,Model.training, col = "orange")
plot(TestingSet$medv,Model.testing, col = "orange")

#Model Performance Summary
summary(Model)

#Calculate Pearson's Correlate Coefficient
R.Training <- cor(TrainingSet$medv,Model.training)
R.Testing <- cor(TestingSet$medv,Model.testing)


#Calculate R^
R2.training<- R.Training^2
R2.testing<- R.Testing^2


#############################################################
#Test Partial Least Squares PLS

set.seed(100)
pcr_model <- train(medv~., data = TrainingSet,
                   method = "pcr",
                   preProcess = c("center", "scale"),
                   tuneGrid = expand.grid(ncomp = 1:13),
                   trControl = trainControl(method = "cv"))
plsFit<- train(medv ~ ., data=BostonHousing, "pls")

summary(pcr_model)

pcr_pred <- predict(pcr_model, TestingSet)
mean((pcr_pred - TestingSet$medv)^2)

pcr_pred <- predict(pcr_model, TestingSet)
mean((pcr_pred - TestingSet$medv)^2)

df_pcr <- data.frame(predicted = pcr_pred, observed = TestingSet$medv, residual = TestingSet - pcr_pred)


ggplot(df_pcr, aes(x = predicted, y = observed)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, colour = "blue") + 
  ggtitle("Principal Component Regression Predicted VS Observed")

set.seed(100)
pls_model <- train(medv~., data = TrainingSet, 
                   method = "pls",
                   preProcess = c("scale", "center"),
                   tuneGrid = expand.grid(ncomp = 1:13),
                   trControl = trainControl(method = "cv"))
pls_model

pls_pred <- predict(pls_model,  TestingSet)
mean((pls_pred - TestingSet$medv)^2)

df_pls <- data.frame(predicted = pls_pred, observed = TestingSet$medv, residual = TestingSet - pls_pred)

ggplot(df_pls, aes(x = predicted, y = observed)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, colour = "blue") + 
  ggtitle("Partial Least Squares Predicted VS Observed")

pcr_model$results$model <- "pcr"
pls_model$results$model <- "pls"

df_pcr_pls <- rbind(pcr_model$results, pls_model$results)
ggplot(df_pcr_pls, aes(x = ncomp, y = RMSE, colour = model)) + 
  geom_line() + 
  geom_point() +
  ggtitle("PCR VS PLS")

pls_imp <- varImp(pls_model, scale = F)
plot(pls_imp, scale = list(y = list(cex = .95)))
#############################################################

#Test Partial Least Squares SIMPLS

set.seed(100)
pcr_model <- train(medv~., data = TrainingSet,
                   method = "pcr",
                   preProcess = c("center", "scale"),
                   tuneGrid = expand.grid(ncomp = 1:13),
                   trControl = trainControl(method = "cv"))
simplsFit<- train(medv ~ ., data=BostonHousing, "simpls")

summary(pcr_model)

pcr_pred <- predict(pcr_model, TestingSet)
mean((pcr_pred - TestingSet$medv)^2)

pcr_pred <- predict(pcr_model, TestingSet)
mean((pcr_pred - TestingSet$medv)^2)

df_pcr <- data.frame(predicted = pcr_pred, observed = TestingSet$medv, residual = TestingSet - pcr_pred)


ggplot(df_pcr, aes(x = predicted, y = observed)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, colour = "blue") + 
  ggtitle("Principal Component Regression Predicted VS Observed")

set.seed(100)
simpls_model <- train(medv~., data = TrainingSet, 
                      method = "simpls",
                      preProcess = c("scale", "center"),
                      tuneGrid = expand.grid(ncomp = 1:13),
                      trControl = trainControl(method = "cv"))
simpls_model

simpls_pred <- predict(simpls_model,  TestingSet)
mean((simpls_pred - TestingSet$medv)^2)

df_simpls <- data.frame(predicted = simpls_pred, observed = TestingSet$medv, residual = TestingSet - simpls_pred)

ggplot(df_simpls, aes(x = predicted, y = observed)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, colour = "blue") + 
  ggtitle("Partial Least Squares Predicted VS Observed")

pcr_model$results$model <- "pcr"
simpls_model$results$model <- "simpls"

df_pcr_simpls <- rbind(pcr_model$results, simpls_model$results)
ggplot(df_pcr_simpls, aes(x = ncomp, y = RMSE, colour = model)) + 
  geom_line() + 
  geom_point() +
  ggtitle("PCR VS SIMPLS")

simpls_imp <- varImp(simpls_model, scale = F)
plot(simpls_imp, scale = list(y = list(cex = .95)))



##############################################################################################
#Test Partial Least Squares widekernelpls
set.seed(100)
pcr_model <- train(medv~., data = TrainingSet,
                   method = "pcr",
                   preProcess = c("center", "scale"),
                   tuneGrid = expand.grid(ncomp = 1:13),
                   trControl = trainControl(method = "cv"))
widekernelplsFit<- train(medv ~ ., data=BostonHousing, "widekernelpls")

summary(pcr_model)

pcr_pred <- predict(pcr_model, TestingSet)
mean((pcr_pred - TestingSet$medv)^2)

pcr_pred <- predict(pcr_model, TestingSet)
mean((pcr_pred - TestingSet$medv)^2)

df_pcr <- data.frame(predicted = pcr_pred, observed = TestingSet$medv, residual = TestingSet - pcr_pred)


ggplot(df_pcr, aes(x = predicted, y = observed)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, colour = "blue") + 
  ggtitle("Principal Component Regression Predicted VS Observed")

set.seed(100)
widekernelpls_model <- train(medv~., data = TrainingSet, 
                             method = "widekernelpls",
                             preProcess = c("scale", "center"),
                             tuneGrid = expand.grid(ncomp = 1:13),
                             trControl = trainControl(method = "cv"))
widekernelpls_model

widekernelpls_pred <- predict(widekernelpls_model,  TestingSet)
mean((widekernelpls_pred - TestingSet$medv)^2)

df_widekernelpls <- data.frame(predicted = widekernelpls_pred, observed = TestingSet$medv, residual = TestingSet - widekernelpls_pred)

ggplot(df_widekernelpls, aes(x = predicted, y = observed)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, colour = "blue") + 
  ggtitle("Partial Least Squares Predicted VS Observed")

pcr_model$results$model <- "pcr"
widekernelpls_model$results$model <- "widekernelpls"

df_pcr_widekernelpls <- rbind(pcr_model$results, widekernelpls_model$results)
ggplot(df_pcr_widekernelpls, aes(x = ncomp, y = RMSE, colour = model)) + 
  geom_line() + 
  geom_point() +
  ggtitle("PCR VS WIDEKERNELPLS")

widekernelpls_imp <- varImp(widekernelpls_model, scale = F)
plot(widekernelpls_imp, scale = list(y = list(cex = .95)))

###############################################################################
#Test Partial Least Squares oscorepls
set.seed(100)
pcr_model <- train(medv~., data = TrainingSet,
                   method = "pcr",
                   preProcess = c("center", "scale"),
                   tuneGrid = expand.grid(ncomp = 1:13),
                   trControl = trainControl(method = "cv"))
oscoreplsFit<- train(medv ~ ., data=BostonHousing, "oscorepls")

summary(pcr_model)

pcr_pred <- predict(pcr_model, TestingSet)
mean((pcr_pred - TestingSet$medv)^2)

pcr_pred <- predict(pcr_model, TestingSet)
mean((pcr_pred - TestingSet$medv)^2)

df_pcr <- data.frame(predicted = pcr_pred, observed = TestingSet$medv, residual = TestingSet - pcr_pred)


ggplot(df_pcr, aes(x = predicted, y = observed)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, colour = "blue") + 
  ggtitle("Principal Component Regression Predicted VS Observed")

set.seed(100)
oscorepls_model <- train(medv~., data = TrainingSet, 
                         method = "oscorepls",
                         preProcess = c("scale", "center"),
                         tuneGrid = expand.grid(ncomp = 1:13),
                         trControl = trainControl(method = "cv"))
oscorepls_model

oscorepls_pred <- predict(oscorepls_model,  TestingSet)
mean((oscorepls_pred - TestingSet$medv)^2)

df_oscorepls <- data.frame(predicted = oscorepls_pred, observed = TestingSet$medv, residual = TestingSet - oscorepls_pred)

ggplot(df_oscorepls, aes(x = predicted, y = observed)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, colour = "blue") + 
  ggtitle("Partial Least Squares Predicted VS Observed")

pcr_model$results$model <- "pcr"
oscorepls_model$results$model <- "oscorepls"

df_pcr_oscorepls <- rbind(pcr_model$results, oscorepls_model$results)
ggplot(df_pcr_oscorepls, aes(x = ncomp, y = RMSE, colour = model)) + 
  geom_line() + 
  geom_point() +
  ggtitle("PCR VS OSCOREPLS")

oscorepls_imp <- varImp(oscorepls_model, scale = F)
plot(oscorepls_imp, scale = list(y = list(cex = .95)))

################################################################################

#Ridge Regression


set.seed(100)
ridge<- train(medv~., data= TrainingSet, 
              method = "ridge", lamda = 4, preProcess = c('scale', 'center'))
ridge
ridge.pred <- predict(ridge, TestingSet)

mean((ridge.pred - TestingSet$medv)^2)

ridge_grid <- expand.grid(lambda = seq(0,0.1, length = 15))
set.seed(100)
ridge_model <- train(medv~., data = TrainingSet, 
                     method = "ridge", 
                     preProcess = c('center', 'scale'), 
                     tuneGrid = ridge_grid, 
                     trControl = trainControl(method = "cv"))
ridge.pred <- predict(ridge_model, TestingSet)

update(plot(ridge_model), xlab = "Penalty", 
       main = "The Cross-Valiation Profiles for Ridge Regression Model")

plot(varImp(ridge_model, scale=F))

#Lasso Regression

lasso <- train(medv~., TrainingSet, 
               method = "lasso", 
               preProcess = c("center", "scale"))
lasso





lasso.pred <- predict(lasso, TestingSet)
mean((lasso.pred - TestingSet$medv)^2)

lasso.grid <- expand.grid(fraction =seq(0.05,1, length = 20))
set.seed(1050)
lasso_model <- train(medv~., data = TrainingSet,
                     method = "lasso", 
                     preProcess = c("center", "scale"),
                     tuneGrid = lasso.grid,
                     trControl = trainControl(method = "cv"))

lasso_model

lasso.pred <- predict(lasso_model, TestingSet)
mean((lasso.pred - TestingSet$medv)^2)

update(plot(lasso_model), main = "The Cross-validation Profiles for Elastic Net Model")

plot(varImp(lasso_model, scale=F))


#Elastic Net Regression

set.seed(100)

e_net <- train(medv~., data = TrainingSet,
               method = "enet",
               preProcess = c("scale", "center"))
e_net

enet.pred <-  predict(e_net, TestingSet)
mean((enet.pred - TestingSet$medv)^2)

enet.grid <- expand.grid(lambda =seq(0,0.01,.1), fraction = seq(0.05,1, length = 20))
set.seed(100)

enet_model <- train(medv~., data = TrainingSet,
                    method = "enet", 
                    preProcess = c("center", "scale"),
                    tuneGrid = enet.grid,
                    trControl = trainControl(method = "cv"))
enet_model
enet.pred <-  predict(enet_model, TestingSet)
mean((enet.pred - TestingSet$medv)^2)
update(plot(enet_model), main = "The Cross-validation Profiles for Elastic Net Model")

plot(varImp(e_net, scale=F))

###########################################################################
#MARS Regression
data("BostonHousing")
set.seed(100)

#Examine Data
BostonHousing%>%
  ggplot(aes(x=lstat, y=medv)) +
  geom_point() +
  geom_smooth(method='lm')
BostonHousing%>%
  ggplot(aes(x = lstat, y = medv) ) +
  geom_point()+
  geom_smooth()
#perform stratified random split of the data
TrainingIndex<- createDataPartition(BostonHousing$medv, p=.75, list = FALSE)
TrainingSet <- BostonHousing[TrainingIndex,] #Training Set
TestingSet <- BostonHousing[-TrainingIndex,] #Testing Set

#Define values for X & Y
ncol(BostonHousing)
x<- TrainingSet [, -14]
y<- TrainingSet [, 14]

#create a Parameter Tuning Grid
parameter_grid<- floor(expand.grid(degree = 1:4, nprune = seq(5, 50, by = 5)))
view(parameter_grid)
cv_mars_boston<-train(x=x,
                      y=y,
                      method = "earth",
                      metric = "RMSE",
                      trControl = trainControl(method = "cv", number = 10),
                      tuneGrid = parameter_grid)
#Plot  using GGPLOT
ggplot(cv_mars_boston)

#Use Best Model to Predict Response Variable

mars_predict<-predict(object=cv_mars_boston$finalModel,
        newdata=TestingSet)

head(mars_predict)
nrow(mars_predict)
nrow(TestingSet)
#Calculate the SSE
mars_sse <- sum(TestingSet$medv - mars_predict^2)
mars_sse
#Calculate the MAE
mars_mae<- mean(abs(TestingSet$medv - mars_predict))
mars_mae  

#plot 
plot(TestingSet$medv - mars_predict,
     main = "Residuals for MARS model",
     xlab = "Observation number in the set",
     ylab = "REsidual")

################################################################################
#Support Vector Machines (page 167)
set.seed(100)
svmRTune <- train(medv~., data = TrainingSet,
                  method = "svmRadial",
                  preProc = c("center", "scale"),
                  tuneLength = 14,
                  trControl = trainControl(method = "cv", number = 10))

svmRTune

plot(svmRTune, scales = list(x = list(log = 2)))

svmRTune$finalModel 

#Polynomial
set.seed(100)
svmPTune <- train(medv~., data = TrainingSet,
                  method = "svmPoly",
                  preProc = c("center", "scale"),
                  trControl = trainControl(method = "cv", number = 10))

svmPTune

plot(svmPTune, 
     scales = list(x = list(log = 2), 
                   between = list(x = .5, y = 1)))

################################################################################
#K-Nearest Neighbors (page 168)
set.seed(100)
knnTune <- train(medv~., data = TrainingSet,
                 method = "knn",
                 preProc = c("center", "scale"),
                 trControl = trainControl(method = "cv", number = 10))
knnTune
