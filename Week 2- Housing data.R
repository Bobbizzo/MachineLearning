#Robert Agueros

library(mlbench)
library(AppliedPredictiveModeling)
library(elasticnet)
library(lars)
library(pls)
library(stats)
library(MASS)
library(caret)
library(lattice)


# The following is built-in data set that comes along with the package.
data("BostonHousing")
View(BostonHousing)

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

# The following is the string of code for linear regression and points 

set.seed(1)
#perform stratified random split of the data
TrainingIndex<- createDataPartition(BostonHousing$medv, p=.75, list = FALSE)
#Training Set
TrainingSet <- BostonHousing[TrainingIndex,] 
#Testing Set
TestingSet <- BostonHousing[-TrainingIndex,] 

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
#Test Partial Least Squares
plsFit<- train(medv ~ ., data=BostonHousing, "pls")

#Conduct Post Resampling for Partial Least Squares
postResample(pred = predict(plsFit), obs = BostonHousing$medv)

#############################################################

### PARTIAL LEAST SQUARES ###
set.seed(100)
plsMDL <- train(medv~., data = TrainingSet,
                method = "pls",
                tuneGrid = expand.grid(ncomp=1:13),
                trControl = trainControl(method = "cv"))
plsMDL

#simpls
set.seed(154)
simplsMDL <- train(medv~., data = TrainingSet,
                   method = "simpls",
                   tuneGrid = expand.grid(ncomp=1:13),
                   trControl = trainControl(method = "cv"))
simplsMDL

#WIDEKERNELPLS
set.seed(154)
widekernelplsMDL <- train(medv~., data = TrainingSet,
                          method = "widekernelpls",
                          tuneGrid = expand.grid(ncomp=1:13),
                          trControl = trainControl(method = "cv"))

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

update(plot(ridge_model), xlab = "penalty", 
       main = "The CV Profiles for Ridge Regression Model")

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