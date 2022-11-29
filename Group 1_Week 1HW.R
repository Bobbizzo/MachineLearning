#Robert Agueros

library(mlbench)
library(caret)
library(corrplot)
library(AppliedPredictiveModeling)

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

#calculate the pre-process parameter of Scale Source: https://machinelearningmastery.com/pre-process-your-dataset-in-r/
preprocessParams <- preProcess(BostonHousing, method=c("scale"))

#summarize transform parameters
print(preprocessParams)

# transform the dataset using the parameters
transformed <- predict(preprocessParams, BostonHousing)
                       
# summarize the transformed dataset
summary(transformed)

# Now lets go for the outcome variable "chas"
predictors <- subset(BostonHousing,select= -chas)
chas <- subset(transformed,select="chas")

#set the seed
set.seed(1)

#Create Training at 75%
trainRows <- createDataPartition(transformed$chas,
                                 p=0.75,
                                 list = FALSE)

head(trainRows)

trainPred <- predictors[trainRows,]
trainChas <- chas[trainRows,]

testPred <- predictors[-trainRows,]  
testChas <- chas[-trainRows,] 

#check correlation plot using numbers
correlations <- cor(trainPred[,1:13])
corrplot(correlations, method="number", type = "upper", diag = FALSE)

#Resampling using KNN 
?trainControl
set.seed(1)
resample <- train(chas~., data = BostonHousing,
             method = "knn",
             tuneLength = 12)
resample


#build a linear regression model
model1 <- lm(medv ~ ., data = trainPred[,1:13])
model1.sum<- summary(model1)
model1.sum
