#Robert Agueros

library(mlbench)
library(caret)
library(corrplot)
library(AppliedPredictiveModeling)

# The following is built-in data set that comes along with the package

data("BostonHousing")

# Let's learn a trick about extracting any built-in data set. 
install.packages("xlsx")
library(xlsx)
write.xlsx(BostonHousing, file = "Group1.xlsx")

#Remove missing observations
data1 <- BostonHousing[complete.cases(BostonHousing),]

data1

####Lets carry out the analysis including the missing data.


#Separating outcome variable from the rest

# The following code is selecting all rows excluding Yield column 
# (i.e. all independent variables)

predictors <- subset(BostonHousing,select= -chas)

#Now lets go for the outcome variable
chas <- subset(BostonHousing,select="chas")

#Split data into training and tests sets

set.seed(1)
trainRows <- createDataPartition(BostonHousing$chas,
                                 p= 0.75,
                                 list =FALSE)
head(trainRows)

trainPred <- predictors[trainRows,]
trainChas <- chas[trainRows,]

testPred <- predictors[-trainRows,]
testChas <- chas[-trainRows,]

correlations <- cor(trainPred[,1:13])
corrplot(correlations, method="number")

#Resampling with knn
?trainControl
set.seed(1)
resample <- train(chas~., data = BostonHousing,
            method = "knn",
            tuneLength = 12)
resample
