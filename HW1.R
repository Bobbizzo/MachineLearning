#Robert Agueros


library(mlbench)
library(caret)
library(corrplot)
library(AppliedPredictiveModeling)

# The following is built-in data set that comes from "mlbench".
# Upload the CSV file into RStudio
data("BostonHousing")
# Check for missing values (N/A's)
sum(is.na(BostonHousing))

# Do some exploration & also check correlation plot (using numbers)

#Extract as excel file and name as group.
install.packages("xlsx")
library(xlsx)
write.xlsx(BostonHousing, file = "Group1.xlsx")

str(BostonHousing)
str(BostonHousing$chas)


predictors <- subset(BostonHousing,select= -chas)

# Outcome variable "Chas" indicates if 1) tract bounds river or 0) does not bound river
chas <- subset(BostonHousing,select="chas")

# Split data into training and test sets

set.seed(1)
trainRows <- createDataPartition(BostonHousing$chas,
                                    p = 0.75, 
                                    list = FALSE)
head(trainRows)

trainPred <- predictors[trainRows,]
trainChas <- chas[trainRows,]

testPred <- predictors[-trainRows,]  
testChas <- chas[-trainRows,] 

correlations <- cor(trainPred[,1:13])
corrplot(correlations, method="number")
