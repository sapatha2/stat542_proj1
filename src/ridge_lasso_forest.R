# Conduct Ridge, Lasso and Random Forest 
# Coded by Jean Liu, cleaned-up for submission by Shivesh Pathak

####Ridge####
### Convert any qualitative variables to dummy variables
print("Beginning Ridge fit")
training_x=data.matrix(traindata[,-ncol(traindata)])
training_y=traindata[,ncol(traindata)]
testing_x=data.matrix(testdata)

library(glmnet)
cv_error = cv.glmnet(training_x,training_y,alpha = 0)
predicted_y = predict(cv_error, s="lambda.min", newx = testing_x)

### BestPredRIDGE.csv
predicty<- exp(predicted_y)
BestPredRIDGE <- data.frame(Id = testdata$Id, SalePrice= predicty)
print("Writing RIDGE prediction")
names(BestPredRIDGE)[2]="SalePrice"
write.table(BestPredRIDGE, "mysubmission2.txt",sep=",",row.names=FALSE)
print("Finished writing Ridge prediction")

#### Random Forests, "anova" ####
library(randomForest)
# Make categories with > 53 categories numerical
for(i in 1:ncol(traindata)){
  # Only consider categorical ones
  if(is.factor(traindata[,i])){
    if(length(levels(traindata[,i]))>53){
      traindata[,i]=as.numeric(traindata[,i])
      testdata[,i]=as.numeric(testdata[,i])
    }
  }
}
model2 <- randomForest(SalePrice ~., data = traindata, method = "anova",
                       ntree = 300,
                       mtry = 26,
                       replace = F,
                       nodesize = 1,
                       importance = T)

predict2 <- predict(model2, testdata)

### BestPredForest
BestPredForest <- data.frame(Id= testdata$Id, SalePrice= exp(predict2))
print("Writing Random Forest prediction")
names(BestPredForest)[2]="SalePrice"
write.table(BestPredForest, "mysubmission3.txt",sep=",",row.names=FALSE)
