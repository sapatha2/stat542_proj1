# Conduct Ridge, Lasso and Random Forest 
# Coded by Jean Liu, cleaned-up for submission by Shivesh Pathak

####Ridge####
### Convert any qualitative variables to dummy variables
training_x=model.matrix(SalePrice~.,traindata)[,-1]
testing_x=model.matrix(SalePrice~.,testdata)[,-1]
training_y=traindata$SalePrice
testing_y=testdata$SalePrice

library(glmnet)
grid = 10^seq (10,-2,length =100)
ridge_model = glmnet(training_x,training_y,alpha = 0,lambda = grid, standardize = FALSE)
dim(coef(ridge_model))

#plot(ridge_model, xvar = "lambda", label = TRUE)

## Best Lambda
cv_error = cv.glmnet(training_x,
                     training_y,
                     alpha = 0) #default 10 fold cv
#plot(cv_error) # cross-validation curve (red dotted line), upper and lower standard deviation curves

best_lambda = cv_error$lambda.min
best_lambda

### Model with Best Lambda
model_coef = predict(ridge_model,
                     type = "coefficients",
                     s= best_lambda)

### Test the Model
predicted_y = predict(ridge_model,
                      s= best_lambda,
                      newx = testing_x)
### RMSE
RMSE_RIDGE=RMSE(predicted_y, testing_y)
print("Ridge RMSE:")
print(RMSE_RIDGE)

### BestPredRIDGE.csv
predicty<- exp(predicted_y)
BestPredRIDGE <- data.frame(Id = testdata$Id, SalePrice= predicty)

####LASSO####
### Convert any qualitative variables to dummy variables
x=model.matrix(SalePrice~.,myData)[,-1] # get rid of intercept column
y=myData$SalePrice

### Split the dataset into training and testing
grid = 10^seq (10,-2,length =100)

lasso_model = glmnet(training_x,
                     training_y,
                     alpha =1,
                     lambda=grid,
                     standardize=FALSE)

#plot(lasso_model, xvar = "lambda",label = TRUE)
cv_error = cv.glmnet(training_x,
                     training_y,
                     alpha = 1)
best_lambda = cv_error$lambda.min
best_lambda
#plot(cv_error)

### OUR FINAL LASSO
model_coef = predict(lasso_model,
                     type = "coefficients",
                     s= best_lambda)

### Test the Model
predicted_yL = predict(lasso_model,
                       s= best_lambda,
                       newx = testing_x)
### RMSE
RMSE_LASSO<- RMSE(predicted_yL, testing_y) 
RMSE_LASSO
print("Lasso RMSE")
print(RMSE_LASSO)

### BestPredLASSO.csv
BestPredLASSO <- data.frame(Id = testdata$Id, SalePrice= exp(predicted_yL))

# Best prediction between Lasso and Ridge
if(RMSE_LASSO<RMSE_RIDGE){
  print("Writing LASSO prediction")
  names(BestPredLASSO)[2]="SalePrice"
  write.table(BestPredLASSO, "mysubmission2.txt",sep=",",row.names=FALSE)
}else{
  print("Writing RIDGE prediction")
  names(BestPredRIDGE)[2]="SalePrice"
  write.table(BestPredRIDGE, "mysubmission2.txt",sep=",",row.names=FALSE)
}

library(randomForest)
#### Random Forests, "anova" ####
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
RMSE2 <- RMSE(predict2, testdata$SalePrice)
print("Random Forest RMSE:")
print(RMSE2) 

### BestPredForest
BestPredForest <- data.frame(Id= testdata$Id, SalePrice= exp(predict2))
print("Writing Random Forest prediction")
names(BestPredForest)[2]="SalePrice"
write.table(BestPredForest, "mysubmission3.txt",sep=",",row.names=FALSE)