# Conduct Simple Linear Regression
# Coded by Ashley Hong, cleaned-up for submission by Shivesh Pathak
lmfit=lm(traindata$SalePrice~.,data=traindata)
options(warn=-1)
Ytest.pred=predict(lmfit,newdata=testdata)
options(warn=0)
RMSE_lin <- RMSE(Ytest.pred, testdata$SalePrice)

print("Simple Linear RMSE:")
print(RMSE_lin) 

BestPredSimple <- data.frame(Id= testdata$Id, SalePrice= exp(Ytest.pred))
print("Writing Simple linear prediction")
names(BestPredSimple)[2]="SalePrice"
write.table(BestPredSimple, "mysubmission1.txt",sep=",",row.names=FALSE)
