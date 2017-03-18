# Conduct Simple Linear Regression
# Coded by Ashley Hong, cleaned-up for submission by Shivesh Pathak
print("Beginning linear fit")
lmfit=lm(traindata$SalePrice~.,data=traindata)
options(warn=-1)
Ytest.pred=predict(lmfit,newdata=testdata)
options(warn=0)

BestPredSimple <- data.frame(Id= testdata$Id, SalePrice= exp(Ytest.pred))
names(BestPredSimple)[2]="SalePrice"
write.table(BestPredSimple, "mysubmission1.txt",sep=",",row.names=FALSE)
print("Finished writing Simple linear prediction")