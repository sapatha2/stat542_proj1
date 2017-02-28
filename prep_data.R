# Impute and transform data for analysis

# Impute raw data using the technique described here:
# https://www.kaggle.com/clustersrus/house-prices-advanced-regression-techniques/house-prices-dealing-with-the-missing-data/notebook
setwd("/Users/shiveshpathak/Documents/School Work/STAT 542/Rcode/stat542_proj1/")
train <- read.csv("./train.csv",stringsAsFactors=T)
SalePrice = train$SalePrice
test <- read.csv("./test.csv",stringsAsFactors=T)
df <- rbind(train[,!(names(train) %in% c("SalePrice"))],test)
print(sum(is.na(df)))
# Some of these variables are ordinal or simply factors encoded with numerical values.
df$YearBuilt<-as.factor(df$YearBuilt)
df$YearRemodAdd<-as.factor(df$YearRemodAdd)
df$MSSubClass<-as.factor(df$MSSubClass)
df$OverallQual<-as.factor(df$OverallQual)
df$OverallCond<-as.factor(df$OverallCond)
df$MoSold<-as.factor(df$MoSold)
df$YrSold<-as.factor(df$YrSold)
df$GarageYrBlt<-as.factor(df$GarageYrBlt)

# Imputing missing data
#Let's Formalize these ideas in one function:
dfClean <-function(df){
  # Pool Variable: If PoolQC = NA and PoolArea = 0 , assign factor NoPool
  df$PoolQC <- as.character(df$PoolQC)
  df$PoolQC[df$PoolArea %in% c(0,NA) & is.na(df$PoolQC)] <- "NoPool"
  df$PoolQC <- as.factor(df$PoolQC)
  # MiscFeature Variable: If MiscFeature = NA and MiscVal = 0, assign factor None
  df$MiscFeature <- as.character(df$MiscFeature)
  df$MiscFeature[df$MiscVal %in% c(0,NA) & is.na(df$MiscFeature)] <- "None"
  df$MiscFeature <- as.factor(df$MiscFeature)
  # Alley Variable: If Alley = NA, assign factor NoAccess
  df$Alley <- as.character(df$Alley)
  df$Alley[is.na(df$Alley)] <- "NoAccess"
  df$Alley <- as.factor(df$Alley)
  # Fence Variable: If Fence = NA, assign factor NoFence
  df$Fence <- as.character(df$Fence)
  df$Fence[is.na(df$Fence)] <- "NoFence"
  df$Fence <- as.factor(df$Fence)
  # FireplaceQu Variable: If FireplaceQu = NA and Fireplaces = 0 , assign factor NoFirePlace
  df$FireplaceQu <- as.character(df$FireplaceQu)
  df$FireplaceQu[df$Fireplaces %in% c(0,NA) & is.na(df$FireplaceQu)] <- "NoFirePlace"
  df$FireplaceQu <- as.factor(df$FireplaceQu)
  # GarageYrBlt Variable: If GarageYrBlt = NA and GarageArea = 0 assign factor NoGarage
  df$GarageYrBlt <- as.character(df$GarageYrBlt)
  df$GarageYrBlt[df$GarageArea %in% c(0,NA) & is.na(df$GarageYrBlt)] <- "NoGarage"
  df$GarageYrBlt <- as.factor(df$GarageYrBlt)
  # GarageFinish Variable: If GarageFinish = NA and GarageArea = 0 assign factor NoGarage
  df$GarageFinish <- as.character(df$GarageFinish)
  df$GarageFinish[df$GarageArea %in% c(0,NA) & is.na(df$GarageFinish)] <- "NoGarage"
  df$GarageFinish <- as.factor(df$GarageFinish)
  # GarageQual Variable: If GarageQual = NA and GarageArea = 0 assign factor NoGarage
  df$GarageQual <- as.character(df$GarageQual)
  df$GarageQual[df$GarageArea %in% c(0,NA) & is.na(df$GarageQual)] <- "NoGarage"
  df$GarageQual <- as.factor(df$GarageQual)
  # GarageCond Variable: If GarageCond = NA and GarageArea = 0 assign factor NoGarage
  df$GarageCond <- as.character(df$GarageCond)
  df$GarageCond[df$GarageArea %in% c(0,NA) & is.na(df$GarageCond)] <- "NoGarage"
  df$GarageCond <- as.factor(df$GarageCond)
  # GarageType Variable: If GarageType = NA and GarageArea = 0 assign factor NoGarage
  df$GarageType <- as.character(df$GarageType)
  df$GarageType[df$GarageArea %in% c(0,NA) & is.na(df$GarageType)] <- "NoGarage"
  df$GarageType <- as.factor(df$GarageType)

  df$GarageArea[is.na(df$GarageArea) & df$GarageCars %in% c(0,NA)] <- 0
  df$GarageCars[is.na(df$GarageCars) & df$GarageArea %in% c(0,NA)] <- 0


  # BsmtFullBath Variable: If BsmtFullBath = NA and TotalBsmtSF = 0 assign 0
  df$BsmtFullBath[df$TotalBsmtSF %in% c(0,NA) & is.na(df$BsmtFullBath)] <- 0
  # BsmtHalfBath Variable: If BsmtHalfBath = NA and TotalBsmtSF = 0 assign 0
  df$BsmtHalfBath[df$TotalBsmtSF %in% c(0,NA) & is.na(df$BsmtHalfBath)] <- 0

  # BsmtFinSF1 Variable: If BsmtFinSF1 = NA and TotalBsmtSF = 0 assign 0
  df$BsmtFinSF1[df$TotalBsmtSF %in% c(0,NA) & is.na(df$BsmtFinSF1)] <- 0
  # BsmtFinSF2 Variable: If BsmtFinSF2 = NA and TotalBsmtSF = 0 assign 0
  df$BsmtFinSF2[df$TotalBsmtSF %in% c(0,NA) & is.na(df$BsmtFinSF2)] <- 0
  # BsmtUnfSF Variable: If BsmtUnfSF = NA and TotalBsmtSF = 0 assign 0
  df$BsmtUnfSF[df$TotalBsmtSF %in% c(0,NA) & is.na(df$BsmtUnfSF)] <- 0
  # TotalBsmtSF Variable: If TotalBsmtSF = NA and TotalBsmtSF = 0 assign 0
  df$TotalBsmtSF[df$TotalBsmtSF %in% c(0,NA) & is.na(df$TotalBsmtSF)] <- 0

  # BsmtQual Variable: If BsmtQual = NA and TotalBsmtSF = 0 assign factor NoBasement
  df$BsmtQual <- as.character(df$BsmtQual)
  df$BsmtQual[df$TotalBsmtSF %in% c(0,NA) & is.na(df$BsmtQual)] <- "NoBasement"
  df$BsmtQual <- as.factor(df$BsmtQual)
  # BsmtFinType1 Variable: If BsmtFinType1 = NA and TotalBsmtSF = 0 assign factor NoBasement
  df$BsmtFinType1 <- as.character(df$BsmtFinType1)
  df$BsmtFinType1[df$TotalBsmtSF %in% c(0,NA) & is.na(df$BsmtFinType1)] <- "NoBasement"
  df$BsmtFinType1 <- as.factor(df$BsmtFinType1)
  # BsmtFinType2 Variable: If BsmtFinType2 = NA and TotalBsmtSF = 0 assign factor NoBasement
  df$BsmtFinType2 <- as.character(df$BsmtFinType2)
  df$BsmtFinType2[df$TotalBsmtSF %in% c(0,NA) & is.na(df$BsmtFinType2)] <- "NoBasement"
  df$BsmtFinType2 <- as.factor(df$BsmtFinType2)
  # BsmtExposure Variable: If BsmtExposure = NA and TotalBsmtSF = 0 assign factor NoBasement
  df$BsmtExposure <- as.character(df$BsmtExposure)
  df$BsmtExposure[df$TotalBsmtSF %in% c(0,NA) & is.na(df$BsmtExposure)] <- "NoBasement"
  df$BsmtExposure <- as.factor(df$BsmtExposure)
  # BsmtCond Variable: If BsmtCond = NA and TotalBsmtSF = 0 assign factor NoBasement
  df$BsmtCond <- as.character(df$BsmtCond)
  df$BsmtCond[df$TotalBsmtSF %in% c(0,NA) & is.na(df$BsmtCond)] <- "NoBasement"
  df$BsmtCond <- as.factor(df$BsmtCond)
  return(df)
}
df <- dfClean(df)

# Additional cleaning: MasVnrType, MasVnrArea
df$MasVnrType <- as.character(df$MasVnrType)
df$MasVnrType[is.na(df$MasVnrType)] <- "None"
df$MasVnrType <- as.factor(df$MasVnrType)
df$MasVnrArea[is.na(df$MasVnrArea)] <- 0

# MSZoning
df$MSZoning <- as.character(df$MSZoning)
df$MSZoning[is.na(df$MSZoning)] <- "RL"
df$MSZoning <- as.factor(df$MSZoning)

# BsmtExposure
df$BsmtExposure <- as.character(df$BsmtExposure)
df$BsmtExposure[is.na(df$BsmtExposure)]<-"No"
df$BsmtExposure <- as.factor(df$BsmtExposure)

# BsmtFinType2
df$BsmtFinType2 <- as.character(df$BsmtFinType2)
df$BsmtFinType2[is.na(df$BsmtFinType2)]<-"ALQ"
df$BsmtFinType2 <- as.factor(df$BsmtFinType2)

# BsmtQual
df$BsmtQual <- as.character(df$BsmtQual)
df$BsmtQual[is.na(df$BsmtQual) & df$HouseStyle == "2Story"]<-"Gd"
df$BsmtQual[is.na(df$BsmtQual) & df$HouseStyle == "1.5Fin"]<-"TA"
df$BsmtQual <- as.factor(df$BsmtQual)

# BsmtCond
df$BsmtCond <- as.character(df$BsmtCond)
df$BsmtCond[is.na(df$BsmtCond)]<-"TA"
df$BsmtCond <- as.factor(df$BsmtCond)

# Replace all the other numerical values with the mode
fillMiss<- function(x){
  ux <- unique(x[!is.na(x)])
  x <- as.character(x)
  mode <- ux[which.max(tabulate(match(x[!is.na(x)], ux)))]
  x[is.na(x)] <- as.character(mode)
  x <- as.factor(x)
  return(x)
}
df[,sapply(df,function(x){!(is.numeric(x))}) ]<-as.data.frame(apply(df[,sapply(df,function(x){!(is.numeric(x))}) ],2,fillMiss))

# Set LotFrontage where it is not defined to sqrt(LotArea)
# Inspired by https://www.kaggle.com/meikegw/house-prices-advanced-regression-techniques/filling-up-missing-values
df$LotFrontage[is.na(df$LotFrontage)]=sqrt(df$LotArea)[is.na(df$LotFrontage)]

# Finally get the training data back and transform
myData=df[1:nrow(train),]
myData$SalePrice=log(train$SalePrice)
myData$LotArea=sqrt(myData$LotArea)
myData$MasVnrArea=sqrt(myData$MasVnrArea)

# Year built as numerical
myData$YearBuilt=as.numeric(myData$YearBuilt)

# Remove Utilities and Street
myData=subset(myData,select=-c(Utilities,Street))

# Break into traindata and testdata
ntrain=round(nrow(myData)*0.75)
train.id=sample(1:nrow(myData), ntrain)
traindata=myData[train.id,]
testdata=myData[-train.id,]

# See if there are any new levels!
# Loop over all predictors
for(i in 1:ncol(traindata)){
  # Only consider categorical ones
  if(is.factor(traindata[,i])){
    tt <- table(traindata[,i]) # Most frequent level
    #Loop over all levels of the categorical predcitor
    for(l in levels(traindata[,i])){
      # If the level is not completely ommited, and is occupied in the test but not train set 
      if(!is.na(summary(traindata[,i])[l])){
        if(summary(traindata[,i])[l]==0){
          if(summary(testdata[,i])[l]!=0){
            # Before: There is a discrepancy between the following printouts
            # print(summary(testdata[,i])[l])
            # print(summary(traindata[,i])[l])
            # If satisfied, just replace it with the most common category!
            testdata[,i][testdata[,i]==l]=names(tt[which.max(tt)])
            # After: The discrepancy is gone!
            # print(summary(testdata[,i])[l])
            # print(summary(traindata[,i])[l])
          }
        }
      }else{
        # If the level isn't defined in the train but is in the test data
        if(!is.na(summary(testdata[,i])[l])){
          # If satisfied, just replace it with the most common category!
          testdata[,i][testdata[,i]==l]=names(tt[which.max(tt)])
        }
      }
    }
  }
}

# Do a simple linear regression
# Typical errors: 
# 1) The break-up above can lead to categories that have no flling at all!
#    This leads to the "contrasts can be applied only to factors with 2 or more levels!"
# 2) There are categories occupied in the test data, but not in the training data!
#    This leads to "___Categorical Predictor___ has new levels __{x}__"
#    I have seen this for ___Categorical Predictor___=Condition2,OverallQual,YearBuilt

# After the additional cleaning this works fine. You get a rank-deficiency error because 
# some of the categorical levels are linearly dependent when you do the dummy coding!

lmfit=lm(traindata$SalePrice~.,data=traindata)
Ytest.pred=predict(lmfit,newdata=testdata)
print(summary(lmfit))