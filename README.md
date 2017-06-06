train <- read.csv("train.csv", stringsAsFactors = F)
test <- read.csv("test.csv", stringsAsFactors = F)
str(train)
na.cols <- which(colSums(is.na(train)) > 0)
sort(colSums(sapply(train[na.cols], is.na)), decreasing = TRUE)
train<-train[,-1]
train$Alley[is.na(train$Alley)] <- rep('None', 1369)
train$PoolQC[is.na(train$PoolQC)] <- rep('None', 1453)
train$MiscFeature[is.na(train$MiscFeature)] <- rep('None', 1406)
train$Fence[is.na(train$Fence)] <- rep('None', 1179)
train$FireplaceQu[is.na(train$FireplaceQu)] <- rep('None', 690)
train$GarageType[is.na(train$GarageType)] <- rep('None', 81)
train$GarageYrBlt[is.na(train$GarageYrBlt)] <- rep('None', 81)
train$GarageFinish[is.na(train$GarageFinish)] <- rep('None', 81)
train$GarageQual[is.na(train$GarageQual)] <- rep('None', 81)
train$GarageCond[is.na(train$GarageCond)] <- rep('None', 81)
train$BsmtExposure[is.na(train$BsmtExposure)] <- rep('None', 38)
train$BsmtFinType2[is.na(train$BsmtFinType2)] <- rep('None', 38)
train$BsmtQual[is.na(train$BsmtQual)] <- rep('None', 37)
train$BsmtCond[is.na(train$BsmtCond)] <- rep('None', 37)
train$BsmtFinType1[is.na(train$BsmtFinType1)] <- rep('None', 37)
require(mice)
mice.data <- mice(train,
                  m = 1,           
                  maxit = 50,      
                  method = "cart", 
                  seed = 87)      

df <- complete(mice.data, 1)
na.cols <- which(colSums(is.na(df)) > 0)
sort(colSums(sapply(df[na.cols], is.na)), decreasing = TRUE)
rm.data <- df[complete.cases(df), ]
na.cols <- which(colSums(is.na(rm.data)) > 0)
sort(colSums(sapply(rm.data[na.cols], is.na)), decreasing = TRUE)
summary(rm.data)
tmp=0
for(i in 1:1451){
  if(rm.data[i,80]>214000)tmp[i]="A"
  else if(rm.data[i,80]>162500)tmp[i]="B"
  else if(rm.data[i,80]>129900)tmp[i]="C"
  else tmp[i]="D"
}
rm.data[,80]=factor(tmp)
