getwd()
setwd("D:\\Media\\Documents\\Data Science\\Repositories\\Big mart sales")

library(mlr)#For SummarizeColumns function
library(caret)#For Confusuion Matrix
library(class)#For KNN
library(car)#for Linear regression
library(dummies)#for creating dummy variables
library(MASS)#For box cox transformation
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(randomForest)
library(corrplot)
library(ggplot2)

big<-read.csv("Train.csv", header = TRUE, na.strings = c("","NA","-","NaN"))#<---
bigm<-big#<---
bigm$Item_Fat_Content[which(bigm$Item_Fat_Content=="LF"|bigm$Item_Fat_Content=="low fat")]<-"Low Fat"#<---
bigm$Item_Fat_Content[which(bigm$Item_Fat_Content=="reg")]<-"Regular"#<---
bigm$Outlet_Establishment_Year<-as.factor(bigm$Outlet_Establishment_Year)#<---
array_Item_Identifier<-bigm$Item_Identifier[which(is.na(bigm$Item_Weight))]#<---
i=1#<---
for (i in 1:length(array_Item_Identifier)) {#<---
  item_weight_val<-bigm$Item_Weight[which(bigm$Item_Identifier==array_Item_Identifier[i])]
  avg_item<-mean(item_weight_val,na.rm = T)
  bigm$Item_Weight[bigm$Item_Identifier==array_Item_Identifier[i]]<-avg_item
}
#________________________________________________________________________________________________________
bigm$Item_Visibility[which(bigm$Item_Visibility==0)]<-NA#<---
summarizeColumns(bigm)

#MV

#MV_(Item_Weight)
bigm$Item_Weight[which(is.na(bigm$Item_Weight))]<-mean(bigm$Item_Weight,na.rm = T)#<---

#MV_(outlet_Size)
#MV_(Outlet_Size)_Outlier
bigmo<-bigm#<---

boxplot(bigmo$Item_Visibility)
boxplot(bigmo$Item_Weight)
boxplot(bigmo$Item_MRP)
boxplot(bigmo$Item_Outlet_Sales)

#MV_(Outlet_Size)_Outlier_(Item_Visibility)
boxplot((bigmo$Item_Visibility)^(1/3))
bigmo$Item_Visibility<-(bigmo$Item_Visibility)^(1/3)#<---
quantile(bigmo$Item_Visibility,0.75,na.rm = TRUE)+(1.5*IQR(bigmo$Item_Visibility,na.rm = T))
bigmo$Item_Visibility[which(bigmo$Item_Visibility>0.6797936)]<-NA#<---
length(bigmo$Item_Visibility[which(is.na(bigmo$Item_Visibility))])

#MV_(Outlet_Size)_Outlier_(Item_Outlet_Sales)
boxplot((bigm$Item_Outlet_Sales)^(1/3))
bigmo$Item_Outlet_Sales<-(bigmo$Item_Outlet_Sales)^(1/3)#<---
IQR(bigmo$Item_Outlet_Sales)*1.5+quantile(bigmo$Item_Outlet_Sales,0.75)
bigmo$Item_Outlet_Sales[which(bigmo$Item_Outlet_Sales>22.33688)]<-NA#<---
length(bigmo$Item_Outlet_Sales[which(is.na(bigmo$Item_Outlet_Sales))])
mean(bigmo$Item_Outlet_Sales,na.rm = T)
bigmo$Item_Outlet_Sales[which(is.na(bigmo$Item_Outlet_Sales))]<-11.98796#<---

#MV_(Outlet size)_BA
#MV_(Outlet size)_BA_(Cont-Cat)
#MV_(Outlet size)_BA_(Cont-Cat)_(Item_Outlet_Sales vs Outlet_Size)
f<-aov(Item_Outlet_Sales~Outlet_Size,data = bigmo)
summary(f)


#MV_(Outlet_Size)_KNN
bigmo<-bigmo[,-c(1,4)]#<---
#MV_(Outlet_Size)_KNN_(Convert cat to numeric)#<------------------------
bigmo$Item_Fat_Content<-as.character(bigmo$Item_Fat_Content)
bigmo$Item_Fat_Content[which(bigmo$Item_Fat_Content=="Low Fat")]<-1
bigmo$Item_Fat_Content[which(bigmo$Item_Fat_Content=="Regular")]<-2
bigmo$Item_Fat_Content<-as.numeric(bigmo$Item_Fat_Content)
bigmo$Item_Type<-as.character(bigmo$Item_Type)
bigmo$Item_Type[which(bigmo$Item_Type=="Baking Goods")]<-1
bigmo$Item_Type[which(bigmo$Item_Type=="Breads")]<-2
bigmo$Item_Type[which(bigmo$Item_Type=="Breakfast")]<-3
bigmo$Item_Type[which(bigmo$Item_Type=="Canned")]<-4
bigmo$Item_Type[which(bigmo$Item_Type=="Dairy")]<-5
bigmo$Item_Type[which(bigmo$Item_Type=="Frozen Foods")]<-6
bigmo$Item_Type[which(bigmo$Item_Type=="Fruits and Vegetables")]<-7
bigmo$Item_Type[which(bigmo$Item_Type=="Hard Drinks")]<-8
bigmo$Item_Type[which(bigmo$Item_Type=="Health and Hygiene")]<-9
bigmo$Item_Type[which(bigmo$Item_Type=="Household")]<-10
bigmo$Item_Type[which(bigmo$Item_Type=="Meat")]<-11
bigmo$Item_Type[which(bigmo$Item_Type=="Others")]<-12
bigmo$Item_Type[which(bigmo$Item_Type=="Seafood")]<-13
bigmo$Item_Type[which(bigmo$Item_Type=="Snack Foods")]<-14
bigmo$Item_Type[which(bigmo$Item_Type=="Soft Drinks")]<-15
bigmo$Item_Type[which(bigmo$Item_Type=="Starchy Foods")]<-16
bigmo$Item_Type<-as.numeric(bigmo$Item_Type)
bigmo$Outlet_Establishment_Year<-as.numeric(bigmo$Outlet_Establishment_Year)
bigmo$Outlet_Establishment_Year[which(bigmo$Outlet_Establishment_Year==1985)]<-1
bigmo$Outlet_Establishment_Year[which(bigmo$Outlet_Establishment_Year==1987)]<-2
bigmo$Outlet_Establishment_Year[which(bigmo$Outlet_Establishment_Year==1997)]<-3
bigmo$Outlet_Establishment_Year[which(bigmo$Outlet_Establishment_Year==1998)]<-4
bigmo$Outlet_Establishment_Year[which(bigmo$Outlet_Establishment_Year==1999)]<-5
bigmo$Outlet_Establishment_Year[which(bigmo$Outlet_Establishment_Year==2002)]<-6
bigmo$Outlet_Establishment_Year[which(bigmo$Outlet_Establishment_Year==2004)]<-7
bigmo$Outlet_Establishment_Year[which(bigmo$Outlet_Establishment_Year==2007)]<-8
bigmo$Outlet_Establishment_Year[which(bigmo$Outlet_Establishment_Year==2009)]<-9
bigmo$Outlet_Location_Type<-as.character(bigmo$Outlet_Location_Type)
bigmo$Outlet_Location_Type[which(bigmo$Outlet_Location_Type=="Tier 1")]<-1
bigmo$Outlet_Location_Type[which(bigmo$Outlet_Location_Type=="Tier 2")]<-2
bigmo$Outlet_Location_Type[which(bigmo$Outlet_Location_Type=="Tier 3")]<-3
bigmo$Outlet_Location_Type<-as.numeric(bigmo$Outlet_Location_Type)
bigmo$Outlet_Type<-as.character(bigmo$Outlet_Type)
bigmo$Outlet_Type[which(bigmo$Outlet_Type=="Grocery Store")]<-1
bigmo$Outlet_Type[which(bigmo$Outlet_Type=="Supermarket Type1")]<-2
bigmo$Outlet_Type[which(bigmo$Outlet_Type=="Supermarket Type2")]<-3
bigmo$Outlet_Type[which(bigmo$Outlet_Type=="Supermarket Type3")]<-4
bigmo$Outlet_Type<-as.numeric(bigmo$Outlet_Type)
bigmo$Outlet_Identifier<-as.character(bigmo$Outlet_Identifier)
bigmo$Outlet_Identifier[which(bigmo$Outlet_Identifier=="OUT010")]<-1
bigmo$Outlet_Identifier[which(bigmo$Outlet_Identifier=="OUT013")]<-2
bigmo$Outlet_Identifier[which(bigmo$Outlet_Identifier=="OUT017")]<-3
bigmo$Outlet_Identifier[which(bigmo$Outlet_Identifier=="OUT018")]<-4
bigmo$Outlet_Identifier[which(bigmo$Outlet_Identifier=="OUT019")]<-5
bigmo$Outlet_Identifier[which(bigmo$Outlet_Identifier=="OUT027")]<-6
bigmo$Outlet_Identifier[which(bigmo$Outlet_Identifier=="OUT035")]<-7
bigmo$Outlet_Identifier[which(bigmo$Outlet_Identifier=="OUT045")]<-8
bigmo$Outlet_Identifier[which(bigmo$Outlet_Identifier=="OUT046")]<-9
bigmo$Outlet_Identifier[which(bigmo$Outlet_Identifier=="OUT049")]<-10
bigmo$Outlet_Identifier<-as.numeric(bigmo$Outlet_Identifier)
str(bigmo)#<--------------------------------------------------------------------------------

#MV_(Outlet_Size)_KNN_Sampling
bigmo_nomissing<-bigmo[-which(is.na(bigmo$Outlet_Size)),]#<---
bigmo_test<-bigmo[which(is.na(bigmo$Outlet_Size)),-7]#<---
set.seed(100)#<---
index_bigmo<-sample(1:nrow(bigmo_nomissing),0.70*nrow(bigmo_nomissing),replace = F)#<---
bigmo_train<-bigmo_nomissing[index_bigmo,]#<---
bigmo_vali<-bigmo_nomissing[-index_bigmo,]#<---

#MV_(Outlet_Size)_KNN_(Extracting train and vali class labels)
bigmo_train_class<-bigmo_train$Outlet_Size#<---
bigmo_vali_class<-bigmo_vali$Outlet_Size#<---
bigmo_train<-bigmo_train[,-7]#<---
bigmo_vali<-bigmo_vali[,-7]#<---

#MV_(Outlet_Size)_KNN_Normalize(Train, Vali, Test)
normalize<-function(x){#<----
  return((x-min(x))/(max(x)-min(x)))
}
bigmo_train<-as.data.frame(lapply(bigmo_train,normalize))#<---
bigmo_vali<-as.data.frame(lapply(bigmo_vali,normalize))#<---
bigmo_test<-as.data.frame(lapply(bigmo_test,normalize))#<---

#MV_(Outlet_Size)_KNN

vali_pred_bigmo<-knn(bigmo_train,bigmo_vali,cl=bigmo_train_class,k=26)
confusionMatrix(vali_pred_bigmo,bigmo_vali_class)
test_pred_bigmo<-knn(bigmo_train,bigmo_test,cl=bigmo_train_class,k=26)#<---
bigm$Outlet_Size[which(is.na(bigm$Outlet_Size))]<-test_pred_bigmo#<---

#MV_(Outlet_Size)_KNN_(Imputation_check and changes)
table(bigm$Outlet_Identifier,bigm$Outlet_Size)

summarizeColumns(bigm)

#MV_(Item_Visibility)
#MV_(Item_Visibility)_Outlier
bigmv<-bigm[,-1]#<---

boxplot(bigmv$Item_Weight)
boxplot(bigmv$Item_Visibility)
boxplot(bigmv$Item_MRP)
boxplot(bigmv$Item_Outlet_Sales)

#MV_(Item_Visibility)_Outlier_(Item_Visibility)
boxplot((bigmv$Item_Visibility)^(1/3))
bigmv$Item_Visibility<-(bigmv$Item_Visibility)^(1/3)#<---
quantile(bigmo$Item_Visibility,0.75,na.rm = TRUE)+(1.5*IQR(bigmo$Item_Visibility,na.rm = T))
bigmv$Item_Visibility[which(bigmv$Item_Visibility>0.6797936)]<-NA#<---
length(bigmv$Item_Visibility[which(is.na(bigmv$Item_Visibility))])

#MV_(Item_Visibility)_Outlier_(Item_Outlet_Sales)
boxplot((bigmv$Item_Outlet_Sales)^(1/3))
bigmv$Item_Outlet_Sales<-(bigmv$Item_Outlet_Sales)^(1/3)#<---
IQR(bigmv$Item_Outlet_Sales)*1.5+quantile(bigmv$Item_Outlet_Sales,0.75)
bigmv$Item_Outlet_Sales[which(bigmv$Item_Outlet_Sales>22.33688)]<-NA#<---
length(bigmv$Item_Outlet_Sales[which(is.na(bigmv$Item_Outlet_Sales))])
mean(bigmv$Item_Outlet_Sales,na.rm = T)
bigmv$Item_Outlet_Sales[which(is.na(bigmv$Item_Outlet_Sales))]<-11.98796#<---
length(bigmv$Item_Outlet_Sales[which(is.na(bigmv$Item_Outlet_Sales))])

#MV_(Item_Visibility)_BA
#MV_(Item_Visibility)_BA(Cont-cont)
#MV_(Item_Visibility)_BA(Cont-cont)_(Item_Visibility vs Item_Outlet_Sales)
cor.test(bigmv$Item_Visibility,bigmv$Item_Outlet_Sales)
#MV_(Item_Visibility)_BA(Cont-cat)_(Item_Visibility vs Item_Outlet_Sales)
f<-aov(Item_Visibility~Item_Type,data = bigmv)
summary(f)
f<-aov(Item_Visibility~Outlet_Identifier,data = bigmv)
summary(f)
f<-aov(Item_Visibility~Outlet_Type,data = bigmv)
summary(f)
f<-aov(Item_Visibility~Outlet_Establishment_Year,data = bigmv)
summary(f)
f<-aov(Item_Visibility~Item_Fat_Content,data = bigmv)
summary(f)
f<-aov(Item_Visibility~Outlet_Size,data = bigmv)
summary(f)
f<-aov(Item_Visibility~Outlet_Location_Type,data = bigmv)
summary(f)


str(bigmv)

#MV_(Item_Visibility)_(preparing the dataset converting all variables into numeric for ML imputation)
bigmv_dummy<-dummy.data.frame(bigmv[,c(-1,-3,-5,-11,-2,-8,-9)])#<---
bigmv_dummy<-as.data.frame(lapply(bigmv_dummy,FUN = as.numeric))#<---
bigmv_dummy<-cbind(bigmv[,c(1,3,5,11,2,8,9)],bigmv_dummy)#<---
for(i in 5:7){#<---
  bigmv_dummy[,i]<-as.character(bigmv_dummy[,i])
}
bigmv_dummy$Item_Fat_Content<-ifelse(bigmv$Item_Fat_Content=="Regular",1,0)#<---
bigmv_dummy$Outlet_Size[which(bigmv_dummy$Outlet_Size=="High")]<-2#<---
bigmv_dummy$Outlet_Size[which(bigmv_dummy$Outlet_Size=="Medium")]<-1#<---
bigmv_dummy$Outlet_Size[which(bigmv_dummy$Outlet_Size=="Small")]<-0#<---
bigmv_dummy$Outlet_Location_Type[which(bigmv_dummy$Outlet_Location_Type=="Tier 1")]<-2#<---
bigmv_dummy$Outlet_Location_Type[which(bigmv_dummy$Outlet_Location_Type=="Tier 2")]<-1#<---
bigmv_dummy$Outlet_Location_Type[which(bigmv_dummy$Outlet_Location_Type=="Tier 3")]<-0#<---
bigmv_dummy[,6]<-as.numeric(bigmv_dummy[,6])#<---
bigmv_dummy[,7]<-as.numeric(bigmv_dummy[,7])#<---
bigmv_pprocess<-preProcess(bigmv_dummy,method = "range")#<---
bigmv_dummy<-predict(bigmv_pprocess,bigmv_dummy)#<---

#MV_(Item_Visibility)_(preparing the dataset converting all variables into numeric for ML imputation)_Sampling
bigmv_nomissing<-bigmv_dummy[-which(is.na(bigmv_dummy$Item_Visibility)),]#<---
bigmv_test<-bigmv_dummy[which(is.na(bigmv_dummy$Item_Visibility)),]#<---
bigmv_test<-bigmv_test[,-2]#<---
set.seed(111)#<---
index_bigmv<-sample(1:nrow(bigmv_nomissing),0.70*nrow(bigmv_nomissing),replace = F)#<---
bigmv_train<-bigmv_nomissing[index_bigmv,]#<---
bigmv_vali<-bigmv_nomissing[-index_bigmv,]#<---


#MV_(Item_Visibility)_KNN modeling_train
bigmv_trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)#<---
bigmv_metric <- "RMSE"#<---
set.seed(7)#<---
mvfit<- train(Item_Visibility~., data=bigmv_train,method="knn", metric=bigmv_metric,trControl=bigmv_trainControl)#<---
print(mvfit)

#MV_(Item_Visibility)_KNN modeling_validating
bigmv_target_vali<-bigmv_vali[,2]
bigmv_target_pred_vali<-predict(mvfit,bigmv_vali[,-2],k=9)
bigmv_vali_RMSE<-RMSE(bigmv_target_pred_vali,bigmv_target_vali)

#MV_(Item_Visibility)_KNN_Validation
bigmv_pred_test<-predict(mvfit,bigmv_test,k=9)#<---
bigmv$Item_Visibility[which(is.na(bigmv$Item_Visibility))]<-bigmv_pred_test#<---
bigmv$Item_Visibility<-(bigmv$Item_Visibility)^3#<---
bigm$Item_Visibility<-bigmv$Item_Visibility#<---

summarizeColumns(bigm)

library(mlr)

summarizeColumns(bigm)


#Data Exploration for all variables for modeling
#Model_UV Analysis
#Model_UV Analysis_ContinuousV
#Model_UV Analysis_ContinuousV_Histogram
par(mfrow=c(2,2))
for (i in c(2,4,6,12)) {
  hist(bigm[,i],main = names(bigm)[i])
}

#Model_UV Analysis_ContinuousV_Density plot
par(mfrow=c(2,2))
for (i in c(2,4,6,12)) {
  plot(density(bigm[,i]),main = names(bigm)[i])
}

#Model_UV Analysis_ContinuousV_Boxplot
for (i in c(2,4,6,12)) {
  boxplot(bigm[,i],main = names(bigm)[i])
}

Item_Visibility<-bigm[order(-bigm$Item_Visibility),4]
Item_Outlet_Sales<-bigm[order(-bigm$Item_Outlet_Sales),12]
head(Item_Visibility, 100)
head(Item_Outlet_Sales, 100)

#Model_UV Analysis_CategoricalV_

par(mfrow=c(3,3))
for (i in c(3,5,7,8,9,10,11)) {
  barplot(table(bigm[,i]),main = names(bigm)[i],col = bigm[,i])
}
dev.off()

for (i in c(3,5,7,8,9,10,11)) {
print(cbind(freq=table(bigm[,i]),prob=prop.table(table(bigm[,i]))*100))
}

#Model_BV_Cont-cont
bigm_corr<-cor(bigm[,c(2,4,6,12)])
corrplot(bigm_corr,method = "circle")
cor.test(bigm$Item_Weight,bigm$Item_Visibility)
cor.test(bigm$Item_Weight,bigm$Item_MRP)
cor.test(bigm$Item_Weight,bigm$Item_Weight)
cor.test(bigm$Item_Visibility,bigm$Item_MRP)
cor.test(bigm$Item_Visibility,bigm$Item_Outlet_Sales)
cor.test(bigm$Item_MRP,bigm$Item_Outlet_Sales)
pairs(bigm[,c(2,4,6,12)])
pairs(bigm[,c(2,4,6,12)], col=bigm$Outlet_Location_Type)

#Model_BV_(Cat-cat)
chisq.test(bigm$Outlet_Size,bigm$Item_Type)
chisq.test(bigm$Outlet_Size,bigm$Item_Fat_Content)
outlet_unique<-unique(bigm[,c(9,11,7)])
chisq.test(outlet_unique$Outlet_Size,outlet_unique$Outlet_Type)
chisq.test(bigm$Item_Type,bigm$Item_Fat_Content)

#Model_Outliers treatment
bigm$Item_Visibility<-(bigm$Item_Visibility)^(1/3)#<---
bigm$Item_Visibility[which(bigm$Item_Visibility>0.6797936)]<-NA#<---
mean(bigm$Item_Visibility,na.rm = T)
bigm$Item_Visibility[which(is.na(bigm$Item_Visibility))]<-0.3932236#<---

bigm$Item_Outlet_Sales<-(bigm$Item_Outlet_Sales)^(1/3)#<---
bigm$Item_Outlet_Sales[which(bigm$Item_Outlet_Sales>22.33688)]<-NA#<---
bigm$Item_Outlet_Sales[which(is.na(bigm$Item_Outlet_Sales))]<-11.98796#<---