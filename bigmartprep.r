getwd()
setwd("D:\\Media\\Documents\\Data Science\\Repositories\\Big mart sales")

library(mlr)
library(caret)

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
library(class)
library(caret)
vali_pred_bigmo<-knn(bigmo_train,bigmo_vali,cl=bigmo_train_class,k=26)
confusionMatrix(vali_pred_bigmo,bigmo_vali_class)
test_pred_bigmo<-knn(bigmo_train,bigmo_test,cl=bigmo_train_class,k=26)#<---
bigm$Outlet_Size[which(is.na(bigm$Outlet_Size))]<-test_pred_bigmo#<---

#MV_(Outlet_Size)_KNN_(Imputation_check and changes)
table(bigm$Outlet_Identifier,bigm$Outlet_Size)


