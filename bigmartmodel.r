library(mlr)
library(dummies)
library(caret)
library(Cubist)
library(ggplot2)
library(class)
summarizeColumns(bigm)
bigmm<-bigm
bigmm<-bigmm[,-1]
str(bigmm)

#To create dummy variables for columns: 2,4,6,7,8,9,10
bigmm_dummy<-dummy.data.frame(bigmm[,c(2,4,6,7,8,9,10)])
bigmm<-cbind(bigmm[,c(1,3,5,11)],bigmm_dummy)
str(bigmm)
for (i in (5:51)) {
  bigmm[,i]<-as.numeric(bigmm[,i])
  
}

#Sampling
set.seed(57)
index_model<-sample(1:nrow(bigmm),0.70*nrow(bigmm),replace = F)
bigm_train<-bigmm[index_model,]
bigm_vali<-bigmm[-index_model,]

str(bigm_train)

#Data transformation
par(mfrow=c(2,2))
for(i in 1:4){
  plot(density(bigm_train[,i]))
}

preprocessparams<-preProcess(bigm_train,method = c("center","scale","BoxCox"))
bigm_traintt<-predict(preprocessparams,bigm_train)

par(mfrow=c(2,2))
for(i in 1:4){
  plot(density(bigm_traintt[,i]))
}

#Apply standardize, boxcox transform

#Looking for correlations and removing
set.seed(50)
cutoff<-0.70
correlations<-cor(bigm_train[,1:51])
highllycorrelated<-findCorrelation(correlations,cutoff = cutoff)
print(names(bigm_train[,highllycorrelated]))

bigm_train<-bigm_train[,-highllycorrelated]

#Model Spot check
model_tc<-trainControl(method = "repeatedcv",number = 10, repeats = 3,verboseIter = TRUE)
metric<-"RMSE"

#LM
set.seed(6)
fit.lm<-train(Item_Outlet_Sales~.,data = bigm_train,method = "lm",trControl = model_tc,metric = metric,preProc=c("center","scale","BoxCox"))

#GLM
set.seed(6)
fit.glm<-train(Item_Outlet_Sales~.,data = bigm_train,method = "glm",trControl = model_tc,metric = metric, preProc =c("center","scale","BoxCox"))

#GLMNET
set.seed(6)
fit.glmnet<-train(Item_Outlet_Sales~.,data = bigm_train,method = "glmnet",trControl = model_tc,metric = metric, preProc =c("center","scale","BoxCox"))

#KNN 
set.seed(6)
fit.knn<-train(Item_Outlet_Sales~.,data = bigm_train,method = "knn",trControl = model_tc,metric = metric, preProc =c("center","scale","BoxCox"))

#SVM
set.seed(6)
fit.svm<-train(Item_Outlet_Sales~.,data = bigm_train,method="svmRadial",metric=metric, trControl=model_tc,
               preProc =c("center","scale","BoxCox"))

#Ensembling
#Random Forest
set.seed(6)
fit.rf<-train(Item_Outlet_Sales~.,data = bigm_train,method = "rf",trControl = model_tc,metric = metric, preProc ="BoxCox")

#Gradient boosting
set.seed(6)
fit.gbm<-train(Item_Outlet_Sales~.,data = bigm_train,method = "gbm",trControl = model_tc,metric = metric, preProc ="BoxCox")

#XGBoost
set.seed(6)
fit.xgb<-train(Item_Outlet_Sales~.,data=bigm_train,method="xgbLinear",trControl=model_tc,metric=metric,
               preProc="BoxCox")

#Cubist
set.seed(6)
fit.cubist<-train(Item_Outlet_Sales~.,data = bigm_train,method = "cubist",trControl = model_tc,metric = metric, preProc ="BoxCox")

results<-resamples(list(LM=fit.lm, GLM=fit.glm, GLMNET=fit.glmnet,
                        KNN=fit.knn, XGBoost=fit.xgb, Cubist=fit.cubist, svm=fit.svm))

dotplot(results)
summary(results)

#Confirming Cubist and tuning

print(fit.cubist)
grid<-expand.grid(.committees=seq(15,25,by=1),.neighbors=c(0,3,5,7))
tune.cubist<-train(Item_Outlet_Sales~.,data = bigm_train,method="cubist",metric=metric,preProc="BoxCox",
                   tuneGrid=grid,trControl=model_tc)

print(tune.cubist)

#Applying the model in validation set
#transforming train set
set.seed(6)
x_train<-bigm_train[,-4]
y_train<-bigm_train[,4]
preproctrain<-preProcess(x_train,method = "BoxCox")
trans_x_train<-predict(preproctrain,x_train)


#final model
finalModel<-cubist(x=trans_x_train,y=(y_train),committees = 23)
summary(finalModel)

#transforming validation set
set.seed(6)
bigm_vali<-bigm_vali[,-highllycorrelated]
x_vali<-bigm_vali[,-4]
y_vali<-bigm_vali[,4]
trans_x_vali<-predict(preproctrain,x_vali)



#applying the final model
pred_train<-predict(finalModel,newdata = trans_x_train,neighbors = 0)
train_rmse<-RMSE(pred_train,y_train)
train_rmse

pred_vali<-predict(finalModel,newdata = trans_x_vali,neighbors = 0)
vali_rmse<-RMSE(pred_vali,y_vali)
vali_rmse
final<-data.frame(pred_vali,y_vali)
head(cbind(pred_vali,y_vali),50)

#Actual vs Predicted
p<-ggplot(final,aes(x=row(final)[,2],y=pred_vali))
p+geom_line(colour="red")+geom_line(data=final,aes(y=final[,2]),colour="white")

write.csv(final,"D:\\Media\\Documents\\Abi\\Data Science\\Repositories\\vali.csv")



#Applying the model for test dataset
#Preparing the text dataset
bigmt<-read.csv("D:\\Media\\Documents\\Abi\\Data Science\\Repositories\\Big mart sales\\Test.csv",na.strings = c("NA","","-"))
summarizeColumns(bigmt)

#test_(Outlet_Size)_m
bigmto<-bigmt
bigmto<-bigmto[,c(-1,-2)]
str(bigmto)
bigmto$Outlet_Establishment_Year<-as.factor(bigmto$Outlet_Establishment_Year)

bigmto<-dummy.data.frame(bigmto[,-7])
bigmto$Outlet_Size<-bigmt$Outlet_Size
str(bigmto)
for(i in 1:49){
  bigmto[,i]<-as.numeric(bigmto[,i])
}

#spliting into train and test
#To find correlations
set.seed(51)
bigmtocorr<-cor(bigmto[1:49])
bigmtocorfind<-findCorrelation(bigmtocorr,cutoff = cutoff)
bigmto<-bigmto[,-bigmtocorfind]
bigmto_nomissing<-bigmto[-which(is.na(bigmto$Outlet_Size)),]
bigmto_test<-bigmto[which(is.na(bigmto$Outlet_Size)),]

set.seed(52)
bigmto_index<-sample(1:nrow(bigmto_nomissing),size = 0.70*nrow(bigmto_nomissing),replace = FALSE)
bigmto_train<-bigmto_nomissing[bigmto_index,]
bigmto_vali<-bigmto_nomissing[-bigmto_index,]

#KNN imputation
bigmtotc<-trainControl(method = "repeatedcv",number = 10,repeats = 3,verboseIter = TRUE)
bigmto.fit<-train(Outlet_Size~.,data=bigmto_train,trControl=bigmtotc,preProc=c("scale","center","BoxCox"),method="knn")
grid<-expand.grid(.k=seq(2,8,by=1))
bigmto.fit<-train(Outlet_Size~.,data=bigmto_train,trControl=bigmtotc,preProc=c("scale","center","BoxCox"),method="knn",
                  tuneGrid=grid)

#Finalizing k=7
bigmto_pred<-knn(bigmto_train[,-38],bigmto_vali[,-38],cl=bigmto_train$Outlet_Size,k=7)
bigmto_cm<-confusionMatrix(bigmto_pred,bigmto_vali$Outlet_Size)
bigmto_cm

#Imputing missing values
bigmto_final_pred<-knn(bigmto_train[,-38],bigmto_test[,-38],cl=bigmto_train$Outlet_Size,k=7)
bigmt$Outlet_Size[which(is.na(bigmt$Outlet_Size))]<-bigmto_final_pred

summarizeColumns(bigmt)

#test_(Item_weight)_missing
bigmti<-bigmt
bigmti<-bigmti[,-1]
str(bigmti)
bigmti$Outlet_Establishment_Year<-as.factor(bigmti$Outlet_Establishment_Year)

bigmti_dummy<-dummy.data.frame(bigmti)
str(bigmti_dummy)
for(i in 1:53){
  bigmti_dummy[,i]<-as.numeric(bigmti_dummy[,i])
}
bigmti<-bigmti_dummy
str(bigmti)

#find correlation for Item_weigth
bigmticor<-cor(bigmti[2:53])
bigmtifindcor<-findCorrelation(bigmticor,cutoff = 0.70)
bigmti<-bigmti[,-bigmtifindcor]

#train, text, vali
bigmti_nomissing<-bigmti[-which(is.na(bigmti$Item_Weight)),]
bigmti_test<-bigmti[which(is.na(bigmti$Item_Weight)),]

set.seed(53)
bigmti_index<-sample(1:nrow(bigmti_nomissing),0.70*nrow(bigmti_nomissing),replace = FALSE)
bigmti_train<-bigmti_nomissing[bigmti_index,]
bigmti_vali<-bigmti_nomissing[-bigmti_index,]



#KNN imputation
bigmtitc<-trainControl(method = "repeatedcv",number = 10,repeats = 3,verboseIter = TRUE)
bigmti.fit<-train(Item_Weight~.,data=bigmti_train,trControl=bigmtitc,preProc=c("scale","center","BoxCox"),method="knn")
grid<-expand.grid(.k=seq(7,11,by=1))
bigmti.fit<-train(Item_Weight~.,data=bigmti_train,trControl=bigmtitc,preProc=c("scale","center","BoxCox"),method="knn",
                  tuneGrid=grid)

#Finalizing k=11
bigmti_pred<-knn(bigmti_train[,-1],bigmti_vali[,-1],cl=bigmti_train$Item_Weight,k=11)
bigmti_RMSE<-RMSE(as.numeric(bigmti_pred),bigmti_vali$Item_Weight)
bigmti_RMSE

bigmti_final_pred<-knn(bigmti_train[,-1],bigmti_test[,-1],cl=bigmti_train$Item_Weight,k=11)
bigmt$Item_Weight[which(is.na(bigmt$Item_Weight))]<-bigmti_final_pred

summarizeColumns(bigmt)
summary(bigmt$Item_Fat_Content)
summary(bigm$Item_Fat_Content)
bigmt$Item_Fat_Content[which(bigmt$Item_Fat_Content=="LF")]<-"Low Fat"
bigmt$Item_Fat_Content[which(bigmt$Item_Fat_Content=="low fat")]<-"Low Fat"
bigmt$Item_Fat_Content[which(bigmt$Item_Fat_Content=="reg")]<-"Regular"

bigmtf<-bigmt
bigmtf<-bigmtf[,-1]
str(bigmtf)
bigmtf$Outlet_Establishment_Year<-as.factor(bigmtf$Outlet_Establishment_Year)
bigmtf<-dummy.data.frame(bigmtf)
str(bigmtf)
for(i in 1:50){
  bigmtf[,i]<-as.numeric(bigmtf[,i])
}
str(bigm_train)
bigmtf<-bigmtf[,-which(names(bigmtf)%in%c("Outlet_SizeMedium", "Outlet_SizeHigh","Outlet_Establishment_Year1985",
                                          "Outlet_IdentifierOUT027", "Outlet_IdentifierOUT018", "Outlet_Establishment_Year2009",
                                          "Outlet_IdentifierOUT010", "Outlet_IdentifierOUT013", "Outlet_IdentifierOUT046",      
                                          "Outlet_IdentifierOUT049","Outlet_IdentifierOUT035", "Outlet_IdentifierOUT017",      
                                          "Outlet_IdentifierOUT045", "Item_Fat_ContentLow Fat"))]



#Applying the model to test dataset
trans_test<-predict(preproctrain,bigmtf)
finalpred<-predict(finalModel,newdata = trans_test,neighbors = 0)
resultdataset<-data.frame(Item_Identifier=bigmt$Item_Identifier,
                                Outlet_Identifier=bigmt$Outlet_Identifier,Item_Outlet_Sales=finalpred)
write.csv(resultdataset,"D:\\Media\\Documents\\Abi\\Data Science\\Repositories\\Big mart sales\\result.csv",row.names=FALSE)

head(resultdataset)
