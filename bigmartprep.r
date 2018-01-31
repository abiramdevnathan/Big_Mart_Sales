getwd()
setwd("D:\\Media\\Documents\\Data Science\\Repositories\\Big mart sales")

big<-read.csv("Train.csv", header = TRUE, na.strings = c("","NA","-"))#<---
bigm<-big#<---
bigm$Item_Fat_Content[which(bigm$Item_Fat_Content=="LF"|bigm$Item_Fat_Content=="low fat")]<-"Low Fat"#<---
bigm$Item_Fat_Content[which(bigm$Item_Fat_Content=="reg")]<-"Regular"#<---
bigm$Outlet_Establishment_Year<-as.factor(bigm$Outlet_Establishment_Year)#<---
array_Item_Identifier<-bigm$Item_Identifier#<---
Item_unique<-unique(bigm[c("Item_Identifier","Item_Fat_Content","Item_Type")])
Outlet_unique<-unique(bigm[c("Outlet_Identifier","Outlet_Type","Outlet_Establishment_Year","Outlet_Size","Outlet_Location_Type")])
i=1#<---
for (i in 1:length(array_Item_Identifier)) {#<---
  item_weight_val<-bigm$Item_Weight[which(bigm$Item_Identifier==array_Item_Identifier[i])]
  avg_item<-mean(item_weight_val,na.rm = T)
  bigm$Item_Weight[which(is.na(bigm$Item_Weight)&bigm$Item_Identifier==array_Item_Identifier[i])]<-avg_item
}

#____Outlier treatment______
#Item_Visibility
bigm$Item_Visibility[which(bigm$Item_Visibility==0)]<-NA#<---
boxplot(bigm$Item_Visibility)
boxplot((bigm$Item_Visibility)^(1/3))
items_for_contv<-bigm[,c("Item_Weight","Item_Visibility","Item_MRP","Item_Outlet_Sales")]#<---
items_for_contv$cubeVisi<-(bigm$Item_Visibility)^(1/3)#<---
quantile(items_for_contv$cubeVisi,0.75,na.rm = TRUE)+(1.5*IQR(items_for_contv$cubeVisi,na.rm = T))
items_for_contv$cubeVisi[which(items_for_contv$cubeVisi>0.6797936)]<-NA#<---
bigm$Item_Visibility<-items_for_contv$cubeVisi#<---

pairs(items_for_contv[,-2])


#Item_Outlet_Sales
boxplot(bigm$Item_Outlet_Sales)
boxplot((bigm$Item_Outlet_Sales)^(1/3))
items_for_contv$cubesales<-(bigm$Item_Outlet_Sales)^(1/3)#<---
IQR(bigm$Item_Outlet_Sales)*1.5+quantile(bigm$Item_Outlet_Sales,0.75)
length(bigm$Item_Outlet_Sales[which(bigm$Item_Outlet_Sales>6501.87)])
IQR(items_for_contv$cubesales)*1.5+quantile(items_for_contv$cubesales,0.75)
length(items_for_contv$cubesales[which(items_for_contv$cubesales>22.33688)])

items_for_contv$cubesales[which(items_for_contv$cubesales>22.33688)]<-NA#<---
bigm$Item_Outlet_Sales<-items_for_contv$cubesales#<---
mean(bigm$Item_Outlet_Sales,na.rm = T)
bigm$Item_Outlet_Sales[which(is.na(bigm$Item_Outlet_Sales))]<-11.98796#<---
pairs(items_for_contv[,-c(2,4)])

#_____Bivariate analysis Continuous-Continuous after outlier treatment___
cor.test(bigm$Item_Weight,bigm$Item_Visibility)
cor.test(bigm$Item_Weight,bigm$Item_MRP)
cor.test(bigm$Item_Weight,bigm$Item_Outlet_Sales)
cor.test(bigm$Item_Visibility,bigm$Item_MRP)
cor.test(bigm$Item_Visibility,bigm$Item_Outlet_Sales)
cor.test(bigm$Item_MRP,bigm$Item_Outlet_Sales)

#____Missing values imputation________
summarizeColumns(bigm)
#Treating outlet size
#Anova test for outlet size(without missing values) vs all cont variables
bigm_for_osize_missing<-bigm[-which(is.na(bigm$Outlet_Size)),]
f<-aov(Item_Weight~Outlet_Size,data=bigm_for_osize_missing)
summary(f)
f<-aov(Item_Visibility~Outlet_Size,data=bigm_for_osize_missing[-which(is.na(bigm_for_osize_missing$Item_Visibility)),])
summary(f)
f<-aov(Item_MRP~Outlet_Size,data=bigm_for_osize_missing)
summary(f)
f<-aov(Item_Outlet_Sales~Outlet_Size,data = bigm_for_osize_missing)
summary(f)

#Considering Item_Outlet_Sales for outlet_size
tapply(bigm_for_osize_missing$Item_Outlet_Sales,bigm_for_osize_missing$Outlet_Size,mean)
mean(bigm$Item_Outlet_Sales[which(bigm$Outlet_Identifier=="OUT010")])
mean(bigm$Item_Outlet_Sales[which(bigm$Outlet_Identifier=="OUT045")])
mean(bigm$Item_Outlet_Sales[which(bigm$Outlet_Identifier=="OUT017")])

#Considering Item_Visibility for outlet_size
tapply(bigm_for_osize_missing$Item_Visibility[-which(is.na(bigm_for_osize_missing$Item_Visibility))
                                              ],bigm_for_osize_missing$Outlet_Size[-which(is.na(bigm_for_osize_missing$Item_Visibility))],mean)
mean(bigm$Item_Visibility[which(bigm$Outlet_Identifier=="OUT010")],na.rm = T)
mean(bigm$Item_Visibility[which(bigm$Outlet_Identifier=="OUT045")],na.rm=T)
mean(bigm$Item_Visibility[which(bigm$Outlet_Identifier=="OUT017")],na.rm = T)       

#Imputation
bigm$Outlet_Size[which(bigm$Outlet_Identifier=="OUT010")]<-"Small"#<--- 
bigm$Outlet_Size[which(bigm$Outlet_Identifier=="OUT017")]<-"High"#<---
bigm$Outlet_Size[which(bigm$Outlet_Identifier=="OUT045")]<-"High"#<---

#Treating Item_Visibility




length(bigm$Outlet_Size[which(is.na(bigm$Outlet_Size))])
test<-bigm[which(bigm$Item_Visibility==0),]

