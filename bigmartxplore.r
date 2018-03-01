getwd()
setwd("C:\\Users\\HP\\Documents\\Data Science\\Repositories\\Big mart sales")

bigm<-read.csv("Train.csv", header = TRUE, na.strings = c("","NA","-"))#<---

summary(bigm$Item_Fat_Content)
summary(bigm$Outlet_Size)
summary(bigm$Outlet_Size)
summary(bigm$Outlet_Location_Type)

#_____________________Data Exploration______________
#Univariate analysis_____________
#For Continous variables____
par(mfrow=c(1,4))

boxplot(bigm$Item_Weight, col="Blue")
abline(h=mean(bigm$Item_Weight,na.rm = T),lwd=2, col="yellow")

boxplot(bigm$Item_Visibility,col="red")
abline(h=mean(bigm$Item_Visibility,na.rm = T),lwd=2, col="yellow")

boxplot(bigm$Item_MRP,col = "pink")
abline(h=mean(bigm$Item_MRP,na.rm = T),lwd=2, col="yellow")

boxplot(bigm$Item_Outlet_Sales,col = "magenta")
abline(h=mean(bigm$Item_Outlet_Sales,na.rm = T),lwd=2, col="yellow")

dev.off()

#For categorical varaibles____
library(mlr)
length(unique(bigm$Item_Identifier))

summary(bigm$Item_Type)
table(bigm$Item_Type)
prop.table(table(bigm$Item_Type))*100

length(unique(bigm$Outlet_Identifier))
summary(bigm$Outlet_Type)
Outlet_unique<-unique(bigm[c("Outlet_Identifier","Outlet_Type","Outlet_Establishment_Year","Outlet_Size","Outlet_Location_Type")])
table(Outlet_unique$Outlet_Type)

table(Outlet_unique$Outlet_Establishment_Year)

table(Outlet_unique$Outlet_Type,Outlet_unique$Outlet_Establishment_Year)

class(bigm$Item_Fat_Content)
summary(bigm$Item_Fat_Content)

bigm$Item_Fat_Content[which(bigm$Item_Fat_Content=="LF"|bigm$Item_Fat_Content=="low fat")]<-"Low Fat"#<---
bigm$Item_Fat_Content[which(bigm$Item_Fat_Content=="reg")]<-"Regular"#<---
Item_unique<-unique(bigm[c("Item_Identifier","Item_Fat_Content","Item_Type")])
summary(Item_unique$Item_Fat_Content)

table(Outlet_unique$Outlet_Location_Type)

#Bivariate analysis_________
#Continuous-continuous_
plot(bigm$Item_Weight,bigm$Item_Visibility)
library(caret)
bigm_for_contv<-bigm[,c("Item_Weight","Item_Visibility","Item_MRP","Item_Outlet_Sales")]
pairs(bigm_for_contv)
cor(bigm_for_contv)
cor.test(bigm_for_contv$Item_Weight,bigm_for_contv$Item_Visibility)
cor.test(bigm_for_contv$Item_Weight,bigm_for_contv$Item_MRP)
cor.test(bigm_for_contv$Item_Weight,bigm_for_contv$Item_Outlet_Sales)
cor.test(bigm_for_contv$Item_Visibility,bigm_for_contv$Item_MRP)
cor.test(bigm_for_contv$Item_Visibility,bigm_for_contv$Item_Outlet_Sales)
cor.test(bigm_for_contv$Item_MRP,bigm_for_contv$Item_Outlet_Sales)

#Categorical-Categorical__
#Item_Identifier vs all
itype_itemid<-table(Item_unique$Item_Type,Item_unique$Item_Identifier)
chisq.test(itype_itemid)
itemid_oid<-table(bigm$Item_Identifier,bigm$Outlet_Identifier)
chisq.test(itemid_oid)
itemid_otype<-table(bigm$Item_Identifier,bigm$Outlet_Type)
chisq.test(itemid_otype)
summary(bigm$Outlet_Type)
class(bigm$Outlet_Establishment_Year)
bigm$Outlet_Establishment_Year<-as.factor(bigm$Outlet_Establishment_Year)#<---
itemid_oestyear<-table(bigm$Item_Identifier,bigm$Outlet_Establishment_Year)
chisq.test(itemid_oestyear)
summary(bigm$Outlet_Establishment_Year)
itemid_ifatcont<-table(Item_unique$Item_Identifier,Item_unique$Item_Fat_Content)
chisq.test(itemid_ifatcont[,c(-1,-2,-4)])
itemid_osize<-table(bigm$Item_Identifier,bigm$Outlet_Size)
chisq.test(itemid_osize[!rowSums(itemid_osize==0),])
summary(bigm$Outlet_Size)
Item_Out_Unique<-unique(bigm[,c("Item_Identifier","Outlet_Location_Type")])
itemid_oloc<-table(bigm$Item_Identifier,bigm$Outlet_Location_Type)
chisq.test(itemid_oloc)
summary(Item_Out_Unique$Outlet_Location_Type)

#Item_Type vs rest
itype_outid<-table(bigm$Item_Type,bigm$Outlet_Identifier)
chisq.test(itype_outid)
itype_otype<-table(bigm$Item_Type,bigm$Outlet_Type)
chisq.test(itype_otype)
itype_outest<-table(bigm$Item_Type,bigm$Outlet_Establishment_Year)
chisq.test(itype_outest)
itype_ifatcont<-table(bigm$Item_Type,bigm$Item_Fat_Content)
chisq.test(itype_ifatcont[,c(-1,-2,-4)])
itype_ifatcont[,c(-1,-2,-4)]
itype_osize<-table(bigm$Item_Type,bigm$Outlet_Size)
chisq.test(itype_osize)
itype_oloc<-table(bigm$Item_Type,bigm$Outlet_Location_Type)
chisq.test(itype_oloc)

#Outid vs rest
outid_otype<-table(Outlet_unique$Outlet_Identifier,Outlet_unique$Outlet_Type)
chisq.test(outid_otype)
outid_ifatcont<-table(bigm$Outlet_Identifier,bigm$Item_Fat_Content)
chisq.test(outid_ifatcont[,c(-1,-2,-4)])

#Outtype vs rest
otype_oestyear<-table(Outlet_unique$Outlet_Type,Outlet_unique$Outlet_Establishment_Year)
chisq.test(otype_oestyear)
otype_ifatcont<-table(bigm$Outlet_Type,bigm$Item_Fat_Content)
chisq.test(otype_ifatcont[,c(-1,-2,-4)])
otype_osize<-table(Outlet_unique$Outlet_Type,Outlet_unique$Outlet_Size)
chisq.test(otype_osize)
otype_oloc<-table(Outlet_unique$Outlet_Type,Outlet_unique$Outlet_Location_Type)
chisq.test(otype_oloc)

#outletest vs rest
oestyear_ifatcont<-table(bigm$Outlet_Establishment_Year,bigm$Item_Fat_Content)
chisq.test(oestyear_ifatcont[,c(-1,-2,-4)])

#Itemfatcont vs rest
ifatcont_osize<-table(bigm$Item_Fat_Content,bigm$Outlet_Size)
chisq.test(ifatcont_osize[c(-1,-2,-4),])
ifatcont_oloc<-table(bigm$Item_Fat_Content,bigm$Outlet_Location_Type)
chisq.test(ifatcont_oloc[c(-1,-2,-4),])

#Outlet_Size vs rest(independent)
chisq.test(bigm$Outlet_Identifier,bigm$Outlet_Size)
chisq.test(Outlet_unique$Outlet_Identifier,Outlet_unique$Outlet_Size)

#Continuous-Categorical___
#Item weight vs all
plot(bigm$Item_Type,bigm$Item_Weight)
abline(h=mean(bigm$Item_Weight,na.rm = T),lwd=2,col="black")

#Filling the unnecessary missing values for Item_Weight
array_Item_Identifier<-bigm$Item_Identifier[which(is.na(bigm$Item_Weight))]#<---
i=1#<---
for (i in 1:length(array_Item_Identifier)) {#<---
  item_weight_val<-bigm$Item_Weight[which(bigm$Item_Identifier==array_Item_Identifier[i])]
  avg_item<-mean(item_weight_val,na.rm = T)
  bigm$Item_Weight[bigm$Item_Identifier==array_Item_Identifier[i]]<-avg_item
}

tapply(bigm$Item_Weight,bigm$Item_Type,mean,na.rm=T)
f<-aov(Item_Weight~Item_Type,data = bigm)
summary(f)
tapply(bigm$Item_Weight,bigm$Outlet_Identifier,mean,na.rm=T)
f<-aov(Item_Weight~Outlet_Identifier,data = bigm)
summary(f)
tapply(bigm$Item_Weight,bigm$Outlet_Type,mean,na.rm=T)
f<-aov(Item_Weight~Outlet_Type,data = bigm)
summary(f)
tapply(bigm$Item_Weight,bigm$Outlet_Establishment_Year,mean,na.rm=T)
f<-aov(Item_Weight~Outlet_Establishment_Year,data = bigm)
summary(f)
tapply(bigm$Item_Weight,bigm$Item_Fat_Content,mean,na.rm=T)
f<-aov(Item_Weight~Item_Fat_Content,data = bigm)
summary(f)
tapply(bigm$Item_Weight,bigm$Outlet_Location_Type,mean,na.rm=T)
f<-aov(Item_Weight~Outlet_Location_Type,data = bigm)
summary(f)

#Item_Visibility vs all 
mean(bigm$Item_Visibility)
tapply(bigm$Item_Visibility,bigm$Item_Type,mean,na.rm=T)
f<-aov(Item_Visibility~Item_Type,data=bigm)
summary(f)
tapply(bigm$Item_Visibility,bigm$Outlet_Identifier,mean,na.rm=T)
f<-aov(Item_Visibility~Outlet_Identifier,data=bigm)
summary(f)
tapply(bigm$Item_Visibility,bigm$Outlet_Type,mean,na.rm=T)
f<-aov(Item_Visibility~Outlet_Type,data=bigm)
summary(f)
tapply(bigm$Item_Visibility,bigm$Outlet_Establishment_Year,mean,na.rm=T)
f<-aov(Item_Visibility~Outlet_Establishment_Year,data=bigm)
summary(f)
tapply(bigm$Item_Visibility,bigm$Item_Fat_Content,mean,na.rm=T)
f<-aov(Item_Visibility~Item_Fat_Content,data=bigm)
summary(f)
tapply(bigm$Item_Visibility,bigm$Outlet_Location_Type,mean,na.rm=T)
f<-aov(Item_Visibility~Outlet_Location_Type,data=bigm)
summary(f)
f<-aov(Item_Visibility~Outlet_Size,data=bigm)
summary(f)


#Item_MRP
mean(bigm$Item_MRP)
tapply(bigm$Item_MRP,bigm$Item_Type,mean,na.rm=T)
f<-aov(Item_MRP~Item_Type,data=bigm)
summary(f)
tapply(bigm$Item_MRP,bigm$Outlet_Identifier,mean,na.rm=T)
f<-aov(Item_MRP~Outlet_Identifier,data=bigm)
summary(f)
tapply(bigm$Item_MRP,bigm$Outlet_Type,mean,na.rm=T)
f<-aov(Item_MRP~Outlet_Type,data=bigm)
summary(f)
tapply(bigm$Item_MRP,bigm$Item_Fat_Content,mean,na.rm=T)
f<-aov(Item_MRP~Item_Fat_Content,data=bigm)
summary(f)
tapply(bigm$Item_MRP,bigm$Outlet_Location_Type,mean,na.rm=T)
f<-aov(Item_MRP~Item_Identifier,data=bigm)
summary(f)

#Item_Outlet_Sales
mean(bigm$Item_Outlet_Sales)
tapply(bigm$Item_Outlet_Sales,bigm$Item_Type,mean)
f<-aov(bigm$Item_Outlet_Sales~Item_Type,data = bigm)
summary(f)
tapply(bigm$Item_Outlet_Sales,bigm$Outlet_Identifier,mean)
f<-aov(bigm$Item_Outlet_Sales~Outlet_Identifier,data = bigm)
summary(f)
tapply(bigm$Item_Outlet_Sales,bigm$Outlet_Type,mean)
f<-aov(bigm$Item_Outlet_Sales~Outlet_Type,data = bigm)
summary(f)
tapply(bigm$Item_Outlet_Sales,bigm$Item_Fat_Content,mean)
f<-aov(bigm$Item_Outlet_Sales~Item_Fat_Content,data = bigm)
summary(f)
f<-aov(bigm$Item_Outlet_Sales~Outlet_Establishment_Year,data = bigm)
summary(f)
tapply(bigm$Item_Outlet_Sales,bigm$Outlet_Location_Type,mean)
f<-aov(Item_Outlet_Sales~Outlet_Location_Type,bigm)
summary(f)
tapply(bigm$Item_Outlet_Sales,bigm$Item_Type,sum)
tapply(bigm$Item_Outlet_Sales,bigm$Outlet_Identifier,sum)
tapply(bigm$Item_Outlet_Sales,bigm$Outlet_Type,sum)
tapply(bigm$Item_Outlet_Sales,bigm$Item_Fat_Content,sum)
f<-aov(bigm$Item_Outlet_Sales~Item_Fat_Content,data = bigm)
summary(f)
tapply(bigm$Item_Outlet_Sales,bigm$Outlet_Location_Type,sum)
f<-aov(Item_Outlet_Sales~Item_Type,bigm)
summary(f)

#Outlet_Size vs all continuous variables
f<-aov(Item_Weight~Outlet_Size,data=bigm)
summary(f)
f<-aov(Item_Visibility~Outlet_Size,data=bigm)
summary(f)
f<-aov(Item_MRP~Outlet_Size,data=bigm)
summary(f)
f<-aov(Item_Outlet_Sales~Outlet_Size,data=bigm)
summary(f)
