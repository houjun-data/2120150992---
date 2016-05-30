#读取数据
algae <- read.table('Analysis.txt',
header=F,
dec='.',
col.names=c('season','size','speed','mxPH','mnO2','Cl','NO3','NH4','oPO4','PO4','Chla',
	'a1','a2','a3','a4','a5','a6','a7'),
na.string=c('XXXXXXX'))
#分析摘要
summary(algae)

#数据可视化
library(car)

#绘制直方图
hist(algae$mxPH)
#绘制QQ图
qqPlot(algae$mxPH,main='Norm QQ Plot of mxPH')
#绘制箱形图
boxplot(algae$mxPH,ylab='mxPH')
rug(algae$mxPH,side=4)
abline(h=mean(algae$mxPH,na.rm=T),lty=2)

hist(algae$mnO2)
qqPlot(algae$mnO2,main='Norm QQ Plot of mnO2')
boxplot(algae$mnO2,ylab='mnO2')
rug(algae$mnO2,side=4)
abline(h=mean(algae$mnO2,na.rm=T),lty=2)

hist(algae$Cl)
qqPlot(algae$Cl,main='Norm QQ Plot of Cl')
boxplot(algae$Cl,ylab='Cl')
rug(algae$Cl,side=4)
abline(h=mean(algae$Cl,na.rm=T),lty=2)

hist(algae$NO3)
qqPlot(algae$NO3,main='Norm QQ Plot of NO3')
boxplot(algae$NO3,ylab='NO3')
rug(algae$NO3,side=4)
abline(h=mean(algae$NO3,na.rm=T),lty=2)

hist(algae$NH4)
qqPlot(algae$NH4,main='Norm QQ Plot of NH4')
boxplot(algae$NH4,ylab='NH4')
rug(algae$NH4,side=4)
abline(h=mean(algae$NH4,na.rm=T),lty=2)

hist(algae$oPO4)
qqPlot(algae$oPO4,main='Norm QQ Plot of oPO4')
boxplot(algae$oPO4,ylab='oPO4')
rug(algae$oPO4,side=4)
abline(h=mean(algae$oPO4,na.rm=T),lty=2)

hist(algae$PO4)
qqPlot(algae$PO4,main='Norm QQ Plot of PO4')
boxplot(algae$PO4,ylab='PO4')
rug(algae$PO4,side=4)
abline(h=mean(algae$PO4,na.rm=T),lty=2)

hist(algae$Chla)
qqPlot(algae$Chla,main='Norm QQ Plot of Chla')
boxplot(algae$Chla,ylab='Chla')
rug(algae$Chla,side=4)
abline(h=mean(algae$Chla,na.rm=T),lty=2)

#绘制河流大小与海藻的条件盒图
library(lattice)

bwplot(size~a1,data=algae,ylab='River Size',xlab='a1')
bwplot(size~a2,data=algae,ylab='River Size',xlab='a2')
bwplot(size~a3,data=algae,ylab='River Size',xlab='a3')
bwplot(size~a4,data=algae,ylab='River Size',xlab='a4')
bwplot(size~a5,data=algae,ylab='River Size',xlab='a5')
bwplot(size~a6,data=algae,ylab='River Size',xlab='a6')
bwplot(size~a7,data=algae,ylab='River Size',xlab='a7')

#绘制河水速度与海藻的条件盒图
library(lattice)

bwplot(speed~a1,data=algae,ylab='River Speed',xlab='a1')
bwplot(speed~a2,data=algae,ylab='River Speed',xlab='a2')
bwplot(speed~a3,data=algae,ylab='River Speed',xlab='a3')
bwplot(speed~a4,data=algae,ylab='River Speed',xlab='a4')
bwplot(speed~a5,data=algae,ylab='River Speed',xlab='a5')
bwplot(speed~a6,data=algae,ylab='River Speed',xlab='a6')
bwplot(speed~a7,data=algae,ylab='River Speed',xlab='a7')

#绘制季节与海藻的条件盒图
library(lattice)

bwplot(season~a1,data=algae,ylab='Season',xlab='a1')
bwplot(season~a2,data=algae,ylab='Season',xlab='a2')
bwplot(season~a3,data=algae,ylab='Season',xlab='a3')
bwplot(season~a4,data=algae,ylab='Season',xlab='a4')
bwplot(season~a5,data=algae,ylab='Season',xlab='a5')
bwplot(season~a6,data=algae,ylab='Season',xlab='a6')
bwplot(season~a7,data=algae,ylab='Season',xlab='a7')

#缺失数据处理
#剔除缺失数据
omiteddata = na.omit(algae)
write.table(omiteddata,'D:/DataMining/OmitedData.txt',col.names = F,row.names = F, quote = F)

#使用高频数据替换
library(DMwR)
preprocess2 = algae[-manyNAs(algae),]
preprocess2 = centralImputation(preprocess2)
write.table(preprocess2,'D:/DataMining/CentralImputationData.txt',
col.names = F,row.names = F, quote = F)

#通过变量相关性填补缺失值
symnum(cor(algae[,4:18],use='complete.obs'))
lm(formula=PO4~oPO4, data=algae)
preprocess3 = algae[-manyNAs(algae),]
fillPO4 <- function(oP){
	if(is.na(oP))
		return(NA)
	else return (42.897 + 1.293 * oP)
}
preprocess3[is.na(preprocess3$PO4),'PO4'] <- sapply(preprocess3[is.na(preprocess3$PO4),'oPO4'],fillPO4)
write.table(preprocess3,'D:/DataMining/linearDefaultData.txt',
col.names = F,row.names = F, quote = F)

#通过案例的相关性填补缺失值
preprocess4 = knnImputation(algae,k=10)
write.table(preprocess4,'D:/DataMining/knnImputationData.txt',
col.names = F,row.names = F, quote = F)