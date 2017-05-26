# Statistics-for-JRP
Statistical analysis used in JRP
setwd('F://R-VB')
getwd()
wt <- read.csv(file = "area_wt.csv", header = T)
head(wt)
wt <- wt[,1:3]
head(wt)
odd <- seq(1,nrow(wt)-1,2)
even <- seq(2,nrow(wt),2)
wt$index[odd] <- "insulin"
wt$index[even] <- "gcg"
library(reshape2)
wide_wt <- reshape(wt, v.names = "Area", idvar = "Number",
                timevar = "index", direction = "wide")
wide_wt$ratio <- round(wide_wt$Area.gcg/wide_wt$Area.insulin * 100,digits = 0)
hist(wide_wt$ratio)
tg <- read.csv("area_tg.csv", header = T)
head(tg)
tg <- tg[,1:3]
odd <- seq(1,nrow(tg)-1,2)
even <- seq(2,nrow(tg),2)
tg$index[odd] <- "insulin"
tg$index[even] <- "gcg"
wide_tg <- reshape(tg, v.names = "Area", idvar = "Number",
                timevar = "index", direction = "wide")
wide_tg$ratio <- round(wide_tg$Area.gcg/wide_tg$Area.insulin * 100,digits = 0)
hist(wide_tg$ratio)
summary(wide_wt$ratio)
summary(wide_tg$ratio)
shapiro.test(wide_wt$ratio)
shapiro.test(wide_tg$ratio)
wilcox.test(wide_wt$ratio, wide_tg$ratio)
boxplot(wide_wt$ratio, wide_tg$ratio, names = c("wt", "tg"), main = "Glucagon/Insulin")
head(wide_tg)
wide_tg$total.area <- round((wide_tg$Area.insulin+wide_tg$Area.gcg)/100,0)
wide_wt$total.area <- round((wide_wt$Area.insulin+wide_wt$Area.gcg)/100,0)
mean(wide_tg$total.area)
mean(wide_wt$total.area)
hist(wide_tg$total.area)
hist(wide_wt$total.area)
head(wide_tg)
head(wide_wt)
wilcox.test(wide_tg$total.area,wide_wt$total.area)
write.csv(x = wide_tg,file = "wide_tg.csv")
write.csv(x = wide_wt,file = "wide_wt.csv")
total.area_wt <- read.csv("total.area_wt.csv",header = T)
total.area_tg <- read.csv("total.area_tg.csv",header = T)
head(total.area_tg)
head(total.area_wt)
total.area_tg$V2
wilcox.test(total.area_tg$V2,total.area_wt$V2)
boxplot(total.area_tg$V2,total.area_wt$V2,names = c("Tg","Wt"), main = "Total area",col = heat.colors(2))
data_mean <- c(mean(total.area_tg$V2),mean(total.area_wt$V2))#生成均数 
data_sd <- c(sd(total.area_tg$V2),sd(total.area_wt$V2))#生成标准差 
data_mean
data_sd
par(mfrow = c(1,1))
barcenters1 <- barplot(data_mean,names.arg = c("Tg","Wt"),main = 'Total area',ylim = c(0,200),col = heat.colors(2)) 
text(x = 0.5,y = 160,labels = "P value = 0.24")
segments(barcenters1,data_mean-data_sd,barcenters1,data_mean+data_sd,lty = 1) 
arrows(barcenters1,data_mean-data_sd,barcenters1,data_mean+data_sd,code = 3,angle = 90) 
gcg.vs.ins_wt <- read.csv("gcg.vs.ins_wt.csv",header = T)
gcg.vs.ins_tg <- read.csv("gcg.vs.ins_tg.csv",header = T)
head(gcg.vs.ins_tg)
hist(gcg.vs.ins_tg$V2)
hist(gcg.vs.ins_wt$V2)
wilcox.test(gcg.vs.ins_tg$V2,gcg.vs.ins_wt$V2)
boxplot(gcg.vs.ins_tg$V2,gcg.vs.ins_wt$V2, names = c("Tg","Wt"), main = "gcg/ins(%)",col = heat.colors(2))
data_mean <- c(mean(gcg.vs.ins_tg$V2),mean(gcg.vs.ins_wt$V2))#生成均数 
data_sd <- c(sd(gcg.vs.ins_tg$V2),sd(gcg.vs.ins_wt$V2))#生成标准差 
data_mean
data_sd
par(mfrow = c(1,1))
barcenters2 <- barplot(data_mean,names.arg = c("Tg","Wt"), space = 0.5, width = 0.1,main = 'Total area',ylim = c(0,15),col = heat.colors(2)) 
text(x = 0.1,y = 13,labels = "P value = 0.90")
segments(barcenters2,data_mean-data_sd,barcenters2,data_mean+data_sd,lty = 1) 
arrows(barcenters2,data_mean-data_sd,barcenters2,data_mean+data_sd,code = 3,angle = 90)
pecam_tg <- read.csv("Pecam_tg_sum.csv", header = T)
pecam_wt <- read.csv("Pecam_wt_sum.csv", header = T)
head(pecam_tg)
summary(pecam_tg$V2)
summary(pecam_wt$V2)
hist(pecam_tg$V2)
hist(pecam_wt$V2)
wilcox.test(pecam_tg$V2,pecam_wt$V2)
data_mean <- c(mean(pecam_tg$V2),mean(pecam_wt$V2))#生成均数 
data_sd <- c(sd(pecam_tg$V2),sd(pecam_wt$V2))#生成标准差 
par(mfrow = c(1,1))
barcenters1 <- barplot(data_mean,names.arg = c("Tg","Wt"),main = 'Pecam area (wt without outlier)',ylim = c(0,320),col = heat.colors(2)) 
text(x = 0.5,y = 260,labels = "P value = 0.50")
segments(barcenters1,data_mean-data_sd,barcenters1,data_mean+data_sd,lty = 1) 
arrows(barcenters1,data_mean-data_sd,barcenters1,data_mean+data_sd,code = 3,angle = 90) 
pecam_wt2 <- read.csv("Pecam_wt_sum2.csv", header = T)
wilcox.test(pecam_tg$V2,pecam_wt2$V2)
data_mean <- c(mean(pecam_tg$V2),mean(pecam_wt2$V2))#生成均数 
data_sd <- c(sd(pecam_tg$V2),sd(pecam_wt2$V2))#生成标准差 
par(mfrow = c(1,1))
barcenters1 <- barplot(data_mean,names.arg = c("Tg","Wt"),main = 'Pecam area (wt with outlier)',ylim = c(0,400),col = heat.colors(2)) 
text(x = 0.5,y = 260,labels = "P value = 0.20")
segments(barcenters1,data_mean-data_sd,barcenters1,data_mean+data_sd,lty = 1) 
arrows(barcenters1,data_mean-data_sd,barcenters1,data_mean+data_sd,code = 3,angle = 90) 
