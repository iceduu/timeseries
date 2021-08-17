#清空环境
rm(list=ls())

#导入时间序列数据
cpi <- read.csv("D:/CPILFESL.csv", header = T)
cpi <- ts(cpi$CPILFESL, start = c(1957,1), end = c(2020,12), frequency = 12)
head(cpi)

#时序图
library(forecast)
pdf("fg1.pdf", family = "GB1", width = 8, height = 4)
autoplot(cpi, xlab = "Time", ylab = "Core CPI")
dev.off()

#平稳性检验
library(tseries)
adf.test(cpi)


#序列差分
cpi.dif1 <- diff(cpi)
pdf("fg2.pdf", family = "GB1", width = 8, height = 4)
autoplot(cpi.dif1)
dev.off()
cpi.dif2 <- diff(cpi.dif1)
pdf("fg3.pdf", family = "GB1", width = 8, height = 4)
autoplot(cpi.dif2)
dev.off()

#差分后序列平稳性检验
adf.test(cpi.dif1)
adf.test(cpi.dif2)


#模式识别与定阶
pdf("fg4.pdf", family = "GB1", width = 8, height = 4)
ggAcf(cpi.dif2)
dev.off()
pdf("fg5.pdf", family = "GB1", width = 8, height = 4)
ggPacf(cpi.dif2)
dev.off()

#模型拟合
cpi.fit <- Arima(cpi,order=c(0,2,7),seasonal=c(0,0,1))
cpi.fit
cpi.fit.auto <- auto.arima(cpi)
cpi.fit.auto

#模型参数显著性检验
len <- length(cpi) - 7
se <- c(0.0363,0.0440,0.0430,0.0409,0.0367,0.0405,0.0394)
for(i in 1:7) print(cpi.fit.auto$coef[i]/se[i])
for(i in 1:7){
  if(cpi.fit.auto$coef[i]>0)
    print(pt(cpi.fit.auto$coef[i]/se[i], df = len, lower.tail = FALSE))
  else
    print(pt(cpi.fit.auto$coef[i]/se[i], df = len, lower.tail = TRUE))
}

#剔除变量MA(4)后拟合新模型
cpi.fit.new <- Arima(cpi,order=c(0,2,5),seasonal=c(0,0,2),transform.pars=F,fixed=c(NA,NA,NA,0,NA,NA,NA))
cpi.fit.new

#模型参数显著性检验
len.new <- length(cpi) - 6
se.new <- c(0.0359,0.0454,0.0413,0,0.0307,0.0404,0.0394)
for(i in 1:7) print(cpi.fit.new$coef[i]/se.new[i])
for(i in 1:7){
  if(cpi.fit.new$coef[i]>0)
    print(pt(cpi.fit.new$coef[i]/se.new[i], df = len, lower.tail = FALSE))
  else
    print(pt(cpi.fit.new$coef[i]/se.new[i], df = len, lower.tail = TRUE))
}

#残差白噪声检验
pdf("fg6.pdf", family = "GB1", width = 8, height = 4.5)
checkresiduals(cpi.fit.new)
dev.off()
for(i in 1:4) print(Box.test(cpi.fit.new$residuals, type = "Ljung-Box", lag = 6 * i))

#模型预测效果
cpi.forc <- forecast(cpi.fit.new)
forc.fitted <- cbind(cpi, cpi.forc$fitted)
colnames(forc.fitted) <- c("Actual value","Forecast value")
pdf("fg7.pdf", family = "GB1", width = 8, height = 4)
autoplot(forc.fitted)
dev.off()
cpi.forc$mean[1:5]

#模型预测
forc <- cbind(cpi.forc$lower,cpi.forc$mean,cpi.forc$upper)
colnames(forc) <- c("lower.80%","lower.90%","mean","upper.80%","upper.95%")
new_cols <- c("upper.95%","upper.80%","mean","lower.80%","lower.90%")
forc <- forc[, new_cols]
pdf("fg8.pdf", family = "GB1", width = 8, height = 4)
autoplot(forc)
dev.off()
