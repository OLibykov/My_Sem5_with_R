#Part1
#Done
#Part2
work_data <-data.frame(read.csv(file.choose()))
norm_data <- head(work_data, 1000)
norm_data <- norm_data[, -c(1, 7)]
str(norm_data)
M <- cor(norm_data)
library(corrplot)
col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow",
                           "#7FFF7F", "cyan", "#007FFF", "blue","#00007F")) 
corrplot(M, method = "pie", col = col4(20), cl.length = 21,
         order = "AOE",  tl.col = "black", addCoef.col = "green")

vif(lm(Low~Open + Close + High + Taker.buy.base.asset.volume +
       Taker.buy.quote.asset.volume + Quote.asset.volume + Volume +
       Number.of.trades, data = norm_data))
#Part3
#Непрерывные данные
#Возьмем меньше строк, чтобы было поменьше точек на графике
norm_data <- head(work_data, 100)
norm_data <- norm_data[, -c(1, 7)]
pairs(norm_data, panel = panel.smooth)
library(GGally)
ggpairs(norm_data, 
        upper = list(continuous = "density", combo = "box"), 
        lower = list(continuous = "points", combo = "dot"))
library(mgcv)
summary(gam(Number.of.trades ~ s(Volume), data = norm_data))
#Категориальные переменные
norm_data <- head(work_data, 1000)
norm_data <- norm_data[, -c(1, 7)]
norm_data <- norm_data[norm_data$Number.of.trades < 2000,]
factor_1 <- cut(norm_data[, 3], breaks = quantile(norm_data[, 3], c(0, .25, .50, .75, 1)), 
                      labels = c("A1","A2","A3","A4"), include.lowest = TRUE)
factor_2 <- cut(norm_data[, 8], breaks = quantile(norm_data[, 8], c(0, .25, .50, .75, 1)), 
                      labels = c("B1","B2","B3","B4"), include.lowest = TRUE)
coplot(norm_data$High - norm_data$Low ~ norm_data$Number.of.trades | factor_1 * factor_2,
       ylab = c("$$$$$MONEYMONEY$$$$", colnames(norm_data[8])),
       xlab = c("Число сделок", "Low"),
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })
#Part4.
layout(matrix(1:4, ncol = 2))
norm_data <- head(work_data, 20)
Time <- seq(1, 20)

plot(Time, norm_data$High - norm_data$Low, type = "l", main="delta Q", 
     xlab = "Время (20 минут)", ylab = "$$$")
acf(norm_data$High - norm_data$Low, main = "АКФ", xlab = "Лаг", ylab = "Значение")
plot(Time, norm_data$Number.of.trades, type = "l", main="Number of trade",
     xlab = "Время (20 минут)", ylab = "trade")
acf(Waders$L.dominicanus, main = "АКФ", xlab = "Лаг", ylab = "Значение")
#Part5.
#Однофакторный анализ
layout(matrix(1:1, ncol = 1))
norm_data <- head(work_data, 100)
norm_data <- norm_data[, -c(1, 7)]
norm_data <- norm_data[norm_data$Number.of.trades < 2000,]
Open_Cost <- cut(norm_data[, 3], breaks = quantile(norm_data[, 3], c(0, .25, .50, .75, 1)), 
                labels = c("A1","A2","A3","A4"), include.lowest = TRUE)


stripchart(norm_data$High - norm_data$Low ~ Open_Cost, data = norm_data, pch = 19,
           col = c("blue", "red", "green", "black"),
           ylab = "Квантили", xlab = "delta Q")

summary(aov(norm_data$High - norm_data$Low ~ Open_Cost, data = norm_data))
#Двухфакторный анализ
norm_data <- head(work_data, 10000)
norm_data <- norm_data[, -c(1, 7)]
norm_data <- norm_data[norm_data$Number.of.trades < 2000,]
Open_Cost <- cut(norm_data[, 3], breaks = quantile(norm_data[, 3], c(0, .50, 1)), 
                 labels = c("A1","A2"), include.lowest = TRUE)
Volume <- cut(norm_data[, 8], breaks = quantile(norm_data[, 8], c(0, .25, 1)), 
                labels = c("B1","B2"), include.lowest = TRUE)

summary(aov(norm_data$Number.of.trades ~ Volume*Open_Cost, data = norm_data))