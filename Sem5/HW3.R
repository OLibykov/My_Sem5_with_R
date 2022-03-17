#Part1. Done.
#Part2
work_data <-data.frame(read.csv(file.choose()))
str(work_data)
norm_data <- head(work_data, 100)
#разбиваем непрерывные данные на 4 части, чтобы создать фактор группу.
library(sm)
norm_data$factor_1 <- cut(norm_data$High - norm_data$Low,
                      breaks = quantile(norm_data$High - norm_data$Low,
                      c(0, .25, .50, .75, 1)), labels = c("A1", "A2", "A3", "A4"),
                      include.lowest = TRUE)
norm_data$factor_2 <- cut(norm_data$Number.of.trades, breaks = c(1, 700, 1000, 1500, 2500), 
                       labels = c("B1","B2", "B3", "B4"), include.lowest = TRUE)
#A1, A2, A3, A4 группы изменения цены в минуту (A1 - маленькое изменение, A4 - большое)
#B1, B2, B3, B4 группы кол-ва сделок в минуту (B1 - маленькое кол-во сделок, B4 - большое)

sm.density.compare(norm_data$Number.of.trades, norm_data$factor_1, lwd = 1, lty = "9111",
                   xlab = "Число сделок", xlim = c(0, 5000),
                   ylab = "Плотности", legend.text = c("A1","A2","A3","A4"))
title(main = "Кривые ядерной плотности")
Colfill <- c(2:6)
legend(locator(1), levels(norm_data$factor_1), fill = Colfill)

sm.density.compare(norm_data$Taker.buy.base.asset.volume, norm_data$factor_2, lwd = 2,
                   xlab = "Число сделок", xlim = c(0, 200), col = c(5:9),
                   ylab = "Плотности", legend.text = c("B1","B2","B3","B4"))
title(main = "Кривые ядерной плотности")
Colfill <- c(5:9)
legend(locator(1), levels(norm_data$factor_2), fill = Colfill)
#Part3. cdplot, boxplot
norm_data$factor_3 <- cut(norm_data$Number.of.trades, 
                          breaks = c(1, 1000, 2500), 
                          labels = c("C1","C2"), include.lowest = TRUE)
norm_data$delta <- norm_data$High - norm_data$Low
cdplot(norm_data$factor_3 ~ norm_data$Quote.asset.volume, 
       col = c("coral", "skyblue"),  main = "conditional density plot",
       yaxlabels = c("C1", "C2"), data = norm_data)
cdplot(norm_data$factor_3 ~ norm_data$delta,
       col = c("coral", "skyblue"), main = "conditional density plot",
       yaxlabels = c("C1", "C2"), data = norm_data)

boxplot(norm_data$delta ~ norm_data$factor_2,
        xlab = "Кол-во сделок",
        ylab = "Max-Min",
        main = "Демонстрация Boxplot",
        col = "coral",
        legend.text = c("B1","B2","B3","B4"))

boxplot(norm_data$Number.of.trades ~ norm_data$factor_1,
        xlab = "delta",
        ylab = "Кол-во сделок",
        main = "Демонстрация Boxplot",
        col = "blue",
        legend.text = c("A1","A2","A3","A4"))
#Part4. Pieplot.
par(mfrow = c(1,1))
pie (x = tapply(norm_data$Number.of.trades,norm_data$factor_2,length),
     main = "Демонстрация pieplot",
     col = c(2:6))
Colfill <- c(2:6)
legend(locator(1), levels(norm_data$factor_2), fill = Colfill)

par(mfrow = c(1,1))
pie (x = tapply(norm_data$Taker.buy.base.asset.volume,norm_data$factor_3,mean),
     main = "Демонстрация pieplot",
     col = c(5:7))
Colfill <- c(5:7)
legend(locator(1), levels(norm_data$factor_3), fill = Colfill)