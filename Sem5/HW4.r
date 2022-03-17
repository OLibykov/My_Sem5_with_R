#HW3
work_data <-data.frame(read.csv(file.choose()))
str(work_data)
norm_data <- head(work_data, 1000)
#разбиваем данные на 4 части(по квартилям)  по
norm_data[, 9] <- cut(norm_data[, 9],breaks = quantile(norm_data[, 9], c(0, .25, .50, .75, 1)), 
    labels = c("Q1","Q2","Q3","Q4"), include.lowest = TRUE) # по квартилям
norm_data[, 9]
is.factor(norm_data[, 9])

#Работа с sm.density
library(sm)
sm.density.compare(norm_data$High - norm_data$Low, norm_data$Number.of.trades, lwd = 2, xlab = "Max - Min", 
                                  ylab = "Плотности", legend.text = c("Q1","Q2","Q3","Q4"))
title(main = "Кривые ядерной плотности")
Colfill <- c(2:6)
legend(locator(1), levels(norm_data$Number.of.trades), fill = Colfill)
#cdplot, boxplot, pieplot
norm_data <- head(work_data, 1000)
norm_data[, 9] <- cut(norm_data[, 9],breaks = quantile(norm_data[, 9], c(0,.50, 1)), 
    labels = c("Q1","Q2"), include.lowest = TRUE) # по квартилям
attach(norm_data)
x <- High - Low
Number.of.trades
cdplot((Number.of.trades) ~ x, col = c("coral", "skyblue"),
       yaxlabels = c("Q1", "Q2"), bw = 0.5, data = norm_data)
norm_data <- head(work_data, 1000)
attach(norm_data)
Number.of.trades<- cut(Number.of.trades,breaks = quantile(Number.of.trades, c(0, .25, .50, .75, 1)), 
                                     labels = c("Q1","Q2", "Q3", "Q4"), include.lowest = TRUE) # по квартилям
Number.of.trades
boxplot(High - Low ~ Number.of.trades,
        xlab = "Кол-во сделок",
        ylab = "Max-Min",
        main = "Скрытая угроза",
        col = "coral",
		legend.text = c("Q1","Q2","Q3","Q4"))
norm_data <- head(work_data, 1000)
attach(norm_data)
Number.of.trades<- cut(Number.of.trades,breaks = quantile(Number.of.trades, c(0, .25, .50, .75, 1)), 
                                     labels = c("Q1","Q2", "Q3", "Q4"), include.lowest = TRUE) # по квартилям
pie (x = tapply( Number.of.trades,Number.of.trades,length),
	   main = "Тортик",
		col = c(2:6))
 Colfill <- c(2:6)
legend(locator(1), levels(Number.of.trades), fill = Colfill)
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Разбиение непрерывных данных на части
norm_data <- head(work_data, 1000)
attach(norm_data)
Number.of.trades<- cut(Number.of.trades,breaks = c(1, 700, 1000, 1500, 2500), 
                                     labels = c("Q1","Q2", "Q3", "Q4"), include.lowest = TRUE) # по квартилям
pie (x = tapply( Number.of.trades,Number.of.trades,length),
	   main = "Тортик",
		col = c(2:6))
 Colfill <- c(2:6)
legend(locator(1), levels(Number.of.trades), fill = Colfill)