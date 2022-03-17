#part1
library(reshape) # для функции melt()
library(reshape2)# для функции acast()
library(ggplot2)
drug <-
  array(c(11, 10, 25, 27,
          16, 22, 4, 10,
          14, 7, 5, 12,
          2, 1, 14, 16,
          6, 0, 11, 12,
          1, 0, 10, 10,
          1, 1, 4, 8,
          4, 6, 2, 1),
        dim = c(2, 2, 8),
        dimnames = list(
          Group = c("Препарат", "Контроль"),
          Response = c("Успешно", "Неудачно"),
          Center = c("1", "2", "3", "4", "5", "6", "7", "8")))
drug.df <- data.frame(melt(drug,
                           id=c("Center", "Group", "Response")))
p <- ggplot(data = drug.df, aes(x = Center, y = value, fill = Response))+xlab("Клиника")+ylab("Доля")
p + geom_bar(stat = "identity", position = "fill") + facet_grid(Group~.)
#Задание - Преобразовать график к "парным" столбцам
par(mfrow = c(1, 2))
hlp <- drug.df[drug.df$Group == "Препарат",]
table1 <-acast(hlp[,2:4], Response ~ Center, value.var = "value")
b <- barplot(table1,
             col = topo.colors(2),
             beside = TRUE, xpd = FALSE,
             ylab = "Количество людей", xlab = "Мед.центры",
             legend.text=rownames(table1),
             main = "Препарат")
hlp <- drug.df[drug.df$Group == "Контроль",]
table2 <-acast(hlp[,2:4], Response ~ Center, value.var = "value")
b <- barplot(table2,
             col = topo.colors(2),
             beside = TRUE, xpd = FALSE,
             ylab = "Количество людей", xlab = "Мед.центры",
             legend.text=rownames(table1),
             main = "Контроль")

#Part2. Коэффициенты корреляции Пирсона, Спирмена и Кендалла.
work_data <-data.frame(read.csv(file.choose()))
str(work_data)
norm_data <- head(work_data, 100)
#В моих данных я возьму 2 вектора данных:
#1 - разница между наибольшей и наименьшей ценой биткоина в промежуток времени
#2 - количество денег, котороое "было в торговле" в этот же промежуток времени.
#Проверим данные на нормальность их распределения
#Возьмем 1000 наблюдений
c1 <- norm_data$High - norm_data$Low
c2 <- norm_data$Quote.asset.volume
shapiro.test(c1)
shapiro.test(c2)
#p-value c1 и p-value c2 имеет очень маленькое значение (<0.01)
shapiro.test(log(c1))
shapiro.test(log(c2))
#Все равно p-value < 0.01
#В итоге, я взял не 1000 наблюдений, а 100. В этом случае и shapiro.test(log(c1),
# и shapiro.test(log(c2)) выдают p-value > 0.3, что позволяет сделать вывод о их "нормальности"
hist(log(c1), breaks = 20, freq = FALSE, col = "lightblue")
hist(log(c2), breaks = 20, freq = FALSE, col = "lightblue")
cor.test(log(c1), log(c2))
cor.test(c1, c2, method = "spearman")
cor.test(c1, c2, method = "kendall")
#Part3.
# Для проверки всех вышеперечисленных критериев я нашел таблицу сопряженности,
# где по горизонтали фактор - наличие/отсутствие повышенного давления,
# а по вертикали - курит человек или нет.
matr <- matrix(c(40, 30, 32, 48), nrow = 2, byrow = TRUE,
               dimnames = list(c("Есть повышенное давление", "Нет повышенного давления"),
                               c("Курит", "Не курит")))
matr
chisq.test(matr)
fisher.test(matr)

