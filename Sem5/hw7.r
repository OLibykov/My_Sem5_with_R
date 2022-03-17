#part1
library(reshape) # ��� ������� melt()
library(reshape2)# ��� ������� acast()
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
          Group = c("��������", "��������"),
          Response = c("�������", "��������"),
          Center = c("1", "2", "3", "4", "5", "6", "7", "8")))
drug.df <- data.frame(melt(drug,
                           id=c("Center", "Group", "Response")))
p <- ggplot(data = drug.df, aes(x = Center, y = value, fill = Response))+xlab("�������")+ylab("����")
p + geom_bar(stat = "identity", position = "fill") + facet_grid(Group~.)
#������� - ������������� ������ � "������" ��������
par(mfrow = c(1, 2))
hlp <- drug.df[drug.df$Group == "��������",]
table1 <-acast(hlp[,2:4], Response ~ Center, value.var = "value")
b <- barplot(table1,
             col = topo.colors(2),
             beside = TRUE, xpd = FALSE,
             ylab = "���������� �����", xlab = "���.������",
             legend.text=rownames(table1),
             main = "��������")
hlp <- drug.df[drug.df$Group == "��������",]
table2 <-acast(hlp[,2:4], Response ~ Center, value.var = "value")
b <- barplot(table2,
             col = topo.colors(2),
             beside = TRUE, xpd = FALSE,
             ylab = "���������� �����", xlab = "���.������",
             legend.text=rownames(table1),
             main = "��������")

#Part2. ������������ ���������� �������, �������� � ��������.
work_data <-data.frame(read.csv(file.choose()))
str(work_data)
norm_data <- head(work_data, 100)
#� ���� ������ � ������ 2 ������� ������:
#1 - ������� ����� ���������� � ���������� ����� �������� � ���������� �������
#2 - ���������� �����, �������� "���� � ��������" � ���� �� ���������� �������.
#�������� ������ �� ������������ �� �������������
#������� 1000 ����������
c1 <- norm_data$High - norm_data$Low
c2 <- norm_data$Quote.asset.volume
shapiro.test(c1)
shapiro.test(c2)
#p-value c1 � p-value c2 ����� ����� ��������� �������� (<0.01)
shapiro.test(log(c1))
shapiro.test(log(c2))
#��� ����� p-value < 0.01
#� �����, � ���� �� 1000 ����������, � 100. � ���� ������ � shapiro.test(log(c1),
# � shapiro.test(log(c2)) ������ p-value > 0.3, ��� ��������� ������� ����� � �� "������������"
hist(log(c1), breaks = 20, freq = FALSE, col = "lightblue")
hist(log(c2), breaks = 20, freq = FALSE, col = "lightblue")
cor.test(log(c1), log(c2))
cor.test(c1, c2, method = "spearman")
cor.test(c1, c2, method = "kendall")
#Part3.
# ��� �������� ���� ����������������� ��������� � ����� ������� �������������,
# ��� �� ����������� ������ - �������/���������� ����������� ��������,
# � �� ��������� - ����� ������� ��� ���.
matr <- matrix(c(40, 30, 32, 48), nrow = 2, byrow = TRUE,
               dimnames = list(c("���� ���������� ��������", "��� ����������� ��������"),
                               c("�����", "�� �����")))
matr
chisq.test(matr)
fisher.test(matr)

