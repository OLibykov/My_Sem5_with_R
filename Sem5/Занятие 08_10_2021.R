# ���� � ��������� �������
#data(sleep, package = "VIM")

# ���� � ������������ ����������
load(file = "sleep_imp.Rdata")

# ������������ �������������� �������
M <- cor(sleep_imp3)
library(corrplot)
col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow",
                           "#7FFF7F", "cyan", "#007FFF", "blue","#00007F")) 
corrplot(M, method = "pie", col = col4(20), cl.length = 21,
         order = "AOE",  tl.col = "black", addCoef.col = "red")

# ��������
corrplot(M, method = "color", col = col4(20), cl.length = 21,
         order = "AOE",  tl.col = "black", addCoef.col = "green")

corrplot(M, method = "number", col = col4(20), cl.length = 21,
         order = "AOE",  tl.col = "black")

# ������ �������� ��������� (VIF). ������� ������������� ������
library(car)
vif(lm(Sleep ~ BodyWgt + BrainWgt + Span + Gest + Pred + Exp + Danger,
       data = sleep_imp3))

# ��������� ���������
cars <- mtcars[, 1:7]
pairs(cars, panel = panel.smooth) #�� ������������ ������

# ������� �� ������ ����������� ���������� ��������
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, method = "spearman"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep = "")
  # text(0.5, 0.5, txt)
  #������ ������ ����� �������� �� �������� ������������ ����������
  if(missing(cex.cor))
    cex.cor <- 1.1/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# ����� ������� ������ ���������
pairs(cars, panel = panel.smooth, lower.panel = panel.cor)

# ��������� ��������� ���������
library(lattice)
splom(cars)

# ��������� ��������� ��������� � ���������� �������� �������� ����������
library(GGally)
ggpairs(cars, 
        upper = list(continuous = "density", combo = "box"), 
        lower = list(continuous = "points", combo = "dot"))

# ����������� ������� ������� (EDF)
library(mgcv)
summary(gam(mpg ~ s(hp) + s(qsec), data = cars)) #���������� ���������� ������

# ��������� ������������ ��� �������������� ����������
Sparrows <- read.table(file = "SparrowsElphick.txt", header = TRUE)
# �������� ����������� �������� ������: ������� � ������ ����� >= 65
I1 <- Sparrows$SpeciesCode == 1 & 
  Sparrows$Sex != "0" &
  Sparrows$wingcrd < 65
Wing1 <- Sparrows$wingcrd[I1]
Wei1 <- Sparrows$wt[I1]
Mon1 <- factor(Sparrows$Month[I1])
Sex1 <- factor(Sparrows$Sex[I1])

# ��������� ����� � ��� ��� �������������� ����������
fMonth1 <- factor(Mon1, levels = c(5, 6, 7, 8, 9),
                  labels = c("���", "����",
                             "����", "������", "��������"))
fSex1 <- factor(Sex1, levels = c(4, 5),
                labels = c("�����", "�����"))

# ����� �������������� ���������
coplot(Wei1 ~ Wing1 | fMonth1 * fSex1,
       ylab = c("��� (�)", "���"),
       xlab = c("����� ����� (��)", "�����"),
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

# ������ ������ � ������� ������������������ ������� (���): ������� ������������� ������

# ����������� ���� ����� ����
Waders <- read.table(file = "wader.txt", header = TRUE)
Time <- seq(1, 25)

layout(matrix(1:4, ncol = 2))
plot(Time, Waders$C.fuscicolis, type = "l", main="���������� ��������", 
     xlab = "����� (2 ������)", ylab = "�����������")
acf(Waders$C.fuscicolis, main = "���", xlab = "���", ylab = "��������")
plot(Time, Waders$L.dominicanus, type = "l", main="������������� �����",
     xlab = "����� (2 ������)", ylab = "�����������")
acf(Waders$L.dominicanus, main = "���", xlab = "���", ylab = "��������")


# ������������� ������: ������� ������������� ������

# ������ � ���� ������ �������, ������� ���������� ��� ������ �����, � ����������
# � ����������+���������� 
tomato <- data.frame(weight =
                       c(1.5, 1.9, 1.3, 1.5, 2.4, 1.5, # water
                         1.5, 1.2, 1.2, 2.1, 2.9, 1.6, # nutrient
                         1.9, 1.6, 0.8, 1.15, 0.9, 1.6), # nutrient+24D
                     trt = rep(c("Water", "Nutrient", "Nutrient+24D"),
                               c(6, 6, 6)))
is.factor(tomato$trt)
tomato$trt<-factor(tomato$trt,rep(c("Water", "Nutrient", "Nutrient+24D")))
is.factor(tomato$trt)
levels(tomato$trt)
tomato$trt <- relevel(tomato$trt, ref = "Water")

# ������� �� �������� ����������
Means <- data.frame(weight = as.numeric(tapply(tomato$weight,tomato$trt, mean)),
                    trt = rep("Means", 3))
Means

# ��������� Means � ������� tomato
tomato <- rbind(tomato, Means)

layout(matrix(1:1, ncol = 1))

stripchart(weight ~ trt, data = tomato, pch = 19,
           col = c("blue", "red", "green", "black"),
           ylab = "�������", xlab = "���, ��")

# ��������, ����� ������� �������� � ����� ������ ���������
points(x = Means$weight, y = c(4, 4, 4), pch = 19,
       col = c("blue", "red", "green"))

# ������������� ������������� ������
summary(aov(weight ~ trt, data = tomato))

# �������� ������: ������� ��������� ������
M <- lm(weight ~ trt, data = tomato)
summary(M)

# ������� �������������� �������, �������� � ������������ aov()
anova(M)

# ������������� ������������� ������
library(HSAUR2)
# ������ ����� ���� ���������� ������� � ������ ����������� �����, ������� �����
# ���� ������������ (cereal) � �������� (beef)

# ��������� ������� ���� ������� �� ������� ���� ����
data(weightgain)
str(weightgain)

library(ggplot2)
ggplot(data = weightgain, aes(x = type, y = weightgain)) + 
  geom_boxplot(aes(fill = source))

require(doBy)
summaryBy(weightgain ~ type + source, data = weightgain,
          FUN = c(mean, sd, length))

# ������ ����� ������������
plot.design(weightgain)

# ������� ������ �� ����� ������ � ������, ���� ������ - ����������������
M <- aov(weightgain ~ source + type + source:type, 
          data = weightgain)
summary(M)

# ���������� ������� ��� ����� ������ �������� ������
M <- lm(weightgain ~ type*source, data = weightgain)
summary(M)
anova(M)

# �������� � ������ ��� ��������

# ������� ������ �� ��� � ��������
df <- data.frame(weight = Wei1, length = Wing1, sex = fSex1, month = fMonth1)
df1 <- df[df$month != "May" & df$month != "Sep", ]

# �������������
M <- lm(weight ~ length*month*sex, data = df1)
DT <- anova(M)
DT