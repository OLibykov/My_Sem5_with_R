# ��� ���� ������ �� ������� ������� �������������� ��������� � �� ������������� ��������

# �������������� t-��������: �������� �������� � ��������� �������� ������� ���������� ��������

# ��������� ������: �������� ����������� ������� � ����� � ���
d.intake <- c(5260, 5470, 5640, 6180, 6390, 6515,
              6805, 7515, 7515, 8230, 8770, 7000, 8000)
mean(d.intake)

# 7725 - ������������� �����, ������� � ��� ������� �������� �������
t.test(d.intake, mu = 7725)

# ���������/����������� � ���, ��� ����� p-��������, ������������� ���������

# �������� �������� � ��������� ������� ���� ����������� �������
install.packages("ISwR")
library(ISwR)

# ������ � �������� ������� �������
data(energy)
attach(energy)
head(energy)
tapply(expend, stature, mean)

# ��������: �������� �� ���������� ������� ������� �������������� ����������

# �� ��������� - ���� ����� ��� ������������ ���������
t.test(expend ~ stature) 
# ������������ �������������� t-����
t.test(expend ~ stature, var.equal = TRUE) 

# �������� ������� �� ����� � ��� �� ������ �� � ����� ���������� �����������

data(intake) # �� ������ ISwR
attach(intake)
head(intake)
post - pre
mean(post - pre)

# �������� ������� t-������, ��������� �� �����-���� ������ �����������
# � �������� ��������� �������� �������� � ������� �� � ����� 
t.test(pre, post, paired = TRUE)

# ����������� ����� �������
power.t.test(delta = 3.0, sd = 1.8, sig.level = 0.05, power = 0.8)

# ������ �������� �������� - ���������, ��� ��� �����
power.t.test(n = 15, delta = 3.0, sd = 1.8, sig.level = 0.05)

# ����������� ������ ������� ��� �������� �������� 
power.t.test(delta = 3.0, sd = 1.8, sig.level = 0.05, 
             power = 0.8, type  = "paired")

# ����������� ������ ������� ��� ��������������� �������� 
power.t.test(delta = 3.0, sd = 1.8, sig.level = 0.05, 
             power = 0.8, type  = "one.sample")

# �����������, ��� ����� �������� �������������� ��������

# �������� �������� ����������-�����-�����
# ��� ������� �� �������������, �������� �� �����������

# �������������� �������� ����������
# ��������: ������� �� ������������� ��������� � ��������� ������� mu
d.intake <- c(5260, 5470, 5640, 6180, 6390, 6515,
              6805, 7515, 7515, 8230, 8770)
wilcox.test(d.intake, mu = 7725)

# ��������� ���� ����������� ������� (�������� �����-�����)
# ��������: ������� ������������� �������� ���������� �� �������� mu
library(ISwR)
data(energy)
attach(energy)
wilcox.test(expend ~ stature, paired = FALSE)

# ��������� ���� ��������� �������
data(intake) # �� ������ ISwR
intake
attach(intake)
wilcox.test(pre, post, paired = TRUE)
# ���� �����, �� � ������������� ����������
wilcox.test(pre, post, paired = TRUE, conf.int = TRUE)

# ������ ������������ ��������� � ���� �������
# �������� � ���������� ��������� ���� ��������� �������������� ����������� �������������
data(energy, package = "ISwR")
attach(energy)
# F-���� (������)
var.test(expend ~ stature)

# ������ ������������ ��������� � ���������� �������
# ����������� ����������� ��������, �� ��� �������� ����� �������
# ������������ ���� �� ���� �� ��� ��������������� � �������� ������������
data(InsectSprays)
library(car)

# �������� ������
leveneTest(count ~ spray, data = InsectSprays)
leveneTest(count ~ spray, data = InsectSprays, center = mean)

# �������� ���������
bartlett.test(count ~ spray, data = InsectSprays)

# �������� ��������-������
fligner.test(count ~ spray, data = InsectSprays)