#this script is used to corelate clincal score and pathology score in different organs in GVHD
library("ggpubr")
library(caret)
library(doParallel)
library(reshape2)
library(ggplot2)
library(data.table) 
library(corrplot)
# Read score data
my_data <- read.csv("C:/info/score.csv")
names(my_data)[1]<-paste("clinical")
shapiro.test(my_data$clinical)#W = 0.80217, p-value = 2.251e-05
shapiro.test(my_data$lung)#W = 0.78048, p-value = 8.647e-06
#comments: they don't fit normal distribution

ggscatter(my_data, x = "clinical", y = "lung", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "clinical score", ylab = "lung infiltration grade")

ggqqplot(my_data$clinical, ylab = "clnical score")

res <- cor.test(my_data$clinical, my_data$lung,  method="spearman")
res
RANTES_clinical<-c("clinical","RANTES")
my_data2<- my_data[RANTES_clinical]
my_data2<- na.omit(my_data2)
ggscatter(my_data2, x = "clinical", y = "RANTES", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "clinical score", ylab = "RANTES concentration")

ggqqplot(my_data2$RANTES, ylab = "RANTES")
res <- cor.test(my_data2$clinical, my_data2$RANTES,  method="spearman")
res
M<-cor(my_data)
head(round(M,2))
corrplot(M, method = "circle")
corrplot(M, method = "number")
corrplot.mixed(M)
#title="corelation efficient among clinical score and infiltration grade in different organs" 

