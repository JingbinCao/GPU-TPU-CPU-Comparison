setwd("C:/Users/lenov/OneDrive/Desktop/CU/CUspring2021/Stat5291ADA/project")

library(dplyr)
library(car)

data <- fread("Runtime.csv", header=TRUE)
#View(data)
colnames(data)
dim(data)

head(data)
tail(data)
str(data)
summary(data)

attach(data)

MatrixOperation<-factor(MatrixOperation)
Processor<-factor(Processor)

op=par(mfrow=c(1,1))
#op=par(mfrow=c(2,1))
interaction.plot(data$MatrixSize, data$MatrixOperation, data$Runtime)
interaction.plot(data$MatrixSize, data$MatrixOperation, log(data$Runtime))
#par(op)

CPUdata <- data %>% filter(Processor=="CPU")
GPUdata <- data %>% filter(Processor=="GPU")
TPUdata <- data %>% filter(Processor=="TPU")

#op=par(mfrow=c(1,3))
interaction.plot(CPUdata$MatrixSize, CPUdata$MatrixOperation, CPUdata$Runtime)
interaction.plot(GPUdata$MatrixSize, GPUdata$MatrixOperation, GPUdata$Runtime)
interaction.plot(TPUdata$MatrixSize, TPUdata$MatrixOperation, TPUdata$Runtime)
#par(op)

library(psych)
describeBy(data$Runtime,group = data$MatrixSize, mat = TRUE) %>%  #create dataframe
  select(MatrixSize=group1, N=n, Mean=mean, SD=sd, Median=median, Min=min, Max=max, 
         Skew=skew, Kurtosis=kurtosis, SEM=se)

#boxplot(Runtime~Processor*MatrixOperation)
#tapply(Runtime,list(Processor, MatrixOperation),mean)
#tapply(Runtime,MatrixOperation,mean)

group_by(data, Processor) %>%
  summarise(
    count = n(),
    mean = mean(Runtime, na.rm = TRUE),
    sd = sd(Runtime, na.rm = TRUE)
  )

group_by(data, MatrixOperation) %>%
  summarise(
    count = n(),
    mean = mean(Runtime, na.rm = TRUE),
    sd = sd(Runtime, na.rm = TRUE)
  )

#detach(data) 

size320data <- data %>% filter(MatrixSize==320)
size640data <- data %>% filter(MatrixSize==640)
size1280data <- data %>% filter(MatrixSize==1280)
size2560data <- data %>% filter(MatrixSize==2560)

boxplot(Runtime~Processor*MatrixOperation, data = size320data, main="At the level of matrix size=320")
tapply(Runtime,list(Processor, MatrixOperation),mean, data = size320data)
boxplot(Runtime~Processor*MatrixOperation, data = size640data, main="At the level of matrix size=640")
tapply(Runtime,list(Processor, MatrixOperation),mean, data = size640data)
boxplot(Runtime~Processor*MatrixOperation, data = size1280data, main="At the level of matrix size=1280")
tapply(Runtime,list(Processor, MatrixOperation),mean, data = size1280data)
boxplot(Runtime~Processor*MatrixOperation, data = size2560data, main="At the level of matrix size=2560")
tapply(Runtime,list(Processor, MatrixOperation),mean, data = size2560data)
# At the level of matrix size=2560, avoid using CPU for inversion and multiplication, because its runtimes 
# are much bigger

#320
fit2<-lm(Runtime~Processor+MatrixOperation, data = size320data)
summary(fit2)
fit1<-lm(Runtime~Processor*MatrixOperation, data = size320data)
summary(fit1)
anova(fit2, fit1)

mod320<-aov(Runtime~Processor*MatrixOperation, data = size320data)
#summary(mod320)
Anova(mod320,type="III")

modF<-lm(Runtime~Processor+MatrixOperation, data = size320data)
modA<-lm(Runtime~Processor, data = size320data)
modB<-lm(Runtime~MatrixOperation, data = size320data)

anova(modA, modF)
anova(modB, modF)

#640
fit2_640<-lm(Runtime~Processor+MatrixOperation, data = size640data)
summary(fit2)
fit1_640<-lm(Runtime~Processor*MatrixOperation, data = size640data)
summary(fit1_640)
anova(fit2_640, fit1_640)

mod640<-aov(Runtime~Processor*MatrixOperation, data = size640data)
Anova(mod640,type="III")


modF<-lm(Runtime~Processor+MatrixOperation, data = size640data)
modA<-lm(Runtime~Processor, data = size640data)
modB<-lm(Runtime~MatrixOperation, data = size640data)

anova(modA, modF)
anova(modB, modF)

#1280
fit2_1280<-lm(Runtime~Processor+MatrixOperation, data = size1280data)
summary(fit2)
fit1_1280<-lm(Runtime~Processor*MatrixOperation, data = size1280data)
summary(fit1_1280)
anova(fit2_1280, fit1_1280)

modF<-lm(Runtime~Processor+MatrixOperation, data = size1280data)
modA<-lm(Runtime~Processor, data = size1280data)
modB<-lm(Runtime~MatrixOperation, data = size1280data)

anova(modA, modF)
anova(modB, modF)

mod1280<-aov(Runtime~Processor*MatrixOperation, data = size1280data)
Anova(mod1280,type="III")

#2560
fit2_2560<-lm(Runtime~Processor+MatrixOperation, data = size2560data)
summary(fit2)
fit1_2560<-lm(Runtime~Processor*MatrixOperation, data = size2560data)
summary(fit1_2560)
anova(fit2_2560, fit1_2560)

mod2560<-aov(Runtime~Processor*MatrixOperation, data = size2560data)
Anova(mod2560,type="III")

modF<-lm(Runtime~Processor+MatrixOperation, data = size2560data)
modA<-lm(Runtime~Processor, data = size2560data)
modB<-lm(Runtime~MatrixOperation, data = size2560data)

anova(modA, modF)
anova(modB, modF)

