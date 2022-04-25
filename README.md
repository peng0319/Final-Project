# Final-Project
#import data set
```{r}
library(readxl)
PRF_dataset <- read_excel("~/Downloads/PRF dataset.xlsx")
```

#age frequency (better to use age 11-15, around junior high school range)
```{r}
agechildren1<-table(PRF_dataset$ChildrenInfo_ChildrenInfo_1_1)
plot(agechildren1)
```
```{r}
agechildren2<-table(PRF_dataset$ChildrenInfo_ChildrenInfo_2_1)
plot(agechildren2)
```
```{r}
library(dplyr)
```
#age restriction (data cleaning)
```{r}
shortPRF_dataset <-filter(PRF_dataset, ChildrenInfo_ChildrenInfo_1_1 %in% c("11.0","12.0","12岁半","13.0","13岁","14.0","14岁","十四", "15.0"))
```

#data cleanning for the empty spot

```{r}
shortPRF_dataset$SDQ_6<- ifelse(shortPRF_dataset$SDQ_6 == -99, "", shortPRF_dataset$SDQ_6)
shortPRF_dataset$SDQ_11r<- ifelse(shortPRF_dataset$SDQ_11r == -99, "", shortPRF_dataset$SDQ_11r)
shortPRF_dataset$SDQ_14r <- ifelse(shortPRF_dataset$SDQ_14r== -99, "", shortPRF_dataset$SDQ_14r)
shortPRF_dataset$SDQ_19 <- ifelse(shortPRF_dataset$SDQ_19== -99, "", shortPRF_dataset$SDQ_19)
shortPRF_dataset$SDQ_23 <- ifelse(shortPRF_dataset$SDQ_23== -99, "", shortPRF_dataset$SDQ_23)
shortPRF_dataset$SDQ_1 <- ifelse(shortPRF_dataset$SDQ_1== -99, "", shortPRF_dataset$SDQ_1)
shortPRF_dataset$SDQ_4 <- ifelse(shortPRF_dataset$SDQ_4== -99, "", shortPRF_dataset$SDQ_4)
shortPRF_dataset$SDQ_9 <- ifelse(shortPRF_dataset$SDQ_9== -99, "", shortPRF_dataset$SDQ_9)
shortPRF_dataset$SDQ_17 <- ifelse(shortPRF_dataset$SDQ_17== -99, "",shortPRF_dataset$SDQ_17)
shortPRF_dataset$SDQ_20 <- ifelse(shortPRF_dataset$SDQ_20== -99, "", shortPRF_dataset$SDQ_20)
shortPRF_dataset$Warmth<- ifelse(shortPRF_dataset$Warmth == -99, "", shortPRF_dataset$Warmth)
shortPRF_dataset$Coercion <- ifelse(shortPRF_dataset$Coercion== -99, "", shortPRF_dataset$Coercion)
shortPRF_dataset$Punitive<- ifelse(shortPRF_dataset$Punitive== -99, "", shortPRF_dataset$Punitive)
shortPRF_dataset$Shaming <- ifelse(shortPRF_dataset$Shaming== -99, "", shortPRF_dataset$Shaming)
shortPRF_dataset$Protection <- ifelse(shortPRF_dataset$Protection== -99, "", shortPRF_dataset$Protection)
shortPRF_dataset$PSDQ_1 <- ifelse(shortPRF_dataset$PSDQ_1== -99, "", shortPRF_dataset$PSDQ_1)
shortPRF_dataset$PSDQ_2<- ifelse(shortPRF_dataset$PSDQ_2== -99, "", shortPRF_dataset$PSDQ_2)
shortPRF_dataset$PSDQ_3<- ifelse(shortPRF_dataset$PSDQ_3== -99, "", shortPRF_dataset$PSDQ_3)
shortPRF_dataset$PSDQ_4 <- ifelse(shortPRF_dataset$PSDQ_4== -99, "", shortPRF_dataset$PSDQ_4)
shortPRF_dataset$PSDQ_5<- ifelse(shortPRF_dataset$PSDQ_5== -99, "",shortPRF_dataset$PSDQ_5)
shortPRF_dataset$PSDQ_6<- ifelse(shortPRF_dataset$PSDQ_6== -99, "", shortPRF_dataset$PSDQ_6)
shortPRF_dataset$PSDQ_7<- ifelse(shortPRF_dataset$PSDQ_7== -99, "", shortPRF_dataset$PSDQ_7)
shortPRF_dataset$PSDQ_8 <- ifelse(shortPRF_dataset$PSDQ_8== -99, "", shortPRF_dataset$PSDQ_8)
shortPRF_dataset$PSDQ_9<- ifelse(shortPRF_dataset$PSDQ_9== -99, "", shortPRF_dataset$PSDQ_9)
shortPRF_dataset$PSDQ_10<- ifelse(shortPRF_dataset$PSDQ_10== -99, "", shortPRF_dataset$PSDQ_10)
shortPRF_dataset$PSDQ_11 <- ifelse(shortPRF_dataset$PSDQ_11== -99, "", shortPRF_dataset$PSDQ_11)
shortPRF_dataset$PSDQ_12<- ifelse(shortPRF_dataset$PSDQ_12== -99, "",shortPRF_dataset$PSDQ_12)
shortPRF_dataset$PSDQ_13 <- ifelse(shortPRF_dataset$PSDQ_13== -99, "", shortPRF_dataset$PSDQ_13)
shortPRF_dataset$PSDQ_14 <- ifelse(shortPRF_dataset$PSDQ_14== -99, "", shortPRF_dataset$PSDQ_14)
shortPRF_dataset$PSDQ_15 <- ifelse(shortPRF_dataset$PSDQ_15== -99, "", shortPRF_dataset$PSDQ_15)
shortPRF_dataset$PSDQ_16 <- ifelse(shortPRF_dataset$PSDQ_16== -99, "", shortPRF_dataset$PSDQ_16)
shortPRF_dataset$PSDQ_17 <- ifelse(shortPRF_dataset$PSDQ_17== -99, "", shortPRF_dataset$PSDQ_17)
shortPRF_dataset$PSDQ_18 <- ifelse(shortPRF_dataset$PSDQ_18== -99, "", shortPRF_dataset$PSDQ_18)
shortPRF_dataset$PSDQ_19 <- ifelse(shortPRF_dataset$PSDQ_19== -99, "", shortPRF_dataset$PSDQ_19)
shortPRF_dataset$PSDQ_20 <- ifelse(shortPRF_dataset$PSDQ_20== -99, "", shortPRF_dataset$PSDQ_20)
shortPRF_dataset$PSDQ_21 <- ifelse(shortPRF_dataset$PSDQ_21== -99, "", shortPRF_dataset$PSDQ_21)
shortPRF_dataset$PSDQ_22 <- ifelse(shortPRF_dataset$PSDQ_22== -99, "", shortPRF_dataset$PSDQ_22)

shortPRF_dataset$Age<- ifelse(shortPRF_dataset$Age== -99, "", shortPRF_dataset$Age)
shortPRF_dataset$Gender<- ifelse(shortPRF_dataset$Gender== -99, "", shortPRF_dataset$Gender)
shortPRF_dataset$Edu<- ifelse(shortPRF_dataset$Edu== -99, "", shortPRF_dataset$Edu)
shortPRF_dataset$Income<- ifelse(shortPRF_dataset$Income== -99, "", shortPRF_dataset$Income)
```
#Descriptive analysis

```{r}
summary(shortPRF_dataset)
library(ggplot2)
```

```{r}
ggplot(shortPRF_dataset, aes(Age))+geom_bar()
ggplot(shortPRF_dataset, aes(Edu))+geom_bar()
ggplot(shortPRF_dataset, aes(Income))+geom_bar()
ggplot(shortPRF_dataset, aes(Gender))+geom_bar()

mean(as.numeric(shortPRF_dataset$Age), na.rm = TRUE)
sd(as.numeric(shortPRF_dataset$Age), na.rm = TRUE)
mean(as.numeric(shortPRF_dataset$Edu), na.rm = TRUE)
sd(as.numeric(shortPRF_dataset$Edu), na.rm = TRUE)
library(modeest)
mfv(shortPRF_dataset$Edu)
table(as.numeric(shortPRF_dataset$Gender))
table(shortPRF_dataset$Edu <= 17)
```

```{r}
summary(shortPRF_dataset$Age)
summary(shortPRF_dataset$Edu)
summary(as.factor(shortPRF_dataset$Income))
summary(shortPRF_dataset$Gender)
```


```{r}
table(shortPRF_dataset$Edu)
```



#SDQ calculation
1. Peer problem subscale

```{r}
shortPRF_dataset$SDQ_6<-as.numeric(shortPRF_dataset$SDQ_6)
shortPRF_dataset$SDQ_11r<-as.numeric(shortPRF_dataset$SDQ_11r)
shortPRF_dataset$SDQ_14r<-as.numeric(shortPRF_dataset$SDQ_14r)
shortPRF_dataset$SDQ_19<-as.numeric(shortPRF_dataset$SDQ_19)
shortPRF_dataset$SDQ_23<-as.numeric(shortPRF_dataset$SDQ_23)
shortPRF_dataset$Peerproblem<-rowSums(shortPRF_dataset[,c("SDQ_6","SDQ_11r","SDQ_14r","SDQ_19","SDQ_23")],na.rm = TRUE)/5
```
2. Pro social subscale
```{r}
shortPRF_dataset$SDQ_1<-as.numeric(shortPRF_dataset$SDQ_1)
shortPRF_dataset$SDQ_4<-as.numeric(shortPRF_dataset$SDQ_4)
shortPRF_dataset$SDQ_9<-as.numeric(shortPRF_dataset$SDQ_9)
shortPRF_dataset$SDQ_17<-as.numeric(shortPRF_dataset$SDQ_17)
shortPRF_dataset$SDQ_20<-as.numeric(shortPRF_dataset$SDQ_20)
shortPRF_dataset$prosocial<-rowSums(shortPRF_dataset[,c("SDQ_1","SDQ_4","SDQ_9","SDQ_17","SDQ_20")],na.rm = TRUE)/5
```

#Correlation Test (For each dimension in PSDQ and SDQ) 
```{r}
correlationtest<-cor(shortPRF_dataset[, c('Warmth', 'Coercion', 'Punitive','Shaming','Protection', 'Peerproblem', 'prosocial')],use="complete.obs")
correlationtest
```

```{r}
library(Hmisc)
significantlevel<-rcorr(as.matrix(correlationtest))

Pcorelatoin<- shortPRF_dataset[, c('Warmth', 'Coercion', 'Punitive','Shaming','Protection', 'Peerproblem', 'Prosocial')]
Pvalue<- rcorr(as.matrix(Pcorelatoin))
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
    )
}
flattenCorrMatrix(Pvalue$r, Pvalue$P)
```
# Correlation visualization 

```{r}
library(corrplot)
corrplot(correlationtest, method="circle")
```
#Multiple Regression Test 
1. How gender and age and education and income level predict Parents' Warmth dimension
```{r}
shortPRF_dataset$Age<- as.numeric(shortPRF_dataset$Age)
shortPRF_dataset$Age 
shortPRF_dataset$Edu<- as.numeric(shortPRF_dataset$Edu)
shortPRF_dataset$Gender<-as.factor(shortPRF_dataset$Gender)
```

```{r}
warmthdimension<-lm(shortPRF_dataset$Warmth ~ shortPRF_dataset$Age + shortPRF_dataset$Edu + shortPRF_dataset$Income+shortPRF_dataset$Gender, data=shortPRF_dataset)
summary(warmthdimension)
```

```{r}
Coerciondimension<-lm(shortPRF_dataset$Coercion ~ shortPRF_dataset$Age + shortPRF_dataset$Edu + shortPRF_dataset$Income+shortPRF_dataset$Gender, data=shortPRF_dataset)
summary(Coerciondimension)
```

```{r}
Punitivedimension<-lm(shortPRF_dataset$Punitive ~ shortPRF_dataset$Age + shortPRF_dataset$Edu + shortPRF_dataset$Income+shortPRF_dataset$Gender, data=shortPRF_dataset)
summary(Punitivedimension)
```

```{r}
Shamingdimension<-lm(shortPRF_dataset$Shaming ~ shortPRF_dataset$Age + shortPRF_dataset$Edu + shortPRF_dataset$Income+shortPRF_dataset$Gender, data=shortPRF_dataset)
summary(Shamingdimension)
```

```{r}
Protectiondimension<-lm(shortPRF_dataset$Protection ~ shortPRF_dataset$Age + shortPRF_dataset$Edu + shortPRF_dataset$Income+shortPRF_dataset$Gender, data=shortPRF_dataset)
summary(Protectiondimension)
```



#Multiple regression test visualization 

```{r}
library(car)
avPlots(WarmthDimension, main=paste("Warmth dimention of demographic variables"))
```

```{r}
avPlots(Coerciondimension)
```

```{r}
avPlots(Punitivedimension)
```

```{r}
avPlots(ShamingDimension, main=paste("Shaming dimention of demographic variables"))
```

```{r}
avPlots(Protectiondimension)
```
