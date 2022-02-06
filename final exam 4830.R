view(gene)
dim(gene)
install.packages('tidyverse')
library(tidyverse)
glimpse(gene)
## ques 1 do we need to standardize or normalise
install.packages('Hmisc')
library(Hmisc)
x <-gene[1:500]
sapply(gene[1:500],mean) ## taking  500 observations as the last one is char 
##ques 2
install.packages('factoextra')
library(factoextra)
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = gene$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()
## ques 3
install.packages('factoextra')
library(factoextra)

install.packages("dplyr")
library(dplyr)

bias_sample <- rowMeans(x)
install.packages('ggplot2')
library(ggplot2)
data.frame(pc_1 = pc$x[,1], bias_sample, 
           tissue = gene$y) %>%
  ggplot(aes(pc_1, bias_sample, color = tissue)) +
  geom_point()

##ques4
#BLANK
x <- with(gene, sweep(x, 1, rowMeans(x)))
# END BLANK
install.packages('factoextra')
library(factoextra)
install.packages('dplyr')
library(dplyr)

pc2 <- prcomp(x)
install.packages('ggplot2')
library(ggplot2)
data.frame(pc_1 = pc2$x[,1], pc_2 = pc2$x[,2], 
           tissue = gene$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()
#ques5
boxplot(pc$x[,1] ~ gene$y, main = paste("PC", 1))
boxplot(pc$x[,2] ~ gene$y, main = paste("PC", 2))
boxplot(pc$x[,3] ~ gene$y, main = paste("PC", 3))
boxplot(pc$x[,4] ~ gene$y, main = paste("PC", 4))
boxplot(pc$x[,5] ~ gene$y, main = paste("PC", 5))
boxplot(pc$x[,6] ~ gene$y, main = paste("PC", 6))
boxplot(pc$x[,7] ~ gene$y, main = paste("PC", 7))
boxplot(pc$x[,8] ~ gene$y, main = paste("PC", 8))
boxplot(pc$x[,9] ~ gene$y, main = paste("PC", 9))
boxplot(pc$x[,10] ~ gene$y, main = paste("PC", 10))
# ques 6
summary(pc2)
pca_var <- sapply(pc2$sdev[1:10], function(pc_i){
  pc_i^2 / (t(pc2$sdev) %*% pc2$sdev)  
})
plot(pca_var)
cumsum(pca_var)
# ques 7
install.packages('MASS')
library(MASS)

lagene.data <- sample(x=gene,size = 10)
lagene.data$y <- gene$y
lagene <- lda(y~.,lagene.data)
lagene


lagene$scaling[,1]


par(mfrow=c(1,1))
plot(lagene)

#now, we can compare those values to the ones that we calculated
gene.lda.values <- predict(lagene, gene[])
gene.lda.values$x[,1] 
prediction <- predict(lagene, gene)
prediction

#stacked histogram of discriminant functions
par(mar=c(1,1,1,1))
ldahist(data = prediction$x[, 1], g = gene$y)

#Confusion matrix and accuracy for training data
prediction1 <- predict(lagene, gene)$class
prediction1

confusiontab <- table(Predicted = prediction1, Actual = gene$y)
confusiontab

#sum the leading diagonal of the confusion matrix
sum(diag(confusiontab))/sum(confusiontab)

