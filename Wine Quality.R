#load the dataset
wine_red <- read.csv("winered.csv", header = T, sep = ";")
str(wine_red)

#Check the distribution of target variable 
library(ggplot2)
ggplot(data =  wine_red, aes(wine_red$quality)) +
  geom_histogram(aes(y = ..density..), fill = 'orange') + 
  geom_density()

#target variable quality is not distributed normally

#summary statistics of data 
install.packages("psych")
library(psych)
psych :: describe(wine_red)

#check the outliers using boxplots
library (reshape)
meltData <- melt(wine_red)
p <- ggplot(meltData, aes(factor(variable), value))
p + geom_boxplot() + facet_wrap(~variable, scale="free")

#Correlation Matrix
install.packages("corrgram")
require(corrgram)
corrgram(wine_red, order = TRUE)

library(ggplot2)
library(GGally)
qplot(quality, data = wine_red, binwidth = 1) +
  scale_x_continuous(breaks = seq(3,10,1), lim = c(3,10)) +
  scale_y_sqrt()

n1 <- qplot(x = fixed.acidity, data = wine_red, 
            binwidth = 0.1) +
  scale_x_continuous(breaks = seq(4, 16, 1))
n1

n2 <- qplot(x = volatile.acidity, data = wine_red, 
            binwidth = 0.01) +
  scale_x_continuous(breaks = seq(0.12, 1.58, 0.1))
n2

n3 <- qplot(x = citric.acid, data = wine_red, 
            binwidth = 0.01) +
  scale_x_continuous(breaks = seq(0, 1, 0.1))
n3

n4 <- qplot(x = density, data = wine_red)
n4

n5 <- qplot(x = pH, data = wine_red)
n5

n6 <- qplot(x = alcohol, data = wine_red)
n6

grid.arrange(n1, n2, n3, n4, n5, n6, ncol = 2)

#Frequency Plot 
par(mfrow=c(1,1))
barplot(table(wine_red[[12]]), 
        main = sprintf('Frequency plot of the variable: %s', 
                       colnames(wine_red[12])),
        xlab = colnames(wine_red[12]),
        ylab = 'Frequency')

#Check the class bias
table(wine_red$quality)
round(prop.table((table(wine_red$quality))),2)

# Independent Variable Boxplots
par(mfrow=c(3,4))
for (i in 1:(length(wine_red)-1)){
  boxplot(x = wine_red[i], 
          horizontal = TRUE, 
          main = sprintf('Boxplot of the variable: %s', 
                         colnames(wine_red[i])),
          xlab = colnames(wine_red[i]))
}

# Histogram Plot
par(mfrow=c(3,4))
for (i in 1:(length(wine_red)-1)){
  hist(x = wine_red[[i]], 
       main = sprintf('Histogram of the variable: %s',
                      colnames(wine_red[i])), 
       xlab = colnames(wine_red[i]))
}

# Correlation Matrix 
install.packages("corrplot")
install.packages("ggplot2")
library(ggcorrplot)
ggcorrplot(round(cor(wine_red[-12]), 2), 
           type = "lower", 
           lab = TRUE, 
           title = 
             'Correlation matrix of the red wine quality dataset')

#Check and Identify Outliers
is_outlier <- function(x){
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | 
           x > quantile(x, 0.75) + 1.5 * IQR(x))
}

outlier <- data.frame(variable = character(), 
                      sum_outliers = integer(),
                      stringsAsFactors=FALSE)

for (j in 1:(length(wine_red)-1)){
  variable <- colnames(wine_red[j])
  for (i in wine_red[j]){
    sum_outliers <- sum(is_outlier(i))
  }
  row <- data.frame(variable,sum_outliers)
  outlier <- rbind(outlier, row)
}

# Identify the percentage of Outliers
for (i in 1:nrow(outlier)){
  if (outlier[i,2]/nrow(wine_red) * 100 >= 5){
    print(paste(outlier[i,1], 
                '=', 
                round(outlier[i,2]/nrow(wine_red) * 100, digits = 2),
                '%'))
  }
}
#Inputting outlier values
for (i in 4:5){
  for (j in 1:nrow(wine_red)){
    if (wine_red[[j, i]] > as.numeric(quantile(wine_red[[i]], 0.75) + 
                                      1.5 * IQR(wine_red[[i]]))){
      if (i == 4){
        wine_red[[j, i]] <- round(mean(wine_red[[i]]), digits = 2)
      } else{
        wine_red[[j, i]] <- round(mean(wine_red[[i]]), digits = 3)
      }
    }
  }
}

library(ggplot2)
library(GGally) # for ggpairs
library(memisc)
library(gridExtra)

p1up = qplot(x = quality.cat, y = alcohol, 
             data = wine_red,
             geom = "boxplot")

p2up = qplot(x = quality.cat, y = sulphates, 
             data = wine_red,
             geom = "boxplot")

p3up = qplot(x = quality.cat, y = citric.acid, 
             data = wine_red,
             geom = "boxplot")

p4up = qplot(x = quality.cat, y = fixed.acidity, 
             data = wine_red,
             geom = "boxplot")

grid.arrange(p1up, p2up, p3up, p4up, ncol = 2)

###SUPERVISED LEARNING (STEFANO + SRISHTI)###
STEFANO

#loading data
data = read.csv("C:/Users/billm/OneDrive/Desktop/DSE/STATISTICAL LEARNING/STATISTICAL LEARNING/progetto/winequality-red.csv", header = TRUE)
fix(data)
attach(data)

#add a column for the treshold categorical variable
dummy_treshold= rep("bad",1599)
dummy_treshold[quality>5]="good"
length(dummy_treshold)
data['treshold']= dummy_treshold 
attach(data)

#LINEAR DISCRIMINANT ANALYSIS
#library
library(MASS)
#creating random sample of data
XLDA= data[,1:13]
YLDA= treshold
fix(XLDA)
set.seed(123)
train = sample(1:nrow(x), nrow(x)/1.5)
test=(-train)
XtrainLDA=XLDA[train,]
YtrainLDA=YLDA[train]
dim(XtrainLDA)
YtrainLDA
fix(XtrainLDA)
YtestLDA= YLDA[test]
XtestLDA=XLDA[test,]
dim(XtestLDA)
#implementing the LDA
LDA= lda(treshold???.-quality, data = XtrainLDA)
plot(LDA)
#prediction
LDApred=predict(LDA, XtestLDA)
names(LDApred)
ldaclass=LDApred$class
table(YtestLDA, ldaclass)
#accuracy
#....

#QDA
#implementing QDA
QDA=qda(treshold???.-quality, data = XtrainLDA)
QDA
#prediction
QDAprediction = predict(QDA, XtestLDA)
QDAclass= QDAprediction$class
table(YtestLDA, QDAclass)
#accuracy
#...
# the LDA it's better than the QDA

#KNN
#needed library
library(class)
#standardizing all X variables
standardize.x=scale(data[,1:11])
#creating ramndom sample of data
Xtrain_KNN=standardize.x[train,]
Ytrain_KNN=treshold[train]
Xtest_KNN=standardize.x[test,]
Ytest_KNN= treshold[test]
#implementing the KNN
knn_prediction=knn(Xtrain_KNN, Xtest_KNN, Ytrain_KNN, k=1)
#accuracy
mean(Ytest_KNN!=knn_prediction)
mean(Ytest_KNN=="good")
#KNN seems to be better than the LDA
#we have checked that the one nearest neighbour is the best one
#among 3,5,7,9 nearest neighbours

#plotting the LDA
#converting YtrainLDA in factor
YtrainLDA = as.factor(YtrainLDA)
library(klaR)
partimat(XtrainLDA, grouping=YtrainLDA, method="lda")


#REGRESSION USING CROSS VALIDATION

#library
library(boot)
#REGRESSION
set.seed(123)
cv.error= rep(0,10)
for (i in 1:10){
  regression=glm(quality???poly(fixed.acidity,i)+poly(volatile.acidity,i)+poly(citric.acid,i)+poly(residual.sugar,i)+poly(chlorides,i)+poly(free.sulfur.dioxide,i)+poly(total.sulfur.dioxide,i)+poly(density,i)+poly(pH,i)+poly(sulphates,i)+poly(alcohol,i),data= data)
  cv.error[i]=cv.glm(data, regression, K=10)$delta[1]
}
cv.error
#the best solution is the cubic one
#cubic regression
regression=glm(quality???poly(fixed.acidity,3)+poly(volatile.acidity,3)+poly(citric.acid,3)+poly(residual.sugar,3)+poly(chlorides,3)+poly(free.sulfur.dioxide,3)+poly(total.sulfur.dioxide,3)+poly(density,3)+poly(pH,3)+poly(sulphates,3)+poly(alcohol,3),data= data)
summary(regression)
confint(regression)
#creating random sample
set.seed(123)
train = sample(1:nrow(x), nrow(x)/1.5)
test=(-train)
xTrainR= data[train,1:11]
yTrainR=data[train,12]
y
yTest1R= data[test,12]
yTest1R
xTestR= data[test,]
#prediction
prediction1=predict(regression, xTrainR)
#MSE
mean((prediction1-yTest1R)^2)

#BOOTSTRAP
boot.estimate=function(data,index)
  return(coef(glm(quality???poly(fixed.acidity,3)+poly(volatile.acidity,3)+poly(citric.acid,3)+poly(residual.sugar,3)+poly(chlorides,3)+poly(free.sulfur.dioxide,3)+poly(total.sulfur.dioxide,3)+poly(density,3)+poly(pH,3)+poly(sulphates,3)+poly(alcohol,3),data= data, subset=index)))

boot.estimate(data,1:1599)
#standard errors for the bootstrap
boot(data, boot.estimate, 1000)


#RIDGE REGRESSION
#library
library(glmnet)
#establishing the dataset
x=model.matrix(quality???., data)[,1:12]
y = quality
xTrainR2=x[train,]
xTestR2=x[test,]
yTrainR2=y[train]
yTestR2=y[test]
#establishing the cross validation lambda
grid=10^seq(10,-2, length =100)
grid
#fitting the RIDGE regression using training set
RidgeRegression= glmnet(xTrainR2, yTrainR2, alpha=0, lambda=grid, thresh=1e-12)
#using cross validation
set.seed(123)
CVoutput=cv.glmnet(xTrainR2, yTrainR2, alpha=0)
#plot the output
plot(CVoutput)
bestLambda=CVoutput$lambda.min
bestLambda
#the best lambda is 0.05345188
#this will be the best Ridge regression
BestRidge=glmnet(x, y, alpha=0, lambda = bestLambda)
predict(BestRidge, type="coefficient", s=bestLambda)
#checking the test error for the model estimated with the trainig set
prediction=predict(RidgeRegression, s=bestLambda, newx = xTestR2)
mean((prediction-yTestR2)^2)
#plotting coefficient of variables in relation to log(lambda)
plot(RidgeRegression, "lambda", label=TRUE)

#The LASSO
#library
library(glmnet)
#Adding column of degree up to 3
data['residual.sugar2']= residual.sugar^2
data['residual.sugar3']= residual.sugar^3
data['fixed.acidity2']=fixed.acidity^2
data['fixed.acidity3']=fixed.acidity^3
data['citric.acid2']=citric.acid^2
data['citric.acid3']=citric.acid^3
data['chlorides2']=chlorides^2
data['chlorides3']=chlorides^3
data['total.sulfur.dioxide2']=total.sulfur.dioxide^2
data['total.sulfur.dioxide3']=total.sulfur.dioxide^3
data['pH2']=pH^2
data['pH3']=pH^3
data['alcohol2']=alcohol^2
data['alcohol3']=alcohol^3
data['volatile.acidity2']=volatile.acidity^2
data['volatile.acidity3']=volatile.acidity^3
data['free.sulfur.dioxide2']=free.sulfur.dioxide^2
data['free.sulfur.dioxide3']=free.sulfur.dioxide^3
data['density2']=density^2
data['density3']=density^3
data['sulphates2']=sulphates^2
data['sulphates3']=sulphates^3
dim(data)
#creating the dataset for the LASSO
library(dplyr)
fix(data)
Lasso_Data=select(data, c(1,2,3,4,5,6,7,8,9,10,11,12,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35))
fix(Lasso_Data)
#creating test and training set
x1=model.matrix(quality???., Lasso_Data)
fix(x1)
y1 = quality
set.seed(123)
train = sample(1:nrow(x), nrow(x)/1.5)
train
test=(-train)
y1test = y1[test]
#implementing Lasso
LassoRegression=glmnet(x1[train,], y1[train], alpha=1, lambda= grid)
set.seed(123)
#choosing best lambda
CVoutput2=cv.glmnet(x1[train ,],y1[train],alpha=1)
plot(CVoutput2)
Bestlambda2 =CVoutput2$lambda.min
Bestlambda2
LassoPred=predict(LassoRegression ,s=Bestlambda2 ,newx=x1[test,])
#computing MSE
mean((LassoPred -y1test)^2)

#discovering the variables selcetd by Lasso
LassoRegression2=glmnet(x1, y1, alpha=1, lambda=grid)
LassoCoefficient=predict(LassoRegression2, type="coefficients", s=Bestlambda2)[1:35,]
LassoCoefficient[LassoCoefficient!=0]
#plotting coefficient in relation to the values of L1 norm and log(lambda)
plot(LassoRegression2, se.bands=TRUE)
plot(LassoRegression2, "lambda", label=TRUE)






















###UNSUPERVISED LEARNING

### K-MEANS PART  (With data normalisation)
#Import libraries
library(readxl)
library(dplyr)
library(fpc)
library(MASS)
library(caret)
library(flexclust)
library(NbClust)
library(ggplot2)
library(GGally)
#Read the file
original <- read.csv("/Users/vladislavluksha/PROJECT/winequality-red.csv", sep =",")
boxplot(original)

####Step 2: Remove the extreme outliers#####

#show the Min. 1st Qu , Median , Mean ,3rd Qu, Max. for residual sugar
original_summary <- summary(original$`residual.sugar`)
original_summary

# Estimate interquartile range (3rd quartile minus 1st quartile)
iqr <- original_summary[[5]] - original_summary[[2]]

# Identify bounds for outliers
lower_bound <- original_summary[[2]] - (1.5 * iqr)
upper_bound <- original_summary[[5]] + (1.5 * iqr)

# Identify outlier(s)
outliers <- original %>% 
  filter( `residual.sugar`> upper_bound | `residual.sugar`< lower_bound)

# Remove outliers from dataframe, but store as new dataframe "no_outliers"
no_outliers <- original %>%
  filter(`residual.sugar` < upper_bound & `residual.sugar` > lower_bound)

####
original_summary <- summary(original$`free.sulfur.dioxide`)
iqr <- original_summary[[5]] - original_summary[[2]]
# The bounds are established with the original data
lower_bound <- original_summary[[2]] - (1.5 * iqr)
upper_bound <- original_summary[[5]] + (1.5 * iqr)

outliers <- rbind(outliers,original %>% 
                    filter(`free.sulfur.dioxide` > upper_bound | `free.sulfur.dioxide` < lower_bound))

no_outliers <- no_outliers %>%
  filter(`free.sulfur.dioxide` < upper_bound & `free.sulfur.dioxide` > lower_bound)

# Repeat for fixed acidity
original_summary <- summary(original$`total.sulfur.dioxide`)
iqr <- original_summary[[5]] - original_summary[[2]]
# Remember that the bounds are based on the original data
lower_bound <- original_summary[[2]] - (1.5 * iqr)
upper_bound <- original_summary[[5]] + (1.5 * iqr)

# Removing fixed acidity outliers from the no_outliers data, not the original
outliers <- rbind(outliers,original %>% 
                    filter(`total.sulfur.dioxide` > upper_bound | `total.sulfur.dioxide` < lower_bound))

no_outliers <- no_outliers %>%
  filter(`total sulfur dioxide` < upper_bound & `total sulfur dioxide` > lower_bound)

boxplot(no_outliers)

#Scaling 
original<- scale(original[-12])
summary(original)

#NbClust() 
set.seed(1234)

nc <- NbClust(original,
              min.nc=2, max.nc=8,
              method="kmeans")

barplot(table(nc$best.n[1,]),    # provide bar chart
        xlab="Number of Clusters",
        ylab="Number of Criteria",
        main="Number of Clusters Chosen by 9 Criteria")

ws <- 0
for (i in 1:9){
  ws[i] <-
    sum(kmeans(original, centers=i)$withinss)}

plot(1:9,
     ws,
     type="b",    
     xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#kmeans=2
fit.km2 <- kmeans(original,2)

plotcluster(original, fit.km2$cluster)

#Evaluation for k=2
confuse <- table(original$quality,fit.km2$cluster)
confuse
original$quality
fit.km2$cluster
#MASS plot
parcoord(original, fit.km2$cluster)

#kmeans=3
fit.km3 <- kmeans(original , 3)
fit.km3
#Evaluation for k=3
confuse3 <- table(original$quality,fit.km3$cluster)
parcoord(original, fit.km3$cluster)

#kmeans=4
fit.km4 <- kmeans(original, 4)
parcoord(original, fit.km4$cluster)
#Evaluation for k=4
confuse4 <-table(original$quality,fit.km4$cluster)

#kmeans=5
fit.km5 <- kmeans(original, 5)
#Evaluation for k=5
confuse5 <-table(original$quality,fit.km5$cluster)

plotcluster(original,fit.km5$cluster)
parcoord(original, fit.km5$cluster)
#Evaluation with ARI for k=2 
randIndex(confuse5)

plotcluster(original,fit.km4$cluster)
parcoord(original, fit.km4$cluster)

ggpairs(cbind(original, Cluster=as.factor(fit.km4$cluster)),
        columns=6:12, aes(colour=Cluster, alpha=0.5),
        lower=list(continuous="points"),
        upper=list(continuous="blank"),
        axisLabels="none", switch="both") +
  theme_bw()




###hierarchical clustering - 

wq <- wine_red
wq_sub = subset(wq, select =  -c(quality))

#Scale data
wq.scaled <- scale(wq_sub)

#Compute distance (euclidean)
d <- dist(wq.scaled, method = "euclidean")
round(as.matrix(d) [1:12, 1:12],2)

#Heat map of distances
library(factoextra)
fviz_dist(d)

#Methods to assess
library(cluster)
m <- c("average","single", "complete","ward")
names(m) <- c("average","single", "complete","ward")
ac <- function(x) {
  agnes(d, method = x)$ac
}

library(purrr)
map_dbl(m, ac)

#Cluster using ward linkage
hclust_ward <- agnes(d, method = "ward")
pltree(hclust_ward, cex = 0.6, hang = -1)

#Cluster using complete linkage
hclust_complete <- agnes(d, method = "complete")
pltree(hclust_complete, cex = 0.6, hang = -1)

#Compare two methods
suppressPackageStartupMessages(library (dendextend))
dend1 <- as.dendrogram(hclust_ward)
dend2 <- as.dendrogram(hclust_complete)

#Measure difference between two methods
dend_list <- dendlist(dend1, dend2)
tanglegram(dend1, dend2, highlight_distinct_edges = FALSE, common_subtrees_color_lines = FALSE, common_subtrees_color_branches = TRUE, main = paste("entanglement =", round(entanglement(dend_list), 2)))

#Determine number of clusters
#Elbow Method
fviz_nbclust(wq.scaled, FUN = hcut, method = "wss")

#Average Silhouette Method
fviz_nbclust(wq.scaled, FUN = hcut, method = "silhouette")

#Gap Statistic Method
gap_stat <- clusGap(wq.scaled, FUN = hcut, nstart = 25, K.max = 10, B = 100)
fviz_gap_stat(gap_stat)

#Comparing number of k
k2 <- cutree(hclust_ward, k = 2)
k3 <- cutree(hclust_ward, k = 3)
k4 <- cutree(hclust_ward, k = 4)
k5 <- cutree(hclust_ward, k = 5)

p1 <- fviz_cluster(list(data = wq.scaled, cluster = k2)) + ggtitle("k = 2")
p2 <- fviz_cluster(list(data = wq.scaled, cluster = k3)) + ggtitle("k = 3")
p3 <- fviz_cluster(list(data = wq.scaled, cluster = k4)) + ggtitle("k = 4")
p4 <- fviz_cluster(list(data = wq.scaled, cluster = k5)) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

#Color the dendrogram
dend <- as.dendrogram(hclust_ward)
dend1 = color_branches(dend, k = 4)
plot(dend1)

#Assessment
library(dplyr)
wq.scaled <- mutate(wq_sub, cluster = k4)
count(wq.scaled, cluster)

#Results
wq_sub %>%
  mutate(cluster = k4) %>%
  head

#Interpretation of the results
aggregate(wq.scaled,list(k4),median)
aggregate(wq_sub,list(k4),median)

