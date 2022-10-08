#DSE HDB Project

#In this project, I aim to predict the price of hdb using 
rm(list=ls(x))
#setwd('/Users/jamesteo/Desktop/DSE1101')
library(readxl)
data <- read_excel("HDB_data_2021_sample.xlsx")

#normalize the resale price by dividing 1000
#install.packages('dplyr')
library(dplyr)
data <- data %>% 
  mutate(resale_price = resale_price / 1000)

#I will not be removing any outliers. and there are no missing values.
#Split data into test and train
ntrain=4500 #split data using 75/25 ratio
set.seed(567834)
tr = sample(1:nrow(data),ntrain)  # draw ntrain observations from original data
train = data[tr,]   # Training sample: 4500 obs. of 14 variables
test = data[-tr,]   # Testing sample: 1500 obs. of 14 variables
attach(train)

# Create a dummy variable that equals to one if the resale_price is the median of the sample
price_high = ifelse(resale_price > quantile(resale_price, 0.50), 1, 0)


###########################
###Unsupervised Learning
###########################

###########################
###Kernel density estimates
############################LAB 11
#Here we plot the kernel density estimates of Resale Prices for a range of bandwiths
#kde(data,h) gives the kernel density estimate of the data using bandwidth h
library(ks)
plot(kde(resale_price, h = 0.1), main = "Resale Price Kernel Density Estimate - h=0.1", xlab = "Resale Price")
plot(kde(resale_price, h = 0.5), main = "Resale Price Kernel Density Estimate - h=0.5", xlab = "Resale Price")
plot(kde(resale_price, h = 0.7), main = "Resale Price Kernel Density Estimate - h=0.7", xlab = "Resale Price")
#Contrast with the bandwith selected to minimize MISE
#To get this bandwith, we first invoke the function hlscv(data), which gives a data-dependent minimum-MISE bandwith recommendation
plot(kde(resale_price, h = hlscv(resale_price)), main = "Resale price Kernel Density Estimate - min MISE", xlab = "Resale Price(thousands)")
#most common at around 500000


################
###K-Means
################LAB 11
#Examine kmeans for resale price and distance to cbd only (note scale(data) standardizes data):
grp1 = kmeans(train[,c("resale_price", "floor_area_sqm", "Dist_CBD", "max_floor_lvl")], centers = 4, nstart = 20)
grp2 = kmeans(scale(train[,c("resale_price", "floor_area_sqm", "Dist_CBD", "max_floor_lvl")]), centers = 4, nstart = 20)
grp1$cluster
grp2$cluster
par(mfrow=c(1,2))
plot(floor_area_sqm, resale_price, main = "K = 4(standardized)", xlab="Floor Area Sqm", ylab="Resale Price(thousands)", type="n")
text(floor_area_sqm, resale_price, col = rainbow(4)[grp2$cluster]) 
plot(floor_area_sqm, resale_price, main = "K = 4(not stanardized)", xlab="Floor Area Sqm", ylab="Resale Price(thousands)", type="n")
text(floor_area_sqm, resale_price, col = rainbow(4)[grp1$cluster]) 
#plot suggests that there are many fcators that can affect resale price such as 
#distance to cbd as blue cluster suggests a cluster of high priced buildings
#beyond factors such as floor area sqm

#Choosing K with AICc and BIC. Information criteria.
n = nrow(train) #get sample size
d=4 #dimension of the data (for naive degrees of freedom estimate)
kt = 1:50 #consider K from 1 to 20
bic = rep(0,50) #blanks for BIC. information criteria
aicc = rep(0,50) #blanks for AIC
for(ii in 1:50) { #loop over K
  fit = kmeans(scale(train[,c("resale_price", "floor_area_sqm", "Dist_CBD", "max_floor_lvl")]), centers = ii, nstart = 20) #do k-means with ii clusters
  df = d*ii #estimate for degrees of freedom
  #The measure of sum of squares here is total within sum of squares: 
  bic[ii] = fit$tot.withinss + log(n)*df #BIC from the slides
  aicc[ii] = fit$tot.withinss + 2*df*n/(n-df-1) #AICc from the slides
} #whenever i see ii, i will insert in the kmeans

#Get selected K from the IC:
bicsel=which.min(bic) #K=36
aiccsel=which.min(aicc) #K=49

#Plot the AICc and BIC curves
plot(kt, bic, main = "IC", xlab="K", ylab="IC", type="l", col = "red", ylim = c(40,20000))
lines(kt, aicc, type="l", col = "blue")
legend("topleft", c("BIC","AICc"), lty=c(1,1) ,col=c("red","blue"))

#Examine kmeans with min bic:
grp3 = kmeans(scale(train[,c("resale_price", "floor_area_sqm", "Dist_CBD", "max_floor_lvl")]), centers = 36, nstart = 20)
grp3$cluster #number of clusters
plot(floor_area_sqm, resale_price, main = "K = 36", xlab="Floor Area Sqm", ylab="Resale Price", type="n")
text(floor_area_sqm, resale_price, col = rainbow(36)[grp3$cluster]) 
#now we will observe 36 clusters.


#############################
#####Hierarchical clustering
#############################
sd.dataSmall = scale(train[,c("resale_price", "floor_area_sqm", "Dist_CBD")])
distSmall = dist(sd.dataSmall) 
#Plot the dendgrogram for GDP and Exprop using complete linkage
plot(hclust(distSmall, method = "complete"), 
     main = "HDB Resale Price - Complete Linkage", xlab = "", sub = "")



#############################
#####PCA
#############################
#As there are many variables, i will firstly do a PCA to try to reduce the dimension
#take out all continuous variables
df_train = subset(train, select = c(resale_price, Remaining_lease,Dist_CBD, Dist_nearest_A_hospital,
                                    Dist_nearest_ADF, Dist_nearest_beach, Dist_nearest_CC, Dist_nearest_G_jc, Dist_nearest_G_primary_school,
                                    Dist_nearest_G_secondary_school, Dist_nearest_GAI_jc, Dist_nearest_GAI_primary_school,
                                    Dist_nearest_GAI_secondary_school, Dist_nearest_GHawker, Dist_nearest_hospital, Dist_nearest_jc,
                                    Dist_nearest_mall, Dist_nearest_polytechnic, Dist_nearest_primary_school, Dist_nearest_secondary_school,
                                    Dist_nearest_station, Dist_nearest_university, Dist_nearest_waterbody,floor_area_sqm, max_floor_lvl,
                                    total_dwelling_units, X1room_rental, X1room_sold, X2room_rental, X2room_sold, X3room_rental,
                                    X3room_sold, X4room_sold, X5room_sold,nearest_ghawker_no_of_cooked_food_stalls, nearest_ghawker_no_of_mkt_produce_stalls,
                                    nearest_ghawker_no_of_stalls,no_G_primary_schools_1km, no_G_primary_schools_2km,
                                    no_GAI_primary_schools_1km, no_GAI_primary_schools_2km, no_malls_0.5km, no_malls_1km, no_malls_2km,
                                    no_primary_schools_1km, no_primary_schools_2km) )

df_test = subset(test, select = c(resale_price, Remaining_lease,Dist_CBD, Dist_nearest_A_hospital,
                                  Dist_nearest_ADF, Dist_nearest_beach, Dist_nearest_CC, Dist_nearest_G_jc, Dist_nearest_G_primary_school,
                                  Dist_nearest_G_secondary_school, Dist_nearest_GAI_jc, Dist_nearest_GAI_primary_school,
                                  Dist_nearest_GAI_secondary_school, Dist_nearest_GHawker, Dist_nearest_hospital, Dist_nearest_jc,
                                  Dist_nearest_mall, Dist_nearest_polytechnic, Dist_nearest_primary_school, Dist_nearest_secondary_school,
                                  Dist_nearest_station, Dist_nearest_university, Dist_nearest_waterbody,floor_area_sqm, max_floor_lvl,
                                  total_dwelling_units, X1room_rental, X1room_sold, X2room_rental, X2room_sold, X3room_rental,
                                  X3room_sold, X4room_sold, X5room_sold,nearest_ghawker_no_of_cooked_food_stalls, nearest_ghawker_no_of_mkt_produce_stalls,
                                  nearest_ghawker_no_of_stalls,no_G_primary_schools_1km, no_G_primary_schools_2km,
                                  no_GAI_primary_schools_1km, no_GAI_primary_schools_2km, no_malls_0.5km, no_malls_1km, no_malls_2km,
                                  no_primary_schools_1km, no_primary_schools_2km) )
prall = prcomp(df_train, scale =  TRUE, validation = 'CV') 
biplot(prall)
prall.s = summary(prall)
scree = prall.s$importance[2,]
sum(scree[1:20])
#PC1     PC2     PC3     PC4     PC5     PC6     PC7     PC8     PC9    PC10    PC11    PC12    PC13    PC14    PC15    PC16 
#0.15703 0.10077 0.07368 0.05849 0.05619 0.04636 0.04022 0.03716 0.03203 0.03053 0.02888 0.02590 0.02407 0.02276 0.02221 0.02101 
#PC17    PC18    PC19    PC20    PC21    PC22    PC23    PC24    PC25    PC26    PC27    PC28    PC29    PC30    PC31    PC32 
#0.01975 0.01853 0.01772 0.01686 0.01602 0.01461 0.01258 0.01186 0.01126 0.01087 0.01005 0.00985 0.00793 0.00749 0.00679 0.00535 
#PC33    PC34    PC35    PC36    PC37    PC38    PC39    PC40    PC41    PC42    PC43    PC44    PC45    PC46 
#0.00458 0.00438 0.00355 0.00315 0.00251 0.00234 0.00162 0.00144 0.00073
plot(scree, main = "HDB Prices Scree Plot", xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", ylim = c(0,1), type = 'b', cex = .8)


###Fitting PC regression
#install.packages("pls") #package to fit PCR
library(pls) #load the library to run PCR
set.seed(3457534)
pcr.fit=pcr(price_high~.,data=df_train, scale=TRUE, validation="CV")

plot(pcr.fit, "loadings", comps = 1:3, legendpos = "topleft")
abline(h = 0) #add the zero line for reference
#Plot CV MSE
validationplot(pcr.fit, val.type="MSEP", main="CV",legendpos = "topright")

pcr.pred=predict(pcr.fit, newdata=test, ncomp=25) #using training data but usually is test data
mean((resale_price-pcr.pred)^2) #MSE for PCR = 0.9656047

#some unsupervised learning to attempt to get some value out of these 
#data and understand the factors behind the difference in housing prices before building
#any forms of model



###########################
#knnpred
###########################
knnpred=kknn(resale_price~floor_area_sqm,train,test,k=20,kernel = "rectangular")
head(knnpred$fitted.values)
summary(knnpred)



#######################################
#DESCRIPTIVE ANALYSIS
#######################################
#Histogram of resale price
hist(resale_price, 
     main = "Histogram of Resale Price",
     xlab = "Resale Price(thousands)",
     col = "wheat", breaks = 20)
summary(resale_price) 
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 180000  380000  468000  497486  585000 1220000 
#From histogram, it could be seen that resale price of hdb is skewed towards
#the right and that it is unimodal.The mean is $468000 and median is $497486.

#BOX PLOT
boxplot(floor_area_sqm,
        main = "Floor Area Sqm",
        xlab = "", ylab = "", horizontal = TRUE)

#SCATTER PLOT
plot(floor_area_sqm, resale_price,
     xlab = "Floor Area Sqm", ylab = "Resale Price")
lm.mean = lm(resale_price~1)
abline(lm.mean, col = "red")
#from scatter plot, a positive relationship can be seen between floor area sqm
#and resale price. I can attempt to fit it into a linear regression.



#######################################
#REGRESSION ANALYSIS
#######################################
attach(train)
#FORWARD SELECTION
# 1. Simple linear regression of price (Y) on floor are sqm(x)
lm_fit1 = lm(resale_price ~ floor_area_sqm)
summary(lm_fit1) #Adjusted R-squared:  0.3898

#Consider other flat properties such as the max floor lvl
#Multiple linear regression of price(Y) on floor_area_sqm(X1), Remaining_lease(X2)
lm_fit = lm(resale_price ~ floor_area_sqm + max_floor_lvl)
summary(lm_fit) #Adjusted R-squared:  0.6693

#Considering other features such as amenities, geographic and time controls
#3. Multiple linear regression of price(Y) on floor_area_sqm(X1), max_floor_lvl(X2), Dist_CBD(X3), Remaining_lease(X4), Dist_nearest_jc(X5) 

lm_fit2 = lm(resale_price ~ floor_area_sqm + max_floor_lvl + Dist_CBD + Remaining_lease + Dist_nearest_jc)
summary(lm_fit2) #Adjusted R-squared:  0.8076
predlm = predict.lm(lm_fit2, newdata = test)
mean((test$resale_price-predlm)^2) #MSE=5431.123
#I will stop my forward selection here since R-squared is above 0.8, which is my stopping rule 

#full model
lm_full = lm(resale_price ~ ., data = train)
summary(lm_full) #Adjusted R-squared:  0.9266 
predlm = predict.lm(lm_full, newdata = test)
mean((test$resale_price-predlm)^2) #2029.528


###############################
# Multicollinearity. 
###############################
col_var = subset(train, select = c(Remaining_lease,Dist_CBD, max_floor_lvl, Dist_nearest_jc, floor_area_sqm))
#some of the models are highly related with each other
#compute pair-wise correlation matrix of the 6 variables used
round(cor(col_var), 2) #round to 0.01
#install.packages("corrplot")
library(corrplot)
cor_matrix = round(cor(col_var), 2) 
corrplot(cor_matrix, type = "upper", order = 'alphabet',
         tl.col = "black", tl.srt = 45, tl.cex = 0.9,   # 'options on text
         method = "circle") 

lm_non_add = lm(resale_price ~ max_floor_lvl * Remaining_lease, data = train)
summary(lm_non_add) #tvalue of max_floor_lvl * Remaining_lease is > 1.96 while age is not

lm_fit3 = lm(resale_price ~ floor_area_sqm + max_floor_lvl + Dist_CBD + Remaining_lease + Dist_nearest_jc + 
               max_floor_lvl * Remaining_lease + Dist_nearest_jc * Remaining_lease)

summary(lm_fit3) #Adjusted R-squared:  0.8121 vs 0.8076
predlm = predict.lm(lm_fit4, newdata = test)
mean((test$resale_price-predlm)^2) #MSE=5357.794


#######################################
#Polynomial model regression
####################################### 

#Non-linear relationship of price(Y) on floor_area_sqm(X1), max_floor_lvl(X2), Dist_CBD(X3), Remaining_lease(X4)
lm_fit4 = lm(resale_price ~ poly(floor_area_sqm, degree = 2, raw = TRUE) + max_floor_lvl+ Dist_CBD + Remaining_lease+ Dist_nearest_jc)
summary(lm_fit4) #Adjusted R-squared:  0.8083

# K-fold cross validation (K-fold CV) to get best polynomial degree
#glm_fit1 = glm(medv ~ lstat, data = train) # polynomial of degree 1
attach(train)
library(boot) 
set.seed(21) 
cv_error = rep(0, 20)
for (i in 1:20){
  glm_fit = glm(resale_price ~ poly(max_floor_lvl, i, raw = TRUE)+ floor_area_sqm + Dist_CBD + Remaining_lease + Dist_nearest_jc, data = train) 
  cv_error[i] = cv.glm(train, glm_fit, K = 5)$delta[1]  # K-fold CV where K = 10
}
min(cv_error)  # test MSE minimum of 4643.673(max_floor_lvl) at k =18, 

lm_fit4 = lm(resale_price ~ poly(max_floor_lvl, degree = 18, raw = TRUE) +floor_area_sqm 
             + max_floor_lvl+ Dist_CBD + Remaining_lease+ Dist_nearest_jc)

summary(lm_fit4) #Adjusted R-squared:  0.8249
predlm = predict.lm(lm_fit5, newdata = test)
mean((test$resale_price-predlm)^2) #5024.892

predict(lm_fit4, newdata = data.frame(
  floor_area_sqm = c(67,124,93), max_floor_lvl = c(11,18,28), 
  Dist_CBD= c(13.12,16.91,4.23), Remaining_lease = c(61,74,94), 
  Dist_nearest_jc= c(2.85,1.42,1.41)), type = "response")
#Prices predicted: 229.1511 545.0284 764.2532 



#######################################
#Logistic regression model
#######################################
# Logistic regression model of price_high (Y) on floor_area_sqm(X1), max_floor_lvl(X2), Dist_CBD(X3), Remaining_lease(X4), Dist_nearest_jc(X5)
glm_fit = glm(price_high ~ floor_area_sqm + max_floor_lvl + Dist_CBD + Remaining_lease + Dist_nearest_jc,
              data = train, family = binomial)
# Predicted probability for all observations in the dataset
glm_prob = predict(glm_fit, type = "response")
# Confusion matrix at threshold = 0.5
confusion_matrix = table(glm_prob > 0.5, price_high)
sum(diag(confusion_matrix)) / sum(confusion_matrix)  #accuracy = 0.8293333

# ROC CURVE: FOR ALL POSSIBLE THRESHOLDS
pred = prediction(glm_prob, price_high)          
perf = performance(pred, measure = "tpr", x.measure = "fpr") 
auc_perf = performance(pred, measure = "auc")               
round(auc_perf@y.values[[1]], 2)     
plot(perf, col = "steelblue", lwd = 2, main="ROC for Logistic Regression") 
abline(0, 1, lwd = 1, lty = 2)       
text(0.6, 0.6, paste("AUC =", round(auc_perf@y.values[[1]], 2)))  #AUC = 0.94

# FIND CUTOFF THAT MAXIMIZES ACCURACY
accuracy_perf = performance(pred, measure = "acc")     # extract the performance measures ACC
plot(accuracy_perf, col = "deeppink3", lwd = 2)
ind = which.max(slot(accuracy_perf, "y.values")[[1]])  # Find the maximum point
acc = slot(accuracy_perf, "y.values")[[1]][ind]        
cutoff = slot(accuracy_perf, "x.values")[[1]][ind]
print(c(accuracy = acc, cutoff = cutoff))           
#accuracy= 0.8462222  cutoff.4981= 0.3274392

# Confusion matrix at the optimal cutoff
confusion_matrix = table(glm_prob > cutoff, price_high)
sum(diag(confusion_matrix)) / sum(confusion_matrix) #accuracy = 0.846
confusion_matrix

#VALIDATION SET APPROACH
price_high_test = ifelse(test$resale_price > quantile(test$resale_price, 0.50), 1, 0)
glm_prob = predict(glm_fit, 
                   newdata = test,
                   type = "response")
table(glm_prob > cutoff, price_high_test)
sum(diag(confusion_matrix)) / sum(confusion_matrix) # Accuracy = 0.846



#######################################
#KNN regression
#######################################
attach(train)
library(ROCR) 
library(kknn) 

hdbcv=train.kknn(resale_price~floor_area_sqm + max_floor_lvl + Dist_CBD + Remaining_lease 
                 + Dist_nearest_jc,data=train,kmax=100, kernel = "rectangular")
plot((1:100),hdbcv$MEAN.SQU, type="l", col = "blue", main="LOOCV MSE")
kbest=hdbcv$best.parameters$k 

knnreg = kknn(resale_price~floor_area_sqm + max_floor_lvl + Dist_CBD + Remaining_lease 
              + Dist_nearest_jc,train,test,k=kbest,kernel = "rectangular")
predlm = predict(knnreg, newdata = test)
summary(predlm)
knnmse=mean((test$resale_price-knnreg$fitted.values)^2)  #KNN MSE = 2639.093 vs  #MSE=5431.123  

predict(knnreg, newdata = data.frame(
  floor_area_sqm = c(100,90,80), max_floor_lvl = c(17,22,10), 
  Dist_CBD= c(7,3,12), Remaining_lease = c(90,72,80), 
  Dist_nearest_jc= c(5,8,2)))


#############################################################################
## Regression tree using only resale_price in the HDB. #Lab 10
#############################################################################
library(tree)
library(rpart)
library(ROCR)

temp = tree(resale_price~floor_area_sqm,data=train,mindev=0.0001)

#tree1 showing with  resale_price~floor_area_sqm
cv.hdb = cv.tree(temp,, prune.tree) #10-fold cross-validation
bestcp = cv.hdb$size[max(which(cv.hdb$dev == min(cv.hdb$dev)))]
bestcp #8 is the optimal cross-validation choice
hdb.tree=prune.tree(temp,best=4)
length(unique(hdb.tree$where))#4

#Let's plot the tree and the fitted values side-by-side
#par(mfrow=c(1,2)) #c(nr,nc) here means we want an nr-by-nc array of graphs
plot(hdb.tree,type="uniform")
text(hdb.tree,col="blue",label=c("yval"),cex=.8)
#plot data with the fitted step function
hdb.fit = predict(hdb.tree,newdata=test) 
plot(floor_area_sqm,resale_price,cex=.5,pch=16) #plot data
oo=order(floor_area_sqm) #note we need to keep track of observation order for nonlinear plot
lines(floor_area_sqm[oo],hdb.fit[oo],col='red',lwd=3) #step function fit
#tbh this step function is not v useful here. only can show 2 dimensions.

#tree 2 with resale_price~floor_area_sqm + max_floor_lvl + Dist_CBD + Remaining_lease + Dist_nearest_jc . 12 trees
temp2 = tree(resale_price~floor_area_sqm + max_floor_lvl + Dist_CBD + 
               Remaining_lease + Dist_nearest_jc ,data=train,mindev=0.0001)

cv.hdb = cv.tree(temp2,, prune.tree) #10-fold cross-validation
bestcp = cv.hdb$size[max(which(cv.hdb$dev == min(cv.hdb$dev)))]
bestcp #12 is the optimal cross-validation choice
hdb.tree=prune.tree(temp2,best=12)
length(unique(hdb.tree$where))#12

#plot decision tree for regression
#dev.new()
#par(mfrow=c(1,1))
plot(hdb.tree,type="uniform")
text(hdb.tree,col="blue",label=c("yval"),cex=.8)

#mean((test$resale_price-predlm)^2)
treereg = predict(hdb.tree, newdata = test)
summary(treereg)
mean((test$resale_price-treereg)^2)  #7070.801

#Now, I will predict some prices.
predict(hdb.tree, newdata = data.frame(floor_area_sqm = c(100,90,80), max_floor_lvl = c(17,22,10), Dist_CBD= c(7,3,12), Remaining_lease = c(90,72,80), Dist_nearest_jc= c(5,8,2)), type = "vector")
#1        2        3 
#677.3180 498.9877 324.4909 



#############################################################################
## Decision Tree
#############################################################################
#Decision Tree if house high will be predicted correctly
price_high_test = ifelse(test$resale_price > quantile(test$resale_price, 0.50), 1, 0)
treeGini= rpart(price_high~ floor_area_sqm + max_floor_lvl + Dist_CBD + Remaining_lease + Dist_nearest_jc,
                data=train, method = "class", minsplit = 10, cp = .0001, maxdepth = 30)
bestcp=treeGini$cptable[which.min(treeGini$cptable[,"xerror"]),"CP"]
bestGini = prune(treeGini,cp=bestcp) #prune tree with bestcp
treepred = predict(bestGini,newdata = test) #predict the test observations
treepred2 = predict(bestGini,test, type='class') #for confusion matrix
length(treepred2) #1500
confusion_matrix = table(treepred2, price_high_test) 
confusion_matrix 
sum(diag(confusion_matrix)) / sum(confusion_matrix) #0.878

#plot decision tree for binomial
#par(mfrow=c(1,1))
text(hdb.tree,col="blue",label=c("yval"),cex=.8)
library(rpart.plot)
rpart.plot(bestGini, shadow.col = "gray")

#ROC curve for Big Tree Method
pred = prediction(treepred[,2], price_high_test)
perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc_perf = performance(pred, measure = "auc") # Calculate AUC
plot(perf, col = "steelblue", lwd = 2, main="ROC for Tree") # Plot ROC curve
abline(0, 1, lwd = 1, lty = 2) # Add dashed diagonal line
text(0.4, 0.8, paste("AUC =", round(auc_perf@y.values[[1]], 2))) 
#AUC = 0.92 vs 0.94 for logit method


