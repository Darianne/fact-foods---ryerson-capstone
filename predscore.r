#CORRELATION NUTRITION FACTS
#1.import data

relscore <- read.csv('C:/Roxana/ryerson/finalproject/factfoods/FoodFactsCorrClean.csv')


#2. handle missing values
relscore$additives_n[is.na(relscore$additives_n)]<-ave( relscore[!is.na(relscore$additives_n),"additives_n"])[1]
relscore$energy_100g[is.na(relscore$energy_100g)]<-ave( relscore[!is.na(relscore$energy_100g),"energy_100g"])[1]
relscore$fat_100g[is.na(relscore$fat_100g)]<-ave( relscore[!is.na(relscore$fat_100g),"fat_100g"])[1]
relscore$salt_100g[is.na(relscore$salt_100g)]<-ave( relscore[!is.na(relscore$salt_100g),"salt_100g"])[1]
relscore$sugars_100g[is.na(relscore$sugars_100g)]<-ave( relscore[!is.na(relscore$sugars_100g),"sugars_100g"])[1]
relscore$proteins_100g[is.na(relscore$proteins_100g)]<-ave( relscore[!is.na(relscore$proteins_100g),"proteins_100g"])[1]
relscore$carbohydrates_100g[is.na(relscore$carbohydrates_100g)]<-ave( relscore[!is.na(relscore$carbohydrates_100g),"carbohydrates_100g"])[1]
relscore$fiber_100g[is.na(relscore$fiber_100g)]<-ave( relscore[!is.na(relscore$fiber_100g),"fiber_100g"])[1]
relscore$saturated_fat_100g[is.na(relscore$saturated_fat_100g)]<-ave( relscore[!is.na(relscore$saturated_fat_100g),"saturated_fat_100g"])[1]
relscore$sodium_100g[is.na(relscore$sodium_100g)]<-ave( relscore[!is.na(relscore$sodium_100g),"sodium_100g"])[1]

#3.write new file
write.csv(predgrade,"C:/Roxana/ryerson/finalproject/factfoods/FoodFactsCorrCleanReplaced.csv")




library(corrplot)
mycor=cor(relscore[,c("additives_n","energy_100g","fat_100g","saturated_fat_100g","sugars_100g","carbohydrates_100g","fiber_100g","proteins_100g","salt_100g","sodium_100g")],use="complete")
corrplot(mycor,method="ellipse",tl.cex = .65)

#PREDICT NUTRITION GRADE

library(leaps);
library(caret);

#install.packages("gplots")
#library(gplots)
#install.packages("ROCR")#Nope - only binary classification
#library(ROCR)
# install.packages("gbm")#multiple classification
# library(gbm)
# install.packages("glmnet")#multiple classification
# library(glmnet)

library(nnet)


predgrade <- read.csv('C:/Roxana/ryerson/finalproject/factfoods/FoodFactsPredNGrade4.csv')



#step wants numeric nutrition grade
levels(predgrade$nutrition_grade_fr)<-c(1:5)
predgrade$nutrition_grade_fr<-as.numeric(predgrade$nutrition_grade_fr)

#if binary make it binary - I want multiple....(a, bc, c, , e)
# levels(predgrade$nutrition_grade_fr)[match("a",levels(predgrade$nutrition_grade_fr))] <- "1" 


#deal with missing values
predgrade$additives_n[is.na(predgrade$additives_n)]<-ave( predgrade[!is.na(predgrade$additives_n),"additives_n"])[1]
predgrade$energy_100g[is.na(predgrade$energy_100g)]<-ave( predgrade[!is.na(predgrade$energy_100g),"energy_100g"])[1]
#predgrade$fat_100g[is.na(predgrade$fat_100g)]<-ave( predgrade[!is.na(predgrade$fat_100g),"fat_100g"])[1]
predgrade$fat_100g[is.na(predgrade$fat_100g)]<- predgrade$saturated_fat_100g[is.na(predgrade$fat_100g)]
predgrade$salt_100g[is.na(predgrade$salt_100g)]<-ave( predgrade[!is.na(predgrade$salt_100g),"salt_100g"])[1]
predgrade$sugars_100g[is.na(predgrade$sugars_100g)]<-ave( predgrade[!is.na(predgrade$sugars_100g),"sugars_100g"])[1]
predgrade$proteins_100g[is.na(predgrade$proteins_100g)]<-ave( predgrade[!is.na(predgrade$proteins_100g),"proteins_100g"])[1]
#predgrade$carbohydrates_100g[is.na(predgrade$carbohydrates_100g)]<-ave( predgrade[!is.na(predgrade$carbohydrates_100g),"carbohydrates_100g"])[1]
predgrade$fiber_100g[is.na(predgrade$fiber_100g)]<-0
predgrade$carbohydrates_100g[is.na(predgrade$carbohydrates_100g)]<-predgrade$sugars_100g[is.na(predgrade$carbohydrates_100g)]+predgrade$fiber_100g[is.na(predgrade$carbohydrates_100g)]
predgrade$saturated_fat_100g[is.na(predgrade$saturated_fat_100g)]<-ave( predgrade[!is.na(predgrade$saturated_fat_100g),"saturated_fat_100g"])[1]
predgrade$sodium_100g[is.na(predgrade$sodium_100g)]<-ave( predgrade[!is.na(predgrade$sodium_100g),"sodium_100g"])[1]



#deal with food groups pnns_groups_1

lev<-levels(predgrade$pnns_groups_1)
# ""                        "Beverages"               "cereals-and-potatoes"   
# [4] "Cereals and potatoes"    "Composite foods"         "Fat and sauces"         
# [7] "Fish Meat Eggs"          "fruits-and-vegetables"   "Fruits and vegetables"  
# [10] "Milk and dairy products" "salty-snacks"            "Salty snacks"           
# [13] "sugary-snacks"           "Sugary snacks"           "unknown"      

lev[1] <- lev[15]
lev[3]<-lev[4]
lev[8]<-lev[9]
lev[11]<-lev[12]
lev[13]<-lev[14]
levels(predgrade$pnns_groups_1)<-lev

predgrade$pnns_groups_1<-as.character(predgrade$pnns_groups_1)
predgrade$pnns_groups_1[predgrade$pnns_groups_1=='unknown']<-median(predgrade$nutrition_grade_fr[predgrade$pnns_groups_1=='unknown'])
predgrade$pnns_groups_1[predgrade$pnns_groups_1=='Cereals and potatoes']<-median(predgrade$nutrition_grade_fr[predgrade$pnns_groups_1=='Cereals and potatoes'])
predgrade$pnns_groups_1[predgrade$pnns_groups_1=='Composite foods']<-median(predgrade$nutrition_grade_fr[predgrade$pnns_groups_1=='Composite foods'])
predgrade$pnns_groups_1[predgrade$pnns_groups_1=='Salty snacks']<-median(predgrade$nutrition_grade_fr[predgrade$pnns_groups_1=='Salty snacks'])
predgrade$pnns_groups_1[predgrade$pnns_groups_1=='Fruits and vegetables']<-median(predgrade$nutrition_grade_fr[predgrade$pnns_groups_1=='Fruits and vegetables'])
predgrade$pnns_groups_1[predgrade$pnns_groups_1=='Sugary snacks']<-median(predgrade$nutrition_grade_fr[predgrade$pnns_groups_1=='Sugary snacks'])
predgrade$pnns_groups_1[predgrade$pnns_groups_1=='Beverages']<-median(predgrade$nutrition_grade_fr[predgrade$pnns_groups_1=='Beverages'])
predgrade$pnns_groups_1[predgrade$pnns_groups_1=='Fat and sauces']<-median(predgrade$nutrition_grade_fr[predgrade$pnns_groups_1=='Fat and sauces'])
predgrade$pnns_groups_1[predgrade$pnns_groups_1=='Fish Meat Eggs']<-median(predgrade$nutrition_grade_fr[predgrade$pnns_groups_1=='Fish Meat Eggs'])
predgrade$pnns_groups_1[predgrade$pnns_groups_1=='Milk and dairy products']<-median(predgrade$nutrition_grade_fr[predgrade$pnns_groups_1=='Milk and dairy products'])
predgrade$pnns_groups_1<-as.numeric(predgrade$pnns_groups_1)


#1 more ore
mycor1=cor(predgrade[,c("pnns_groups_1","additives_n","energy_100g","fat_100g","saturated_fat_100g","sugars_100g","carbohydrates_100g","fiber_100g","proteins_100g","salt_100g","sodium_100g")],use="complete")
corrplot(mycor1,method="ellipse",tl.cex = .60)





#forward/backward selection
nullModel=lm(nutrition_grade_fr~1, data=predgrade) 
nullModel



fullModel=lm(nutrition_grade_fr~., data=predgrade)
fullModel

step(nullModel, scope=list(lower=nullModel, upper=fullModel), direction="forward") 
step(fullModel, scope=list(lower=nullModel, upper=fullModel), direction="backward")  

#forward - backward gave the same formula
# Start:  AIC=19512.5
# nutrition_grade_fr ~ 1
# 
# Df Sum of Sq   RSS     AIC
# + pnns_groups_1       1   21994.8 36232  4769.0
# + saturated_fat_100g  1   16790.6 41436  8940.6
# + energy_100g         1   16755.5 41471  8966.9
# + fat_100g            1   15254.7 42972 10071.9
# + sugars_100g         1   11297.6 46929 12809.9
# + additives_n         1    2697.1 55529 18040.3
# + carbohydrates_100g  1    2255.8 55971 18286.4
# + fiber_100g          1    1404.1 56822 18755.8
# + salt_100g           1     926.3 57300 19016.0
# + sodium_100g         1     926.2 57300 19016.1
# + proteins_100g       1     220.2 58006 19396.7
# <none>                            58226 19512.5
# 
# Step:  AIC=4769.03
# nutrition_grade_fr ~ pnns_groups_1
# 
# Df Sum of Sq   RSS     AIC
# + saturated_fat_100g  1    7387.2 28844 -2316.2
# + energy_100g         1    7291.1 28941 -2212.7
# + fat_100g            1    6115.6 30116  -975.2
# + sugars_100g         1    1840.8 34391  3150.3
# + proteins_100g       1    1118.2 35113  3796.6
# + salt_100g           1    1060.3 35171  3847.9
# + sodium_100g         1    1060.2 35172  3848.0
# + carbohydrates_100g  1     656.4 35575  4202.7
# + additives_n         1     631.6 35600  4224.5
# + fiber_100g          1     403.9 35828  4422.6
# <none>                            36232  4769.0
# 
# Step:  AIC=-2316.18
# nutrition_grade_fr ~ pnns_groups_1 + saturated_fat_100g
# 
# Df Sum of Sq   RSS     AIC
# + sugars_100g         1   1981.85 26863 -4526.7
# + energy_100g         1   1951.70 26893 -4491.8
# + additives_n         1    978.18 27866 -3386.5
# + salt_100g           1    977.80 27867 -3386.1
# + sodium_100g         1    977.73 27867 -3386.0
# + carbohydrates_100g  1    924.06 27920 -3326.2
# + fat_100g            1    549.02 28295 -2911.5
# + fiber_100g          1    521.13 28323 -2880.9
# + proteins_100g       1    285.82 28559 -2623.7
# <none>                            28844 -2316.2
# 
# Step:  AIC=-4526.67
# nutrition_grade_fr ~ pnns_groups_1 + saturated_fat_100g + sugars_100g
# 
# Df Sum of Sq   RSS     AIC
# + salt_100g           1   1387.59 25475 -6173.2
# + sodium_100g         1   1387.46 25475 -6173.0
# + fat_100g            1   1106.55 25756 -5832.2
# + energy_100g         1   1027.56 25835 -5737.0
# + additives_n         1    801.15 26062 -5465.8
# + fiber_100g          1    782.04 26081 -5443.0
# + proteins_100g       1    711.64 26151 -5359.2
# + carbohydrates_100g  1      9.99 26853 -4536.2
# <none>                            26863 -4526.7
# 
# Step:  AIC=-6173.16
# nutrition_grade_fr ~ pnns_groups_1 + saturated_fat_100g + sugars_100g + 
#   salt_100g
# 
# Df Sum of Sq   RSS     AIC
# + fat_100g            1   1043.21 24432 -7470.8
# + energy_100g         1    911.82 24563 -7304.1
# + fiber_100g          1    755.97 24719 -7107.5
# + additives_n         1    728.03 24747 -7072.4
# + proteins_100g       1    481.87 24993 -6764.7
# + sodium_100g         1     19.35 25456 -6194.8
# + carbohydrates_100g  1      8.53 25467 -6181.6
# <none>                            25475 -6173.2
# 
# Step:  AIC=-7470.77
# nutrition_grade_fr ~ pnns_groups_1 + saturated_fat_100g + sugars_100g + 
#   salt_100g + fat_100g
# 
# Df Sum of Sq   RSS     AIC
# + fiber_100g          1    964.88 23467 -8721.2
# + additives_n         1    781.83 23650 -8479.7
# + proteins_100g       1    432.40 23999 -8023.8
# + energy_100g         1     94.18 24338 -7588.8
# + sodium_100g         1     18.49 24413 -7492.3
# <none>                            24432 -7470.8
# + carbohydrates_100g  1      1.43 24430 -7470.6
# 
# Step:  AIC=-8721.19
# nutrition_grade_fr ~ pnns_groups_1 + saturated_fat_100g + sugars_100g + 
#   salt_100g + fat_100g + fiber_100g
# 
# Df Sum of Sq   RSS     AIC
# + additives_n         1    668.23 22799 -9617.1
# + proteins_100g       1    647.50 22819 -9588.9
# + energy_100g         1    435.76 23031 -9301.8
# + carbohydrates_100g  1    100.56 23366 -8852.7
# + sodium_100g         1     14.28 23453 -8738.1
# <none>                            23467 -8721.2
# 
# Step:  AIC=-9617.11
# nutrition_grade_fr ~ pnns_groups_1 + saturated_fat_100g + sugars_100g + 
#   salt_100g + fat_100g + fiber_100g + additives_n
# 
# Df Sum of Sq   RSS      AIC
# + proteins_100g       1    675.83 22123 -10550.4
# + energy_100g         1    358.84 22440 -10108.2
# + carbohydrates_100g  1     53.64 22745  -9688.3
# + sodium_100g         1     13.76 22785  -9633.9
# <none>                            22799  -9617.1
# 
# Step:  AIC=-10550.42
# nutrition_grade_fr ~ pnns_groups_1 + saturated_fat_100g + sugars_100g + 
#   salt_100g + fat_100g + fiber_100g + additives_n + proteins_100g
# 
# Df Sum of Sq   RSS    AIC
# + energy_100g         1   105.723 22017 -10697
# + carbohydrates_100g  1    36.265 22087 -10599
# + sodium_100g         1     9.009 22114 -10561
# <none>                            22123 -10550
# 
# Step:  AIC=-10697.31
# nutrition_grade_fr ~ pnns_groups_1 + saturated_fat_100g + sugars_100g + 
#   salt_100g + fat_100g + fiber_100g + additives_n + proteins_100g + 
#   energy_100g
# 
# Df Sum of Sq   RSS    AIC
# + carbohydrates_100g  1    118.55 21899 -10863
# + sodium_100g         1      9.11 22008 -10708
# <none>                            22017 -10697
# 
# Step:  AIC=-10863.13
# nutrition_grade_fr ~ pnns_groups_1 + saturated_fat_100g + sugars_100g + 
#   salt_100g + fat_100g + fiber_100g + additives_n + proteins_100g + 
#   energy_100g + carbohydrates_100g
# 
# Df Sum of Sq   RSS    AIC
# + sodium_100g  1    10.958 21888 -10877
# <none>                     21899 -10863
# 
# Step:  AIC=-10876.69
# nutrition_grade_fr ~ pnns_groups_1 + saturated_fat_100g + sugars_100g + 
#   salt_100g + fat_100g + fiber_100g + additives_n + proteins_100g + 
#   energy_100g + carbohydrates_100g + sodium_100g
# 
# 
# Call:
#   lm(formula = nutrition_grade_fr ~ pnns_groups_1 + saturated_fat_100g + 
#        sugars_100g + salt_100g + fat_100g + fiber_100g + additives_n + 
#        proteins_100g + energy_100g + carbohydrates_100g + sodium_100g, 
#      data = predgrade)
# 
# Coefficients:
#   (Intercept)       pnns_groups_1  saturated_fat_100g         sugars_100g           salt_100g  
# 1.066e+00           3.385e-01           3.549e-02           2.009e-02           1.980e+01  
# fat_100g          fiber_100g         additives_n       proteins_100g         energy_100g  
# -5.993e-03          -6.062e-02           6.027e-02           9.790e-03           6.711e-04  
# carbohydrates_100g         sodium_100g  
# -9.226e-03          -5.011e+01  
# 




#factor again
predgrade$nutrition_grade_fr<-factor(predgrade$nutrition_grade_fr)

#formula
formula <- as.formula(nutrition_grade_fr ~ saturated_fat_100g + sugars_100g + 
                          fat_100g + additives_n + salt_100g + fiber_100g + 
                        pnns_groups_1 + carbohydrates_100g + energy_100g + sodium_100g + proteins_100g ) 
formula


#model
fit <- multinom(formula,data=predgrade)

summary(fit)
# Call:
#   multinom(formula = formula, data = predgrade)
# 
# Coefficients:
#   (Intercept) saturated_fat_100g sugars_100g     fat_100g additives_n  salt_100g fiber_100g pnns_groups_1
# 2   -4.049251          0.7598804   0.1100343 -0.007790402 0.011628241   1.172082 -0.3073236      1.051533
# 3   -9.372652          1.2236006   0.2480236 -0.079121442 0.009954909 -12.819336 -0.3962371      2.048411
# 4  -13.018778          1.4422799   0.2820149 -0.112240177 0.050200843 -10.174805 -0.5997114      2.515421
# 5  -16.305978          1.5095860   0.3055380 -0.146648588 0.086767918  32.094079 -0.7330689      3.029272
# carbohydrates_100g  energy_100g sodium_100g proteins_100g
# 2         0.00484251 0.0009198843    3.293675    -0.1175413
# 3        -0.03069868 0.0036717036   44.054429    -0.2342494
# 4        -0.03936583 0.0050212423   37.941688    -0.2192608
# 5        -0.06086246 0.0059273025  -69.336984    -0.2393773
# 
# Std. Errors:
#   (Intercept) saturated_fat_100g sugars_100g   fat_100g additives_n   salt_100g fiber_100g pnns_groups_1
# 2  0.01010259        0.014380561 0.005051253 0.01152341  0.01176296 0.027603648 0.01357415    0.01431671
# 3  0.03526894        0.006178989 0.005520424 0.01243071  0.01277034 0.010649675 0.01514386    0.01751714
# 4  0.04297076        0.005647181 0.005656218 0.01312856  0.01363887 0.008620560 0.01703919    0.01960738
# 5  0.01439591        0.005836803 0.005780503 0.01435455  0.01474415 0.009039356 0.01844003    0.02182465
# carbohydrates_100g  energy_100g sodium_100g proteins_100g
# 2        0.004555455 0.0002732337 0.010867453   0.006858934
# 3        0.005214539 0.0003106096 0.004192827   0.007758058
# 4        0.005563105 0.0003303208 0.003394183   0.008122454
# 5        0.006203319 0.0003645099 0.003558799   0.008782379
# 
# Residual Deviance: 55535.69 
# AIC: 55631.69 
# > 


#check performance split in test and training
rn_train <- sample(nrow(predgrade), floor(nrow(predgrade)*0.7))
train <- predgrade[rn_train,] 
test <- predgrade[-rn_train,]


#fit model on training data
fit <- multinom(formula,data=train)
#predict on test andcheck performance
test$scores <- predict(fit, type="class", newdata=test)

c <- confusionMatrix(test$scores, test$nutrition_grade_fr) 
c
# Confusion Matrix and Statistics
# 
# Reference
# Prediction    1    2    3    4    5
# 1 1454  264   35    3    5
# 2  275  799  264   76   55
# 3   46  328 1312  447  147
# 4    0   11  318 1644  542
# 5    0    0   42  378  880
# 
# Overall Statistics
# 
# Accuracy : 0.663           
# 95% CI : (0.6432, 0.6626)
# No Information Rate : 0.2732          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.5611          
# Mcnemar's Test P-Value : < 2.2e-16       
# 
# Statistics by Class:
# 
# Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
# Sensitivity            0.8192  0.56990   0.6657   0.6452  0.54021
# Specificity            0.9593  0.91544   0.8684   0.8715  0.94543
# Pos Pred Value         0.8257  0.54391   0.5754   0.6537  0.67692
# Neg Pred Value         0.9576  0.92324   0.9065   0.8673  0.90667
# Prevalence             0.1903  0.15035   0.2114   0.2732  0.17469
# Detection Rate         0.1559  0.08568   0.1407   0.1763  0.09437
# Detection Prevalence   0.1888  0.15753   0.2445   0.2697  0.13941
# Balanced Accuracy      0.8892  0.74267   0.7670   0.7583  0.74282
# > 




#visualise - I could not

pred<-prediction(test$scores, test$nutrition_grade_fr) 
