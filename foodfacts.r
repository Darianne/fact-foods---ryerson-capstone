#CORRELATION NUTRITION FACTS

#1 Libraries
library(leaps);
library(caret);
library(corrplot)
library(nnet)



#2 PREDICT NUTRITION GRADE

#N1 Import data and fix levels of food categories, display food grades that is the output of classif/prediction rediction

#import
predgrade <- read.csv('C:/Roxana/ryerson/finalproject/factfoods/FoodFactsNutrition1.csv')

#fix levels of food categories (Fish Meat Eggs appears twice)
summary(predgrade$pnns_groups_1)
lev<-levels(predgrade$pnns_groups_1)
lev
# [1] "Beverages"               "Cereals and potatoes"    "Composite foods"         "Fat and sauces"          "Fish meat eggs"         
# [6] "Fish Meat Eggs"          "Fruits and vegetables"   "Milk and dairy products" "Salty snacks"            "Sugary snacks"          
# [11] "unknown" 
lev[5]<-lev[6]
lev
levels(predgrade$pnns_groups_1)<-lev  
levels(predgrade$pnns_groups_1)
# [1] "Beverages"               "Cereals and potatoes"    "Composite foods"         "Fat and sauces"          "Fish Meat Eggs"         
# [6] "Fruits and vegetables"   "Milk and dairy products" "Salty snacks"            "Sugary snacks"           "unknown"                
# > 

#display food grades that we learn to predict
levels(predgrade$nutrition_grade_fr) #[1] "a" "b" "c" "d" "e"

#N2 deal with missing values in numerical data (replace fat with saturated fat where available; carbohydrates with sugar + fiber)
predgrade$additives_n[is.na(predgrade$additives_n)]<-ave( predgrade[!is.na(predgrade$additives_n),"additives_n"])[1]
predgrade$energy_100g[is.na(predgrade$energy_100g)]<-ave( predgrade[!is.na(predgrade$energy_100g),"energy_100g"])[1]
predgrade$saturated_fat_100g[is.na(predgrade$saturated_fat_100g)]<-ave( predgrade[!is.na(predgrade$saturated_fat_100g),"saturated_fat_100g"])[1]
predgrade$fat_100g[is.na(predgrade$fat_100g)]<- predgrade$saturated_fat_100g[is.na(predgrade$fat_100g)]
predgrade$salt_100g[is.na(predgrade$salt_100g)]<-ave( predgrade[!is.na(predgrade$salt_100g),"salt_100g"])[1]
predgrade$sugars_100g[is.na(predgrade$sugars_100g)]<-ave( predgrade[!is.na(predgrade$sugars_100g),"sugars_100g"])[1]
predgrade$proteins_100g[is.na(predgrade$proteins_100g)]<-ave( predgrade[!is.na(predgrade$proteins_100g),"proteins_100g"])[1]
predgrade$fiber_100g[is.na(predgrade$fiber_100g)]<-0
predgrade$carbohydrates_100g[is.na(predgrade$carbohydrates_100g)]<-predgrade$sugars_100g[is.na(predgrade$carbohydrates_100g)]+predgrade$fiber_100g[is.na(predgrade$carbohydrates_100g)]
predgrade$sodium_100g[is.na(predgrade$sodium_100g)]<-ave( predgrade[!is.na(predgrade$sodium_100g),"sodium_100g"])[1]


#N3 deal with food groups in categorical data pnns_groups_1
# give value based on nutrition grade numeric to use pnns_groups_1 in multinom and correlation needs numeric food grade
predgrade$nutrition_grade_fr<-as.numeric(predgrade$nutrition_grade_fr)
predgrade$pnns_groups_1<-as.character(predgrade$pnns_groups_1)

predgrade$pnns_groups_1[predgrade$pnns_groups_1=='unknown']<-3 #most common in unknown
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


 
 #N4 write my data
 write.csv(predgrade,"C:/Roxana/ryerson/finalproject/factfoods/FoodFactsCorrCleanReplaced.csv")
 


#N5 Model
# formula (includes pnns_froup_1 -not continuous ???)
formula <- as.formula(nutrition_grade_fr ~ saturated_fat_100g + sugars_100g + 
                        fat_100g + additives_n + salt_100g + fiber_100g + 
                        pnns_groups_1 + carbohydrates_100g + energy_100g + sodium_100g + proteins_100g ) 
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


#N6 Split in test and training, Fit model on training data
rn_train <- sample(nrow(predgrade), floor(nrow(predgrade)*0.7))
train <- predgrade[rn_train,] 
test <- predgrade[-rn_train,]
fit <- multinom(formula,data=train)


#N7 Predict nutrition grade on test and check performance
test$scores <- predict(fit, type="class", newdata=test)

c <- confusionMatrix(test$scores, test$nutrition_grade_fr) 
c
#Confusion Matrix and Statistics

# Reference
# Prediction    a    b    c    d    e
# a 1519  278   34    8    7
# b  263  902  331   15    4
# c   33  290 1474  506  164
# d    1    3  370 1822  343
# e    6   68   59  238 1181
# 
# Overall Statistics
# 
# Accuracy : 0.6954          
# 95% CI : (0.6863, 0.7045)
# No Information Rate : 0.261           
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.6153          
# Mcnemar's Test P-Value : < 2.2e-16       
# 
# Statistics by Class:
# 
# Class: a Class: b Class: c Class: d Class: e
# Sensitivity            0.8337  0.58533   0.6499   0.7037   0.6951
# Specificity            0.9596  0.92683   0.8702   0.9022   0.9549
# Pos Pred Value         0.8229  0.59538   0.5975   0.7176   0.7610
# Neg Pred Value         0.9625  0.92396   0.8935   0.8961   0.9381
# Prevalence             0.1837  0.15536   0.2287   0.2610   0.1713
# Detection Rate         0.1531  0.09094   0.1486   0.1837   0.1191
# Detection Prevalence   0.1861  0.15274   0.2487   0.2560   0.1565
# Balanced Accuracy      0.8967  0.75608   0.7601   0.8030   0.8250

#N9 Where are the most errors. What is the accuracy if we allow confusion between neighboring classes  

a<-c(1519,  278,   34,   8,    7)
b<-c(263,  902,  331,   15 ,   4)
c<-c(33,   290,  1474,  506,   164)
d<-c(1,    3,    370,   1822,  343)
e <-c(6,   68,   59,    238,   1181)
conf <- matrix(c(a,b,c,d,e),nrow=5,byrow = T)

e1<-conf[1,2]+ conf[2,1]+ conf[2,3]+ conf[3,2]+ conf[3,4]+ conf[4,3]+ conf[5,4]+conf[4,5]#2619 1 class difference error
e2<-conf[1,3]+conf[3,1]+conf[2,4]+conf[4,2]+conf[3,5]+conf[5,3]                          #308  2 class difference error
e3<-conf[1,4]+conf[4,1]+conf[2,5]+conf[5,2]                                              #81   3 class difference error
e4<-conf[1,5]+conf[5,1]                                                                  #13   4 class difference error

alltrue<-conf[1,1]+ conf[2,2]+ conf[3,3]+ conf[4,4]+ conf[5,5] #6898
all<-alltrue+e1+e2+e3+e4 #9919
accura<-alltrue/all #0.69543307

alltrue1 <- alltrue+e1 #9517
accura1<-alltrue1/all #0.9594717



#PREDICT FOOD CATEGORY
#C1 import
predcat <- read.csv('C:/Roxana/ryerson/finalproject/factfoods/FoodFactsNutrition2.csv')

#C2 -fix levels in food categories - fish meat eggs appears twice
summary(predcat$pnns_groups_1)
lev<-levels(predcat$pnns_groups_1)
lev
# [1] "Beverages"               "Cereals and potatoes"    "Composite foods"         "Fat and sauces"          "Fish meat eggs"         
# [6] "Fish Meat Eggs"          "Fruits and vegetables"   "Milk and dairy products" "Salty snacks"            "Sugary snacks"
lev[5]<-lev[6]
lev
levels(predcat$pnns_groups_1)<-lev  
# "Beverages"               "Cereals and potatoes"    "Composite foods"         "Fat and sauces"          "Fish Meat Eggs"         
# [6] "Fruits and vegetables"   "Milk and dairy products" "Salty snacks"            "Sugary snacks"          
# > 

# C3 deal with missing values in numerical data (replace fat with saturated fat where available; carbohydrates with sugar + fiber)
predcat$additives_n[is.na(predcat$additives_n)]<-ave( predcat[!is.na(predcat$additives_n),"additives_n"])[1]
predcat$energy_100g[is.na(predcat$energy_100g)]<-ave( predcat[!is.na(predcat$energy_100g),"energy_100g"])[1]
predcat$saturated_fat_100g[is.na(predcat$saturated_fat_100g)]<-ave( predcat[!is.na(predcat$saturated_fat_100g),"saturated_fat_100g"])[1]
predcat$fat_100g[is.na(predcat$fat_100g)]<- predcat$saturated_fat_100g[is.na(predcat$fat_100g)]
predcat$salt_100g[is.na(predcat$salt_100g)]<-ave( predcat[!is.na(predcat$salt_100g),"salt_100g"])[1]
predcat$sugars_100g[is.na(predcat$sugars_100g)]<-ave( predcat[!is.na(predcat$sugars_100g),"sugars_100g"])[1]
predcat$proteins_100g[is.na(predcat$proteins_100g)]<-ave( predcat[!is.na(predcat$proteins_100g),"proteins_100g"])[1]
predcat$fiber_100g[is.na(predcat$fiber_100g)]<-0
predcat$carbohydrates_100g[is.na(predcat$carbohydrates_100g)]<-predcat$sugars_100g[is.na(predcat$carbohydrates_100g)]+predcat$fiber_100g[is.na(predcat$carbohydrates_100g)]
predcat$sodium_100g[is.na(predcat$sodium_100g)]<-ave( predcat[!is.na(predcat$sodium_100g),"sodium_100g"])[1]




#C4 - missing nutrition grades - replace with most common grade in that food category;assign numeric 
cerealspot <-predcat$nutrition_grade_fr[predcat$pnns_groups_1=='Cereals and potatoes']
summary(cerealspot) #a
#       a    b    c    d    e 
# 1261 1897  517  695  410   48 
predcat$nutrition_grade_fr[predcat$pnns_groups_1=='Cereals and potatoes' & predcat$nutrition_grade_fr==""]<-'a'


composites <-predcat$nutrition_grade_fr[predcat$pnns_groups_1=='Composite foods'] # not clear lookat subcategories pnns_group2 
summary(composites) #b ?
#       a   b   c   d   e 
# 388 732 997 842 415  27 
onedish<-predcat$nutrition_grade_fr[predcat$pnns_groups_1=='Composite foods'& predcat$pnns_groups_2=='One-dish meals']
summary(onedish)
#     a   b   c   d   e 
# 828 689 891 652 159  10 #b
predcat$nutrition_grade_fr[predcat$pnns_groups_1=='Composite foods' & predcat$pnns_groups_2=='One-dish meals'& predcat$nutrition_grade_fr==""]<-'b'

pizza<-predcat$nutrition_grade_fr[predcat$pnns_groups_1=='Composite foods'& predcat$pnns_groups_2=='Pizza pies and quiche']
summary(pizza)
#     a  b  c  d  e 
# 52  5 38 66 91  2 # d
predcat$nutrition_grade_fr[predcat$pnns_groups_1=='Composite foods' & predcat$pnns_groups_2=='Pizza pies and quiche'& predcat$nutrition_grade_fr==""]<-'d'

sandwich<-predcat$nutrition_grade_fr[predcat$pnns_groups_1=='Composite foods'& predcat$pnns_groups_2=='Sandwich']
summary(sandwich) #d
predcat$nutrition_grade_fr[predcat$pnns_groups_1=='Composite foods' & predcat$pnns_groups_2=='Sandwich'& predcat$nutrition_grade_fr==""]<-'d'


fatsauce <-predcat$nutrition_grade_fr[predcat$pnns_groups_1=='Fat and sauces']
summary(fatsauce) #d
predcat$nutrition_grade_fr[predcat$pnns_groups_1=='Fat and sauces'& predcat$nutrition_grade_fr==""]<-'d'

meat <-predcat$nutrition_grade_fr[predcat$pnns_groups_1=='Fish Meat Eggs'] #no missing food grades
summary(meat) 
#     a    b    c    d    e 
# 0  477  366 1844  769 1118 


milk <-predcat$nutrition_grade_fr[predcat$pnns_groups_1=='Milk and dairy products']  #no missing food grades
summary(milk) 
#     a    b    c    d    e 
# 0  380  872 1513 2390  119 

fruitveg <-predcat$nutrition_grade_fr[predcat$pnns_groups_1=='Fruits and vegetables']
summary(fruitveg) # a
predcat$nutrition_grade_fr[predcat$pnns_groups_1=='Fruits and vegetables' &  predcat$nutrition_grade_fr==""]<-'a'

salty <-predcat$nutrition_grade_fr[predcat$pnns_groups_1=='Salty snacks']
summary(salty) # d
predcat$nutrition_grade_fr[predcat$pnns_groups_1=='Salty snacks'& predcat$nutrition_grade_fr==""]<-'d'

sweet <-predcat$nutrition_grade_fr[predcat$pnns_groups_1=='Sugary snacks']
summary(sweet) # e
predcat$nutrition_grade_fr[predcat$pnns_groups_1=='Sugary snacks'& predcat$nutrition_grade_fr==""]<-'e'

#fix levels for food grades- remove blank
leveng<-levels(predcat$nutrition_grade_fr)
leveng #[1] ""  "a" "b" "c" "d" "e"
leveng[1]<-leveng[2]
leveng
levels(predcat$nutrition_grade_fr)<-leveng  
levels(predcat$nutrition_grade_fr) #[1] "a" "b" "c" "d" "e"

#assign numeric nutrition grade
predcat$nutrition_grade_fr<-as.numeric(predcat$nutrition_grade_fr) #1:5


#C5 MODEL formula includes food grades (not continous)???
formula <- as.formula(pnns_groups_1 ~ saturated_fat_100g + sugars_100g + 
                        fat_100g + additives_n + salt_100g + fiber_100g + 
                        + carbohydrates_100g + energy_100g + sodium_100g + proteins_100g + nutrition_grade_fr)


fit <- multinom(formula,data=predcat)

summary(fit)
# Call:
#   multinom(formula = formula, data = predcat)
# 
# Coefficients:
#   (Intercept) saturated_fat_100g sugars_100g     fat_100g  additives_n salt_100g   fiber_100g carbohydrates_100g
# Cereals and potatoes    -1.84108471        0.007010142  0.01080211 -0.105046581 -0.086336070  2.387777 -0.009996866       -0.002008672
# Composite foods          0.06082599        0.014816473 -0.16734006  0.047416342  0.142854451  2.293630 -0.030541927       -0.012625859
# Fat and sauces          -1.06512898        0.001662961  0.03465108  0.101793424 -0.042243626  2.594053 -0.131120124       -0.044711406
# Fish Meat Eggs          -1.41829565       -0.148466027  0.02550992 -0.052471285  0.024873653  2.096419 -0.243419406       -0.124995222
# Fruits and vegetables    2.33431709        0.014452409  0.09131576  0.002578445 -0.695849493  2.730728  0.037673784       -0.082939013
# Milk and dairy products -0.33924377        0.141604591  0.09642254  0.014821802  0.073607642  1.364561 -0.575838477       -0.079959812
# Salty snacks            -6.70534499       -0.085682635 -0.10198230 -0.121200065 -0.004350842  2.285748 -0.068259853       -0.054251271
# Sugary snacks           -2.69642613        0.005722725  0.02195855 -0.050968853  0.107689560  1.044241 -0.056977615       -0.003889809
# energy_100g sodium_100g proteins_100g nutrition_grade_frb nutrition_grade_frc nutrition_grade_frd
# Cereals and potatoes    0.005249946   0.6043614    0.28438636           -3.230264           -5.085714           -6.662875
# Composite foods         0.001732555   0.7834468    0.24007529           -1.347140           -2.646049           -5.587491
# Fat and sauces          0.002452200   0.9479491   -0.09556861           -1.523715           -1.421648           -3.855527
# Fish Meat Eggs          0.005035686   1.1652009    0.35557061           -2.410947           -1.547814           -5.217516
# Fruits and vegetables   0.003986761   1.0838166   -0.04884607           -4.102387           -6.274277          -10.386999
# Milk and dairy products 0.001447884  -0.8168222    0.31675112           -1.249132           -1.107744           -2.555051
# Salty snacks            0.009360174   0.8457631    0.15128379           -1.067249           -1.671867           -2.710320
# Sugary snacks           0.004314655   0.1920396    0.17500051           -1.183043           -1.907223           -2.737070
# nutrition_grade_fre
# Cereals and potatoes              -9.573965
# Composite foods                   -8.169503
# Fat and sauces                    -5.233682
# Fish Meat Eggs                    -4.570391
# Fruits and vegetables             -9.148458
# Milk and dairy products           -5.702670
# Salty snacks                      -4.152075
# Sugary snacks                     -1.616983
# 
# Std. Errors:
#   (Intercept) saturated_fat_100g sugars_100g   fat_100g additives_n  salt_100g fiber_100g carbohydrates_100g
# Cereals and potatoes     0.06981844         0.01732145 0.003511471 0.01111545  0.01611522 0.02114609 0.01262477        0.003367733
# Composite foods          0.05601949         0.01709932 0.007382095 0.01215236  0.01308096 0.02345428 0.01415761        0.003783579
# Fat and sauces           0.05131117         0.01536989 0.004658979 0.01112434  0.01713338 0.01890290 0.01839667        0.004478351
# Fish Meat Eggs           0.06742661         0.01717513 0.006135140 0.01065102  0.01447808 0.02066629 0.02140092        0.004769628
# Fruits and vegetables    0.05516610         0.01705952 0.004784046 0.01122118  0.02593413 0.01822310 0.01212114        0.004463494
# Milk and dairy products  0.05360647         0.01517904 0.004484487 0.01013949  0.01238061 0.03525324 0.02718437        0.004230867
# Salty snacks             0.02277361         0.01689282 0.006163310 0.01159197  0.01874127 0.02744885 0.01601407        0.004043958
# Sugary snacks            0.06500154         0.01572268 0.003060892 0.01035152  0.01283347 0.04130301 0.01228301        0.002932039
# energy_100g sodium_100g proteins_100g nutrition_grade_frb nutrition_grade_frc nutrition_grade_frd
# Cereals and potatoes    0.0002047355 0.008321347    0.01510741          0.06589607          0.06399528          0.06711231
# Composite foods         0.0002364303 0.009234041    0.01498026          0.05131118          0.05510469          0.06172502
# Fat and sauces          0.0002248225 0.007443447    0.01694240          0.05382694          0.04507895          0.05412589
# Fish Meat Eggs          0.0002003739 0.008141794    0.01471532          0.05910860          0.03954006          0.04982530
# Fruits and vegetables   0.0002104668 0.007175899    0.01717263          0.06112436          0.07315075          0.01629587
# Milk and dairy products 0.0001746661 0.013878556    0.01455797          0.05207780          0.03835990          0.04060280
# Salty snacks            0.0002449881 0.010804810    0.01592711          0.04795907          0.06125848          0.04961700
# Sugary snacks           0.0001822137 0.016262702    0.01548947          0.06186880          0.04687144          0.04445931
# nutrition_grade_fre
# Cereals and potatoes             0.02014016
# Composite foods                  0.01914044
# Fat and sauces                   0.03502359
# Fish Meat Eggs                   0.05236969
# Fruits and vegetables            0.03014348
# Milk and dairy products          0.05579634
# Salty snacks                     0.04521989
# Sugary snacks                    0.05110232
# 
# Residual Deviance: 60903.2 
# AIC: 61143.2 



rn_train <- sample(nrow(predcat), floor(nrow(predcat)*0.7))
train <- predcat[rn_train,] 
test <- predcat[-rn_train,]


#fit model on training data
fit <- multinom(formula,data=train)
#predict on test andcheck performance
test$scores <- predict(fit, type="class", newdata=test)

c <- confusionMatrix(test$scores, test$pnns_groups_1) 
c

# Confusion Matrix and Statistics
# Confusion Matrix and Statistics
# 
# Reference
# Prediction                Beverages Cereals and potatoes Composite foods Fat and sauces Fish Meat Eggs Fruits and vegetables
# Beverages                     702                    2               4             12              0                    15
# Cereals and potatoes            2                  842              36              1              2                    36
# Composite foods                16                  107             673             39             63                    90
# Fat and sauces                  5                    4              22            479              8                    26
# Fish Meat Eggs                  9                   39              44             10            960                     5
# Fruits and vegetables          47                  287             103             28             55                  1018
# Milk and dairy products        74                    7             151             88            117                     3
# Salty snacks                    2                   39               9              6              0                     1
# Sugary snacks                  67                   96               1              5            141                    21
# Reference
# Prediction                Milk and dairy products Salty snacks Sugary snacks
# Beverages                                    76            0            92
# Cereals and potatoes                          2           27            41
# Composite foods                              12           11            44
# Fat and sauces                                5            5             2
# Fish Meat Eggs                              169           29            13
# Fruits and vegetables                        84            4             7
# Milk and dairy products                    1236           90            31
# Salty snacks                                  3          324            21
# Sugary snacks                                25           28          2050
# 
# Overall Statistics
# 
# Accuracy : 0.7497          
# 95% CI : (0.7415, 0.7577)
# No Information Rate : 0.2082          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.7116          
# Mcnemar's Test P-Value : < 2.2e-16       
# 
# Statistics by Class:
# 
# Class: Beverages Class: Cereals and potatoes Class: Composite foods Class: Fat and sauces Class: Fish Meat Eggs
# Sensitivity                   0.75974                      0.5917                0.64525               0.71707               0.71322
# Specificity                   0.98015                      0.9847                0.96183               0.99258               0.96723
# Pos Pred Value                0.77741                      0.8514                0.63791               0.86151               0.75117
# Neg Pred Value                0.97812                      0.9423                0.96298               0.98199               0.96050
# Prevalence                    0.08362                      0.1288                0.09439               0.06045               0.12181
# Detection Rate                0.06353                      0.0762                0.06090               0.04335               0.08688
# Detection Prevalence          0.08172                      0.0895                0.09548               0.05032               0.11566
# Balanced Accuracy             0.86995                      0.7882                0.80354               0.85482               0.84023
# Class: Fruits and vegetables Class: Milk and dairy products Class: Salty snacks Class: Sugary snacks
# Sensitivity                               0.83786                         0.7667             0.62548               0.8909
# Specificity                               0.93747                         0.9406             0.99231               0.9561
# Pos Pred Value                            0.62339                         0.6878             0.80000               0.8422
# Neg Pred Value                            0.97908                         0.9594             0.98178               0.9709
# Prevalence                                0.10995                         0.1459             0.04688               0.2082
# Detection Rate                            0.09213                         0.1119             0.02932               0.1855
# Detection Prevalence                      0.14778                         0.1626             0.03665               0.2203
# Balanced Accuracy                         0.88766                         0.8537             0.80890               0.9235
# 
# 



#VISUALS

#COUNTRIES AND FOOD CATEGORIES
cntrycat<-read.csv('C:/Roxana/ryerson/finalproject/factfoods/FoodFactsCountriesCategoriesExpanded.csv')

levels(cntrycat$pnns_groups_1)
# [1] "Alcoholic beverages"     "Beverages"               "Cereals and potatoes"   
# [4] "Composite foods"         "Fat and sauces"          "Fish Meat Eggs"         
# [7] "Fruits and vegetables"   "Milk and dairy products" "Salty snacks"           
# [10] "Sugary snacks"          
# > 


par(xpd = TRUE)
par(mar = c(5, 0, 5, 0))
par(las=1)

#colors=c(" red", "azure","Yellow","deeppink","coral","brown","green","white", "lightgoldenrod2","orange")
colors=c(" yellow", "azure","orange","green","coral","brown","deeppink","red", "white","lightgoldenrod2")

#32000 out of 43.000 from France alone
t<-table(cntrycat$pnns_groups_1,cntrycat$countriesgroups)#rows is foodgrps, columns cntries
barplot(prop.table(t,1),ylab="food groups", xlab="countries", beside=TRUE,
        col=colors, cex.names = 0.9, 
        legend=unique(cntrycat$pnns_groups_1), args.legend = list(y="top", x="left",fill=colors,cex=0.7,bty="n") 
        )

#NUTRITION ELEMENTS
# - correlation of nutrition elements
mycor1=cor(predgrade[,c("nutrition_grade_fr","pnns_groups_1","additives_n","energy_100g","fat_100g","saturated_fat_100g","sugars_100g","carbohydrates_100g","fiber_100g","proteins_100g","salt_100g","sodium_100g")],use="complete")
corrplot(mycor1,method="ellipse",tl.cex = .55)

#factor again
#predgrade$nutrition_grade_fr<-factor(predgrade$nutrition_grade_fr)

