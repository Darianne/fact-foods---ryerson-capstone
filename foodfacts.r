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
# formula 

formula2 <- as.formula(nutrition_grade_fr ~ saturated_fat_100g + sugars_100g + 
                        fat_100g + additives_n + salt_100g + fiber_100g + 
                         carbohydrates_100g + energy_100g + sodium_100g + proteins_100g ) 

fit <- multinom(formula2,data=predgrade)
# # weights:  60 (44 variable)
# initial  value 53212.845699 
# iter  10 value 45690.422268
# iter  20 value 42540.422029
# iter  30 value 40775.288111
# iter  40 value 40397.213073
# iter  50 value 33842.312739
# iter  60 value 33783.282601
# iter  70 value 33782.965612
# iter  80 value 33782.173445
# final  value 33782.082644 
# converged

summary(fit)
# Call:
#   multinom(formula = formula2, data = predgrade)
# 
# Coefficients:
#   (Intercept) saturated_fat_100g sugars_100g   fat_100g additives_n  salt_100g fiber_100g
# 2  -0.9852142          0.6715088  0.09977646 0.04154775   0.1316794 -10.978504 -0.3438830
# 3  -2.5523785          0.8467398  0.21538882 0.12375213   0.1397195  -8.033201 -0.3919027
# 4  -4.5198636          1.0164385  0.24816927 0.14455379   0.1816822  -5.067740 -0.5993707
# 5  -6.0480583          1.0855706  0.27682408 0.12408549   0.2293385  26.344654 -0.7104296
# carbohydrates_100g   energy_100g sodium_100g proteins_100g
# 2        0.008262050 -0.0003765918    31.26989   -0.06220956
# 3        0.013578369 -0.0010890207    27.15415   -0.09926112
# 4        0.021494716 -0.0007026227    19.69746   -0.07383597
# 5        0.009724884 -0.0001263193   -60.06261   -0.09899476
# 
# Std. Errors:
#   (Intercept) saturated_fat_100g sugars_100g   fat_100g additives_n   salt_100g fiber_100g
# 2  0.03780047         0.03022247 0.004468744 0.01003808  0.01074999 0.025475928 0.01215514
# 3  0.03044115         0.02961650 0.004173044 0.01056698  0.01126940 0.007919241 0.01236619
# 4  0.03078904         0.02968243 0.004189718 0.01095271  0.01175894 0.007886983 0.01405202
# 5  0.03261189         0.02975095 0.004279378 0.01163419  0.01241132 0.008108398 0.01551734
# carbohydrates_100g  energy_100g sodium_100g proteins_100g
# 2        0.003390561 0.0002061516 0.010027778   0.005479483
# 3        0.003919664 0.0002342946 0.003116407   0.005914637
# 4        0.004156952 0.0002460873 0.003105250   0.006130925
# 5        0.004598260 0.0002672330 0.003192219   0.006623878
# 
# Residual Deviance: 67564.17 
# AIC: 67652.17 


#N6 Split in test and training, Fit model on training data
rn_train <- sample(nrow(predgrade), floor(nrow(predgrade)*0.7))
train <- predgrade[rn_train,] 
test <- predgrade[-rn_train,]
fit <- multinom(formula2,data=train)


#N7 Predict nutrition grade on test and check performance
test$scores <- predict(fit, type="class", newdata=test)

c <- confusionMatrix(test$scores, test$nutrition_grade_fr) 
c
# Confusion Matrix and Statistics
# 
# Reference
# Prediction    1    2    3    4    5
# 1           1431  354  188   35   30
# 2           262  673  245   84  178
# 3           135  378 1387  440   55
# 4           4   49  434   1780  602
# 5           0    1   19   322  833
# 
# Overall Statistics
# 
# Accuracy : 0.6154         
# 95% CI : (0.6057, 0.625)
# No Information Rate : 0.2683         
# P-Value [Acc > NIR] : < 2.2e-16      
# 
# Kappa : 0.5116         
# Mcnemar's Test P-Value : < 2.2e-16      
# 
# Statistics by Class:
# 
# Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
# Sensitivity            0.7811  0.46254   0.6102   0.6689  0.49058
# Specificity            0.9249  0.90914   0.8682   0.8500  0.95840
# Pos Pred Value         0.7022  0.46671   0.5791   0.6204  0.70894
# Neg Pred Value         0.9491  0.90775   0.8822   0.8750  0.90108
# Prevalence             0.1847  0.14669   0.2292   0.2683  0.17119
# Detection Rate         0.1443  0.06785   0.1398   0.1795  0.08398
# Detection Prevalence   0.2055  0.14538   0.2415   0.2892  0.11846
# Balanced Accuracy      0.8530  0.68584   0.7392   0.7594  0.72449

#N9 Where are the most errors. What is the accuracy if we allow confusion between neighboring classes  

a<-c(1431,  354,   188,   35,   30)
b<-c(262,  673,  245,   84 ,   178)
c<-c(135,   378,    1387,   440,  55)
d<-c( 4,   49,  434,   1780,  602)
e<-c( 0,    1,   19,   322,  833)
conf <- matrix(c(a,b,c,d,e),nrow=5,byrow = T)

e1<-conf[1,2]+ conf[2,1]+ conf[2,3]+ conf[3,2]+ conf[3,4]+ conf[4,3]+ conf[5,4]+conf[4,5]#3037 1 class difference error
e2<-conf[1,3]+conf[3,1]+conf[2,4]+conf[4,2]+conf[3,5]+conf[5,3]                          #530  2 class difference error
e3<-conf[1,4]+conf[4,1]+conf[2,5]+conf[5,2]                                              #218   3 class difference error
e4<-conf[1,5]+conf[5,1]                                                                  #30   4 class difference error

alltrue<-conf[1,1]+ conf[2,2]+ conf[3,3]+ conf[4,4]+ conf[5,5] #6104
all<-alltrue+e1+e2+e3+e4 #9919
accura<-alltrue/all #0.615384
#allow errors betweenneighboring grades (1 class error distance)
alltrue1 <- alltrue+e1 #9141
accura1<-alltrue1/all #0.9215



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
#   (Intercept)         saturated_fat_100g sugars_100g     fat_100g  additives_n salt_100g   fiber_100g carbohydrates_100g
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

#V1 - Number of entries per country
count1 <- length(which(cntrycat$countriesgroups =='France'))
count2 <- length(which(cntrycat$countriesgroups =='SpainPort'))
count3 <- length(which(cntrycat$countriesgroups =='German'))
count4 <- length(which(cntrycat$countriesgroups =='UKIRSC'))
count5 <- length(which(cntrycat$countriesgroups =='US'))
count6 <- length(which(cntrycat$countriesgroups =='BeNeLux'))
count7 <- length(which(cntrycat$countriesgroups =='AuNZ'))
count8 <- length(which(cntrycat$countriesgroups =='CarribSAme'))
count9 <- length(which(cntrycat$countriesgroups =='ItalGreece'))
count10 <- length(which(cntrycat$countriesgroups =='Canada'))

par(mar = c(4, 4, 4, 1))
count=c(count1,count2,count3,count4,count5,count6,count7, count8, count9,count10)
grNames=c("France","SpainPort","German","UKIRSK","US","BeNeLux","AuNZ","CarribSAme","ItalGreece","Canada");
barplot(count, horiz=T,names.arg = grNames,cex.names=0.7,las =2) 


#V2 Food Categories PerCountry

cntry10=cntrycat[cntrycat$countriesgroups=="Canada",]
cntry5=cntrycat[cntrycat$countriesgroups=="US",]
cntry1=cntrycat[cntrycat$countriesgroups=="France",]
cntry2=cntrycat[cntrycat$countriesgroups=="SpainPort",]
cntry3=cntrycat[cntrycat$countriesgroups=="German",]
cntry4=cntrycat[cntrycat$countriesgroups=="UKIRSC",]
cntry6=cntrycat[cntrycat$countriesgroups=="BeNeLux",]
cntry7=cntrycat[cntrycat$countriesgroups=="AuNZ",]
cntry8=cntrycat[cntrycat$countriesgroups=="CarribSAme",]
cntry9=cntrycat[cntrycat$countriesgroups=="ItalGreece",]

t1<-table(cntry1$pnns_groups_1)
t2<-table(cntry2$pnns_groups_1)
t3<-table(cntry3$pnns_groups_1)
t4<-table(cntry4$pnns_groups_1)
t5<-table(cntry5$pnns_groups_1)
t6<-table(cntry6$pnns_groups_1)
t7<-table(cntry7$pnns_groups_1)
t8<-table(cntry8$pnns_groups_1)
t9<-table(cntry9$pnns_groups_1)
t10<-table(cntry10$pnns_groups_1)



par(xpd = TRUE)
par(mar = c(6.5, 5,1, 5))

colors=c(" red", "azure","yellow","deeppink","coral","brown","green","white", "orange","lightgoldenrod2")

#just legend
# barplot(c(0), beside=TRUE,
#         col=colors,  axes=F,las=2, cex.names = 0.6,
#         legend=levels(cntrycat$pnns_groups_1),
#         args.legend = list(y="top", x="left",fill=colors,cex=1.1,bty="n")) 

barplot(c(0),main="France", beside=TRUE,
        col=colors,  axes=F,las=2, cex.names = 0.6) 
barplot(t2, main="Spain Portugal", beside=TRUE,
        col=colors,  axes=F,las=2, cex.names = 0.6)
barplot(t3, main="Germany Austria Switzerland", beside=TRUE,
        col=colors,  axes=F,las=2, cex.names = 0.6)
barplot(t4, main="England Ireland Scotland", beside=TRUE,
        col=colors,  axes=F,las=2, cex.names = 0.6)
barplot(t5, main="United States", beside=TRUE,
        col=colors,  axes=F,las=2, cex.names = 0.6)
barplot(t6, main="BeNELux Monaco ", beside=TRUE,
        col=colors,  axes=F,las=2, cex.names = 0.6)
barplot(t7, main="Australia New Zealand", beside=TRUE,
        col=colors,  axes=F,las=2, cex.names = 0.6)
barplot(t8, main="South America Carribean", beside=TRUE,
        col=colors,  axes=F,las=2, cex.names = 0.6)
barplot(t9, main="Italy Greece", beside=TRUE,
        col=colors,  axes=F,las=2, cex.names = 0.6)
barplot(t10, main="Canada", beside=TRUE,
        col=colors,  axes=F,las=2, cex.names = 0.6)


#V3 Food Grades per Country
cntrygrade<-read.csv('C:/Roxana/ryerson/finalproject/factfoods/FoodFactsCountriesGrade.csv')

tt <- table(cntrygrade$nutrition_grade_fr,cntrygrade$countriesgroups)
tt
#   AuNZ BeNeLux Canada CarribSAme France German ItalGreece SpainPort UKIRSC   US
# a   84      87     17         46   4643    445         40       548    251  154
# b   85      92     21         42   3747    486         39       318    165  229
# c  118     164     24         82   6597    513         47       494    292  225
# d  126     212     26         49   7153    625         71       427    445  291
# e   90     185     20         42   4909    411         50       254    180  246


levels(cntrygrade$nutrition_grade_fr)
par(mar = c(6, 4,6, 0))

gradecolors = c("green","yellow","orange","pink","red")
barplot(prop.table(tt,2),  horiz=T,
         col = gradecolors,las =2, axes = F, cex.names = 0.7,
        legend=levels(cntrygrade$nutrition_grade_fr),
        args.legend = list(y="top", x="left",fill=gradecolors,cex=1.5,bty="n"))





















#NUTRITION ELEMENTS
# - correlation of nutrition elements
mycor1=cor(predgrade[,c("nutrition_grade_fr","pnns_groups_1","additives_n","energy_100g","fat_100g","saturated_fat_100g","sugars_100g","carbohydrates_100g","fiber_100g","proteins_100g","salt_100g","sodium_100g")],use="complete")
corrplot(mycor1,method="ellipse",tl.cex = .55)

#factor again
#predgrade$nutrition_grade_fr<-factor(predgrade$nutrition_grade_fr)

