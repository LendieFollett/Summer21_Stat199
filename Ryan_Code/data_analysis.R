# I think I got this all to work? I might need to work on the acs file in order to get it to match up more with the
# cps file, not certain though. If not I can begin creating categorical variables and 
# data visualizations and then begin to create randomForest, ROCCurve,
# etc. 

rm(list = ls())

# These are all of the libraries needed for creating a heatmap of areas in danger of food insecurity.
library(ggplot2)
library(dplyr)
library(DT)
library(rmapshaper)
library(leaflet)
library(htmltools)
library(tigris)
library(totalcensus)
library(rpart)
library(rpart.plot)
library(pROC)
library(randomForest)
library(pscl)
library(boot)
library(reshape2)
library(RColorBrewer)
#These are for the zero-inflation model specifically
#install.packages('pscl')
library(gamlss.dist)

cps = read.csv("Ryan_Data/cps(clean).csv")

# ADD STATE AS A CATEGORICAL VARIABLE
# ADD REGION

# P-VALUE, NOT ALWAYS THAT IMPORTANT

# LOOKUP FOOD PANTRIES IN EACH STATE 

# POPULATION PER COUNTY COULD ADD A RURAL INDICATION LEVEL
# ADD IN BOTH ACS AND CPS

# ISSUES WITH THE CPS DATASET, SEEMS THAT THERE ARE ISSUES WITH HOW FEXPEND WAS
# COLLECTED. 

# NEED TO DO AN IF ELSE FOR urban_code
# CREATE A NEW VARIABLE? TECHNICALLY I DON'T NEED TO, I COULD
# JUST FIX THE ALREADY EXISTING URBAN VARIABLE

# This just cleans up the urbanicity variable and removes the NA's from the list while also
# making it a factorized variable not a numeric variable. 
cps$urban_c <- cps$urban

cps$urban_c <- ifelse(cps$urban_c == 1, "Large Central Metro",
                          ifelse(cps$urban_c == 2, "Large Fringe Metro",
                            ifelse(cps$urban_c == 3, "Medium Metro", 
                                 ifelse(cps$urban_c == 4, "Small Metro",
                                        ifelse(cps$urban_c == 5, "Micropolitan", "Non-Core/Possibly Rural")))))

cps$urban_c[is.na(cps$urban_c)] <- c("Possibly Non-core/Rural")

cps <- subset(cps, select = -c(urban_C))

cps$fsecurity_f = ifelse(cps$fsecurity > 0, "yes", "no")

str(urban_c)
# CREATE SUB-DATASETS OF CPS FOR FEXPEND AND FSECURITY

cps_fsecurity <- cps[!is.na(cps$fsecurity),]

cps_fexpend <- cps[!is.na(cps$fexpend),]

# REMOVE ID, Binary Fsecurity and, Factorized Fsecurity, what is weight?

cps_fsecurity = subset(cps_fsecurity, select = -c(id, weight, fexpend))

cps_fexpend = subset(cps_fexpend, select = -c(id, weight, fsecurity))

# ANALYSIS OF VARIABLES
ggplot(data = cps_fsecurity, aes (x = fsecurity))+geom_bar() + geom_text(stat = 'count',aes(label=..count..), vjust = -1) + labs(x = "Food Insecure", y = "Number of Households")

# ANALYSIS OF DISABILITY VARIABLE

cps_fsecurity$disability_cat = ifelse(cps_fsecurity$disability > 0, "Yes", "No")
cps_fsecurity$disability_cat = as.factor(cps_fsecurity$disability_cat)

cps_fsecurity$fsecurity_cat = ifelse(cps_fsecurity$fsecurity > 0, "yes", "no")
cps_fsecurity$fsecurity_cat = as.factor(cps_fsecurity$fsecurity_cat)

ggplot(data = cps_fsecurity, aes (x = disability))+geom_bar() + geom_text(stat = 'count',aes(label=..count..), vjust = -1) + 
  labs(x = "Disabled Individual Living Within Household", y = "Number of Households") 

cps_disability <- cps_fsecurity %>% group_by(disability_cat) %>% summarise(Average = mean(fsecurity))

ggplot(aes(x = disability_cat, y = Average, fill = Average), data = cps_disability ) + geom_bar(stat = "Identity") +
  labs(x = "Disabled Individual Living Within Household", y = "Average Level of Food Insecurity", fill = "Average Level")

ggplot() + geom_boxplot(aes(group = disability_cat, x = disability_cat, y = fsecurity, fill = disability_cat), data = cps_fsecurity) +
  labs(x = "Disabled Individual Living Within Household", y = "Level of Food Insecurity", fill = "If Disabled")


# ggplot(data = cps_fsecurity) +
#   geom_histogram(aes(x = disability, fill = fsecurity_f), position = 'fill', binwidth = 1) +
#   ggtitle("Food Insecurity as Disabled Individuals Increases") +
#   labs(x = "Number of Disabled Individuals in Household", y = "Average Level of Food Insecurity") +
#   scale_fill_brewer("Food Insecure") +
#   theme_bw()

# ANALYSIS OF ELDERLY VARIABLE

ggplot(data = cps_fsecurity, aes (x = elderly))+geom_bar() + scale_fill_brewer(palette = "Blues") + geom_text(stat = 'count',aes(label=..count..), vjust = -1) + 
  labs(x = "Number of Elderly in Household", y = "Number of Households")

cps_elderly <- cps_fsecurity %>% group_by(elderly) %>% summarise(meld = mean(fsecurity))

ggplot(aes(x = elderly, y = meld, fill = elderly), data = cps_elderly) + geom_bar(stat = "Identity") + 
  labs(x = "Number of Elderly in Household", y = "Average Level Of Food Insecurity", fill = "Number of Elderly")

ggplot() + geom_boxplot(aes(group = elderly, x = elderly, y = fsecurity), data = cps_fsecurity)


# ANALYSIS OF EDUCATION VARIABLE

ggplot(data = cps_fsecurity, aes(x = education))+ geom_bar() + geom_text(stat = 'count',aes(label=..count..), vjust = -1) +
  labs(x = "Number of Educated Individuals Within Household", y = "Number of Households")

cps_education <- cps_fsecurity %>% group_by(education) %>% summarise(med = mean(fsecurity))

ggplot(aes(x = education, y = med, fill = education), data = cps_education) + geom_bar(stat = "Identity") +
  labs(x = "Number of Educated Individuals Within Household", y = "Average Level of Food Insecurity", fill = "Number of Educated")

# ANALYSIS OF EMPLOYED VARIABLE

ggplot(data = cps_fsecurity, aes(x = employed))+ geom_bar() + geom_text(stat = 'count', aes(label = ..count..), vjust = -1) +
  labs(x = "Number of Employed Individuals Within Household", y = "Number of Households")

cps_employed <- cps_fsecurity %>% group_by(employed) %>% summarise(memp = mean(fsecurity))

ggplot(aes(x = employed, y = memp), data = cps_employed) + geom_bar(stat = "Identity") +
  labs(x = "Number of Employed Individuals Within Household", y = "Average Level of Food Insecurity")

# ANALYSIS OF HHSIZE VARIABLE

ggplot(data = cps_fsecurity, aes(x = hhsize)) + geom_bar() + geom_text(stat = 'count', aes(label = ..count..), vjust = -1) +
  labs(x = "Number of Family Members Within Household", y = "Number of Households")



ggplot(data = cps_fexpend, aes(x = fexpend))+geom_histogram(binwidth = 5)+ labs(x = "Food Expense", y = "Numer of Households" )



# Create the Forrest

#test.df = 

train.df = cps_fsecurity


# This will create a beginner forest, but we need to tune the forest so that we
# can determine the correct number of _________ (whatever mtry stands for, ntree doesn't
# change at all.)

# fsecurity_forest = randomForest(fsecurity ~ female + kids + elderly + black + hispanic +
#                                   education + employed + married + disability + hhsize + urban_c, data = train.df, 
#                                 ntree = 1000, mtry = 3, importance = T)
# 
# 
# # CREATE THE MTRY STUFF AND FOREST
# 
# # Set up mtry to be 
# 
# mtry = c(1:(ncol(cps_fsecurity) - 1))
# 
# # Make room for B, OOB ERROR
# # Why is it ntree = rep and not m = rep? Is that because of the 
# # difference in the type of response variable?
# keeps <- data.frame(m = rep(NA, length(mtry)),
#                     OOB_Err_Rate = rep(NA, length(mtry)))
# 
# for(idx in  1:length(mtry)){
#   print(paste0("Now testing mtry = ", mtry[idx]))
#   tempForest = randomForest(fsecurity ~female + kids + elderly + black + hispanic +
#   education + employed + married + disability + hhsize + urban_c,
#                             data = cps_fsecurity,
#                             mtry = mtry[idx])
# 
# keeps[idx, "m"] = mtry[idx]
#   
# # We do this since we are using a continuous response variable rather than a binary categorical
# # variable.
# keeps[idx, "OOB_Err_Rate"] = mean((predict(tempForest) - cps_fsecurity$fsecurity)^2)
# 
#   }
# 
# qplot(m, OOB_Err_Rate, geom = c("line", "point"), data = keeps) +
#   theme_bw() + labs(x = "m (mtry) value", y = "OOB Error Rate")
# 
# 

# OOB Error Rate is lowest at 2 it seems, so I'll go with 2.
# final_forest = randomForest(fsecurity ~ female + kids + elderly + black + hispanic +
#                              education + employed + married + disability + hhsize + urban_c, data = train.df, 
#                             ntree = 1000, mtry = 2, importance = T)

saveRDS(final_forest, "final_forest.RDS")
final_forest <- readRDS("final_forest.RDS")

varImpPlot(final_forest, type = 1)


# ZERO-INFLATED POISSON WITH REGARDS TO THE FSECURITY DATA

# DO THE VARIABLES NEED TO BE FACTORS? WHY?

# Negative binomial model vs logit part of model, which one goes where?
# I just made multiple models and then I plan to test them all against one another
# THE STRONGEST SHALL SURVIVE!!! (LOWEST AIC VALUE)

# THIS IS THE SELECTED MODEL, RIGHT NOW, WE WILL NOW INTERPRET THE COEFFICIENTS FOR THIS MODEL.
fsecurity.glm =  zeroinfl(fsecurity ~ disability + education | disability + education, data = cps_fsecurity)

fsecurity.glm2 = zeroinfl(fsecurity ~ disability + education + elderly | disability + education + elderly, data = cps_fsecurity)

fsecurity.glm3 = zeroinfl(fsecurity ~ disability + education + elderly + employed | disability + education + employed, data = cps_fsecurity)

fsecurity.glm4 = zeroinfl(fsecurity ~ disability + education +  elderly + employed + hhsize | disability + education + employed + hhsize, data = cps_fsecurity)

fsecurity.glm2
beta_hat <- coef(fsecurity.glm2)
exp(beta_hat)
exp(confint(fsecurity.glm2))

# CREATE A POISSON MODEL FOR COMPARISON AND TO DEMONSTRATE NEED FOR ZERO-INFLATED


# CREATE A ROCCURVE AND PREDICTIONS FOR THE ACS - TEST DATASET ALSO CONFUSION MATRIX 

acs_test.df$pred = predict(final_forest, acs_test.df, type = "class")

table(acs_train.df$pred, acs_train.df$fsecurity)

pi_hat = predict(final_forest, acs_test.df, type = "prob")[,]

rocCurve = roc(response = acs_test.df$fsecurity,
               predictor = pi_hat,
               levels = c())


# Disability Interpretation (Count Model): With all other variables held constant,for vulnerable individuals, the level of food insecurity increases by 
# a factor of exp(.2593606) = 1.2961011 if there is a disabled person living within the house.This means that if there is a disabled person
# living within the house, food insecurity of the house decreases from the mean by about 30%.

# Education Interpretation (Count Model): With all other variables held constant, for vulnerable individuals, the mean amount of food insecurity decreases by 
# a factor of exp(-.1114594) = .8945277 for every educated person within the house. This means that for every new educated individual
# living within the house, food insecurity of the house decreases from the mean by about 10.55%.

# Elderly Interpretation (Count Model): With all other variables held constant, for vulnerable individuals, the level of food insecurity decreases by a factor of
# exp(-.1748214) = .8396069 for every educated person within the house. This means that for every new elderly person living within
# the house, food insecurity of the house decreases from the mean by about 16.04%.

# Disability Interpretation (Zero-Inflation Model): With all other variables held constant, the odds that a household is within the 
# "Certain Zero" Group, meaning that they are certain to be food secure is exp(-1.0453348) = 0.3515741. This means that the odds
# of a house with a disabled person being food secure is 64.84% less likely than in other households. 

# Education Interpretation (Zero-Inflation Model): With all other variables held constant, the odds that a household is within the 
# "Certain Zero" Group, meaning that they are certain to be food secure is exp(0.7795625) = 2.1805181. This means that the odds 
# that a house is food secure increases by 118% for every person with an associates degree or higher. 

# Elderly Interpretation (Zero-Inflation Model): With all other variables held constant, the odds that a household is within the 
# "Certain Zero" Group, meaning that they are certain to be food secure is exp(6967225) = 2.0071634.  This means that the odds
# that a house is food secure increases by 100% for every elderly person within the house.

# THIS IS USED TO TEST BETWEEN MODELS
# THERE'S GOT TO BE A BETTER WAY, THIS WOULD TAKE TOO LONG, WHAT GOES INTO THE LOGIT AREA, 
# WHAT GOES INTO THE OTHER AREA, I FEEL LIKE I'M FORGETTING SOMETHING.
# vuong(fsecurity.glm, fsecurity.glm3), Don't use this, just use AIC values for comparison
# Vuong, which stands for Vuong's closeness test (I believe), which uses the Kullback - Leibler
# Information Criterion. 

# COLLECT AIC's INDIVIDUALLY

AIC(fsecurity.glm)
# AIC = 

AIC(fsecurity.glm2)
# AIC = 

AIC(fsecurity.glm3)
# AIC = 

AIC(fsecurity.glm4)
# AIC = 

# It seems that fsecurity.glm2 is the best

summary(fsecurity.glm)

summary(fsecurity.glm3)

# THIS DOES NOT WORK FORE ZERO INFLATION MODELS, WHY VUONG IS USED.
#anova(fsecurity.glm, test = "Chisq")


# WORK ON FEXPEND, THAT WILL BE THE NEXT SECTION
# IF I HAVE TO CREATE A FORREST I THINK ILL TRY TO USE MY GPU's
# ALONG IN THE PROCESS, THAT MIGHT HELP TIME WISE
# 
# fexpend_train.df <- cps_fexpend
# 
# fexpend_forest = randomForest(fexpend ~., data = fexpend_train.df, ntree = 1000, mtry = 3, importance = TRUE)
# 
# keeps = data.frame(m = rep(NA, length(mtry)),
#                    OOB_Err_Rate = rep(NA, length(mtry)))
# 
# for(idx in 1:length(mtry)){
#   print(paste0("Now testing mtry = ", mtry[idx]))
#   fexpend_forest = randomForest(fexpend ~., data = fexpend_train.df, ntree = 1000, mtry = mtry[idx])
#   
#   keeps[idx, "m"] = mtry[idx]
#   
#   keeps[idx, "OOB_Err_Rate"] = mean((predict(fexpend_forest) - cps_fexpend$fexpend)^2)
# }
# 
# qplot(m, OOB_Err_Rate, geom = c("line", "point"), data = keeps) +
#   theme_bw() + labs(x = "m (mtry) value", y = "OOB Error Rate")
# 
# fexpend_final_forest = randomForest(fexpend ~., data = fexpend_train.df, ntree = 1000, mtry = 2, importance = TRUE)
# 
# saveRDS(fexpend_final_forest, "fexpend_final_forest.RDS")
fexpend_final_forest <- readRDS("fexpend_final_forest.RDS")

varImpPlot(fexpend_final_forest, type = 1)


#  CREATE LOGIT LINKED NORMAL DISTRIBUTION, IF TIME PERMITS COMPARE IT TO ZERO ADJUSTED GAMMA - NEED TO 
# ADD 0.001 TO EVERYTHING AS GAMMA NEEDS DATA BETWEEN 0 AND INFINITY.

ZAGA_Model = gamlss(formula = y~., sigma.formula = y~., nu.formula = y~., data = cps_fexpend, family = ZAGA())

cps_fexpend$fexpend_0 <- ifelse(cps_fexpend$fexpend == 0, 1, 0)

cps_fexpend_new <- subset(cps_fexpend, fexpend != 0)

fexpend.glm <- glm(fexpend ~ hhsize + elderly + employed + disability + education, data = cps_fexpend, family = gaussian(link = "identity"))

fexpend.glm2 <- glm(fexpend ~ hhsize + elderly + employed + disability + education, data = cps_fexpend_new, family = Gamma(link = "log"))

fexpend.glm3 <- glm(fexpend_0 ~ hhsize + elderly + employed + disability + education, data = cps_fexpend, family = binomial(link = "log"))

fexpend.glm4 <- glm(fexpend ~ hhsize + elderly + employed + disability, data = cps_fexpend_new, family = Gamma(link = "log"))

fexpend.glm5 <- glm(fexpend ~ hhsize + elderly + employed, data = cps_fexpend_new, family = Gamma(link = "log"))

AIC(fexpend.glm)

AIC(fexpend.glm2)

AIC(fexpend.glm3)

AIC(fexpend.glm4)

AIC(fexpend.glm5)

fexpend.glm2

fexpend.glm3

beta_hat1 <- coef(fexpend.glm2)
exp(beta_hat1)
exp(confint(fexpend.glm2))

beta_hat2 <- coef(fexpend.glm3)
exp(beta_hat2)
exp(confint(fexpend.glm3))


# ANALYSIS FOR FEXPEND - Gamma Count Model

# HHSIZE INTERPRETATION (Count Model): With all other variables held constant, for individuals who purchase food, the mean amount of food expenditure
# changes by a factor of exp(-.22794) = .7961747. This means that for every new person that lives within the house the amount of money used to
# buy food for each individual decreases by about 20.4% per person compared to the mean.

# ELDERLY INTERPRETATION (Count Model): With all other variables held constant, for individuals who purchase food, the mean amount of food expenditure
# changes by a factor of exp(-.04794) = .9531918. This means that for every new elderly person within the household, the amount of money in U.S. dollars
# used to buy food for each individual decreases by about 4.68% per person compared to the mean.

# EMPLOYED INTERPRETATION (Count Model): With all other variables held constant, for individuals who purchase food, the mean amount of food expenditure
# changes by a factor of exp(.06672) = 1.0690010. This means that for every new employed individual within the household, the amount of money in U.S. dollars
# used to buy food for each individual increase by about 6.9% per person compared to the mean.

# DISABILITY INTERPRETATION (Count Model): With all other variables held constant, for individuals who purchase food, the mean amount of food expenditure
# changes by a factor of exp(-0.06584) = .9362819. This means that for every new employed individual within the household, the amount of money in U.S. dollars
# used to buy food for each individual decreases by about 6.37% per person compared to the mean.

# EDUCATION INTERPRETATION (Count Model): With all other variables held constant, for individuals who purchase food, the mean amount of food expenditure
# changes by a factor of exp(0.12464) = 1.1327362.This means that for every new educated individual within the household, the amount of money in U.S. dollars
# used to buy food for each individual increases by about 13.27% per person compared to the mean.

# HHSIZE INTERPRETATION (Zero-Inflation Model(Binary)): With all other variables held constant, the odds of spending $0 changes by a factor of
# exp(-0.07890) = .9241349. This means that for every new person within the household, the odds of said household spending $0 on food per person 
# decreases by about 7.6%. 

# ELDERLY INTERPRETATION (Zero-Inflation Model(Binary)): With all other variables held constant, the odds of spending $0 changes by a factor of 
# exp(-0.01478) = .9853277. This means that for every new elderly person within the household, the odds of said household spending $0 on food per person 
# decreases by about 1.47%. 

# EMPLOYED INTERPRETATION (Zero-Inflation Model(Binary)): With all other variables held constant, the odds of spending $0 changes by a factor of 
# exp(-0.30615)= .7362754. This means that for every new employed person within the household, the odds of said household spending $0 on food per person
# decreases by about 26.37%.

# DISABILITY INTERPRETATION (Zero-Inflation Model(Binary)): With all other variables held constant, the odds of spending $0 changes by a factor of 
# exp(0.25147) = 1.2859122. This means that for every new disabled person within the household, the odds of said household spending $0 on food per person
# increases by about 28.59%.

# EDUCATION INTERPRETATION (Zero-Inflation Model(Binary)): With all other variables held constant, the odds of spending $0 changes by a factor of 
# exp(-0.25484) = .7750404. This means that for every new educated person within the household, the odds of said household spending $0 on food per person
# decreases by about 22.50%.

cps_fexpend_f <- cps_fexpend

cps_fexpend_f$disability <- ifelse(cps_fexpend_f$disability > 0, "Yes", "No")

breaks <- c(0, 1, 2, 3, 4, 5, 6, 7, 8 ,9 ,10, 11, 12, 13)

tags <- c('0','1', '2', '3', '4', '5', '6', '7', '8' ,'9' ,'10', '11', '12')

cps_fexpend_f$hhsize_f <- cut(cps_fexpend_f$hhsize, breaks = breaks, include.lowest = TRUE, right = FALSE, labels = tags)

# ANALYSIS OF VARIABLES
ggplot(data = cps_fexpend, aes (x = fexpend, fill = fexpend))+geom_histogram(binwidth = 5) + labs(x = "Food Expense In USD", y = "Number of Households") +
  scale_x_continuous(labels=scales::dollar_format()) + scale_fill_brewer(palette = "Blues")


# ANALYSIS OF DISABILITY VARIABLE

ggplot(data = cps_fexpend_f, aes (x = disability))+geom_bar() + geom_text(stat = 'count',aes(label=..count..), vjust = -1) + 
  labs(x = "If Disabled Individual Within Household", y = "Number of Households") 

cps_fexpend_disability <- cps_fexpend_f %>% group_by(disability) %>% summarise(me = mean(fexpend))

ggplot(aes(x = disability, y = me), data = cps_fexpend_disability) + geom_bar(stat = "Identity") +
  labs(x = "Number of Disabled Individuals in Household", y = "Average Level of Food Insecurity") +
  scale_y_continuous(labels=scales::dollar_format())

ggplot() + geom_boxplot(aes(group = disability, x = disability, y = fexpend), data = cps_fexpend) +
  scale_y_continuous(labels=scales::dollar_format())

# ANALYSIS OF ELDERLY VARIABLE

ggplot(data = cps_fexpend, aes (x = elderly))+geom_bar() + geom_text(stat = 'count',aes(label=..count..), vjust = -1) + 
  labs(x = "Elderly In Household", y = "Number of Households") +
  scale_fill_brewer(palette = "Blues")

cps_fexpend_elderly <- cps_fexpend %>% group_by(elderly) %>% summarise(mexp = mean(fexpend))

ggplot(aes(x = elderly, y = mexp), data = cps_fexpend_elderly) + geom_bar(stat = "Identity") + 
  labs(x = "Number of Elderly in Household", y = "Average Amount of Food Expenditure in Household") + 
  scale_y_continuous(labels=scales::dollar_format())

ggplot() + geom_boxplot(aes(group = elderly, x = elderly, y = fexpend), data = cps_fexpend)

# ANALYSIS OF EDUCATION VARIABLE

ggplot(data = cps_fexpend, aes(x = education))+ geom_bar() + geom_text(stat = 'count',aes(label=..count..), vjust = -1) +
  labs(x = "Number of Educated Individuals Within Household", y = "Number of Households")

cps_fexpend_education <- cps_fexpend %>% group_by(education) %>% summarise(med = mean(fexpend))

ggplot(aes(x = education, y = med), data = cps_fexpend_education) + geom_bar(stat = "Identity") +
  labs(x = "Number of Elderly in Household", y = "Average Amount of Food Expenditure in Household") +
  scale_y_continuous(labels=scales::dollar_format())

ggplot() + geom_boxplot(aes(group = education, x = education, y = fexpend), data = cps_fexpend)

# ANALYSIS OF EMPLOYED VARIABLE

ggplot(data = cps_fexpend, aes(x = employed))+ geom_bar() + geom_text(stat = 'count', aes(label = ..count..), vjust = -1) +
  labs(x = "Number of employed Individuals Within Household", y = "Number of Households")

cps_fexpend_employed <- cps_fexpend %>% group_by(employed) %>% summarise( Average = mean(fexpend))

ggplot(aes(x = employed, y = Average, fill = Average), data = cps_fexpend_employed) + geom_bar(stat = "Identity") +
  labs(x = "Number of Employed in Household", y = "Average Amount of Food Expenditure Per Household") +
  scale_y_continuous(labels=scales::dollar_format()) 

ggplot() + geom_boxplot(aes(group = employed, x = employed, y = fexpend), data = cps_fexpend)

# ANALYSIS OF HHSIZE VARIABLE



ggplot(data = cps_fsecurity, aes(x = hhsize)) + geom_histogram(binwidth = 1) + 
  #geom_text(stat = 'count', aes(label = ..count..), vjust = -1) +
  labs(x = "Number of Family Members Household", y = "Number of Households")

cps_fexpend_employed <- cps_fexpend_f %>% group_by(hhsize_f) %>% summarise(Average = mean(fexpend))

ggplot(aes(x = hhsize_f, y = Average, fill = Average), data = cps_fexpend_employed) + geom_bar(stat = "Identity") +
  labs(x = "Number of Individuals Within Household", y = "Average Level of Food Insecurity") +
  scale_y_continuous(labels=scales::dollar_format())

ggplot() + geom_boxplot(aes(group = hhsize, x = disability, y = fexpend), data = cps_fexpend)


ggplot(data = cps_fexpend, aes(x = hhsize, y = fexpend))+geom_jitter()+ labs(x = "Number of Family Members Within Household", y = "Food Expense" ) +
  scale_y_continuous(labels=scales::dollar_format())

# FOR LATER


# CREATE PLOTS FOR urban_c 

ggplot(data = cps_fsecurity, aes(x = urban_c)) + geom_bar() + geom_text(stat = 'count', aes(label = ..count..), vjust = -1) +
  labs(x = "", y =  "")

cps_fsecurity_urban <- cps_fsecurity %>% group_by(urban_c) %>% summarise(Average = mean(fsecurity))

ggplot(aes(x = urban_c, y = Average, fill = Average), data = cps_fsecurity_urban) + geom_bar(stat = "Identity") 
#  labs(x = "Number of Individuals Within Household", y = "Average Level of Food Insecurity")

ggplot(data = cps_fexpend, aes(x = urban_c)) + geom_bar() + geom_text(stat = 'count', aes(label = ..count..), vjust = -1) +
  labs()

cps_fsecurity_urban <- cps_fexpend  %>% group_by(urban_c) %>% summarise(Average = mean(fexpend))

ggplot(aes(x = urban_c, y = Average, fill = Average), data = cps_fsecurity_urban) + geom_bar(stat = "Identity") 
#  labs(x = "Number of Individuals Within Household", y = "Average Level of Food Insecurity")

# CREATE Heatmap, other cluster based visualizations?

acs$GEOID = as.character(paste0(acs$GEOID, substr(acs$X, 13, 13)))


#this is block groups w/in tracts
ia_shp = block_groups(state = "IA")

county_list = unique(counties("Iowa")$NAME)
county_list = county_list[order(county_list)]
all_counties = block_groups(state = 'IA', county = county_list)

is_shp_join = left_join(ia_shp, acs, by='GEOID') %>%
  rmapshaper::ms_simplify(keep = 0.01, keep_shapes = TRUE)


pal_bin = colorBin(
  palette = "YlOrRd", domain = ia_shp_join$elderly,
  bins = seq(0, max(ia_shp_join$elderly, na.rm = TRUE), length.out = 9)
)

leaflet(ia_shp_join, height = 500, width = 1000) %>%
  addTiles() %>%
  addPolygons(
    fillColor =~ pal_bin(elderly),
    color = "white",
    stroke = FALSE,
    fillOpacity = 0.6,
    highlight = highlightOptions(
      color = "black",
      bringToFront = TRUE
    )
  )%>%
  leaflet::addLegend(
    pal = pal_bin, values=~elderly,
    opacity = 0.7, title = "Iowa Elderly",
    position = "bottomright"
  )