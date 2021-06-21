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
#These are for the zero-inflation model specifically
#install.packages('pscl')

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

cps$urban_c <- cps$urban

cps$urban_c <- ifelse(cps$urban_c == 1, "Large Central Metro",
                          ifelse(cps$urban_c == 2, "Large Fringe Metro",
                            ifelse(cps$urban_c == 3, "Medium Metro", 
                                 ifelse(cps$urban_c == 4, "Small Metro",
                                        ifelse(cps$urban_c == 5, "Micropolitan", "Non-Core/Possibly Rural")))))

cps$urban_c[is.na(cps$urban_c)] <- c("Possibly Non-core/Rural")

cps <- subset(cps, select = -c(urban_C))

str(urban_c)

# CREATE SUB-DATASETS OF CPS FOR FEXPEND AND FSECURITY

cps_fsecurity <- cps[!is.na(cps$fsecurity),]

cps_fexpend <- cps[!is.na(cps$fexpend),]

# REMOVE ID, Binary Fsecurity and, Factorized Fsecurity, what is weight?

cps_fsecurity = subset(cps_fsecurity, select = -c(id, weight, fexpend))

cps_fexpend = subset(cps_fexpend, select = -c(id, weight, fsecurity))

# Create the Forrest

#test.df = 

train.df = cps_fsecurity


# This will create a beginner forest, but we need to tune the forest so that we
# can determine the correct number of _________ (whatever mtry stands for, ntree doesn't
# change at all.)

# fsecurity_forest = randomForest(fsecurity ~ female + kids + elderly + black + hispanic +
#                                   education + employed + elderly + disability + hhsize, data = train.df, 
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
#   tempForest = randomForest(fsecurity ~.,
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
# # OOB Error Rate is lowest at 2 it seems, so I'll go with 2, I guess?
# final_forest = randomForest(fsecurity ~ female + kids + elderly + black + hispanic +
#                               education + employed + elderly + disability + hhsize, data = train.df, 
#                             ntree = 1000, mtry = 2, importance = T)

#saveRDS(final_forest, "final_forest.RDS")
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

# Disability Interpretation (Count Model): With all other variables held constant, the level of food insecurity increases by 
# exp(.2593606) = 1.2961011 if there is a disabled person living within the house.This means that if there is a disabled person
# living within the house than the expected level of food insecurity would increase by 1.2961011.

# Education Interpretation (Count Model): With all other variables held constant, the level of food insecurity decreases by 
# exp(-.1114594) = .8945277 for every educated person within the house. 

# Elderly Interpretation (Count Model): With all other variables held constant, the level of food insecurity decreases by
# exp(-.1748214) = .8396069 for every educated person within the house. 

# Disability Interpretation (Zero-Inflation Model): With all other variables held constant, the odds that a household is within the 
# "Certain Zero" Group, meaning that they are certain to be food secure is exp(-1.0453348) = 0.3515741. This means that being certain the odds
# of a house with a disabled person will be food secure is 64.84% less likely than in other households. 

# Education Interpretation (Zero-Inflation Model): With all other variables held constant, the odds that a household is within the 
 # "Certain Zero" Group, meaning that they are certain to be food secure is exp(0.7795625) = 2.1805181. This means that the odds of being certain
# that a house is food secure increases by 118% for every person with an associates degree or higher. 

# Elderly Interpretation (Zero-Inflation Model): With all other variables held constant, the odds that a household is within the 
# "Certain Zero" Group, meaning that htey are certain to be food secure is exp(6967225) = 2.0071634.  This means that the odds of being certain
# that a house is food secure increases by 100% for every elderly person within the house. This is odd, and probably is more 
# indicative of how elderly people eat less, not that we should put an elderly person in each home. 

# THIS IS USED TO TEST BETWEEN MODELS
# THERE'S GOT TO BE A BETTER WAY, THIS WOULD TAKE TOO LONG, WHAT GOES INTO THE LOGIT AREA, 
# WHAT GOES INTO THE OTHER AREA, I FEEL LIKE I'M FORGETTING SOMETHING.
vuong(fsecurity.glm, fsecurity.glm3)
# Vuong, which stands for Vuong's closeness test (I believe), which uses the Kullback - Leibler
# Information Criterion. 

# COLLECT AIC's INDIVIDUALLY

AIC(fsecurity.glm)
# AIC = 53419.87

AIC(fsecurity.glm2)
# AIC = 56816.14

AIC(fsecurity.glm3)
# AIC = 56394.28

AIC(fsecurity.glm4)
# AIC = 55793.17

# It seems that fsecurity.glm2 is the best

summary(fsecurity.glm)

summary(fsecurity.glm3)

# THIS DOES NOT WORK FORE ZERO INFLATION MODELS, WHY VUONG IS USED.
#anova(fsecurity.glm, test = "Chisq")


# WORK ON FEXPEND, THAT WILL BE THE NEXT SECTION
# IF I HAVE TO CREATE A FORREST I THINK ILL TRY TO USE MY GPU's
# ALONG IN THE PROCESS, THAT MIGHT HELP TIME WISE

fexpend_train.df <- cps_fexpend

fexpend_forest = randomForest(fexpend ~., data = fexpend_train.df, ntree = 1000, mtry = 3, importance = TRUE)

keeps = data.frame(m = rep(NA, length(mtry)),
                   OOB_Err_Rate = rep(NA, length(mtry)))

for(idx in 1:length(mtry)){
  print(paste0("Now testing mtry = ", mtry[idx]))
  fexpend_forest = randomForest(fexpend ~., data = fexpend_train.df, ntree = 1000, mtry = mtry[idx])
  
  keeps[idx, "m"] = mtry[idx]
  
  keeps[idx, "OOB_Err_Rate"] = mean((fexpend_forest - cps_fexpend$fexpend)^2)
}

qplot(m, OOB_Err_Rate, geom = c("line", "point"), data = keeps) +
  theme_bw() + labs(x = "m (mtry) value", y = "OOB Error Rate")

fexpend_final_forest = randomForest()

saveRDS(fexpend_final_forest, "fexpend_final_forest.RDS")
fexpend_final_forest <- readRDS("fexpend_final_forest.RDS")



# FOR LATER

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