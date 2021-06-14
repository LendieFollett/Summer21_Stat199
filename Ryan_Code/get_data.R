# Output: acs(clean).csv, which will serve as 'testing' data, and cps(clean).csv, which will serve as 'training' data.

# Note: Make sure you are in an R PROJECT - that will set your working directory
#       from there you can use short, relative paths
# Note: See https://censusreporter.org for a listing of ACS tables. For a tutorial on the 
# acs package, see https://rpubs.com/rosenblum_jeff/censustutorial1. The CPS data was 
# downloaded from https://cps.ipums.org.

# Set up
library(acs)

# API key
api.key.install(key="4a6312d77db109879039890c413e71c845d9c3ee") #this may not work? we'll see

# Get ACS data
myendyear <- 2019
myspan <- 5
geo.lookup(state=c("IA", "CA", "NY", "TX", "FL", "WA", "NJ", "MI", "AZ", "PA", "AK", "MA", "OH", "GA", "MN", "HI", "IL", 
"CO", "NC", "VA", "AL", "OR", "IN", "MD", "MO", "TN", "WI", "MT", "SC", "ME", "UT", "CT", "MS", "WY", "LA", "NV", "KY",
"RI", "AR", "NM", "ID", "DE", "NE", "KS", "VT", "NH", "SD", "OK", "WV", "ND"))
# geo.lookup didn't have a whole use version.

#mygeo <- geo.make(state="*", county="*", tract="*", block.group="*") #Ryan: We want for all states, not just iowa. How to change?

#mygeo <- geo.make(us = "*") # This grabs all of the U.S., no other geographic arguments allowed so hopefully it grabs
# a more complex look of each state.
# Oops, this just grabs the U.S. as a whole and doesn't look at things on a per state basis

# This works, although it takes a long time to create the csv file, could be a better way but
# I think in any way it would be a hastle. 
mygeo <- geo.make(state=c("IA", "CA", "NY", "TX", "FL", "WA", "NJ", "MI", "AZ", "PA", "AK", "MA", "OH", "GA", "MN", "HI", "IL", 
                          "CO", "NC", "VA", "AL", "OR", "IN", "MD", "MO", "TN", "WI", "MT", "SC", "ME", "UT", "CT", "MS", "WY", "LA", "NV", "KY",
                          "RI", "AR", "NM", "ID", "DE", "NE", "KS", "VT", "NH", "SD", "OK", "WV", "ND"), county="*", tract="*", block.group="*")



mytable1 <- acs.lookup(endyear=2015, table.number="B01001") # Use 2015 just for a "look up" year
mytable2 <- acs.lookup(endyear=2015, table.number="B25010")
mytable3 <- acs.lookup(endyear=2015, table.number="B03003")
mytable4 <- acs.lookup(endyear=2015, table.number="B02001")
mytable5 <- acs.lookup(endyear=2015, table.number="B15003")
mytable6 <- acs.lookup(endyear=2015, table.number="B23025")
mytable7 <- acs.lookup(endyear=2015, table.number="B12001")
mytable8 <- acs.lookup(endyear=2015, table.number="B22010")
myvars <- mytable1[c(1,3:6,20:30,44:49)] + mytable2[1] + mytable3[3] + 
  mytable4[3] + mytable5[21:25] + mytable6[c(4,6)] + mytable7[c(4,13)] + mytable8[c(3,6)]
mydata <- acs.fetch(endyear=myendyear, span=myspan, geography=mygeo, variable=myvars)
acs <- data.frame(GEOID = paste0(str_pad(mydata@geography$state, 2, "left", pad="0"), 
                                 str_pad(mydata@geography$county, 3, "left", pad="0"), 
                                 str_pad(mydata@geography$tract, 6, "left", pad="0")),
                  mydata@estimate)
acs$kids <- rowSums(acs[,c("B01001_003","B01001_004","B01001_005","B01001_006",
                           "B01001_027","B01001_028","B01001_029","B01001_030")])
acs$elderly <- rowSums(acs[,c("B01001_020","B01001_021","B01001_022","B01001_023",
                              "B01001_024","B01001_025","B01001_044","B01001_045","B01001_046","B01001_047",
                              "B01001_048","B01001_049")])
acs$education <- rowSums(acs[,c("B15003_021","B15003_022","B15003_023","B15003_024",
                                "B15003_025")])
acs$employed <- rowSums(acs[,c("B23025_004","B23025_006")])
acs$married <- rowSums(acs[,c("B12001_004","B12001_013")])
acs$disability <- rowSums(acs[,c("B22010_003","B22010_006")])
acs <- acs[,c("GEOID","B01001_001","B01001_026","B25010_001","B03003_003","B02001_003",
              "kids","elderly","education","employed","married","disability")]
colnames(acs) <- c("GEOID","population", "female", "avg_hhsize","hispanic","black","kids",
                   "elderly","education","employed","married","disability")
acs$households <- acs$population/acs$avg_hhsize
write.csv(acs, "Ryan_data/acs(clean).csv") #Ryan: note the use of relative filepaths - this should work if you use a project

# Get CPS data
cps <- read.csv("Ryan_data/cps(raw).csv")

# Can't we just get rid of this then? It seems to confine cps to only the state with the
# STATEFIP to 19. If we comment this out it'll work for us, right?
#cps <- cps[cps$STATEFIP==19,]#Ryan: again, we want it for all states, not just iowa. 
cps <- cps[, c("CPSID", "PERNUM", "FSRAWSCRA","FSTOTXPNC", "AGE", "SEX",  "FAMSIZE", "RACE", 
               "HISPAN", "EDUC", "EMPSTAT","MARST", "DIFFHEAR", "DIFFEYE", "DIFFREM", "DIFFPHYS", 
               "DIFFMOB", "DIFFCARE", "HWTFINL")]
cps$SEX <- cps$SEX - 1    # Create dummy variables
cps$CHILD <- ifelse(cps$AGE < 18, 1, 0)
cps$ELDERLY <- ifelse(cps$AGE > 64, 1, 0)
cps$BLACK <- ifelse(cps$RACE==200, 1, 0)
cps$HISPANIC <- ifelse(cps$HISPAN>0, 1, 0)
cps$EDUC <- as.integer(cps$EDUC %in% c(91,92,111,123,124,125))
cps$EMP <- as.integer(cps$EMPSTAT %in% c(1,10,12))
cps$MARRIED <- as.integer(cps$MARST %in% c(1,2))
cps$DIFF <- apply(cps[, c("DIFFHEAR","DIFFEYE","DIFFREM","DIFFPHYS","DIFFMOB","DIFFCARE")], 1, max)
cps$DIFF <- ifelse(cps$DIFF==2, 1, 0)
cps <- merge(
  aggregate(list(fsecurity=cps$FSRAWSCRA, fexpend=cps$FSTOTXPNC, hhsize=cps$FAMSIZE), 
            by = list(id=cps$CPSID), mean),
  aggregate(list(female=cps$SEX, kids=cps$CHILD, elderly=cps$ELDERLY, black=cps$BLACK, 
                 hispanic=cps$HISPANIC, education=cps$EDUC, employed=cps$EMP,
                 married=cps$MARRIED, disability=cps$DIFF,weight = cps$HWTFINL), by = list(id=cps$CPSID), sum))
cps$disability <- ifelse(cps$disability>0, 1, 0)  # Recode to dummy variable
cps$fsecurity[cps$fsecurity==98] <- NA   # Clean up missing values
cps$fsecurity[cps$fsecurity==99] <- NA
cps$fexpend[cps$fexpend==999] <- NA
cps$fexpend <- cps$fexpend/cps$hhsize  # In per person terms
write.csv(cps, "Ryan_data/cps(clean).csv")


# I think I got this all to work? I might need to work on the acs file in order to get it to match up more with the
# cps file, not certain though. If not I can begin creating categorical variables and 
# data visualizations and then begin to create randomForest, ROCCurve,
# etc. 

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

# Create a factorized fsecurity

cps$fsecurity_f = as.factor(cps$fsecurity)

# We want to create a binary variable of fsecurity, with anything  >= 1 being food insecure

cps$fsecurity_b = ifelse(cps$fsecurity > 0, 1, 0)


ggplot(data = cps) + geom_histogram(aes(x = hhsize), binwidth = 1)

ggplot(data = cps, aes(x = fsecurity)) + geom_bar() +  geom_text(stat = 'count', aes(label=..count..), vjust = -1)

ggplot(data = cps) + geom_histogram()




# CREATE FOREST, ROCCurve, Variable Importance Plot, confusion matrix

# Test set
test.df =  "Ryan_data/cps(clean).csv"
# Will this work or do I have to read it in? Probably read it in, but we'll see

# Training set
train.df = cps

# This will create a beginner forest, but we need to tune the forest so that we
# can determine the correct number of _________ (whatever mtry stands for, ntree doesn't
# change at all.)

fsecurity_forest = randomForest(fsecurity_f ~ female + kids + elderly + black + hispanic +
                                  education + employed + elderly + disability + hhsize, data = train.df, 
                                ntree = 1000, mtry = 3, importance = T)

# Should I get rid of all of the NA's? Is there a way to use them?

# MAKE SURE TO STANDARDIZE ACS 

# Should we tune the forest now?
# Do a for loop to go through every possible value for mtry

mtry = c(1:10)

keeps = data.frame(m = rep(NA, length(mtry)),
                           OOB_err_Rate = rep(NA, length(mtry)))


# could I do for idx in 1:10 instead?
for (idx in 1: length(mtry)){
  tempforest = randomForest(fsecurity_f ~ female + kids + elderly + black + hispanic +
                              education + employed + elderly + disability + hhsize, data = train.df,
                            ntree = 1000, mtry = mtry[idx])
  
  keeps[idx, "m"] = mtry[idx]
  
  keeps[idx, "OOB_err_Rate"] = mean(predict(tempforest) != train.df$fsecurity_f)
}

qplot(m, OOB_err_Rate,  geom = c("line", "point"), data = keeps) +
  them_bw() + labs(x = "m (mtry) value", y = "OOB Error Rate")

final_forest = randomForest()

varImpPlot(final_forest , type = 1)

# CREATE GLM Based off of important variables

fsecurity.glm = glm(fsecurity_b ~ , data = cps, family = binomial(link = "logit"))

beta_hat = coef(fsecurity.glm)

cps$pred = predict(fsecurity.glm, cps, type = 'response')


# The response variable is fsecurity_b which is a binary numeric variable, therefore
# we will probably use a bernoulli distributed logistic model with logit link to keep
# the variable between 0 and 1. 



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


