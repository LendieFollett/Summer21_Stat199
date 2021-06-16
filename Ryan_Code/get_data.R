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
new_cps <- new_cps[, c("CPSID", "PERNUM", "FSRAWSCRA","FSTOTXPNC", "AGE", "SEX",  "FAMSIZE", "RACE", 
               "HISPAN", "EDUC", "EMPSTAT","MARST", "DIFFHEAR", "DIFFEYE", "DIFFREM", "DIFFPHYS", 
               "DIFFMOB", "DIFFCARE", "HWTFINL", "urban_code")]
new_cps$SEX <- new_cps$SEX - 1    # Create dummy variables
new_cps$CHILD <- ifelse(new_cps$AGE < 18, 1, 0)
new_cps$ELDERLY <- ifelse(new_cps$AGE > 64, 1, 0)
new_cps$BLACK <- ifelse(new_cps$RACE==200, 1, 0)
new_cps$HISPANIC <- ifelse(new_cps$HISPAN>0, 1, 0)
new_cps$EDUC <- as.integer(new_cps$EDUC %in% c(91,92,111,123,124,125))
new_cps$EMP <- as.integer(new_cps$EMPSTAT %in% c(1,10,12))
new_cps$MARRIED <- as.integer(new_cps$MARST %in% c(1,2))
new_cps$DIFF <- apply(new_cps[, c("DIFFHEAR","DIFFEYE","DIFFREM","DIFFPHYS","DIFFMOB","DIFFCARE")], 1, max)
new_cps$DIFF <- ifelse(new_cps$DIFF==2, 1, 0)
new_cps <- merge(
  aggregate(list(fsecurity=new_cps$FSRAWSCRA, fexpend=new_cps$FSTOTXPNC, hhsize=new_cps$FAMSIZE, urban = new_cps$urban_code), 
            by = list(id=new_cps$CPSID), mean),
  aggregate(list(female=new_cps$SEX, kids=new_cps$CHILD, elderly=new_cps$ELDERLY, black=new_cps$BLACK, 
                 hispanic=new_cps$HISPANIC, education=new_cps$EDUC, employed=new_cps$EMP,
                 married=new_cps$MARRIED, disability=new_cps$DIFF,weight = new_cps$HWTFINL), by = list(id=new_cps$CPSID), sum))
new_cps$disability <- ifelse(new_cps$disability>0, 1, 0)  # Recode to dummy variable
new_cps$fsecurity[new_cps$fsecurity==98] <- NA   # Clean up missing values
new_cps$fsecurity[new_cps$fsecurity==99] <- NA
new_cps$fexpend[new_cps$fexpend==999] <- NA
new_cps$fexpend <- new_cps$fexpend/new_cps$hhsize  # In per person terms
write.csv(cps, "Ryan_data/cps(clean).csv")

# THE NA's are around 15007 out of 49259

table(new_cps$fexpend > 0)

# I think I got this all to work? I might need to work on the acs file in order to get it to match up more with the
# cps file, not certain though. If not I can begin creating categorical variables and 
# data visualizations and then begin to create randomForest, ROCCurve,
# etc. 

county_codes = read.csv("Ryan_Data/NCHSURCodes2013.csv")

county_codes = rename(county_codes, "COUNTY" = "ï..FIPS.code")

cps_raw = read.csv("Ryan_Data/cps(raw).csv")

cps_raw$COUNTY = as.numeric(cps_raw$COUNTY)

county_codes$COUNTY = as.numeric(county_codes$COUNTY)

new_cps = merge(x = cps_raw, y = county_codes, by = "COUNTY" , all.x = TRUE)

new_cps = rename(new_cps, "urban_code" = "X2013.code")
