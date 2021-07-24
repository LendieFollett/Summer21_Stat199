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





# COMBINE ACS AND COUNTY_CODES DATASETS

county_codes = read.csv("Ryan_Data/NCHSURCodes2013.csv")

acs = read.csv("Ryan_data/acs(clean).csv")

acs = subset(acs, select = -c(county_name))

county_codes = rename(county_codes, "county_name" = "County.name")

acs = rename(acs, "location" = "X")

acs$county = acs$location

acs$county = gsub("[[:digit:]]", "", acs$county)

acs$county = gsub("Block Group ,", "", acs$county)

acs$county = gsub("Census Tract ,", "", acs$county)

acs$county = gsub("Census Tract .,", "", acs$county)

#acs$county = gsub("(.*),.*", "\\1", acs$county)

acs$county = gsub("  ", "", acs$county)

acs = rename(acs, "state_and_county" = "county") 

county_codes$state_name = county_codes$State.Abr.

county_codes$state_name <- ifelse(county_codes$state_name == "AL", "Alabama",
                            ifelse(county_codes$state_name == "AK", "Alaska",
                            ifelse(county_codes$state_name == "AZ", "Arizona",
                            ifelse(county_codes$state_name == "AR", "Arkansas",
                            ifelse(county_codes$state_name == "CA", "California",
                            ifelse(county_codes$state_name == "CO", "Colorado",
                            ifelse(county_codes$state_name == "CT", "Connecticut",
                            ifelse(county_codes$state_name == "DE", "Delaware",
                            ifelse(county_codes$state_name == "FL", "Florida",
                            ifelse(county_codes$state_name == "GA", "Georgia",
                            ifelse(county_codes$state_name == "HI", "Hawaii",
                            ifelse(county_codes$state_name == "ID", "Idaho",
                            ifelse(county_codes$state_name == "IL", "Illinois",
                            ifelse(county_codes$state_name == "IN", "Indiana",
                            ifelse(county_codes$state_name == "IA", "Iowa",
                            ifelse(county_codes$state_name == "KS", "Kansas",
                            ifelse(county_codes$state_name == "KY", "Kentucky",
                            ifelse(county_codes$state_name == "LA", "Louisiana",
                            ifelse(county_codes$state_name == "ME", "Maine",
                            ifelse(county_codes$state_name == "MD", "Maryland",
                            ifelse(county_codes$state_name == "MA", "Massachusetts",
                            ifelse(county_codes$state_name == "MI", "Michigan",
                            ifelse(county_codes$state_name == "MN", "Minnesota",
                            ifelse(county_codes$state_name == "MS", "Mississippi",
                            ifelse(county_codes$state_name == "MO", "Missouri",
                            ifelse(county_codes$state_name == "MT", "Montana",
                            ifelse(county_codes$state_name == "NE", "Nebraska",
                            ifelse(county_codes$state_name == "NV", "Nevada",
                            ifelse(county_codes$state_name == "NH", "New Hampshire",
                            ifelse(county_codes$state_name == "NJ", "New Jersey",
                            ifelse(county_codes$state_name == "NM", "New Mexico",
                            ifelse(county_codes$state_name == "NY", "New York",
                            ifelse(county_codes$state_name == "NC", "North Carolina",
                            ifelse(county_codes$state_name == "ND", "North Dakota",
                            ifelse(county_codes$state_name == "OH", "Ohio",
                            ifelse(county_codes$state_name == "OK", "Oklahoma",
                            ifelse(county_codes$state_name == "OR", "Oregon",
                            ifelse(county_codes$state_name == "PA", "Pennsylvania",
                            ifelse(county_codes$state_name == "RI", "Rhode Island",
                            ifelse(county_codes$state_name == "SC", "South Carolina",
                            ifelse(county_codes$state_name == "SD", "South Dakota",
                            ifelse(county_codes$state_name == "TN", "Tennessee",
                            ifelse(county_codes$state_name == "TX", "Texas",
                            ifelse(county_codes$state_name == "UT", "Utah",
                            ifelse(county_codes$state_name == "VT", "Vermont",
                            ifelse(county_codes$state_name == "VA", "Virginia",
                            ifelse(county_codes$state_name == "WA", "Washington",
                            ifelse(county_codes$state_name == "WV", "West Virginia",
                            ifelse(county_codes$state_name == "WI", "Wisconsin", 
                            ifelse(county_codes$state_name == "WY", "Wyoming", "NA"))))))))))))))))))))))))))))))))))))))))))))))))))

county_codes = county_codes[-c(324),]

county_codes$state_and_county <- paste(county_codes$county_name,",",county_codes$state_name)

county_codes$state_and_county <- gsub(" ,", ",", county_codes$state_and_county)

county_codes$state_and_county = as.factor(county_codes$state_and_county)

acs$state_and_county = as.factor(acs$state_and_county)

acs$state_and_county <- tolower(acs$state_and_county)

county_codes$state_and_county <- tolower(county_codes$state_and_county)

# BEFORE THIS WILL WORK I NEED TO CHANGE LASALLE ILLINOIS INTO LA SALLE IN THE ACS DATABASE, I NEED TO CHANGE DONA ANA IN THE ACS DATABASE
# TO DONA ANA, AND I NEED TO CHANGE PETERSBURG BOROUGH INTO PETERSBURG CENSUS AREA OR JUST CHANGE IT TO 6.

acs$state_and_county <- gsub("lasalle county, illinois", "la salle county, illinois", acs$state_and_county)

acs$state_and_county <- gsub("doã±a ana county, new mexico", "dona ana county, new mexico", acs$state_and_county)

acs$state_and_county <- gsub("petersburg borough, alaska", "petersburg census area, alaska", acs$state_and_county)

acs = merge(x = acs, y = county_codes, by = "state_and_county", all.x = TRUE)

# IT WORKS!! NOW I NEED TO REMOVE EVERYTHING I DON'T NEED AND KEEP THE 2013 CODE

acs <- acs[, c("location","avg_hhsize", "hispanic","elderly","black","kids","education","employed","married","disability","households",
                       "X2013.code", "female" )]

acs = rename(acs, "urban_code" = "X2013.code")

acs$urban_c <- acs$urban_code

acs$urban_c <- ifelse(acs$urban_c == 1, "Large Central Metro",
                          ifelse(acs$urban_c == 2, "Large Fringe Metro",
                                 ifelse(acs$urban_c == 3, "Medium Metro", 
                                        ifelse(acs$urban_c == 4, "Small Metro",
                                               ifelse(acs$urban_c == 5, "Micropolitan", "Non-Core/Possibly Rural")))))


#acs$urban_c <- factor(acs$urban_c, levels = c("Large Central Metro", "Large Fringe Metro",  "Medium Metro",
#                                                        "Small Metro","Micropolitan", "Non-Core/Possibly Rural" ))

# THESE ARE CAUSING ISSUES, I NEED TO ONLY DIVIDE WHERE THERE ARE ACTUAL VALUES

acs = na.omit(acs)

acs$hispanic <- round(acs$hispanic/acs$households, digits = 3)

acs$elderly <- round(acs$elderly/acs$households, digits = 3)

acs$black <- round(acs$black/acs$households, digits = 3)

acs$kids <- round(acs$kids/acs$households, digits = 3)

acs$education <- round(acs$education/acs$households, digits = 3)

acs$employed <- round(acs$employed/acs$households, digits = 3)

acs$married <- round(acs$married/acs$households, digits = 3)

acs$disability <- round(acs$disability/acs$households, digits = 3)

acs$female <- round(acs$female/acs$households, digits = 3)

acs = rename(acs, "hhsize" = "avg_hhsize")

acs = subset(acs, select = -c(urban_code))



# Get CPS data & The FIPS codes for each county
county_codes = read.csv("Ryan_Data/NCHSURCodes2013.csv")

county_codes = rename(county_codes, "COUNTY" = "ï..FIPS.code")

cps_raw = read.csv("Ryan_Data/cps(raw).csv")

cps_raw$COUNTY = as.numeric(cps_raw$COUNTY)

county_codes$COUNTY = as.numeric(county_codes$COUNTY)

cps = merge(x = cps_raw, y = county_codes, by = "COUNTY" , all.x = TRUE)

cps = rename(cps, "urban_code" = "X2013.code")

# Can't we just get rid of this then? It seems to confine cps to only the state with the
# STATEFIP to 19. If we comment this out it'll work for us, right?
#cps <- cps[cps$STATEFIP==19,]#Ryan: again, we want it for all states, not just iowa. 
#Ryan: what is cps? i get an error here
# I think I've fixed it, I had everything out of order, now it goes from creating the cps dataset and combining 
# it with the county_code dataset to cleaning up that dataset so that it can be more easily used. 
cps <- cps[, c("CPSID", "PERNUM", "FSRAWSCRA","FSTOTXPNC", "AGE", "SEX",  "FAMSIZE", "RACE", 
               "HISPAN", "EDUC", "EMPSTAT","MARST", "DIFFHEAR", "DIFFEYE", "DIFFREM", "DIFFPHYS", 
               "DIFFMOB", "DIFFCARE", "HWTFINL", "urban_code")]
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
  aggregate(list(fsecurity=cps$FSRAWSCRA, fexpend=cps$FSTOTXPNC, hhsize=cps$FAMSIZE, urban = cps$urban_code), 
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

# THE NA's are around 15007 out of 49259

table(cps$fexpend > 0)
