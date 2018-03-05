##############################
# Data Analysis Programm  ####
# Created by NJ and UT    ####
##############################

#install.packages("plyr")
install.packages("openxlsx")
library(plyr)
library(openxlsx)

UserName = "nj995"

################################
# Demographic Statistics
###############################

setwd(paste("/Users/", UserName, "/Dropbox/DataPiche/USElections2016/Input/statedemographics", sep = ""))

temp <- list.files(pattern = "*.xlsx",full.names = FALSE)

#State names
names <- c("",length(temp))
for (i in 1:length(temp)) 
  names[i] <- substr(temp[i], 1,nchar(temp[i])-5)

#Variable names
namevariables <- read.xlsx(paste(names[1], ".xlsx", sep = ""), 1, 
                           rowIndex = 3:29, header = F, colIndex = c(1))
var <- c(t(namevariables))

#Let's start with Alabama
data1 <- read.xlsx(paste(names[1], ".xlsx", sep = ""), 1, rowIndex = 3:29, header = F, colIndex = c(4))
data2 <- as.data.frame(t(data1))
data2$State <- names[1]
demographics <- data2
rownames(demographics)[1] <- names[1]

#Let's finish
for (i in 2:length(temp)){
  data1 <- read.xlsx(paste(names[i], ".xlsx", sep = ""), 1, rowIndex = 3:29, header = F, colIndex = c(4))
  data2 <- as.data.frame(t(data1))
  data2$State <- names[i]
  demographics<- rbind(demographics,data2)
  rownames(demographics)[i] <- names[i]
}

#Right variable names
for (j in 1:27){
  colnames(demographics)[j] <- var[j]
}

#Delete irrelevant varibles
drops <- c("Citizen, Voting-Age Population", 
           "Sex", 
           "Race*", 
           "Hispanic Origin**", 
           "Citizens 25 years and older", 
           "Citizens for whom Poverty Status is determined", 
           "Households***", 
           "White alone, Not Hispanic or Latino",
           "Some Other Race alone",
           "Two or More Races")
demographics <- demographics[, !(names(demographics) %in% drops)]

demographics <- rename(demographics, c("Total of citizens 18 years and older" = "TotalVotingPopulation",
                                       "18 to 29 years" = "Age.18_29",
                                       "30 to 44 years" = "Age.30_44",
                                       "45 to 64 years" = "Age.45_64",
                                       "65 years and over" = "Age.Over65",
                                       "White alone" = "White",
                                       "Black or African American alone" = "AfricanAmerican",
                                       "American Indian and Alaska Native alone" = "AmericanIndian",
                                       "Asian alone" = "Asian",
                                       "Native Hawaiian and Other Pacific Islander alone" = "Pacific",
                                       "Hispanic or Latino" = "Latino",
                                       "Not Hispanic or Latino" = "NotLatino",
                                       "Bachelor's Degree or Higher" = "CollegeEducation",
                                       "Below Poverty Level (Poverty Rate)" = "PovertyRate",
                                       "Households with income $100,000 or more" = "HouseholdIncomeOver100k"
                                       ))

############################################
# Obesity, Unemployment, Gun Ownership
############################################

setwd(paste("/Users/", UserName, "/Dropbox/DataPiche/USElections2016/Input", sep = ""))

# Obesity data
obesity <- read.xlsx("Obesity Statistics.xlsx", 1)
obesity <- rename(obesity, c("Adult.Obesity.Rate.2015" = "ObesityRate"))

# Gun Ownership data
guns <- read.xlsx("Gun Ownership Statistics.xlsx", 1)
guns <- rename(guns, c("Gun.Ownership" = "GunOwnership"))

# Unemployment data
unemployment <- read.xlsx("Unemployment Statistics.xlsx", 1)
unemployment <- rename(unemployment, c("Unemployment.rate"="UnemploymentRate"))

###########################
#Polls and results
###########################

# 2016 Election Results
results.2016 <- read.xlsx("2016 Election Results.xlsx", 1)
results.2016$Clinton.2016 <- results.2016$Clinton.pct
results.2016$Trump.2016 <- results.2016$Trump.pct
results.2016$Others.2016 <- results.2016$Johnson.pct + results.2016$Other.pct
results.2016$TotalVote.2016 <- results.2016$Total.Vote
results.2016 <- results.2016[, c("State", "TotalVote.2016", 
                                 "Clinton.2016", "Trump.2016", "Others.2016")]

results.2012 <- read.xlsx("2012 Election Results.xlsx", 1)
results.2012$Obama.2012 <- results.2012$Barack.Obama
results.2012$Romney.2012 <- results.2012$Mitt.Romney
results.2012$Others.2012 <- results.2012$Gary.Johnson + results.2012$Jill.Stein + results.2012$Other
results.2012$TotalVote.2012 <- results.2012$Total
results.2012 <- results.2012[, c("State", "TotalVote.2012", 
                                 "Obama.2012", "Romney.2012", "Others.2012")]


# Polls
polls <- read.xlsx("2016 Election Polls.xlsx", 1)
swing <- c("Arizona", 
           "Colorado", 
           "Florida", 
           "Georgia", 
           "Iowa", 
           "Michigan", 
           "Minnesota",
           "Nevada", 
           "NewHampshire",
           "NewMexico",
           "NorthCarolina", 
           "Ohio", 
           "Oregon",
           "Pennsylvania", 
           "Virginia", 
           "Wisconsin")
polls$SwingState = polls$State %in% swing
polls <- rename(polls, c("Clinton.Poll" = "Clinton.Poll.2016", 
                         "Trump.Poll" = "Trump.Poll.2016",
                         "Johnson.Poll" = "Johnson.Poll.2016"))


##########################################
# Combine all data, Export
##########################################

data <- Reduce(function(x, y) merge(x, y, by = "State", all=TRUE), 
               list(demographics, 
                    obesity, 
                    guns, 
                    unemployment, 
                    results.2012, 
                    results.2016,
                    polls))
data$Trump.PollError.2016 <- data$Trump.2016 - data$Trump.Poll.2016
data$StateWinner.2016 <- with(data, ifelse(Clinton.2016 > Trump.2016, "C", "T"))
data$Abstention.2016 <- 1 - (data$TotalVote.2016 / data$TotalVotingPopulation)

# Export
setwd(paste("/Users/", UserName, "/Dropbox/DataAnalysisProject/Output", sep = ""))
write.csv(data, file = "data.csv")

