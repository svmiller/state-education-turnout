library(car)
library(RCurl)

data <- getURL("https://raw.githubusercontent.com/svmiller/state-education-turnout/master/state-education-turnout.csv")
Data <- read.csv(text = data)
summary(Data)

# The Census has a peculiar understanding of regions. They go as follow:
# 1 = "New England" (CT, ME, NH, RI, VT)
# 2 = "Middle Atlantic" (NJ, NY, PA)
# 3 = "East North Central" (IN, IL, TSUN, Ohio -- The Greatest State in the Union, WI)
# 4 = "West North Central" (IA, KS, MN, MO, NE, ND, SD)
# 5 = "South Atlantic" (DE, FL, GA, MD, NC, SC, VA, WV)
# 6 = "East South Central" (AL, KY, MS, TN)
# 7 = "West South Central" (AR, LA, OK, TX)
# 8 = "Mountain" (AZ, CO, ID, NM, MT, UT, NV, WY)
# 9 = "Pacific" (AK, CA, HI, OR, WA)

# The census has four regions with those subregions.
# 1 and 2 are "Northeast"
# 3 and 4 are "Midwest", designated as "North Central Region" before 1984.
# 5-7 are "South"
# 8-9 are "West"



Data$regioncondensed <- with(Data, recode(region, "1:2=1; 3:4=2; 5:7=3; 8:9=0"))

# And, regress:

M1 <- lm(turnout ~ perhsdiploma + factor(regioncondensed), data=Data)
summary(M1)

# How important is education? Let's standardize that variable by two standard deviations (Gelman 2008) to get a sense of its importance.

Data$z.perhsdiploma <- with(Data, (perhsdiploma - mean(perhsdiploma))/(2*sd(perhsdiploma)))
summary(Data$z.perhsdiploma)

M2 <- lm(turnout ~ z.perhsdiploma + factor(regioncondensed), data=Data)
summary(M2)
