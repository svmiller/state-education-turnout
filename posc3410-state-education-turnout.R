library(car) # This is a very useful R package. You should use it a lot if you're using R.

# We need to read in the data first. It's worth clarifying that this is probably the hardest thing for people who first starting using R to do. It's not very intuitive.
# The getwd() function will tell you your current working directory.
getwd()
# setwd() will change your working directory.
# setwd("/home/steve/Dropbox/teaching/posc3410/state-education-turnout") # While using my handsome Ubuntu Desktop.
setwd("/users/stevenmiller/Dropbox/teaching/posc3410/state-education-turnout") # While using Macbook to show the children.
# I forget what this would look like in Windows. Windows' directory structure is kind of stupid from a Linux user's perspective. Well, really stupid.
# I don' think R understands Windows' backslash. I think you have to use forward slash instead. I forget.
# That said, let's read in the data. R is an object-oriented command-based programming language, derivative of C, but also C++ and Fortran. You assign an input to an object.
Data <- read.csv("state-education-turnout.csv")

# Data # This will just spit out the entire data frame.

# Let's find out which state has the lowest and highest of some values. Name and shame the worst of 'em.
minturnout <- min(Data$turnout) 
maxturnout <- max(Data$turnout)
Data$state[Data$turnout == minturnout]
Data$state[Data$turnout == maxturnout]

minperhsdip <- min(Data$perhsdiploma) 
maxperhsdip <- max(Data$perhsdiploma)
Data$state[Data$perhsdiploma == minperhsdip]
Data$state[Data$perhsdiploma == maxperhsdip]

minpercoldip <- min(Data$percollegediploma) 
maxpercoldip <- max(Data$percollegediploma)
Data$state[Data$percollegediploma == minpercoldip]
Data$state[Data$percollegediploma == maxpercoldip]

# Let's find South Carolina because you're probably curious.
subset(Data, Data$state == "Sakerlina")

# Let's say we're interested in state voter turnout among the VEP in 2012 as a function of percent of the school holding a high school diploma (easiest data I could find from 2009).
# Let's correlate the two first.

with(Data, cor(perhsdiploma, turnout))
with(Data, cor(turnout, perhsdiploma))

# The more you use R, the more you'll learn to love R's with() function. Better to use with() than attach(), I think.
# Notice we flipped the two and got the same Pearson's r. Correlation is symmetrical.
# Also notice we got a Pearson's r of .505, basically identical to what Pollock gave.

# Pollock gave a scatterplot. Let's do that too.

with(Data, plot(perhsdiploma, turnout))

# Our scatterplot suggests a positive relationship, just like the Pearson's R.
# It does suggest some noise near the right end of the perhsdiploma variable.
# For example, that's Hawaii you're seeing at the very bottom right.
# North Dakota, Utah, and Wyoming are also pulling that correlation coefficient down.
# You will want to be mindful of these outliers (especially in small samples) when you start doing serious analysis yourself.
# For the meantime, let's gloss over it.

# What can we say about the increase in voter turnout as a function of a state's level education? Let's regress y on x.

M1 <- lm(turnout ~ perhsdiploma, data=Data)
summary(M1)

# Notice the regression coefficient is .9687 with a standard error of .2389.
# The regression coefficient produces a t-statistic more than 4 (well: 4.054) standard errors away from zero.
# This is rather similar to what the author found in his illustration.
# Our y-intercept is -24.2269. That is, a state in which no one graduated from high school will have a turnout of -24.22%. That's ridiculous.
# Center your variables people. Here's how.

Data$c.perhsdiploma <- with(Data, perhsdiploma - mean(perhsdiploma))
mean(Data$c.perhsdiploma)

# Let's do the same regression, but now with our centered perhsdiploma variable.

M2 <- lm(turnout ~ c.perhsdiploma, data=Data)
summary(M2)

# Notice the regression coefficient didn't change at all.
# That said, the y-intercept is much more meaningful.
# Basically, our estimated turnout rate for the state with the average hsdiploma value is 59.934.
# If it helps you, Missouri has the closest value to the mean (c.perhsdiploma = -.082). It's turnout was 62.5, though.

# Let's produce our handsome regression results over a scatterplot like Pollock does.

with(Data, plot(perhsdiploma, turnout))
abline(M1)

# Looks about right. Looks very handsome too if I do say so myself.

# Let's get a bit more creative.
# For example, I think HS attainment is a rather low bar. How about college?
# Let's correlate them first.

with(Data, cor(percollegediploma, turnout))

# Pearson's r of .430, so the relationship is not as strong as before.
# Let's plot them too.

with(Data, plot(percollegediploma, turnout))

# That's Hawaii again that you're seeing in the bottom right. They probably have other things to do on Election Day.
# Must be nice.
# Okay, let's regress the two.

# But first, let's center that variable like we did with the HS variable. Center your variables, people.

Data$c.percollegediploma <- with(Data, percollegediploma - mean(percollegediploma))

# Go go gadget linear regression.

M3 <- lm(turnout ~ c.percollegediploma, data=Data)
summary(M3)

# Statistically significant as well, though not as strong as the HS diploma variable.
# Let's plot the two.

with(Data, plot(c.percollegediploma, turnout))
abline(M3)

# Curious, what would this look like if Hawaii wasn't ruining everything?
M4 <- lm(turnout ~ c.percollegediploma, data=subset(Data, Data$state != "Hawaii"))
summary(M4)

with(Data, plot(c.percollegediploma, turnout))
abline(M3)
abline(M4)

# Not much different in the broad scheme of things. Whatever.

# What about regional effects? The Census has a peculiar understanding of regions. They go as follow:
# 1 = "New England" (CT, ME, NH, RI, VT)
# 2 = "Middle Atlantic" (NJ, NY, PA)
# 3 = "East North Central" (IN, IL, TSUN, Ohio -- The Greatest State in the Union, WI)
# 4 = "West North Central" (IA, KS, MN, MO, NE, ND, SD)
# 5 = "South Atlantic" (DE, FL, GA, MD, NC, SC, VA, WV)
# 6 = "East South Central" (AL, KY, MS, TN)
# 7 = "West South Central" (AR, LA, OK, TX)
# 8 = "Mountain" (AZ, CO, ID, NM, MT, UT, NV, WY)
# 9 = "Pacific" (AK, CA, HI, OR, WA)
# I think I got all those state abbreviations right.

# The census has four regions with those subregions.
# 1 and 2 are "Northeast"
# 3 and 4 are "Midwest", designated as "North Central Region" before 1984.
# 5-7 are "South"
# 8-9 are "West"

# Let's say we're interested in just the South as a dummy variable for the models using the HS diploma variable.
# We'll need to create a dummy variable for the South. So, let's go ahead and do that.
# You'll need John Fox' car package for this, which we loaded earlier.

Data$south <- with(Data, recode(region, "1:4=0; 5:7=1; 8:9=0"))

# Let's do multiple regression.

M5 <- lm(turnout ~ c.perhsdiploma + south, data=Data)
summary(M5)

# Interestingly, the South variable is not statistically significant. I suspect this is because of considerable heterogeneity as it pertains to "South".
# It may also be a result of unique attributes about the 2012 Presidential election that would've otherwise brought out the Southern vote.
# Are there any regional effects? Let's make our baseline group the West here. There's considerable variety in that group.
# In short, let's condense  the regions and make the West the baseline.

Data$regioncondensed <- with(Data, recode(region, "1:2=1; 3:4=2; 5:7=3; 8:9=0"))

# And, regress:

M6 <- lm(turnout ~ c.perhsdiploma + factor(regioncondensed), data=Data)
summary(M6)

# Not much emerges. We do see that the Midwest states seem to have higher turnout than the West, albeit at a level of statistical significance we accept in smaller samples like this.
# In this regression, the y-intercept tell us our estimate of Y in the state in the West at the global mean in the perhsdiploma variable.
