# load standard packages
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

# load data using readr
efficacy <- read_csv("~/friendly-epistaxis/data/efficacy.csv")
randomization <- read_csv("~/friendly-epistaxis/data/randomization.csv")
subject <- read_csv("~/friendly-epistaxis/data/subject.csv")

# take a look at the data tibbles
efficacy
randomization
subject

# all three tables are on subject level
# all three tables have 444 observations
# there are NAs (e.g., eye.colour for subject 564)

# we measure the COUNT of nosebleeds over a specific DURATIOn
# the duration however varies from subject to subject
# we might be able to model it via Poisson with offset
# there is a different name for that...

?glm
# maybe it's offset after all; but I thought there is a difference between
# offset and exposure or something like that; research later

# we have categorical/ordinal data (country, eye.colour, tissue.use)

summary(efficacy$nosebleeds)
# at least half of the subjects didn't have a single nosebleed
# -> overdispersed; zero inflated poisson ??
# don't take for granted that a count model is going to be appropriate

# ah, if the treatment is good then of course the number of recurrent
# nosebleeds should be small. 

sort(table(subject$country))
sort(table(subject$eye.colour)) # that's a lot of blue...
sort(table(subject$tissue.use)) 
# expected also low; did they focus already on medium to high?
# is there no low generally (so that this could be binary instead of ordinal?)
summary(subject$previous.year)
# the minimum is 2 and the median is 2, so it seems as if the 
# treatment might help going from the efficacy$nosebleeds.
# Also, in both we have a maximum of 5.

summary(subject$mucus.viscosity)
boxplot(subject$mucus.viscosity)
# bounded at zero, positive skew (mean > median)

# where do we have NAs?
lapply(subject, anyNA) # -> eye.colour, mucus.viscosity (1)
sum(is.na(subject$eye.colour)) # 60!

################################################################

# next steps: explore treatment effects 
# (join tables, group by treatment, check randomization)