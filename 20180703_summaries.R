# load standard packages
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

# load data using readr
efficacy <- read_csv("~/friendly-epistaxis/data/efficacy.csv")
randomization <- read_csv("~/friendly-epistaxis/data/randomization.csv")
subject <- read_csv("~/friendly-epistaxis/data/subject.csv")

all <- subject %>%
  left_join(randomization) %>%
  left_join(efficacy)

all %>%
  ggplot(aes(x = log(mucus.viscosity), y = nosebleeds/duration,
             group = arm, color = arm, fill = arm)) +
  geom_point() +
  geom_smooth()

all %>%
  ggplot(aes(x = (mucus.viscosity), y = previous.year,
             group = arm, color = arm, fill = arm)) +
  geom_point() +
  geom_smooth()
# the previous.year rate appears constant across mucus.viscosity

# does mucus.viscosity influence the tissue.use (severity of nosebleed)?
all %>%
  mutate(tissue.use.bin = ifelse(tissue.use == "MEDIUM", 0, 1)) %>%
  ggplot(aes(x = mucus.viscosity, y = tissue.use.bin)) +
  geom_point() +
  geom_smooth()

# use logistic regression fit instead
# https://ggplot2.tidyverse.org/reference/geom_smooth.html
binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}

all %>%
  mutate(tissue.use.bin = ifelse(tissue.use == "MEDIUM", 0, 1)) %>%
  ggplot(aes(x = mucus.viscosity, y = tissue.use.bin)) +
  binomial_smooth() +
  geom_jitter(height = 0.05)

# if we assume that mucus.viscosity has the actual impact on the 
# effect of superdupripine, then tissue.use would be confounding...?

# does tissue.use correlate positively with nosebleeds?
all %>%
  mutate(tissue.use.bin = ifelse(tissue.use == "MEDIUM", 0, 1)) %>%
  ggplot(aes(x = previous.year, y = tissue.use.bin)) +
  binomial_smooth() +
  geom_jitter(height = 0.05)

all %>%
  mutate(tissue.use.bin = ifelse(tissue.use == "MEDIUM", 0, 1)) %>%
  ggplot(aes(x = previous.year, y = tissue.use.bin)) +
  binomial_smooth() +
  geom_jitter(height = 0.05)

# go back and use Poisson as smoother
poisson_smooth <- function(...) {
  geom_smooth(method = "glm", 
              method.args = list(family = poisson(link = "log")), ...)
}

all %>%
  ggplot(aes(x = log(1+mucus.viscosity), y = previous.year,
             group = arm, color = arm, fill = arm)) +
  geom_point() +
  poisson_smooth()

all %>%
  ggplot(aes(x = log(1+mucus.viscosity), y = previous.year,
             group = tissue.use, color = tissue.use, fill = tissue.use)) +
  geom_point() +
  poisson_smooth()
# tissue.use does not appear to have a clear impact on the relation between
# mucus.viscosity and previous.year

all %>%
  ggplot(aes(x = (mucus.viscosity), y = previous.year)) +
  geom_point() +
  poisson_smooth()

