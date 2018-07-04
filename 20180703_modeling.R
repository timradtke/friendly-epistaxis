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
  left_join(efficacy) %>%
  as.data.frame

################################################################

# load additionally the rethinking package
library(rethinking)
options(mc.cores = parallel::detectCores())

# prepare for stan
all_stan <- all %>%
  mutate(arm = ifelse(arm == "PLACEBO", 0, 1)) %>%
  select(subject, arm, mucus.viscosity, nosebleeds, duration) %>%
  rename(mucus_viscosity = mucus.viscosity)
# get the offset for the model
all_stan$log_duration <- log(all$duration)

# already remove the observation without mucus.viscosity observation
summary(all_stan$mucus_viscosity)
all_stan <- na.omit(all_stan)

# fit a model including only the offset and treatment
m_treatment <- map2stan(
  alist(
    nosebleeds ~ dpois(lambda),
    log(lambda) <- log_duration + intersection + b*arm,
    intersection ~ dnorm(0,100),
    b ~ dnorm(0,1)
  ),
  data = all_stan, iter = 10000, chains = 2
)

precis(m_treatment)
pairs(m_treatment)
save(m_treatment, file = "~/friendly-epistaxis/model/20180704_m_treatment.rda")

################################################################

# fit a model now including the mucus.viscosity
# take the log of it, because we don't have reason to expect exponential relation

all_stan$log_mucus_viscosity <- log(all_stan$mucus_viscosity+1)

m_treatment_mucus <- map2stan(
  alist(
    nosebleeds ~ dpois(lambda),
    log(lambda) <- log_duration + intersection +
      b_treatment*arm + b_mucus*log_mucus_viscosity + 
      b_interaction*arm*log_mucus_viscosity,
    intersection ~ dnorm(0,100),
    c(b_mucus, b_interaction, b_treatment) ~ dnorm(0,1)
  ),
  data = all_stan, iter = 10000, chains = 2
)

precis(m_treatment_mucus)
pairs(m_treatment_mucus)
plot(precis(m_treatment_mucus))
save(m_treatment_mucus, 
     file = "~/friendly-epistaxis/model/20180704_m_treatment_mucus.rda")

########################################################################

# first, show how the estimated confidence varies based on
# mucus viscosity

extracted_samples <- extract.samples(m_treatment_mucus)
save(extracted_samples, 
     file = "~/friendly-epistaxis/model/20180704_extracted_samples.rda")

effect_estimate <- data.frame(
  mucus_viscosity = rep(seq(0.01,8,by = 0.01), times = 2),
  arm = rep(c(0,1), each = length(seq(0.01,8,by = 0.01))),
  mean = NA,
  lb = NA,
  ub = NA,
  stringsAsFactors = FALSE
)

for(i in 1:dim(effect_estimate)[1]) {
  samples_temp <- exp(extracted_samples$intersection +
        extracted_samples$b_mucus * log(effect_estimate$mucus_viscosity[i]+1) +
        extracted_samples$b_interaction * 
          log(effect_estimate$mucus_viscosity[i]+1) * 
          effect_estimate$arm[i] +
        extracted_samples$b_treatment * effect_estimate$arm[i] +
        log(365))
  
  effect_estimate[i,3:5] <- c(
    mean(samples_temp),
    quantile(samples_temp, 0.07),
    quantile(samples_temp, 0.93)
  )
}

effect_estimate <- effect_estimate %>%
  mutate(Group = ifelse(arm == 1, "ACTIVE", "PLACEBO"))
all_stan_plot <- all_stan %>%
  mutate(Group = ifelse(arm == 1, "ACTIVE", "PLACEBO"))

save(effect_estimate, all_stan_plot,
     file = "~/friendly-epistaxis/model/20180704_m_treatment_mucus_plot_objects.rda")

ggplot() +
  geom_jitter(aes(y = nosebleeds/duration*365, x = mucus_viscosity,
                  color = Group), 
              data = all_stan_plot, height = 0.1, alpha = 0.5) +
  geom_ribbon(aes(x = mucus_viscosity,
                  ymin = lb, ymax = ub, 
                  fill = Group, group = Group), 
              data = effect_estimate,
              alpha = 0.3) +
  geom_line(aes(x = mucus_viscosity, y = mean, color = Group, group = Group),
            data = effect_estimate) +
  coord_cartesian(ylim = c(-0.1,2.1)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Mucus Viscosity", y = "Expected Yearly Nosebleed Rate",
       subtitle = "Ribbon shows 7%-93% credible interval.",
       title = "Estimated Effect of Superdupripine")



summary(
exp(extracted_samples$intersection +
  extracted_samples$b_mucus * log(6.5) +
  extracted_samples$b_interaction * log(6.5) * 1 +
  extracted_samples$b_treatment * 1 +
    log(365))
)

summary(
  exp(extracted_samples$intersection +
        extracted_samples$b_mucus * log(6.5+1) +
        extracted_samples$b_interaction * log(6.5+1) * 0 +
        extracted_samples$b_treatment * 0 +
        log(365))
)

summary(
  exp(extracted_samples$intersection +
        extracted_samples$b_mucus * log(1+1) +
        extracted_samples$b_interaction * log(1+1) * 1 +
        extracted_samples$b_treatment * 1 +
        log(365))
)

summary(
  exp(extracted_samples$intersection +
        extracted_samples$b_mucus * log(1+1) +
        extracted_samples$b_interaction * log(1+1) * 0 +
        extracted_samples$b_treatment * 0 +
        log(365))
)

########################################################################

# plot the effect
log_mucus_range <- range(all_stan$log_mucus_viscosity)
log_mucus_seq <- seq(0.001, log_mucus_range[2]-0.001, by = 0.01)

all_to_plot <- data.frame(
  log_mucus_viscosity = rep(log_mucus_seq, times = 2),
  arm = rep(c(0,1), each = length(log_mucus_seq)),
  log_duration = log(365)
)

set.seed(4294)
sims_treatment_mucus <- rethinking::sim(m_treatment_mucus, all_to_plot, n = 10000)

all_to_plot$mean <- apply(sims_treatment_mucus, 2, mean)
all_to_plot$q05 <- apply(sims_treatment_mucus, 2, quantile, 0.05)
all_to_plot$q95 <- apply(sims_treatment_mucus, 2, quantile, 0.95)

all_to_plot %>%
  mutate(mucus_viscosity = exp(log_mucus_viscosity),
         arm = ifelse(arm == 0, "PLACEBO", "ACTIVE")) %>%
  ggplot(aes(x = mucus_viscosity,
             y = mean)) +
  geom_ribbon(aes(ymin = q05, ymax = q95, 
                  group = arm, fill = arm),
              alpha = 0.3) +
  geom_line(aes(color = arm))

