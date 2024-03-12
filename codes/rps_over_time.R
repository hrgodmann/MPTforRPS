

library(rstan)
library(ggplot2)


model <- "

// Rock-Paper-Scissors Multinomial Processing Tree over time
data {
  int<lower=1> N;                           // Number of games
  int<lower=1, upper=3> choices[N];         // Player's choices: 1 = rock, 2 = paper, 3 = scissors
  int<lower=0, upper=3> prev_choices[N];    // Player's previous choices: 0 = none, 1 = rock, 2 = paper, 3 = scissors
  int<lower=0, upper=3> opp_choices[N];     // Opponent's previous choices: 0 = none, 1 = rock, 2 = paper, 3 = scissors
}
parameters {
  vector<lower=0, upper=1>[N] c;           // Probability of guessing over time
  vector<lower=0, upper=1>[N] r;           // Probability of repeating own choice over time
  vector<lower=0, upper=1>[N] u;           // Probability of copying opponent's choice over time
  real<lower=0> sigma_c;                   // Standard deviation of random walk for c
  real<lower=0> sigma_r;                   // Standard deviation of random walk for r
  real<lower=0> sigma_u;                   // Standard deviation of random walk for u
}
model {
  // Priors for the standard deviations of the random walks
  sigma_c ~ normal(0, 0.1);
  sigma_r ~ normal(0, 0.1);
  sigma_u ~ normal(0, 0.1);
  
  // Priors for initial values of parameters
  c[1] ~ beta(2, 2);
  r[1] ~ beta(2, 2);
  u[1] ~ beta(2, 2);

  // Random walk
  for (t in 2:N) {
    c[t] ~ normal(c[t-1], sigma_c);
    r[t] ~ normal(r[t-1], sigma_r);
    u[t] ~ normal(u[t-1], sigma_u);
  }
  
  for (i in 1:N) {
    // Probabilities for each choice based on the MPT model at time i
    real p_rock = c[i] / 3 + (1 - c[i]) * ((r[i] * (prev_choices[i] == 1)) + ((1 - r[i]) * (u[i] * (opp_choices[i] == 1))));
    real p_paper = c[i] / 3 + (1 - c[i]) * ((r[i] * (prev_choices[i] == 2)) + ((1 - r[i]) * (u[i] * (opp_choices[i] == 2))));
    real p_scissors = c[i] / 3 + (1 - c[i]) * ((r[i] * (prev_choices[i] == 3)) + ((1 - r[i]) * (u[i] * (opp_choices[i] == 3))));
    
    // Normalizing the probabilities
    real sum_p = p_rock + p_paper + p_scissors;
    vector[3] choice_probs = to_vector([p_rock, p_paper, p_scissors]) / sum_p;

    // Likelihood of the observed choices
    choices[i] ~ categorical(choice_probs);
  }
}


"

stan_model <- stan_model(model_code = model)


dat <- read.csv("/Users/henrikgodmann/Desktop/workspace/GitHub/MPTforRPS/data/simulated/simulated_data_time_variant.csv")


stan_data <- list(N = nrow(dat), 
                  choices = dat$PlayerChoice,
                  prev_choices = dat$PlayerPrevChoice,
                  opp_choices = dat$OpponentPrevChoice
)




# Fit model
fit <- sampling(stan_model, data = stan_data, chains = 2, iter = 500, cores = 2, warmup = 100, control = list(adapt_delta = 0.99, max_treedepth = 17))


save.image("/Users/henrikgodmann/Desktop/workspace/GitHub/MPTforRPS/save_image/time_variant.Rdata")

load("/Users/henrikgodmann/Desktop/workspace/GitHub/MPTforRPS/save_image/time_variant.Rdata")



# Extract the posterior samples


posterior_samples_c <- extract(fit)$c
posterior_samples_r <- extract(fit)$r
posterior_samples_u <- extract(fit)$u

# Calculate the mean of the posterior samples at each time point
mean_c_over_time <- apply(posterior_samples_c, 2, mean)
mean_r_over_time <- apply(posterior_samples_r, 2, mean)
mean_u_over_time <- apply(posterior_samples_u, 2, mean)

# Plotting
time_points <- 1:nrow(dat)


# Calculate the 2.5th and 97.5th percentiles for c at each time point
lower_ci_c <- apply(posterior_samples_c, 2, quantile, probs = 0.2)
upper_ci_c <- apply(posterior_samples_c, 2, quantile, probs = 0.8)

lower_ci_r <- apply(posterior_samples_r, 2, quantile, probs = 0.2)
upper_ci_r <- apply(posterior_samples_r, 2, quantile, probs = 0.8)

lower_ci_u <- apply(posterior_samples_u, 2, quantile, probs = 0.2)
upper_ci_u <- apply(posterior_samples_u, 2, quantile, probs = 0.8)




#################

plot_data_simplified <- data.frame(
  Time = rep(1:nrow(dat), 3),
  Estimate = c(mean_c_over_time, mean_r_over_time, mean_u_over_time),
  Lower = c(lower_ci_c, lower_ci_r, lower_ci_u),
  Upper = c(upper_ci_c, upper_ci_r, upper_ci_u),
  Parameter = factor(rep(c("c", "r", "u"), each = nrow(dat)))
)


source("/Users/henrikgodmann/Desktop/workspace/GitHub/hrgodmann/Rcolors.R")

setwd("/Users/henrikgodmann/Desktop/mpt")
# Plot
ggplot(plot_data_simplified, aes(x = Time, y = Estimate, colour = Parameter)) +
  geom_line() +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Parameter), alpha = 0.2) +
  scale_colour_manual(values = c("c" = "blue2", "r" = "red2", "u" = "green2")) +
  scale_fill_manual(values = c("c" = "blue2", "r" = "red2", "u" = "green2")) +
  labs(title = "Parameter Estimates over Time with Credible Intervals",
       x = "Time",
       y = "Estimate",
       colour = "Parameter",
       fill = "Parameter") +
  theme_minimal()

ggsave("parameters_over_time.png", width = 10, height = 6, dpi = 300, bg = back_ground_white)


