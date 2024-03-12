

library(rstan)
library(ggplot2)
library(patchwork)

model <- "

// Rock-Paper-Scissors Multinomial Processing Tree
data {
  int<lower=1> N;                          // Number of games
  int<lower=1, upper=3> choices[N];        // Player's choices: 1 = rock, 2 = paper, 3 = scissors
  int<lower=0, upper=3> prev_choices[N];   // Player's previous choices: 0 = none, 1 = rock, 2 = paper, 3 = scissors
  int<lower=0, upper=3> opp_choices[N];    // Opponent's previous choices: 0 = none, 1 = rock, 2 = paper, 3 = scissors
}
parameters {
  real<lower=0, upper=1> c;        // Probability of guessing
  real<lower=0, upper=1> r;        // Probability of repeating own choice when not guessing
  real<lower=0, upper=1> u;        // Probability of copying opponent's choice when not repeating own
}
model {
  // Prior distributions for our parameters
  c ~ beta(2, 2);
  r ~ beta(2, 2);
  u ~ beta(2, 2);
  
  for (i in 1:N) {
    // Probabilities for each choice based on the MPT model
    real p_rock = c / 3 + (1 - c) * ((r * (prev_choices[i] == 1)) + ((1 - r) * (u * (opp_choices[i] == 1))));
    real p_paper = c / 3 + (1 - c) * ((r * (prev_choices[i] == 2)) + ((1 - r) * (u * (opp_choices[i] == 2))));
    real p_scissors = c / 3 + (1 - c) * ((r * (prev_choices[i] == 3)) + ((1 - r) * (u * (opp_choices[i] == 3))));
    
    // Normalizing the probabilities
    real sum_p = p_rock + p_paper + p_scissors;
    vector[3] choice_probs = to_vector([p_rock, p_paper, p_scissors]) / sum_p;

    // Likelihood of the observed choices
    choices[i] ~ categorical(choice_probs);
  }
}

"

stan_model <- stan_model(model_code = model)


dat <- read.csv("/Users/henrikgodmann/Desktop/workspace/GitHub/MPTforRPS/data/simulated/simulated_data_time_invariant.csv")



stan_data <- list(N = nrow(dat), 
                  choices = dat$PlayerChoice,
                  prev_choices = dat$PlayerPrevChoice,
                  opp_choices = dat$OpponentPrevChoice
                  )


# Fit model
fit <- sampling(stan_model, data = stan_data, chains = 2, iter = 2000, cores = 2, warmup = 500)

# save.image("/Users/henrikgodmann/Desktop/workspace/GitHub/MPTforRPS/save_image/time_invariant.Rdata")

load("/Users/henrikgodmann/Desktop/workspace/GitHub/MPTforRPS/save_image/time_invariant.Rdata")

# posterior samples
posterior_samples <- extract(fit)


posterior_df <- data.frame(
  c = posterior_samples$c,
  r = posterior_samples$r,
  u = posterior_samples$u
)

# Define Beta distribution function for Beta(2,2)
beta_prior <- function(x) dbeta(x, shape1 = 2, shape2 = 2)


# Plot for c with prior
plot_c <- ggplot(posterior_df, aes(x = c)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  stat_function(fun = beta_prior, color = "black", size = 1) +
  labs(title = "Posterior and Prior Distribution of c",
       x = "c",
       y = "Density") +
  theme_minimal() +
  xlim(0, 1) +
  ylim(0, 7)

# Plot for r with prior
plot_r <- ggplot(posterior_df, aes(x = r)) +
  geom_density(fill = "lightgreen", alpha = 0.5) +
  stat_function(fun = beta_prior, color = "black", size = 1) +
  labs(title = "Posterior and Prior Distribution of r",
       x = "r",
       y = "Density") +
  theme_minimal() +
  xlim(0, 1) +
  ylim(0, 7)

# Plot for u with prior
plot_u <- ggplot(posterior_df, aes(x = u)) +
  geom_density(fill = "salmon", alpha = 0.5) +
  stat_function(fun = beta_prior, color = "black", size = 1) +
  labs(title = "Posterior and Prior Distribution of u",
       x = "u",
       y = "Density") +
  theme_minimal() +
  xlim(0, 1) +
  ylim(0, 7)



plot_c + plot_r + plot_u + plot_layout(ncol = 3)