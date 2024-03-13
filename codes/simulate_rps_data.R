
############### Simulate time variant data ###############


set.seed(42)

# Updated probabilities
r <- 0.3  # Probability of repeating own choice when not guessing
c <- 0.7  # Probability of copying opponent's choice when not repeating own

N <- 1000  # Number of games to simulate

# Linear decrease for g
start_g <- 0.2
end_g <- 0.8
g <- seq(from = start_g, to = end_g, length.out = N)

# Initialize vectors to store choices
choices <- integer(N)
prev_choices <- rep(NA, N)  # Use NA for the first previous choice to indicate no previous game
opp_choices <- rep(NA, N)  # Use NA for the first opponent's previous choice to indicate no previous game

# Function to randomly select a choice
random_choice <- function() sample(1:3, size = 1, replace = TRUE)

# Simulate the games
for (i in 1:N) {
  if (is.na(prev_choices[i]) || runif(1) < g[i]) {
    # First game or guessing randomly based on the current probability g[i]
    choices[i] <- random_choice()
  } else {
    # Not the first game and not guessing randomly
    if (runif(1) < r) {
      # Repeating own previous choice
      choices[i] <- prev_choices[i]
    } else if (runif(1) < c) {
      # Copying opponent's previous choice
      choices[i] <- opp_choices[i]
    } else {
      # Neither repeating own choice nor copying opponent's
      choices[i] <- random_choice()
    }
  }
  
  # Record the previous choices for the next game
  if (i < N) {
    prev_choices[i + 1] <- choices[i]
    opp_choices[i + 1] <- random_choice()
  }
}

# Convert NAs to 0 for compatibility with Stan model
prev_choices[is.na(prev_choices)] <- 0
opp_choices[is.na(opp_choices)] <- 0

# Prepare the simulated data frame
simulated_data <- data.frame(Game = 1:N, PlayerChoice = choices, 
                             PlayerPrevChoice = prev_choices, OpponentPrevChoice = opp_choices)


# Save the simulated data
write.csv(simulated_data, "/Users/henrikgodmann/Desktop/workspace/GitHub/MPTforRPS/data/simulated/simulated_data_time_variant_linear_g.csv", row.names = FALSE)

rm(list=ls())



############### Simulate time invariant data ###############


set.seed(42)

# Updated probabilities
r <- 0.5  # Probability of repeating own choice when not guessing
c <- 0.2  # Probability of copying opponent's choice when not repeating own
g <- 0.7

N <- 100  # Number of games to simulate



# Initialize vectors to store choices
choices <- integer(N)
prev_choices <- rep(NA, N)  # Use NA for the first previous choice to indicate no previous game
opp_choices <- rep(NA, N)  # Use NA for the first opponent's previous choice to indicate no previous game

# Function to randomly select a choice
random_choice <- function() sample(1:3, size = 1, replace = TRUE)

# Simulate the games
for (i in 1:N) {
  if (is.na(prev_choices[i]) || runif(1) < g) {
    # First game or guessing randomly based on the current probability g[i]
    choices[i] <- random_choice()
  } else {
    # Not the first game and not guessing randomly
    if (runif(1) < r) {
      # Repeating own previous choice
      choices[i] <- prev_choices[i]
    } else if (runif(1) < c) {
      # Copying opponent's previous choice
      choices[i] <- opp_choices[i]
    } else {
      # Neither repeating own choice nor copying opponent's
      choices[i] <- random_choice()
    }
  }
  
  # Record the previous choices for the next game
  if (i < N) {
    prev_choices[i + 1] <- choices[i]
    opp_choices[i + 1] <- random_choice()
  }
}

# Convert NAs to 0 for compatibility with Stan model
prev_choices[is.na(prev_choices)] <- 0
opp_choices[is.na(opp_choices)] <- 0

# Prepare the simulated data frame
simulated_data <- data.frame(Game = 1:N, PlayerChoice = choices, 
                             PlayerPrevChoice = prev_choices, OpponentPrevChoice = opp_choices)

# Save the simulated data
write.csv(simulated_data, "/Users/henrikgodmann/Desktop/workspace/GitHub/MPTforRPS/data/simulated/simulated_data_time_invariant.csv", row.names = FALSE)

rm(list=ls())
