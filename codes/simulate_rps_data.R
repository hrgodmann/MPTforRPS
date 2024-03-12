
############### Simulate time invariant data ###############


# Set parameters for the simulation
set.seed(42)
c <- 0.5  # Probability of guessing
r <- 0.5  # Probability of repeating own choice when not guessing
u <- 0.5  # Probability of copying opponent's choice when not repeating own

N <- 1000  # Number of games to simulate

# Initialize vectors to store choices
choices <- integer(N)
prev_choices <- rep(NA, N)  # Use NA for the first previous choice to indicate no previous game
opp_choices <- rep(NA, N)  # Use NA for the first opponent's previous choice to indicate no previous game

# Function to randomly select a choice
random_choice <- function() sample(1:3, size = 1, replace = TRUE)

# Simulate the games
for (i in 1:N) {
  if (is.na(prev_choices[i]) || runif(1) < c) {
    # First game or guessing randomly
    choices[i] <- random_choice()
  } else {
    # Not the first game and not guessing randomly
    if (runif(1) < r) {
      # Repeating own previous choice
      choices[i] <- prev_choices[i]
    } else if (runif(1) < u) {
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

simulated_data <- data.frame(Game = 1:N, PlayerChoice = choices, 
                             PlayerPrevChoice = prev_choices, OpponentPrevChoice = opp_choices)


write.csv(simulated_data, "/Users/henrikgodmann/Desktop/workspace/GitHub/MPTforRPS/data/simulated/simulated_data_time_invariant.csv", row.names = F)


rm(list=ls())

############### Simulate time variant data ###############


# Set parameters for the simulation
set.seed(42)
c <- c(0.7, 0.3, 0.1)  # Probability of guessing
r <- 0.5  # Probability of repeating own choice when not guessing
u <- 0.5  # Probability of copying opponent's choice when not repeating own

N <- 1000  # Number of games to simulate
N1 <- 200
N2 <- 400
N3 <- 400

# Initialize vectors to store choices
choices <- integer(N)
prev_choices <- rep(NA, N)  # Use NA for the first previous choice to indicate no previous game
opp_choices <- rep(NA, N)  # Use NA for the first opponent's previous choice to indicate no previous game

# Function to randomly select a choice
random_choice <- function() sample(1:3, size = 1, replace = TRUE)

# Simulate the games
for (i in 1:N1) {
  if (is.na(prev_choices[i]) || runif(1) < c[1]) {
    # First game or guessing randomly
    choices[i] <- random_choice()
  } else {
    # Not the first game and not guessing randomly
    if (runif(1) < r) {
      # Repeating own previous choice
      choices[i] <- prev_choices[i]
    } else if (runif(1) < u) {
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

# Correct the ranges for the loops according to the defined segments
for (i in (N1+1):(N1+N2)) {
  if (is.na(prev_choices[i]) || runif(1) < c[2]) {
    choices[i] <- random_choice()
  } else {
    if (runif(1) < r) {
      choices[i] <- prev_choices[i]
    } else if (runif(1) < u) {
      choices[i] <- opp_choices[i]
    } else {
      choices[i] <- random_choice()
    }
  }
  if (i < N) {
    prev_choices[i + 1] <- choices[i]
    opp_choices[i + 1] <- random_choice()
  }
}

# Note the adjustment here for the correct start and end points of the last segment
for (i in (N1+N2+1):N) {
  if (is.na(prev_choices[i]) || runif(1) < c[3]) {
    choices[i] <- random_choice()
  } else {
    if (runif(1) < r) {
      choices[i] <- prev_choices[i]
    } else if (runif(1) < u) {
      choices[i] <- opp_choices[i]
    } else {
      choices[i] <- random_choice()
    }
  }
  if (i < N) {
    prev_choices[i + 1] <- choices[i]
    opp_choices[i + 1] <- random_choice()
  }
}



# Convert NAs to 0 for compatibility with Stan model
prev_choices[is.na(prev_choices)] <- 0
opp_choices[is.na(opp_choices)] <- 0

# Display the simulated choices
simulated_data <- data.frame(Game = 1:N, PlayerChoice = choices, 
                             PlayerPrevChoice = prev_choices, OpponentPrevChoice = opp_choices)

write.csv(simulated_data, "/Users/henrikgodmann/Desktop/workspace/GitHub/MPTforRPS/data/simulated/simulated_data_time_variant.csv", row.names = F)

rm(list=ls())
