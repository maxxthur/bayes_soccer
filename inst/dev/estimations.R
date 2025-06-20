
pacman::p_load("R2jags", "tidyverse", "arrow")

# load data (last 34 matches)
ds <- readr::read_csv("inst/extdata/results/nationalteams/women/results.csv") |>
  dplyr::filter(.data$date >= lubridate::ymd("2022-08-01"))

# give teams ids
ids <- data.frame(team = unique(c(ds$home_team, ds$away_team)))
ids$id <- 1:nrow(ids)

# ids %>% writexl::write_xlsx("data/germany/mapping/historic_names.xlsx")

data <- ds |>
  dplyr::left_join(ids |>
                     dplyr::select(team, home_id = id), by = c("home_team" = "team")) |>
  dplyr::left_join(ids |>
                     dplyr::select(team, away_id = id), by = c("away_team" = "team"))

# specify model
ngames <- nrow(data)
nteams <- length(unique(c(ds$home_team, ds$away_team)))

# set up the model
model <- function() {
  # LIKELIHOOD AND RANDOM EFFECT MODEL FOR THE SCORING PROPENSITY
  for (g in 1:ngames) {
    # Observed number of goals scored by each team
    y1[g] ~ dpois(theta[g,1])
    y2[g] ~ dpois(theta[g,2])

    # Predictive distribution for the number of goals scored
    ynew[g,1] ~ dpois(theta[g,1])
    ynew[g,2] ~ dpois(theta[g,2])

    # Average Scoring intensities (accounting for mixing components)
    log(theta[g,1]) <- home + att[hometeam[g]] + def[awayteam[g]]
    log(theta[g,2]) <- att[awayteam[g]] + def[hometeam[g]]
  }

  # 1. BASIC MODEL FOR THE HYPERPARAMETERS

  # prior on the home effect
  home ~ dnorm(0,0.0001)

  # Trick to code the ‘‘sum-to-zero’’ constraint
  for (t in 1:nteams){
    att.star[t] ~ dnorm(mu.att,tau.att)
    def.star[t] ~ dnorm(mu.def,tau.def)
    att[t] <- att.star[t] - mean(att.star[])
    def[t] <- def.star[t] - mean(def.star[])
  }

  # priors on the random effects
  mu.att ~ dnorm(0,0.0001)
  mu.def ~ dnorm(0,0.0001)
  tau.att ~ dgamma(.01,.01)
  tau.def ~ dgamma(.01,.01)
}

# create data structure for jags
jagsdata <- with(data, list(hometeam = home_id,
                            awayteam = away_id,
                            y1 = home_score,
                            y2 = away_score,
                            ngames = ngames,
                            nteams = nteams))

# specify which parameters are recorded
params <- c("att", "def", "home")

fit <- R2jags::jags(data = jagsdata, parameters.to.save = params, model.file = model,
            n.chains = 3, n.iter = 20000, n.burnin = 5000, n.thin = 10, DIC = F)

# store results
save(fit, file = "data/jags_output/germany.RData")
