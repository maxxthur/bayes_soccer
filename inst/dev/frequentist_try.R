
matches <- readr::read_csv("inst/extdata/results/nationalteams/women/results.csv") |>
  dplyr::filter(date >= lubridate::ymd("2022-08-01")) |>
  dplyr::mutate(match_id = dplyr::row_number())

home_data <- matches |>
  dplyr::select(attack = home_team, defense = away_team, goals = home_score, match_id) |>
  dplyr::mutate(home = 1)

away_data <- matches |>
  dplyr::select(attack = away_team, defense = home_team, goals = away_score, match_id) |>
  dplyr::mutate(home = 0)

data <- dplyr::bind_rows(home_data, away_data) |>
  dplyr::arrange(match_id)

# Fit Poisson model
model <- glm(goals ~ 0 + home + attack + defense, data = data, family = poisson(link = "log"))

# collect parameters in tidy way
variances <- diag(vcov(model)) |>
  tibble::enframe(name = "term", value = "variance") |>
  dplyr::mutate(parameter = stringr::str_extract(term, "attack|defense|home"),
                country = stringr::str_remove(term, "attack|defense|home"))

parameters <- model |>
  broom::tidy() |>
  dplyr::mutate(parameter = stringr::str_extract(term, "attack|defense|home"),
                country = stringr::str_remove(term, "attack|defense|home")) |>
  dplyr::select(country, parameter, estimate) |>
  dplyr::left_join(variances, by = c("parameter", "country")) |>
  dplyr::filter(parameter != "home") |>
  dplyr::select(-term)

home <- model |>
  broom::tidy() |>
  dplyr::mutate(parameter = stringr::str_extract(term, "attack|defense|home"),
                country = stringr::str_remove(term, "attack|defense|home")) |>
  dplyr::select(country, parameter, estimate) |>
  dplyr::filter(parameter == "home") |>
  dplyr::select(-country) |>
  dplyr::left_join(variances, by = "parameter") |>
  dplyr::select(-country)

# to incorporate uncertainty we simulate with these results

home = "Germany"
away = "Austria"

# stage 1: draw parameters from log normal and get lambdas
home_attack <- rnorm(n = 10000,
                     mean = with(parameters, parameters[country == home & parameter == "attack",])$estimate,
                     sd = sqrt(with(parameters, parameters[country == home & parameter == "attack",])$variance))

away_attack <- rnorm(n = 10000,
                     mean = with(parameters, parameters[country == away & parameter == "attack",])$estimate,
                     sd = sqrt(with(parameters, parameters[country == away & parameter == "attack",])$variance))

home_defense <- rnorm(n = 10000,
                     mean = with(parameters, parameters[country == home & parameter == "defense",])$estimate,
                     sd = sqrt(with(parameters, parameters[country == home & parameter == "defense",])$variance))

away_defense <- rnorm(n = 10000,
                     mean = with(parameters, parameters[country == away & parameter == "defense",])$estimate,
                     sd = sqrt(with(parameters, parameters[country == away & parameter == "defense",])$variance))

lambda_home <- exp(home_attack + away_defense)
lambda_away <- exp(away_attack + home_defense)

# stage 2: draw from poisson

goals_home <- rpois(n = length(lambda_home), lambda = lambda_home)
goals_away <- rpois(n = length(lambda_away), lambda = lambda_away)

# expected goals?
mean(goals_home)
mean(goals_away)

# probabilities
win_home <- mean(round(goals_home, 0) > round(goals_away))
win_away <- mean(round(goals_home, 0) < round(goals_away))
draw <- 1 - win_home - win_away

odds_home <- 1/win_home
odds_way <- 1/win_away
odds_draw <- 1/draw
