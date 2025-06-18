predict_games <- function(matchups, parameters, home = F, momentum = F, n = 10000) {
  # browser()

  # TODO: Check whether all countrys in the matchups are in the data

  predictions <- vector("list", length = nrow(matchups))
  for(i in 1:nrow(matchups)) {
    home <- matchups[i, ]$home
    away <- matchups[i, ]$away

    # stage 1: draw parameters from log normal and get lambdas
    home_attack <- rnorm(n = 10000,
                         mean = with(parameters$parameters, parameters$parameters[country == home & parameter == "attack",])$estimate,
                         sd = sqrt(with(parameters$parameters, parameters$parameters[country == home & parameter == "attack",])$variance))

    away_attack <- rnorm(n = 10000,
                         mean = with(parameters$parameters, parameters$parameters[country == away & parameter == "attack",])$estimate,
                         sd = sqrt(with(parameters$parameters, parameters$parameters[country == away & parameter == "attack",])$variance))

    home_defense <- rnorm(n = 10000,
                          mean = with(parameters$parameters, parameters$parameters[country == home & parameter == "defense",])$estimate,
                          sd = sqrt(with(parameters$parameters, parameters$parameters[country == home & parameter == "defense",])$variance))

    away_defense <- rnorm(n = 10000,
                          mean = with(parameters$parameters, parameters$parameters[country == away & parameter == "defense",])$estimate,
                          sd = sqrt(with(parameters$parameters, parameters$parameters[country == away & parameter == "defense",])$variance))

    lambda_home <- exp(home_attack + away_defense)
    lambda_away <- exp(away_attack + home_defense)

    # stage 2: draw from poisson
    goals_home <- rpois(n = length(lambda_home), lambda = lambda_home)
    goals_away <- rpois(n = length(lambda_away), lambda = lambda_away)

    # get expected goals and odds
    xgoals_home <- mean(goals_home)
    xgoals_away <- mean(goals_away)
    xgoals_home_int <- round(xgoals_home)
    xgoals_away_int <- round(xgoals_away)
    home_win <- mean(goals_home > goals_away)
    away_win <- mean(goals_home < goals_away)
    draw <- 1 - home_win - away_win

    # store predictions
    predictions[[i]] <- data.frame(home = home,
                                   away = away,
                                   xgoals_home = xgoals_home,
                              xgoals_away = xgoals_away,
                              xgoals_home_int = xgoals_home_int,
                              xgoals_away_int = xgoals_away_int,
                              p_home = home_win,
                              p_away = away_win,
                              p_draw = draw,
                              odds_home = 1/home_win,
                              odds_away = 1/away_win,
                              odds_draw = 1/draw)
  }

  return(dplyr::bind_rows(predictions))
}

if(F) {
  devtools::load_all()
  input <- readr::read_csv("inst/extdata/results/nationalteams/women/results.csv")
  data <- reshape_data(input = input,
                       start_date = lubridate::ymd("2022-08-01"),
                       end_date = lubridate::ymd("2025-01-01"))
  parameters <- estimate_params(data = data)

  matchups <- data.frame(home = c("Germany", "Germany"),
                         away = c("France", "Argentina"))

  predict_games(matchups = matchups,
                parameters = parameters)
}
