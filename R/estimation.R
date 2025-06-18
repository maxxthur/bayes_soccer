estimate_params <- function(data = NULL, home = F, momentum = F) {
if(all(!home, !momentum)) {
  model <- glm(goals ~ 0 + attack + defense, data = data, family = poisson(link = "log"))
}

if(home & !momentum) {
  model <- glm(goals ~ 0 + home + attack + defense, data = data, family = poisson(link = "log"))
}

# get variances
  variances <- diag(vcov(model)) |>
    tibble::enframe(name = "term", value = "variance") |>
    dplyr::mutate(parameter = stringr::str_extract(term, "attack|defense|home"),
                  country = stringr::str_remove(term, "attack|defense|home"))
# get parameters
  parameters <- model |>
    broom::tidy() |>
    dplyr::mutate(parameter = stringr::str_extract(term, "attack|defense|home"),
                  country = stringr::str_remove(term, "attack|defense|home")) |>
    dplyr::select(country, parameter, estimate) |>
    dplyr::left_join(variances, by = c("parameter", "country")) |>
    dplyr::filter(parameter != "home") |>
    dplyr::select(-term)

# special: home advantage
  home <- model |>
    broom::tidy() |>
    dplyr::mutate(parameter = stringr::str_extract(term, "attack|defense|home"),
                  country = stringr::str_remove(term, "attack|defense|home")) |>
    dplyr::select(country, parameter, estimate) |>
    dplyr::filter(parameter == "home") |>
    dplyr::select(-country) |>
    dplyr::left_join(variances, by = "parameter") |>
    dplyr::select(-country)

  # output list of parameters containing estimates and variances
  return(list(parameters = parameters, home = home))
}

if(F) {
  devtools::load_all()
  input <- readr::read_csv("inst/extdata/results/nationalteams/women/results.csv")
  data <- reshape_data(input = input)
  estimate_params(data = data)
}
