#' Prepare data for estimation
#'
#' @param input a data frame that holds columns *date*, *home_team*, *away_team*, *home_score*, *away_score*.
#' @param start_date a single date specifying the start of the desired time frame.
#' @param end_date a single date specifying the end of the desired time frame.
#'
#' @return a data frame with columns *attack*, *defense*, *goals*, *match_id*, *home*.
#' @export
#'
#' @examples
reshape_data <- function(input, start_date = NULL, end_date = NULL) {
  # make checks that the necessary columns are available
  names <- names(input)
  if(!all(c("date", "home_team", "away_team", "home_score", "away_score") %in% names)) {
    stop("Data does not have the expected format. Please check ??reshape_data for more information.")
  }

  # filter for time and set match id
  if(is.null(start_date) & is.null(end_date)) {
    input <- input |>
      dplyr::mutate(match_id = dplyr::row_number())
  } else if(is.null(start_date)) {
    input <- input |>
      dplyr::filter(date <= end_date) |>
      dplyr::mutate(match_id = dplyr::row_number)
  } else if(is.null(end_date)) {
    input <- input |>
      dplyr::filter(date >= start_date) |>
      dplyr::mutate(match_id = dplyr::row_number())
  } else {
    input <- input |>
      dplyr::filter(date >= start_date & date <= end_date) |>
      dplyr::mutate(match_id = dplyr::row_number())
  }

  # get matches from home team perspective
  home_data <- input |>
    dplyr::select(attack = home_team, defense = away_team, goals = home_score, match_id) |>
    dplyr::mutate(home = 1)

  # get matches from away team perspective
  away_data <- input |>
    dplyr::select(attack = away_team, defense = home_team, goals = away_score, match_id) |>
    dplyr::mutate(home = 0)

  # combine
  data <- dplyr::bind_rows(home_data, away_data) |>
    dplyr::arrange(match_id)

  # output
  return(data)
}

if(F) {
  devtools::load_all()
  input <- readr::read_csv("inst/extdata/results/nationalteams/women/results.csv")
  reshape_data(input = input,
               start_date = lubridate::ymd("2023-01-01"),
               end_date = lubridate::ymd("2024-01-01"))

}
