get_league_skaters <- function(website = NULL, league = "NHL", season = "2022-2023") {
  #input checks
  if (is.null(website) & is.null(league) & is.null(season)) {
    stop("Please use either the `website` parameter, or the `league` and `season` parameters, not both")
  }
  if (!is.null(website) & (!is.null(league) | !is.null(season))) {
    stop("Please use either the `website` parameter, or the `league` and `season` parameters, not both")
  }
  if (is.null(website) & (is.null(league) | is.null(season))) {
    stop("Please supply both `league` and `season` parameters")
  }
}
