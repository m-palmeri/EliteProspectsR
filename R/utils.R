.get_table_links <- function(table_setup, finder) {
  table_setup %>%
    rvest::html_elements("tbody") %>%
    rvest::html_elements("tr:not([class='title'])") %>%
    rvest::html_element(glue::glue("td[class*='{finder}']")) %>%
    rvest::html_element(glue::glue("a[href*='{finder}']")) %>%
    rvest::html_attr("href")
}


.league_parameter_check <- function(website, league, season, function_call, between) {
  # setting league and season to NULL if only website is specified
  if ("website" %in% names(function_call) &
      all(!(c("league", "season") %in% names(function_call)))) {
    league <- NULL
    season <- NULL
  }

  #input checks
  if (is.null(website) & is.null(league) & is.null(season)) {
    stop("Please specify either the full website, or the league and season")
  }
  if (!is.null(website) & (!is.null(league) | !is.null(season))) {
    stop("Please use either the `website` parameter, or the `league` and `season` parameters, not both")
  }
  if (is.null(website) & (is.null(league) | is.null(season))) {
    stop("Please supply both `league` and `season` parameters")
  }

  if (!is.null(league) & !is.null(season)) {
    website <- paste0("https://www.eliteprospects.com/league/",
                      tolower(league), between, season)
  }

  return(website)
}
