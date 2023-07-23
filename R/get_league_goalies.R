get_league_goalies <- function(website = NULL, league = "NHL", season = "2022-2023") {

  function_call <- match.call()
  website <- .league_parameter_check(website, league, season, function_call, "/stats/")

  # getting number of pages
  page <- rvest::read_html(website)
  number_of_pages <- page %>%
    rvest::html_elements("div[id='goalie-stats']") %>%
    rvest::html_elements("div[class='table-pagination']") %>%
    rvest::html_text() %>%
    gsub(" ", "", .) %>%
    stringr::str_extract("[0-9]+goaliesfound") %>%
    stringr::str_extract("[0-9]+") %>%
    as.numeric() %>%
    magrittr::divide_by(100) %>%
    ceiling()

  if (length(number_of_pages) == 0) number_of_pages <- 1

  full_goalie_list <- purrr::map_df(1:number_of_pages, .f = function(page_num) {
    suppressWarnings(
      .get_league_players_helper(
        paste0(website, "?sort-goalie-stats=svp&page-goalie=", page_num),
        "goalie"
      )
    )
  })

  return(full_goalie_list)
}
