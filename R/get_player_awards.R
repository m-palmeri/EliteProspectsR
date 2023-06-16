get_player_awards <- function(website) {
  page <- rvest::read_html(website)

  awards_temp_list <- page %>%
    rvest::html_elements("div[id='awards']") %>%
    rvest::html_elements("li")

  #if no awards
  if (length(awards_temp_list) == 0) {
    return(data.frame(
      season = character(),
      award = character(),
      award_link = character()
    ))
  }

  #subsetting to overaching lists
  overarching_list <- awards_temp_list %>%
    rvest::html_element("div") %>%
    purrr::map(function(x) !is.na(x)) %>%
    unlist()

  awards_df <- purrr::map_df(
    awards_temp_list[which(overarching_list)],
    .get_player_awards_helper
  )

  return(awards_df)
}

.get_player_awards_helper <- function(li) {
  season <- li %>%
    rvest::html_elements("div[class$='season']") %>%
    rvest::html_text() %>%
    stringr::str_extract(., "[0-9-]+")

  awards <- li %>%
    rvest::html_elements("li") %>%
    rvest::html_text() %>%
    gsub("\\(.*\\)", "", .) %>%
    trimws() %>%
    gsub(" +", " ", .)

  award_links <- li %>%
    rvest::html_elements("li") %>%
    rvest::html_element("a") %>%
    rvest::html_attr("href")

  df <- data.frame(
    season = season,
    award = awards,
    award_link = award_links
  )

  return(df)
}
