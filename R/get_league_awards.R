get_league_awards <- function(website = NULL,
                              league = "NHL",
                              force = F) {

  function_call <- match.call()

  website <- .website_parameter_check(website, league, "", "league",
                                      function_call, "", force)

  page <- rvest::read_html(website)

  awards_setup <- page %>%
    rvest::html_elements("li") %>%
    rvest::html_elements("a[href*='awards']")

  awards_list <- awards_setup %>%
    rvest::html_text() %>%
    trimws()

  awards_links <- awards_setup %>%
    rvest::html_attr("href")

  awards_df <- data.frame(award_name = awards_list, award_link = awards_links)

  return(awards_df)
}
