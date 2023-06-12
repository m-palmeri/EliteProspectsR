get_draft_players <- function(website = NULL, draft_year = NULL) {
  ## input checks
  assertthat::assert_that(!is.null(website) & !is.null(draft_year),
                          "Please use either the `website` or the `draft_year` parameter")

  if (!is.null(draft_year)) {
    website <- paste0("https://www.eliteprospects.com/draft/nhl-entry-draft/", draft_year)
  }


  page <- rvest::read_html(website)

  #getting player info part of table
  player_table <- page %>%
    rvest::html_elements("div[id='drafted-players']") %>%
    rvest::html_elements("table") %>%
    rvest::html_table(fill = T) %>%
    magrittr::extract2(1)
  names(player_table)[1:2] <- c("overall_pick", "remove")
  player_table <- player_table %>%
    dplyr::rename_with(tolower) %>%
    dplyr::select(overall_pick, team, player) %>%
    dplyr::filter(!grepl("ROUND", player)) %>%
    dplyr::mutate(overall_pick = gsub("\\#", "", overall_pick),
                  overall_pick = as.numeric(overall_pick),
                  player = gsub("\\(.*\\)", "", player))

  #getting links for players
}
