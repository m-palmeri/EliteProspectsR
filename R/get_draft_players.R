get_draft_players <- function(website = NULL, draft_year = 2020) {

  function_call <- match.call()
  # setting league and season to NULL if only website is specified
  if ("website" %in% names(function_call) & !("draft_year" %in% names(function_call))) {
    draft_year <- NULL
  }

  ## input checks
  if (!is.null(website) & !is.null(draft_year)) stop("Please use either the `website` or the `draft_year` parameter, not both")
  if (is.null(website) & is.null(draft_year)) stop("Please supply either `website` or `draft_year`")


  if (!is.null(draft_year)) {
    website <- paste0("https://www.eliteprospects.com/draft/nhl-entry-draft/", draft_year)
  }


  page <- rvest::read_html(website)

  #getting player info part of table
  table_setup <- page %>%
    rvest::html_elements("div[id='drafted-players']") %>%
    rvest::html_elements("table")

  player_table <- table_setup %>%
    rvest::html_table(fill = T) %>%
    magrittr::extract2(1)
  names(player_table)[1:2] <- c("overall_pick", "remove")
  player_table <- player_table %>%
    dplyr::rename_with(tolower) %>%
    dplyr::select(overall_pick, team, player) %>%
    dplyr::filter(!grepl("ROUND", player)) %>%
    dplyr::mutate(overall_pick = gsub("\\#", "", overall_pick),
                  overall_pick = as.numeric(overall_pick),
                  player = gsub("\\(.*\\)", "", player),
                  player = trimws(player))

  #getting links for players
  player_links <- .get_table_links(table_setup, "player")
  team_links <- .get_table_links(table_setup, "team")


  full_player_table <- player_table %>%
    dplyr::mutate(player_link = player_links) %>%
    dplyr::mutate(team_link = team_links) %>%
    dplyr::select(overall_pick, player, player_link, team, team_link)

  return(full_player_table)
}


