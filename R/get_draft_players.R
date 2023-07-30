get_draft_players <- function(website = NULL,
                              draft_name = NULL,
                              draft_year = NULL) {

  function_call <- match.call()

  # getting website URL from inputs
  website <- .website_parameter_check(website, draft_name, draft_year,
                                      "draft", function_call, "/")

  draft_year <- .get_website_id(website)
  website_league <- website %>%
    gsub(".*/draft/", "", .) %>%
    gsub("/.*", "", .)
  draft <- draft_names_crosswalk %>%
    dplyr::filter(link_component == website_league) %>%
    .$draft_name

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

  #getting links
  player_links <- .get_table_links(table_setup, "player")
  team_links <- .get_table_links(table_setup, "team")

  #getting ids
  player_ids <- .get_website_id(player_links)
  team_ids <- .get_website_id(team_links)

  full_player_table <- player_table %>%
    dplyr::mutate(draft = draft,
                  draft_year = draft_year,
                  player_id = player_ids,
                  player_link = player_links,
                  team_id = team_ids,
                  team_link = team_links) %>%
    dplyr::select(draft, draft_year, overall_pick, player_id, player,
                  player_link, team_id, team, team_link)

  return(full_player_table)
}


