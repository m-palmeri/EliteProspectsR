get_player_stats <- function(website) {
  page <- rvest::read_html(website)

  raw_player_table <- page %>%
    rvest::html_elements("div[id='league-stats']") %>%
    rvest::html_elements("table[class*='player-stats']") %>%
    rvest::html_table(fill = TRUE) %>%
    magrittr::extract2(1) %>%
    magrittr::set_names(gsub("^$", "NA", names(.))) %>%
    cbind(.player_name(website, page), .)

  player_tables <- list(
    dplyr::select(raw_player_table, id:POST) %>%
      dplyr::select(., -(dplyr::last_col(offset=1):dplyr::last_col())),
    dplyr::select(raw_player_table, id:League, POST:dplyr::last_col()) %>%
      dplyr::select(-POST)
  )

  clean_player_table <- purrr::map(player_tables, .player_stats_cleaner) %>%
    magrittr::set_names(c("Regular Season", "Playoffs")) %>%
    Filter(function(x) nrow(x) > 0, .)

  return(clean_player_table)
}


.player_stats_cleaner <- function(df) {

  rename_cols <- player_stats_vector[player_stats_vector %in% names(df)] %>%
    .[order(match(., names(df)))]
  numeric_cols <- rename_cols %>%
    names() %>%
    .[!(. %in% player_stats_vector_character)]
  rename_function_call <- rename_cols %>%
    paste0(names(.), " = `", ., "`") %>%
    paste(., collapse = ", ") %>%
    paste0("dplyr::rename(df, ", ., ")")

  final_df <- eval(parse(text = rename_function_call)) %>%
    dplyr::rename_with(tolower) %>%
    replace(., . == "-", NA) %>%
    tidyr::separate(team, into = c("team", "captaincy"), sep = " {10,}", fill = "right") %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(numeric_cols), as.numeric),
                  captaincy = gsub("[^a-zA-Z]", "", captaincy),
                  dplyr::across(dplyr::where(is.character), trimws))

  return(final_df)
}

