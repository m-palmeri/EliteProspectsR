.get_league_players_helper <- function(website, player_type) {
  page <- rvest::read_html(website)

  table_setup <- page %>%
    rvest::html_elements(glue::glue("div[id='{player_type}-stats']")) %>%
    rvest::html_elements("table")

  temp_table <- table_setup %>%
    rvest::html_table() %>%
    magrittr::extract2(1)

  rename_temp <- .rename_df_helper(temp_table)
  numeric_cols <- rename_temp[[2]]

  player_table <- rename_temp[[1]] %>%
    dplyr::select(-`#`) %>%
    dplyr::rename_with(tolower) %>%
    dplyr::mutate(player = gsub("\\(.*\\)", "", player)) %>%
    dplyr::filter(games_played != "")

  player_links <- .get_table_links(table_setup, "player")
  player_ids <- .get_website_id(player_links)
  player_table <- player_table %>%
    dplyr::mutate(player_id = player_ids,
                  player_link = player_links)
  char_vec <- c("player_id", "player", "player_link")

  if ("team" %in% names(player_table)) {
    team_links <- .get_table_links(table_setup, "team")
    team_ids <- .get_website_id(team_links)
    player_table <- player_table %>%
      dplyr::mutate(team_id = team_ids,
                    team_link = team_links)
    char_vec <- c(char_vec, "team_id", "team", "team_link")
  }

  full_player_table <- player_table %>%
    dplyr::select(tidyselect::all_of(char_vec), tidyselect::everything()) %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(char_vec), as.character),
                  dplyr::across(games_played:tidyselect::last_col(), as.numeric),
                  dplyr::across(tidyselect::ends_with("_id"), as.numeric)) %>%
    replace(., . == "", NA) %>%
    tidyr::fill(player_id, player, player_link)

  return(full_player_table)
}


# get links from each row in a table using some identifier (team, player, etc.)
.get_table_links <- function(table_setup, finder, skip_td_filter = F) {
  temp <- table_setup %>%
    rvest::html_elements("tbody") %>%
    rvest::html_elements("tr") %>%
    Filter(function(x) !grepl("(space)|(title)", rvest::html_attr(x, "class")), .)
  if (!skip_td_filter) {
    temp <- rvest::html_element(temp, glue::glue("td[class*='{finder}']"))
  }
  temp %>%
    rvest::html_element(glue::glue("a[href*='{finder}']")) %>%
    rvest::html_attr("href")
}

# quick check for parameters given to certain functions
.website_parameter_check <- function(website,
                                     league,
                                     season,
                                     link_type,
                                     function_call,
                                     between) {
  # setting league and season to NULL if only website is specified
  if ("website" %in% names(function_call) &
      all(!(c("league", "season", "draft_year", "draft_name") %in% names(function_call)))) {
    league <- NULL
    season <- NULL
  }

  # setting parameter names based on function call
  if (link_type == "draft") {
    params <- "`draft_name` and `draft_year`"
  } else if (link_type == "league") {
    params <- "`league` and `season`"
  }

  msg <- NULL

  # setting draft_name based on input
  if (link_type == "draft" & !is.null(league)) {
    match_row <- draft_names_crosswalk %>%
      dplyr::mutate_all(tolower) %>%
      magrittr::equals(., tolower(league)) %>%
      rowSums()
    match_row <- which(match_row > 0)
    if (length(match_row) == 0) {
      # no exact match. return an error and point the user towards draft_names_crosswalk
      msg <- glue::glue("'{league}' not a recognized draft name. ",
                        "Use `draft_names_crosswalk` to see a list of draft names.")
    } else {
      league <- draft_names_crosswalk$link_component[match_row]
    }
  }

  #input checks
  if (!is.null(msg)) {
    # intentionally blank in case msg is set earlier
  } else if (is.null(website) & is.null(league) & is.null(season)) {
    msg <- glue::glue("Please specify either the full `website`, or the {params}")
  } else if (!is.null(website) & (!is.null(league) | !is.null(season))) {
    msg <- glue::glue("Please use either the `website` parameter, or the {params} parameters, not both")
  } else if (is.null(website) & (is.null(league) | is.null(season))) {
    msg <- glue::glue("Please supply both {params} parameters")
  }

  if (!is.null(msg)) {
    stop(msg)
  }

  if (!is.null(league) & !is.null(season)) {
    website <- paste0("https://www.eliteprospects.com/", link_type, "/",
                      tolower(league), between, season)
  }

  return(website)
}

# useful for renaming a bunch of columns. Used in get_player_stats and get_player_careertotals
.rename_df_helper <- function(df) {
  rename_cols <- player_stats_vector[player_stats_vector %in% names(df)] %>%
    .[order(match(., names(df)))]
  numeric_cols <- rename_cols %>%
    names() %>%
    .[!(. %in% player_stats_vector_character)] %>%
    tolower()
  rename_function_call <- rename_cols %>%
    paste0(names(.), " = `", ., "`") %>%
    paste(., collapse = ", ") %>%
    paste0("dplyr::rename(df, ", ., ")")

  changed_df <- eval(parse(text = rename_function_call))

  return(list(changed_df, numeric_cols))
}

# helper function to get clean data from the information blocks (date of birth, position, etc.)
.person_info_cleaner <- function(html_node) {
  html_node %>%
    rvest::html_children() %>%
    rvest::html_text() %>%
    trimws() %>%
    magrittr::set_names(c("attribute", "value")) %>%
    t() %>%
    data.frame()
}

# function to get player link/id from staff website, or staff link/id from player website
.get_other_link <- function(page, search) {
  other_link <- page %>%
    rvest::html_elements("div[class='ep-card']") %>%
    rvest::html_elements("[class='ep-text']") %>%
    rvest::html_elements(glue::glue("a[href*='{search}']")) %>%
    rvest::html_attr("href") %>%
    unique()
  if (length(other_link) == 0) other_link <- NA
  other_id <- .get_website_id(other_link)

  return_df <- data.frame(id = other_id, link = other_link)
  colnames(return_df) <- paste0(search, "_", c("id", "link"))

  return(return_df)
}


# small helper to grab id from given website (team, player, staff)
.get_website_id <- function(websites) {
  ids <- stringr::str_split(websites, "/") %>%
    purrr::map(., function(x) Filter(.numeric_identifier, x)) %>%
    purrr::map(., function(x) ifelse(length(x) == 0, NA, x)) %>%
    unlist() %>%
    as.numeric()
  return(ids)
}

# small helper to get name from webpage (player/staff)
.get_person_name <- function(page) {
  page %>%
    rvest::html_elements("h1[class$='name']") %>%
    rvest::html_text() %>%
    trimws()
}

# small helper to determine if you can safely coerce a column to numeric
.is_coerce_numeric  <- function(x) {
  suppressWarnings(is.na(as.numeric(x)))
}

# used in Filter calls to keep just numeric
.numeric_identifier <- function(x) {
  suppressWarnings(as.numeric(x)) %>%
    is.na() %>%
    isFALSE()
}
