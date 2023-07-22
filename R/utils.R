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

# quick check for parameters given to certain get_league... functions
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
    rvest::html_elements("p[class='ep-text']") %>%
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
.get_website_id <- function(website) {
  id <- stringr::str_split(website, "/")[[1]] %>%
    Filter(.numeric_identifier, .) %>%
    as.numeric()
  if (length(id) == 0) id <- NA
  return(id)
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
