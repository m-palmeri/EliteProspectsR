get_staff_info <- function(website) {
  page <- rvest::read_html(website)

  staff_info <- page %>%
    rvest::html_elements("div[class='ep-card']") %>%
    rvest::html_elements("div[class='ep-list']") %>%
    rvest::html_children() %>%
    purrr::map_df(., .person_info_cleaner)
}
