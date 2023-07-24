get_team_staff <- function(website) {
  page <- rvest::read_html(website)

  team_id <- .get_website_id(website)

  staff_table <- page %>%
    rvest::html_elements("ul[id='staff-list']") %>%
    rvest::html_elements("li") %>%
    purrr::map_df(., .team_staff_cleaner) %>%
    replace(., . == "", NA) %>%
    tidyr::fill(role) %>%
    dplyr::mutate(team_id = team_id,
                  staff_id = .get_website_id(staff_link)) %>%
    dplyr::select(team_id, role, staff_id, staff_name, staff_link)

  return(staff_table)
}


.team_staff_cleaner <- function(html_node) {
  staff_link <- html_node %>%
    rvest::html_elements("a[href]") %>%
    rvest::html_attr("href")

  staff_info <- html_node %>%
    rvest::html_children() %>%
    rvest::html_text() %>%
    trimws() %>%
    c(., staff_link) %>%
    magrittr::set_names(c("role", "staff_name", "staff_link")) %>%
    t() %>%
    data.frame()

  return(staff_info)
}
