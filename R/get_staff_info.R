get_staff_info <- function(website) {
  page <- rvest::read_html(website)

  staff_info <- page %>%
    rvest::html_elements("div[class='ep-card']") %>%
    rvest::html_elements("div[class='ep-list']") %>%
    rvest::html_children() %>%
    purrr::map_df(., .person_info_cleaner) %>%
    dplyr::left_join(data.frame(attribute = staff_info_vector), ., by = "attribute") %>%
    dplyr::mutate(attribute = gsub(" ", "_", attribute),
                  attribute = tolower(attribute),
                  value = gsub("\\s+", " ", value),
                  value = trimws(value)) %>%
    dplyr::filter(attribute != "highlights") %>%
    tidyr::pivot_wider(names_from = "attribute", values_from = "value") %>%
    dplyr::mutate(staff_id = .get_website_id(website),
                  staff_name = .get_person_name(page),
                  age = as.numeric(age)) %>%
    cbind(., .get_other_link(page, "player")) %>%
    dplyr::select(staff_id, staff_name, tidyselect::everything())

  return(staff_info)
}
