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


.get_other_link <- function(page, search) {
  other_link <- page %>%
    rvest::html_elements("div[class='ep-card']") %>%
    rvest::html_elements("p[class='ep-text']") %>%
    rvest::html_elements(glue::glue("a[href*='{search}']")) %>%
    magrittr::extract2(1) %>%
    rvest::html_attr("href")
  other_id <- .get_website_id(other_link)

  return_df <- data.frame(id = other_id, link = other_link)
  colnames(return_df) <- paste0(search, "_", c("id", "link"))

  return(return_df)
}
