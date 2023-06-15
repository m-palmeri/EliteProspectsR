.get_table_links <- function(table_setup, finder) {
  table_setup %>%
    rvest::html_elements("tbody") %>%
    rvest::html_elements("tr:not([class])") %>%
    rvest::html_element(glue::glue("td[class='{finder}']")) %>%
    rvest::html_element(glue::glue("a[href*='{finder}']")) %>%
    rvest::html_attr("href")
}
