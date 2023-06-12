load_test_rds <- function(file_name) {
  readRDS(testthat::test_path("test_data", file_name))
}

save_test_rds <- function(results, file_name) {
  saveRDS(results, testthat::test_path("test_data", file_name))
}



expect_na <- function(object, info = NULL, label = NULL) {
  act <- testthat::quasi_label(rlang::enquo(object), label, arg = "object")
  act$val <- as.vector(act$val)
  comp <- testthat:::waldo_compare(act$val, NA, x_arg = "actual",
                        y_arg = "expected")
  expect(all(is.na(act$val)),
         sprintf("%s is not %s\n\n%s", act$lab, NA,
                 paste0(comp, collapse = "\n\n")),
         info = info, trace_env = rlang::caller_env())
}
