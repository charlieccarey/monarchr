context("test-utils.R")


# ----------- Lists and character vectors ----------------

test_that("flatten_list_of_strings handles NAs and NULLs", {
  words <- list("a",
                NULL,
                c("few", "words"),
                NA,
                c("to", "try"),
                c(NA, NA),
                c("and", NA, "finally"))
  r <- flatten_list_of_strings(words)
  expect_identical(r,
                   c("a",
                     NA,
                     "few, words",
                     NA,
                     "to, try",
                     NA,
                     "and, finally"))
})
