context("test-utils.R")


# -----------------------------------------------------------------------------
#
#
#              TEST FXNS USEFUL FOR PROCESSING RESULTS
#
#
# -----------------------------------------------------------------------------


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

# test extract_matching_phrases_from_lists

# test list_of_paths_to_basenames

# -----------------------------------------------------------------------------
#
#
#              TEST FXNS USEFUL FOR GENERATING REQUESTS
#
#
# -----------------------------------------------------------------------------

test_that("build_monarch_url builds a valid url, by simple example.", {
  path <- list("/api/bioentity/gene", "NCBIGene%3A8314")
  query <- list(rows = 100, fetch_objects = "true", format = "json")
  r <- build_monarch_url(path, query)
  expect_identical(r, monarch_homolog_url)
})


test_that("build_monarch_url builds a valid url even if booleans are R booleans", {
  path <- list("/api/bioentity/gene", "NCBIGene%3A8314")
  query = list(rows = 100, fetch_objects = TRUE, unselect_evidence=FALSE, format = "json")
  r <- build_monarch_url(path, query)
  expect_identical(r, monarch_homolog_url_tf)
})


test_that("build_monarch_url builds a valid url even if a parameter is NULL encoded.", {
  path <- list("/api/bioentity/gene", "NCBIGene%3A8314")
  query = list(rows = 100, fetch_objects = TRUE, unselect_evidence=NULL, format = "json")
  r <- build_monarch_url(path, query)
  expect_identical(r, monarch_homolog_url)
})


test_that("clean_query reformats TRUE FALSE to strings, drops NULL parameters.", {
  q <- list(rows = 100,
            fetch_objects = TRUE,
            use_compact_associations = FALSE,
            unselect_evidence = NULL,
            format = "json")
  r <- clean_query(q)
  expect_identical(r, list(rows = 100,
                           fetch_objects = 'true',
                           use_compact_associations = "false",
                           format = "json"))
})

# -----------------------------------------------------------------------------
#
#
#              TEST FXNS USEFUL FOR (TEXTUAL) DISPLAY OF RESULTS
#
#
# -----------------------------------------------------------------------------

