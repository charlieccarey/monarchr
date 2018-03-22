library(testthat)
library(monarchr)
library(httr)

monarch_base_url <- "https://api.monarchinitiative.org"
monarch_gene_url <-
  "https://api.monarchinitiative.org/api/bioentity/gene/NCBIGene%3A84570?rows=100&fetch_objects=true"
monarch_bad_gene_url <-
  "https://api.monarchinitiative.org/api/bioentity/gene/NCBIGene84570?rows=100&fetch_objects=true"
monarch_homolog_url <-
  "https://api.monarchinitiative.org/api/bioentity/gene/NCBIGene%3A8314?rows=100&fetch_objects=true&format=json"
monarch_homolog_url_tf <-
  "https://api.monarchinitiative.org/api/bioentity/gene/NCBIGene%3A8314?rows=100&fetch_objects=true&unselect_evidence=false&format=json"

#------------ Test Possibly Private Functions. ---------------------!


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


#------------ Test Public Utility Functions. -----------------------!


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


test_that("build_monarch_url builds a valid url even if a parameter is encoded.", {
  path <- list("/api/bioentity/gene", "NCBIGene%3A8314")
  query = list(rows = 100, fetch_objects = TRUE, unselect_evidence=NULL, format = "json")
  r <- build_monarch_url(path, query)
  expect_identical(r, monarch_homolog_url)
})


#------------ Test Functions Downstream of Server Response. --------!


test_that("monarch initiative is up!", {
  r <- httr::GET(monarch_base_url)
  expect_identical(status_code(r), as.integer(200))
})

test_that("Success status 200 for gene api.monarchinitiative.org/bioentity/gene/ .", {
  r <- httr::GET(monarch_gene_url)
  expect_identical(status_code(r), as.integer(200))
})

test_that("Failure status 500 for improperly formatted gene id for api.monarchinitiative.org/bioentity/gene/ .", {
  r <- httr::GET(monarch_bad_gene_url)
  expect_identical(status_code(r), as.integer(500))
})

test_that("monarch_api returns an S3 class of type 'monarch_api'.", {
  r <- monarch_api(monarch_gene_url)
  expect_is(r, 'monarch_api')
})
