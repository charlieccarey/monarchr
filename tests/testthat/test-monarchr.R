library(monarchr)
library(httr)
context("monarchr")

monarch_base_url <- "https://api.monarchinitiative.org"
monarch_gene_url <-
  "https://api.monarchinitiative.org/api/bioentity/gene/NCBIGene%3A84570?rows=100&fetch_objects=true"
monarch_bad_gene_url <-
  "https://api.monarchinitiative.org/api/bioentity/gene/NCBIGene84570?rows=100&fetch_objects=true"

# -----------------------------------------------------------------------------
#
#
#              TEST FXNS FOR SERVER RESPONSES
#
#
# -----------------------------------------------------------------------------


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
