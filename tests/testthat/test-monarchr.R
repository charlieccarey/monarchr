library(monarchr)
library(httr)
library(tibble)

context("monarchr")


# -----------------------------------------------------------------------------
#
#
#              TEST FXNS FOR SERVER RESPONSES
#
#
# -----------------------------------------------------------------------------

monarch_base_url <- "https://api.monarchinitiative.org"
monarch_gene_url <-
  "https://api.monarchinitiative.org/api/bioentity/gene/NCBIGene%3A84570?rows=100&fetch_objects=true"
monarch_bad_gene_url <-
  "https://api.monarchinitiative.org/api/bioentity/gene/NCBIGene84570?rows=100&fetch_objects=true"


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

# -----------------------------------------------------------------------------
#
#
#              TEST FXNS FOR BIOENTITY RESULTS
#
#
# -----------------------------------------------------------------------------

bap1 <- "NCBIGene:8314"

#        bioentity homologs

resp <- bioentity_homologs(bap1)

# ----------- Tests helper method common to all (?) bioentity content --------------

test_that("extract_be_content returns content as a non-zero length tibble", {
  df <- jsonlite::flatten(resp$response$content$associations, recursive=TRUE) # a complex data.frame
  content <- extract_be_content(df) # a simple tibble
  expect_gt(nrow(resp$homologs), 0)
})

# ----------- Test each bioentity method --------------

#             bioentity_homologs

test_that("bioentity_homologs returns non-zero length homologs.", {
  expect_gt(nrow(resp$homologs), 0)
})

test_that("bioentity_homologs returns homologs as tibble.", {
  resp <- bioentity_homologs(bap1)
  expect_true(is.tibble(resp$homologs))
})


#             bioentity_gene_info

resp <- bioentity_gene_info(bap1)

test_that("bioentity_gene_info returns non-zero length info.", {
  expect_gt(length(resp$gene_info), 0)
})


#             bioentity_diseases_assoc_w_gene

resp <- bioentity_diseases_assoc_w_gene(bap1)

test_that("bioentity_diseases_assoc_w_gene returns non-zero length disease associations.", {
  expect_gt(nrow(resp$disease), 0)
})


test_that("bioentity_diseases_assoc_w_gene returns disease associations as tibble.", {
  expect_true(is.tibble(resp$disease))
})
