# monarchr.R
#
# This is an interface into the monarch_initiative web services api.
#
# It was written by charlieccarey @ somepopular email service.
#
# For example, get a table of homolog information from
# monarch_initiative.org
#
#     gene <- bioentity_homologs("NCBIGene:8314")
#     homologs <- bioentity_homologs(gene)

# TODO: Find out what all the tags are for each of these types.
EVID_TAGS = c("evidence", "traceable author statement", "asserted information")
# DISEASE_TAGS = c("MONDO:")
PUBS_TAGS = c("PMID:", "-PUB-") # "ZFIN:ZDB-PUB-030905-1"


# TODO(?): handle more than 100 rows of results.
#          We need to know whether the API pages beyond some number of results or size of results. maybe by checking count in facet counts.
# TODO: Add setting query parameter values on each method. for example, 100 row limit is insufficient for interactions.
# TODO: When object.taxon is empty, remove column.
# TODO: Remove any empty columns in tibbles.

# TODO: protect against empty associations on all calls like :   pths <- jsonlite::flatten(resp$content$associations, recursive=TRUE)
# TODO: see example in interactions.

# -----------------------------------------------------------------------------
#
#
#                     MAKING BIOENTITY REQUESTS
#
#
# -----------------------------------------------------------------------------


#' Gets response from monarch API.
#'
#' If a server status error is encoutered, content is set to NULL.
#' Otherwise it is parsed from json to R objects.
#'
#' @param url URL as a string.
#'
#' @return A monarch_api S3 class with content as R objects, the url
#' used for the query and the server response.
#' @export
#'
#' @examples
#' url <- "https://api.monarchinitiative.org/api/bioentity/gene/NCBIGene%3A8314?rows=100"
#' resp <- monarch_api(url)
monarch_api <- function(url) {
  url <- paste0(url, "&format=json")
  result <- NULL
  resp <- httr::GET(url)
  if (httr::status_code(resp) != 200) {
    message(paste("Something went wrong with GET query:", url))
    message(paste("Status code:", httr::status_code(resp)))
  }
  if (httr::http_type(resp) != "application/json") {
    warning("API did not return json", call. = FALSE)
  }
  else {
    result <- jsonlite::fromJSON(httr::content(resp, as = 'text', encoding = 'UTF-8'))
  }

  structure(
    list(
      content = result,
      url = url,
      response = resp
    ),
    class = "monarch_api"
  )
}

# -----------------------------------------------------------------------------
#
#
#           Content extraction common to many Bioentity content.
#
#
# -----------------------------------------------------------------------------

#' Extracts simplified content from response.
#'
#' @param df a data.frame obtained by flattening the json response's content.
#'
#' @return a tibble with just the core information of interest.
#' @export
#'
#' @examples
#' gene <-"NCBIGene:8314"
#' gene <- utils::URLencode(gene, reserved = TRUE)
#' query <- list(rows=100, fetch_objects="true")
#' url <- build_monarch_url(path = list("/api/bioentity/gene",
#'                                      gene,
#'                                      "diseases/"),
#'                          query = query)
#' resp <- monarch_api(url)
#' resp <- jsonlite::flatten(resp$content$associations,
#'                           recursive=TRUE)
#' extract_be_content(resp)
extract_be_content <- function(df) {
  #   debugging:
  #     df <- as_tibble(df[, !apply(is.na(df), 2, all)]) # deselect columns w/ no info
  #     str(df, max.level = 1) # OR
  #     as_tibble(df).

  # Accessing in a legit manner via the evidence_graph.nodes.
  # (As opposed to extract_matching_phrases_from_lists())
  #
  # d_terms <- lapply(df$evidence_graph.nodes, function(x) { # each data.frame 'id', 'lbl'
  #   x[which(grepl(paste(DISEASE_TAGS, collapse = "|"), x$id)),]
  # })
  # d_terms <- dplyr::bind_rows(d_terms)

  evids <- extract_matching_phrases_from_lists(df$evidence_graph.nodes, EVID_TAGS)
  pubs <- extract_matching_phrases_from_lists(df$publications, PUBS_TAGS)
  sources <- list_of_paths_to_basenames(df$provided_by)

  df <- df[c("subject.taxon.label",
             "subject.id",
             "subject.label",
             "relation.label",
             "object.label",
             "object.id",
             "object.taxon.label")]

  df <- do.call('cbind.data.frame',
                list(df,
                     evidence = evids,
                     publications = pubs,
                     provided_by = sources))
  names(df) <- sub('.label', '', names(df))

  return(tibble::as_tibble(df))
}


# -----------------------------------------------------------------------------
#
#
#                   EACH IMPLEMENTED BIOENTITY REQUESTS AND RESULTS
#                   (not all Bioentity APIs are (fully?) functional)
#
#
# -----------------------------------------------------------------------------

# TODO: Arrange methods in order of equivalent ordering of swagger documentation.

#' Gets gene info, like catgories and labels for a gene.
#'
#' The monarch_api class is included in return mainly for debugging the REST requests.
#'
#' https://api.monarchinitiative.org/api/bioentity/gene/NCBIGene%3A8314?rows=100&fetch_objects=true
#' https://api.monarchinitiative.org/api/bioentity/gene/HGNC%3A950?fetch_objects=true&rows=100
#'
#' @param gene A valid monarch initiative gene id.
#'
#' @return A list of (list of gene information, monarch_api S3 class).
#' @export
#'
#' @examples
#' gene <-"NCBIGene:8314"
#' bioentity_gene_info(gene)
bioentity_gene_info <- function(gene, rows=100) { # TODO: definitely want to add response taxons.
  # NOTE: This was recently (apr, 2018) remapped by biolink-api from a previous method that was giving homology assoications.
  # NOTE: The previous method with this same call format was giving homology associations.
  # NOTE: Our previous name for that method was bioentity_gene_homology_associations.
  gene <- utils::URLencode(gene, reserved = TRUE)

  query <- list(rows=rows, fetch_objects="true")
  url <- build_monarch_url(path = list("/api/bioentity/gene", gene),
                           query = query)

  resp <- monarch_api(url)
  info <- resp$content

  return(list(gene_info = info,
              response = resp))
}

#' Gets homologs for a gene.
#'
#' Replicates info in view: https://api.monarchinitiative.org/api/#!/bioentity/get_gene_homologs
#'
#' mimics calls like these
#'
#' https://api.monarchinitiative.org/api/bioentity/gene/NCBIGene%3A8314/homologs/?rows=100&fetch_objects=true
#' https://api.monarchinitiative.org/api/bioentity/gene/HGNC%3A950/homologs/?homolog_taxon=NCBITaxon%3A9606&fetch_objects=true&rows=100
#'
#' @param gene A valid monarch initiative gene id.
#' @param homolog_taxon A taxon for the target organism.
#'
#' Taxon takes the form NCBITaxon:id. (human: NCBITaxon:9606)
#'
#' @return A list of (tibble of homolog information, monarch_api S3 class).
#' @export
#'
#' @examples
#' gene <-"NCBIGene:8314"
#' bioentity_homologs(gene)
bioentity_homologs <- function(gene, homolog_taxon = NULL, rows = 100) {
  # TODO: Needs extensive testing with various NCBIGene and other IDs.
  gene <- utils::URLencode(gene,
                           reserved = TRUE)

  query <- list(rows=rows, fetch_objects="true", homolog_taxon = homolog_taxon)
  url <- build_monarch_url(path = list("/api/bioentity/gene",
                                       gene, "homologs/"),
                           query = query)
  resp <- monarch_api(url)


  if (is.data.frame(resp$content$associations)) {
    homs <- jsonlite::flatten(resp$content$associations,
                              recursive=TRUE)
    tb <- extract_be_content(homs)
  }
  else {
    tb <- tibble::as_tibble(NULL)
  }

  return( list(homologs = tb,
               response = resp) )
}


#' Gets diseases associated with a gene.
#'
#' Given a gene, what diseases are associated with it.
#'
#' https://api.monarchinitiative.org/api/bioentity/gene/NCBIGene%3A8314/diseases/?rows=100&fetch_objects=true&format=json
#'
#' @param gene A valid monarch initiative gene id.
#'
#' @return A list of (tibble of disease information, monarch_api S3 class).
#' @export
#'
#' @examples
#' gene <- "NCBIGene:8314"
#' bioentity_diseases_assoc_w_gene(gene)
bioentity_diseases_assoc_w_gene <- function(gene, rows = 100) {
  gene <- utils::URLencode(gene, reserved = TRUE)
  query <- list(rows=rows, fetch_objects="true")
  url <- build_monarch_url(path = list("/api/bioentity/gene",
                                       gene, "diseases/"),
                           query = query)
  resp <- monarch_api(url)

  dis <- jsonlite::flatten(resp$content$associations,
                           recursive=TRUE)

  tb <- extract_be_content(dis)

  return( list(diseases = tb,
               response = resp) )
}


#' Gets phenotypes associated with a gene.
#'
#' Given a gene, what phenotypes are associated with it.
#'
#' https://api.monarchinitiative.org/api/bioentity/gene/NCBIGene%3A8314/phenotypes/?rows=100&fetch_objects=true
#'
#' @param gene A valid monarch initiative gene id.
#'
#' @return A list of (tibble of phenotype information, monarch_api S3 class).
#' @export
#'
#' @examples
#' gene <- "NCBIGene:8314"
#' bioentity_phenotypes_assoc_w_gene(gene)
bioentity_phenotypes_assoc_w_gene <- function(gene, rows=100) {
  gene <- utils::URLencode(gene, reserved = TRUE)
  query <- list(rows=rows, fetch_objects="true")
  url <- build_monarch_url(path = list("/api/bioentity/gene",
                                       gene, "phenotypes/"),
                           query = query)
  resp <- monarch_api(url)

  phe <- jsonlite::flatten(resp$content$associations,
                           recursive=TRUE)

  tb <- extract_be_content(phe)

  return( list(phenotypes = tb,
               response = resp) )
}


#' Gets expression anatomy associated with a gene.
#'
#' Given a gene, what expression anatomy is associated with it.
#'
#' https://api.monarchinitiative.org/api/bioentity/gene/NCBIGene%3A8314/expression/anatomy?rows=100&fetch_objects=true
#'
#' @param gene A valid monarch initiative gene id.
#'
#' @return A list of (tibble of anatomy information, monarch_api S3 class).
#' @export
#'
#' @examples
#' gene <- "NCBIGene:8314"
#' bioentity_exp_anatomy_assoc_w_gene(gene)
bioentity_exp_anatomy_assoc_w_gene <- function(gene, rows=100) {
  gene <- utils::URLencode(gene, reserved = TRUE)
  query <- list(rows=rows, fetch_objects="true")
  url <- build_monarch_url(path = list("/api/bioentity/gene",
                                       gene, "expression/anatomy"), # Note, slightly different request form.
                           query = query)
  resp <- monarch_api(url)

  anat <- jsonlite::flatten(resp$content$associations,
                            recursive=TRUE)

  tb <- extract_be_content(anat)

  return( list(anatomy = tb,
               response = resp) )
}

# TODO: wait for a cleaner endpoint in the biolinks API.
# #' Gets variants associated with a gene.
# #'
# #' Given a gene, what variants are associated with it.
# #'
# #' https://api.monarchinitiative.org/api/bioentity/gene/NCBIGene%3A8314/phenotypes/?rows=100&fetch_objects=true
# #'
# #' @param gene A valid monarch initiative gene id.
# #'
# #' @return A list of (tibble of variant information, monarch_api S3 class).
# #' @export
# #'
# #' @examples
# #' gene <- "NCBIGene:8314"
# #' bioentity_variants_assoc_w_gene(gene)
# # bioentity_variants_assoc_w_gene <- function(gene) {
#   gene <- utils::URLencode(gene, reserved = TRUE)
#   query <- list(rows=100, fetch_objects="true")
#   url <- build_monarch_url(path = list("/api/bioentity/gene",
#                                        gene, "variants/"),
#                            query = query)
#   resp <- monarch_api(url)
#
#   variants <- jsonlite::flatten(resp$content$associations,
#                            recursive=TRUE)
#
#   tb <- extract_be_content(variants)
#
#   return( list(variants = tb,
#                response = resp) )
# }

#' Gets interactions associated with a gene.
#'
#' Given a gene, what variants are associated with it.
#'
#' https://api.monarchinitiative.org/api/bioentity/gene/HGNC%3A950/interactions/?rows=100&fetch_objects=true
#'
#' @param gene A valid monarch initiative gene id.
#'
#' @return A list of (tibble of interactions information, monarch_api S3 class).
#' @export
#'
#' @examples
#' gene <- "NCBIGene:8314"
#' bioentity_interactions_assoc_w_gene(gene)
bioentity_interactions_assoc_w_gene <- function(gene, rows = 100) {
  gene <- utils::URLencode(gene, reserved = TRUE)
  query <- list(rows=rows, fetch_objects="true")
  url <- build_monarch_url(path = list("/api/bioentity/gene",
                                       gene, "interactions/"),
                           query = query)
  resp <- monarch_api(url)

  if (is.data.frame(resp$content$associations)) {
    intxns <- jsonlite::flatten(resp$content$associations,
                                recursive=TRUE)
    tb <- extract_be_content(intxns)
  }
  else {
    tb <- tibble::as_tibble(NULL)
  }

  return( list(interactions = tb,
               response = resp) )
}


#' Gets pathways associated with a gene.
#'
#' Given a gene, what pathways are associated with it.
#'
#'  https://api.monarchinitiative.org/api/bioentity/gene/HGNC%3A950/pathways/?rows=100&fetch_objects=true
#'
#'
#' @param gene A valid monarch initiative gene id.
#'
#' @return A list of (tibble of pathways information, monarch_api S3 class).
#' @export
#'
#' @examples
#' gene <- "NCBIGene:8314"
#' bioentity_pathways_assoc_w_gene(gene)
bioentity_pathways_assoc_w_gene <- function(gene, rows=100) {
  gene <- utils::URLencode(gene, reserved = TRUE)
  query <- list(rows=rows, fetch_objects="true")
  url <- build_monarch_url(path = list("/api/bioentity/gene",
                                       gene, "pathways/"),
                           query = query)
  resp <- monarch_api(url)

  pths <- jsonlite::flatten(resp$content$associations,
                              recursive=TRUE)

  tb <- extract_be_content(pths)

  return( list(pathways = tb,
               response = resp) )
}

#' Get genes associated with a disease
#'
#' mimics https://api.monarchinitiative.org/api/bioentity/disease/MONDO%3A0006486/genes/?fetch_objects=true&rows=100
#'
#' @param disease An id for a disease, like a MONDO ID.
#'
#' @return A list of (a list of information, monarch_api S3 class).
#'
#' @export
#'
#' @examples
#' disease <- "MONDO:0006486" # uveal melanoma
#' bioentity_genes_assoc_w_disease(disease)
bioentity_genes_assoc_w_disease <- function(disease, rows = 100, start = 0, fetch_objects = FALSE) {
  disease <- utils::URLencode(disease, reserved = TRUE)
  query <- list(rows=rows, fetch_objects=fetch_objects, start=start)
  url <- build_monarch_url(path = list("/api/bioentity/disease",
                                       disease, "genes/"),
                           query = query)
  resp <- monarch_api(url)

  genes <- jsonlite::flatten(resp$content$associations,
                             recursive=TRUE)

  tb <- extract_be_content(genes)

  return( list(genes = tb,
               response = resp) )
}


#' Gets info of a 'type' from an 'id'
#'
#' WARNING: will return a result even if that result doesn't seem to match the type.
#'
#' This is a general method for when other appropriate API calls may not be available.
#' Or you don't want to bother discovering the appropriate function name.
#'
#' As a side effect of our Warning, in the event of an unsuccessful mapping, the info
#' returned  might still be useful for discovery of what categories your id maps to.
#'
#' mimics https://api.monarchinitiative.org/api/bioentity/disease/MONDO%3A0006486?fetch_objects=true&rows=100
#'
#' Types seem to map to Monarch Initiative 'categories' and are limited(?) to:
#'
#'     - gene
#'     - variant
#'     - disease
#'     - genotype
#'     - phenotype
#'     - goterm
#'     - pathway
#'     - anatomy
#'     - substance
#'     - individual
#'
#' @param id A valid monarch initiative id.
#' @param type A category to try for the id.
#'
#' @return A list of (list of info for the id, monarch_api S3 class).
#' @export
#'
#' @examples
#' disease <-"MONDO:0006486"
#' bioentity_id_type(disease, 'disease')
#' #' # Note in this example, the info returned includes 'categories': 'disease' and 'quality'
#' # This is a hint of which valid 'types' will be 'successful' for a particular 'id'.
#' # Warning: Trying other types returns same info, because the other categories
#' # were irrelevant for this id.
#' # bioentity_id_type(disease, 'gene') # Not run. Does not get 'gene'!
bioentity_id_type <- function(id, type, rows=100) {
  #TODO: check on cateogries as Monarch Initiative API evolves.
  #TODO: warning on invalid types.
  gene <- utils::URLencode(id, reserved = TRUE)

  query <- list(rows=rows, fetch_objects="true")
  url <- build_monarch_url(path = list("/api/bioentity", type, id),
                           query = query)

  resp <- monarch_api(url)
  info <- resp$content

  return(list(info = info,
              response = resp))
}


# -----------------------------------------------------------------------------
#
#
#                   BIOLINK REQUESTS
#
#
# -----------------------------------------------------------------------------

#' Get a list of results matching a search term.
#'
#' https://api.monarchinitiative.org/api/search/entity/uveal%20melanoma?rows=20&start=1&category=disease
#'
#' Note, as opposed to the bioentity_ functions, the biolink_search results are not standardized to a tibble?
#'
#'  https://api.monarchinitiative.org/api/search/entity/autocomplete/MELANOMA%2C%20UVEAL?rows=20&start=1&category=disease # finds MONDO:0006486 as 3rd result.
#' @param phrase_or_id Something for which we are looking for more information, or to find useful entities.
#'
#' @return A list of (the search results, and monarch_api S3 class.)
#' @export
#'
#' @examples
#' term <- "uveal melanoma"
#' biolink_search(term)
biolink_search <- function(phrase_or_id, rows=20) {
  term <- utils::URLencode(phrase_or_id, reserved = TRUE)
  query <- list(rows=rows)
  url <- build_monarch_url(path = list("/api/search/entity",
                                       term),
                           query = query)
  resp <- monarch_api(url)

  docs <- jsonlite::flatten(resp$content$docs,
                            recursive=TRUE)

  # Remove mostly redundant columns and simplify everything to character.
  # TODO(?): keep non-character lists as non-character?

  docs<- docs[!grepl( '_kw$|_std$|_eng$', names(docs), perl=TRUE)]
  docs <- lapply(docs, flatten_list_of_strings)
  docs <- tibble::as_tibble(docs)

  return( list(search_results = docs,
               response = resp) )
}
