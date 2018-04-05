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
#                   EACH IMPLEMENTED BIOENTITY REQUESTS AND RESULTS
#                   (not all Bioentity APIs are (fully?) functional)
#
#
# -----------------------------------------------------------------------------

#' Gets homolog associations for a gene.
#'
#' e.g. Given a gene ID, finds homologs for that gene and lists their associations
#'
#' The monarch_api class is included in return mainly for debugging the REST requests.
#'
#' https://api.monarchinitiative.org/api/bioentity/gene/NCBIGene%3A8314?rows=100&fetch_objects=true
#'
#' @param gene A valid monarch initiative gene id.
#'
#' @return A list of (tibble of homolog information, monarch_api S3 class).
#' @export
#'
#' @examples
#' gene <-"NCBIGene:8314"
#' bioentity_gene_homology_associations(gene)
bioentity_gene_homology_associations <- function(gene) { # TODO: definitely want to add response taxons.
  # TODO: Needs extensive testing with various NCBIGene and other IDs.
  # TODO: Synonyms, phenotpye_assoc, disease_assoc, genotype_assoc are also populated.
  # TODO:   do we want to process them all at once and access them individually?
  # TODO: Need a better idea of what this monarch is trying to uncover here,
  # TODO:   and where it comes from vs. other homolog, phenotype, genotype etc. methods.
  gene <- utils::URLencode(gene, reserved = TRUE)

  query <- list(rows=100, fetch_objects="true")
  url <- build_monarch_url(path = list("/api/bioentity/gene", gene),
                           query = query)
  resp <- monarch_api(url)
  homs <- jsonlite::flatten(resp$content$homology_associations, recursive=TRUE)

  # dataframes embedded in lists are most problematic as they aren't flattened.

  evids <- extract_matching_phrases_from_lists(homs$evidence_graph.nodes, 'evidence')
  pubs <- extract_matching_phrases_from_lists(homs$publications, 'PMID')
  sources <- list_of_paths_to_basenames(homs$provided_by)
  homs <- homs[!names(homs) %in% c("publications", "provided_by")]
  homs <- homs[c("subject.taxon.label", "subject.id", "subject.label", "relation.label",  "object.label", "object.id", "object.taxon.label")]
  homs <- do.call('cbind.data.frame',
                  list(homs,
                       evidence = evids,
                       publications = pubs, provided_by = sources))
  homs <- tibble::as_tibble(homs)
  return(list(homologs = homs,
              response = resp))
}

#' Get homologs for a gene.
#'
#' Replicates info in view: https://api.monarchinitiative.org/api/#!/bioentity/get_gene_homologs
#'
#' The monarch_api class is included in return mainly for debugging.
#'
#' https://api.monarchinitiative.org/api/bioentity/gene/NCBIGene%3A8314/homologs/?rows=100&fetch_objects=true
#'
#'
#' @param gene A valid monarch initiative gene id.
#'
#' @return A list of (tibble of homolog information, monarch_api S3 class).
#' @export
#'
#' @examples
#' gene <-"NCBIGene:8314"
#' bioentity_homologs(gene)
bioentity_homologs <- function(gene) { # TODO: definitely want to add response taxons.
  # TODO: Needs extensive testing with various NCBIGene and other IDs.
  gene <- utils::URLencode(gene, reserved = TRUE)

  query <- list(rows=100, fetch_objects="true")
  url <- build_monarch_url(path = list("/api/bioentity/gene",
                                       gene, "homologs/"),
                           query = query)
  resp <- monarch_api(url)
  homs <- jsonlite::flatten(resp$content$associations, recursive=TRUE)

  #   debugging:
  #     homs <- as_tibble(homs[, !apply(is.na(homs), 2, all)]) # deselect columns w/ no info
  #     str(homs, max.level = 1) # OR
  #     as_tibble(homs).

  evids <- extract_matching_phrases_from_lists(homs$evidence_graph.nodes, 'evidence')
  # TODO: We need to know all publications types so we don't miss any. e.g. for NCBIGene:8314, we miss the only publications "ZFIN:ZDB-PUB-030905-1"
  # TODO: OR this is a case where we really do want to drill into data.frames within the lists?
  pubs <- extract_matching_phrases_from_lists(homs$publications, 'PMID')
  sources <- list_of_paths_to_basenames(homs$provided_by)
  homs <- homs[!names(homs) %in% c("publications", "provided_by")]
  homs <- homs[c("subject.taxon.label",
                 "subject.id",
                 "subject.label",
                 "relation.label",
                 "object.label",
                 "object.id",
                 "object.taxon.label")]
  homs <- do.call('cbind.data.frame',
                  list(homs,
                       evidence = evids,
                       publications = pubs,
                       provided_by = sources))
  names(homs) <- sub('.label', '', names(homs))
  homs <- tibble::as_tibble(homs)
  return( list(homologs = homs,
               response = resp) )
}

# -----------------------------------------------------------------------------
#
#
#                   UNIMPLEMENTED BIOENTITY REQUESTS AND RESULTS
#                   (not all Bioentity APIs are (fully?) functional)
#
#
# -----------------------------------------------------------------------------

