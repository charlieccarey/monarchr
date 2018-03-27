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

#' Print method for monarch_api.
#'
#' @param x A monarch_api S3 Object.
#' @param depth An integer indicating how many levels of content to show.
#' @param ... Further options for print.
#'
#' @return A summary of x and, invisibly, x.
#' @export
print.monarch_api <- function(x, depth = 2, ...) {
  cat("<monarch ", x$url, ">\n", sep = "")
  cat("<Showing parsed R objects from json response.", depth, "level(s) deep>\n")
  utils::str(x$content, max.level = depth)
  # TODO: Some response content might be geared towards GUI of monarch_initiative.org website.
  # TODO: To the extent we don't find them of unique utility, we could remove them here (e.g. facet_counts?).
  invisible(x)
}


#' Converts R booleans to monarch_api values and drops NULL parameters from query list.
#'
#' @param query List of api query parameters and their values.
#'
#' @return list of query paramters and cleaned values.
#' @importFrom purrr map compact
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' clean_query(list(rows = 100, fetch_objects = TRUE, use_compact_associations = FALSE,
#'                   unselect_evidence=NULL, format = "json"))
clean_query <- function(query) {
  # TODO: Additional error checking on parameter names and values.
  # TODO: (If these are consistent across most methods for one or more of the APIs.)
  # TODO: (And delete query parameters that seem to have fatal errors.
  # TODO: e.g. bioentity_homologs crashes query specifies 'true' or 'false'?
  # TODO: (And delete query parameters that have no effects.
  # TODO: e.g. I've seen no effect of toggling some of the other parameters?
  # TODO: unselect_evidence?)
  query %>%
    compact() %>%
    map(function(x) ifelse(is.logical(x), tolower(as.character(x)), x))
}

#' Builds URL for a monarch GET request.
#'
#' @param path A path to the monarch resource to use.
#' @param query A list of url parameter settings. TRUE FALSE set to
#' "true" "false".
#'
#' @return URL as a string.
#' @export
#'
#' @examples
#' url <- build_monarch_url(path = list("/api/bioentity/gene", "NCBIGene%3A8314"),
#'                          query = list(rows = 100, fetch_objects = "true",
#'                           format = "json"))
#' url <- build_monarch_url(path = list("/api/bioentity/gene", "NCBIGene%3A8314"),
#'                          query = list(rows = 100, fetch_objects = TRUE,
#'                          unselect_evidence=FALSE, format = "json"))
#' url <- build_monarch_url(path = list("/api/bioentity/gene", "NCBIGene%3A8314"),
#'                          query = list(rows = 100, fetch_objects = TRUE,
#'                          unselect_evidence=NULL, format = "json"))
build_monarch_url <- function(path, query=NULL) {
  q <- clean_query(query)
  url <- httr::modify_url("https://api.monarchinitiative.org", path = path, query = q)
  url
}

#' Flattens and concatenates character vectors in a list
#'
#' @param strings A list of character vectors.
#'
#' @return Character vector, same length as the initial list.
#' @export
#'
#' @examples
#' # words <- list("a", c("few", "words"), NA, NULL, c("to", "try")) # not passing...
#' words <- list("a", c("few", "words"), NA, c("to", "try"))
#' flatten_list_of_strings(words)
flatten_list_of_strings <- function(strings) {
  # TODO: protect against NULLs (warn, or convert to NAs?)
  strings <- lapply(strings,
                    function(x) {
                      result = NA
                      if (length(x) > 0) {
                         result = paste(x, collapse = ', ')
                      }
                      result
                    })
  unlist(strings)
}

#' Extracts and concatenates (possibly multiple) basenames from paths.
#'
#' Basenames are concatenated if there are multiple paths per input.
#'
#' @param paths A list of character vectors representing file or url paths.
#'
#' @return The basenames as a flattened character vector.
#' @export
#'
#' @examples
#' paths <- list('a/b.w', c('c/d.x', 'e/f.y'), 'g/h.z')
#' list_of_paths_to_basenames(paths)
list_of_paths_to_basenames <- function(paths) {
  paths <- lapply(paths,
                  function(x) {
                    y <- sub("\\.[[:alnum:]]+$", '', basename(x))
                  })
  flatten_list_of_strings(paths)
}

#' Finds phrases from list of things, like a list of dataframes.
#'
#' Wherever the phrase exists, it is extracted. Convenient for list of
#' dataframes in which we know the phrase occurs somewhere but we don't
#' no the column, or don't care about preserving the source column name.
#'
#' @param things Lists. Maybe lists of dataframes.
#' @param phrase An anchor phrase. (maybe to fetch cell matching that from df).
#'
#' @return List matching the phrase
#' @export
#'
#' @examples
#' #evidence <- extract_matching_phrases_from_lists(homs$evidence_graph.nodes, 'evidence')
extract_matching_phrases_from_lists <- function(things, phrase) {
  # TODO: Prefill the results?
  matches <- lapply(things,
                    function(x) {
                        y <- unlist(x)
                        y[grepl(phrase, y)]
                    })
  # TODO: We need to fix the ordering or give some conditionals here to get it to work on both
  # TODO: multiple matches, and when there is nothing to match.
  matches <- flatten_list_of_strings(matches)
  matches <- lapply(matches,
                    function(x) ifelse(length(x) == 0, NA, x))
  unlist(matches)
}

#' Gets homolog associations for a gene.
#'
#' The monarch_api class is included in return mainly for debugging.
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
  # TODO While going to all this trouble, probably should collect all associations at once into an S3 class.
  # TODO: NCBIGene:8315 returns non-empty homology_associations, disease_associations, phenotype_associations,
  # TODO: and several NULL other associations
  # TODO: if lucky, we'll be able to process each in same manner?
  gene <- utils::URLencode(gene, reserved = TRUE)

  query <- list(rows=100, fetch_objects="true")
  url <- build_monarch_url(path = list("/api/bioentity/gene", gene), # TODO: confirm this!!!
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
  homs <- tibble::as_tibble(homs)
  return( list(homologs = homs,
               response = resp) )
}


