# utils.R
#
# These are processing and print utility functions for monarchr.

monarch_homolog_url <-
  "https://api.monarchinitiative.org/api/bioentity/gene/NCBIGene%3A8314?rows=100&fetch_objects=true&format=json"
monarch_homolog_url_tf <-
  "https://api.monarchinitiative.org/api/bioentity/gene/NCBIGene%3A8314?rows=100&fetch_objects=true&unselect_evidence=false&format=json"


# -----------------------------------------------------------------------------
#
#
#                     USEFUL FOR GENERATING REQUESTS
#
#
# -----------------------------------------------------------------------------


#' Converts R booleans to monarch_api values and drops NULL parameters from query list.
#'
#' @param query List of api query parameters and their values.
#'
#' @return list of query paramters and cleaned values.
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' clean_query(list(rows = 100,
#'                  fetch_objects = TRUE,
#'                  use_compact_associations = FALSE,
#'                  unselect_evidence=NULL, format = "json"))
clean_query <- function(query) {
  # TODO: Additional error checking on parameter names and values.
  # TODO: (If these are consistent across most methods for one or more of the APIs.)
  # TODO: (And delete query parameters that seem to have fatal errors.
  # TODO: e.g. bioentity_homologs crashes query specifies 'true' or 'false'?
  # TODO: (And delete query parameters that have no effects.
  # TODO: e.g. I've seen no effect of toggling some of the other parameters?
  # TODO: unselect_evidence?)
  query %>%
    purrr::compact() %>%
    purrr::map(function(x) ifelse(is.logical(x), tolower(as.character(x)), x))
}


#' Builds URL for a monarch GET request.
#'
#' @param path A path as a list to the monarch resource to use.
#' @param query A list of url parameter settings. TRUE FALSE set to
#' "true" "false".
#'
#' The path needs to be safely encoded.
#' @seealso \link[utils]{URLencode}
#' @return URL as a safely encoded string.
#' @export
#'
#' @examples
#' m_path <- "/api/bioentity/gene"
#' gene <- "NCBIGene%3A8314"
#' url <- build_monarch_url(path = list(m_path, gene),
#'                          query = list(rows = 100,
#'                          fetch_objects = "true",
#'                          format = "json"))
#' url <- build_monarch_url(path = list(m_path, gene),
#'                          query = list(rows = 100,
#'                          fetch_objects = TRUE,
#'                          unselect_evidence=FALSE,
#'                          format = "json"))
#' url <- build_monarch_url(path = list(m_path, gene),
#'                          query = list(rows = 100,
#'                          fetch_objects = TRUE,
#'                          unselect_evidence=NULL,
#'                          format = "json"))
build_monarch_url <- function(path, query=NULL) {
  q <- clean_query(query)
  url <- httr::modify_url("https://api.monarchinitiative.org", path = path, query = q)
  url
}


# -----------------------------------------------------------------------------
#
#
#                     USEFUL FOR (TEXTUAL) DISPLAY OF RESULTS
#
#
# -----------------------------------------------------------------------------


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


# -----------------------------------------------------------------------------
#
#
#                    USEFUL FOR PROCESSING RESULTS
#
#
# -----------------------------------------------------------------------------


#' Flattens and concatenates character vectors in a list
#'
#' @param strings A list of character vectors.
#'
#' @return Character vector, same length as the initial list.
#' @export
#'
#' @examples
#' words <- list("a", NULL, c("few", "words"), NA, c("to", "try"), c(NA, NA), c("and", NA, "finally"))
#' flatten_list_of_strings(words)
flatten_list_of_strings <- function(strings) {
  strings <- lapply(strings,
                    function(x) {
                      result = NA
                      if (!is.null(x)) {
                        result = paste(stats::na.omit(x), collapse = ', ')
                        if (result == "NA" || result == "") {
                          result = NA_character_
                        }
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
  # e.g. Use to trim 'sources' for the source .ttl files down to simple source names.
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
#' gene <-"NCBIGene:8314"
#' homs <- bioentity_homologs(gene)$homologs
#' evidence <- extract_matching_phrases_from_lists(homs$evidence_graph.nodes, 'evidence')
extract_matching_phrases_from_lists <- function(things, phrase) {
  # Note: This is a hack so we can ignore the actual structures, which might
  #       be graphs as dataframes or simply dataframes.
  # TODO? Target replacing this with better reasoning over the JSON types?
  matches <- lapply(things,
                    function(x) {
                      y <- unlist(x)
                      y[grepl(phrase, y)]
                    })
  matches <- flatten_list_of_strings(matches)
  #matches <- lapply(matches,
  #                  function(x) ifelse(length(x) == 0, NA, x))
  #unlist(matches)
  matches
}
