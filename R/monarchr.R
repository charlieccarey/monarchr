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
  cat("<monarch", x$url, ">\n", sep = "")
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


#' Gets homologs to a gene.
#'
#' The monarch_api class is included in return mainly for debugging.
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
  gene <- utils::URLencode(gene, reserved = TRUE)
  query <- list(rows=100, fetch_objects="true")
  url <- build_monarch_url(path = list("/api/bioentity/gene", gene, "homologs/"), query = query)
  resp <- monarch_api(url)
  homologs <- tibble::tibble()
  return(list(homologs = homologs, response = resp))
}
