# utils.R
#
# These are print and processing utilities functions for monarchr.


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


#' Flattens and concatenates character vectors in a list
#'
#' @param strings A list of character vectors.
#'
#' @return Character vector, same length as the initial list.
#' @export
#'
#' @examples
#' # words <- list("a", c("few", "words"), NA, NULL, c("to", "try")) # not passing...
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
  matches <- lapply(things,
                    function(x) {
                      y <- unlist(x)
                      y[grepl(phrase, y)]
                    })
  matches <- flatten_list_of_strings(matches)
  #matches <- lapply(matches,
  #                  function(x) ifelse(length(x) == 0, NA, x))
  #unlist(matches)
}
