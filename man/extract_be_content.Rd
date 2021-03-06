% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monarchr.R
\name{extract_be_content}
\alias{extract_be_content}
\title{Extracts simplified content from response.}
\usage{
extract_be_content(df)
}
\arguments{
\item{df}{a data.frame obtained by flattening the json response's content.}
}
\value{
a tibble with just the core information of interest.
}
\description{
Extracts simplified content from response.
}
\examples{
gene <-"NCBIGene:8314"
gene <- utils::URLencode(gene, reserved = TRUE)
query <- list(rows=100, fetch_objects="true")
url <- build_monarch_url(path = list("/api/bioentity/gene",
                                     gene,
                                     "diseases/"),
                         query = query)
resp <- monarch_api(url)
resp <- jsonlite::flatten(resp$content$associations,
                          recursive=TRUE)
extract_be_content(resp)
}
