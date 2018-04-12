# Scratch as I implement functions.
#
#' for some reason I can't ignore this file from building.# so its commented out so I can build rest of project.## getting several result types so as to develop off of these while offline.
#'
#' library(httr)
#'
#' gene <- "NCBIGene:8314"
#' gene <- utils::URLencode(gene, reserved = TRUE)
#'
#' query <- list(rows=100, fetch_objects="true")
#' url <- build_monarch_url(path = list("/api/bioentity/gene", gene), # TODO: confirm this!!!
#'                          query = query)
#' resp <- monarch_api(url)
#' homs <- jsonlite::flatten(resp$content$homology_associations, recursive=TRUE)
#'
#' # approach variants like this?
#' # No, need different starting place than disease sum(unlist(lapply(dis$evidence_graph.nodes, nrow))) == 245 (and some of these are disease terms) whereas website shows 308
#'  clin_var <- lapply(dis$evidence_graph.nodes, function(x) { # each data.frame 'id', 'lbl'
#' # Think we previously determined they were in phenotypes.
#' x[which(grepl(paste(CLINVAR_TAGS, collapse = "|"), x$id)),]
#' })
#' d_terms <- dplyr::bind_rows(d_terms)
#'
#' # We think following methods at least are available so start there.
#' # These all return results with human BAP1 at least.
#' #
#' #
#' # What are 'homolog_associations' as we get them from, e.g. HGNC:1100? 10 homologs. vs. 9 on monarch website.
#' # Produces different (but overlapping) results than when we search on monarch website and get.
#' # Is it an accident we are returning only 10 at a time?
#' # Tried upping rows to 300, still get 10.
#' #
#' #
#'
#'
#'
#' # ------ Implemented At Monarch API? -----------------
#' gene <- utils::URLencode("NCBIGene:8314", reserved = TRUE)
#' id <- gene
#' query <- list(rows=100, fetch_objects="true")
#'
#' #' Returns basic info on anatomical entity
#' #'
#' #' gets synonyms, deprecation and replaced by status, categories
#' #'
#' #' ex.
#' #' NCBIGene:8314
#' #' GO:0005634
#' #' UBERON:0002037
#' #' CL:0000540
#' #'
#' #' id = https://api.monarchinitiative.org/api/bioentity/anatomy/NCBIGene%3A8314?rows=100&fetch_objects=true&format=json
#' #'
#' #'
#' #' @param id
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' bioentity_anatomy_id <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/anatomy", id), query=query)
#'   resp <- monarch_api(url)
#' }
#'
#'
#' query <- list(rows=100, fetch_objects="true")
#' url <- build_monarch_url(path = list("/api/bioentity/gene", gene),
#'                          query = query)
#' resp <- monarch_api(url)
#'
#'
#' # ------ Unimplemented At Monarch API? -----------------
#'
#' #' <<<<Not yet implemented in MonarchInitiative API?>>>>
#' #'
#' #' https://api.monarchinitiative.org/api/bioentity/allele/NCBIGene%3A8314?rows=100&fetch_objects=true
#' #'
#' #' Returns genotype object
#' #'
#' #' Will return gene phenotype disease.
#' #'
#' #' @param id
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' bioentity_allele_id <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns expression events for a gene
#' #'
#' #' @param id
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' bioentity_anatomy_id_genes_ <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns gene ids for all genes for a particular anatomy in a taxon
#' #'
#' #' @param id
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' bioentity_anatomy_id_genes_taxid <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns basic info on a disease
#' bioentity_disease_id <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns genes associated with a disease
#' bioentity_disease_id_genes_ <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns associations to models of the disease
#' bioentity_disease_id_models_ <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns associations to models of the disease constrained by taxon
#' bioentity_disease_id_models_taxon <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns phenotypes associated with disease
#' #'
#' #' @param id
#'
#'
#' bioentity_disease_id_phenotypes <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns substances associated with a disease
#' bioentity_disease_id_treatment_ <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns basic info about a gene
#' bioentity_gene_id <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns diseases associated with gene
#' bioentity_gene_id_diseases_ <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns expression events for a gene
#' bioentity_gene_id_expression_anatomy <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns function associations for a gene
#' bioentity_gene_id_function_ <- function(id) {
#'
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns homologs for a gene
#' bioentity_gene_id_homologs_ <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns interactions for a gene
#' bioentity_gene_id_interactions_ <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns pathways associated with gene
#' bioentity_gene_id_pathways_ <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/gene", id, pathways))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns phenotypes associated with gene
#' bioentity_gene_id_phenotypes_ <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns genotype object
#' bioentity_genotype_id <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns diseases associated with a genotype
#' bioentity_genotype_id_diseases_ <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns genes associated with a genotype
#' bioentity_genotype_id_genes_ <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns genotypes-genotype associations
#' bioentity_genotype_id_genotypes_ <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns phenotypes associated with a genotype
#' bioentity_genotype_id_phenotypes_ <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns basic info on a gene
#' bioentity_goterm_id <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns associations to GO terms for a gene
#' bioentity_goterm_id_genes_ <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns basic info on an individual_case
#' bioentity_individual_id <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns basic info on a pathway
#' bioentity_pathway_id <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns genes associated with a pathway
#' bioentity_pathway_id_genes_ <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns basic info on a phenotype
#' bioentity_phenotype_id <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' # Returns anatomical entities associated with a phenotype
#' # known bug... not all their examples work... https://github.com/biolink/biolink-api/issues/122
#' bioentity_phenotype_id_anatomy_ <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns biological functions associated with a Phenotype
#' bioentity_phenotype_id_function_ <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns gene ids for all genes for a particular phenotype in a taxon
#' bioentity_phenotype_id_gene_taxid_ids <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns associated phenotypes
#' bioentity_phenotype_id_genes_ <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns basic info on a substance
#' bioentity_substance_id <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns associations between an activity and process and the specified substance
#' bioentity_substance_id_participant_in_ <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns associations between given drug and roles
#' bioentity_substance_id_roles_ <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns substances associated with a disease
#' bioentity_substance_id_treats_ <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns basic info on a variant
#' bioentity_variant_id <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns genes associated with a variant
#' bioentity_variant_id_genes_ <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns genotypes associated with a variant
#' bioentity_variant_id_genotypes_ <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns phenotypes associated with a variant
#' bioentity_variant_id_phenotypes_ <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns basic info on object of any type
#' bioentity_id <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
#'
#' #' Returns associations for an entity regardless of the type
#' bioentity_id_associations_ <- function(id) {
#'   url <- build_monarch_url(path = list("/api/bioentity/allele", id))
#'   resp <- monarch_api(url)
#' }
