#' Get fasta sequence from UniprotKB identifier
#'
#' @param x uniprot identifier
#' @param dryrun logical, do not execute the function
#' @param verbose logical for functin feedback
#' @return A fasta sequence
#' @examples
#' uniprotID2Sequence("A8K9I2", dryrun=TRUE)
#' @export
#' 
uniprotID2Sequence <- function(x, dryrun=FALSE, verbose=TRUE){
  cat(x)
}