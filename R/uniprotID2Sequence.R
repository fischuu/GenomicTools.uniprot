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
# Prepare for dryrun
  if(dryrun){
    fastaSeq <- ""
  } else {
  # Read in the html source
    numberOfSequences <- length(x)
    if(verbose) cat("Contact UniProtDB...\n")
    fastaSeq <- vector(mode="character", length=numberOfSequences)
    for(seqRun in 1:numberOfSequences){
      if(verbose) cat("Process",x[seqRun],"...\n")
      htmlIn <- readLines(paste('https://www.uniprot.org/uniprot/', x[seqRun], sep="") )
      
    # Grep the sequence start and end postion in the vector
      seqStart <-grep("When browsing through different UniProt", htmlIn) + 1
      seqEnd <- grep("Align this entry with its isoforms.", htmlIn) - 1
      
    # Extract the Sequence
      fastaSeq[seqRun] <- as.character(paste(htmlIn[seqStart:seqEnd],collapse=""))
    }
  }  
# Return the result
  names(fastaSeq) <- x
  class(fastaSeq) <- "fa"
  fastaSeq
}