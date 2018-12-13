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
      
      tryError <- try(htmltab(paste("https://www.uniprot.org/uniprot/?query=", x[seqRun], sep=""), which=1, rm_nodata_cols=FALSE))
      if(sum(is.element("Entry",colnames(tryError)))==0){
        readInThis <-x[seqRun]
      } else {
        ifelse("try-error" %in% class(tryError), readInThis <-x[seqRun], readInThis <- tryError[1,2])
      }
            
      htmlIn <- readLines(paste('https://www.uniprot.org/uniprot/', readInThis, sep=""), warn=FALSE)
      
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