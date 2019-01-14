#' Get UniprotKB identifier from Uniparc accession number
#'
#' @param x uniparc identifier
#' @param dryrun logical, do not execute the function
#' @param verbose logical for function feedback
#' @return A UniprotKB identifier
#' @examples
#' uniprotID2Sequence("EKY21295", dryrun=TRUE)
#' @export
#' 
uniparc2uniprot <- function(x, dryrun=FALSE, verbose=TRUE){
  # Input formatting
  x <- as.character(x)
  
  # Prepare for dryrun
  if(dryrun){
    uniprotID <- ""
  } else {
    # Read in the html source
    numberOfIDs <- length(x)
    if(verbose) cat("Contact UniParc DB...")
    uniprotID <- matrix("NA", nrow=numberOfIDs, ncol=2)
    colnames(uniprotID) <- c("Uniparc", "UniprotKB")
    for(idRun in 1:numberOfIDs){
      # if(verbose) cat("Process ID ",x[idRun]," (",idRun,")...", sep="")
      
      uniparcURL <- paste('https://www.uniprot.org/uniparc/', x[idRun], '.tab', sep="")
      
      if(url.exists(uniparcURL)){
        if(verbose) cat("found! \n")
        uniparcIn <- read.table(uniparcURL, sep="\t", header=TRUE, stringsAsFactors=FALSE)
      } 
      
      # Extract the Sequence
      tmp <- uniparcIn[grep("UniProt",uniparcIn$Database),]
      uniprotID[idRun,] <- c(x[idRun],tmp$Identifier[1])
    }
  }  
  # Return the result
  uniprotID
}