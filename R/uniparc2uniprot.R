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
      
      #uniparcURL <- paste('https://www.uniprot.org/uniparc/', x[idRun], '.tab', sep="")
       uniparcURL <- paste('https://www.uniprot.org/uniparc/?query=',x[idRun],'&format=tab&limit=10&columns=id,organisms,kb,first-seen,last-seen,length&sort=score', sep="")
      
      if(url.exists(uniparcURL)){
        if(verbose) cat(idRun,": ",x[idRun], " found! \n")
        fetchURL <- function(urlIn){
          out <- try(read.table(urlIn, sep="\t", header=TRUE, stringsAsFactors=FALSE))
          if(inherits(out, "try-error"))
            return(NULL)
          else
            return(out)
        }
        uniparcIn <- fetchURL(uniparcURL)
        
        # Extract the Sequence
        tmp <- uniparcIn$UniProtKB
        uniprotID[idRun,] <- c(x[idRun],tmp[1])
      } else {
        if(verbose) cat(x[idRun], " NOT found! \n")
        
        uniprotID[idRun,] <- c(x[idRun], "NA")
      }
    }
    if(verbose) cat("Done!!!")
  }  
  # Return the result
  uniprotID
}