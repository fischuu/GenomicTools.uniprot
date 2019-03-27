#' Get Gene name from UniprotIDr
#'
#' @param x UniprotID
#' @param dryrun logical, do not execute the function
#' @param verbose logical for function feedback
#' @return A Gene Name
#' @examples
#' uniprotID2name("EKY21295", dryrun=TRUE)
#' @export
#' 
uniprot2name <- function(x, dryrun=FALSE, verbose=TRUE){
  # Input formatting
  x <- as.character(x)
  
  # Prepare for dryrun
  if(dryrun){
    uniprotID <- ""
  } else {
    # Read in the html source
    numberOfIDs <- length(x)
    if(verbose) cat("Contact Uniprot DB...")
    uniprotID <- matrix("NA", nrow=numberOfIDs, ncol=2)
    colnames(uniprotID) <- c("UniprotID", "GeneName")
    for(idRun in 1:numberOfIDs){
       uniprotURL <- paste('https://www.uniprot.org/uniprot/?query=',x[idRun],'&format=tab&columns=id,genes', sep="")
      
      if(url.exists(uniprotURL)){
        if(verbose) cat(idRun,": ",x[idRun], " found! \n")
        fetchURL <- function(urlIn){
          out <- try(read.table(urlIn, sep="\t", header=TRUE, stringsAsFactors=FALSE))
          if(inherits(out, "try-error"))
            return(NULL)
          else
            return(out)
        }
        geneNameIn <- fetchURL(uniprotURL)
        
        # Extract the Sequence
        tmp <- geneNameIn[1,2]
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