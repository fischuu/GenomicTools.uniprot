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
  # Input formatting
  x <- as.character(x)
  
  # Prepare for dryrun
  if(dryrun){
    fastaSeq <- ""
  } else {
    # Read in the html source
    numberOfSequences <- length(x)
    if(verbose) cat("Contact UniProtDB...\n")
    fastaSeq <- vector(mode="character", length=numberOfSequences)
    for(seqRun in 1:numberOfSequences){
      if(verbose) cat("Process ID ",x[seqRun]," (",seqRun,")...", sep="")
      
      uniprotURL <- paste('https://www.uniprot.org/uniprot/', x[seqRun],".fasta", sep="")
      
      if(url.exists(uniprotURL)){
        if(verbose) cat("found! \n")
        fastaIn <- readLines(uniprotURL, warn=FALSE)
      } else {
        # Do I first query, if the ID is in Uniprot or not, or if we need to go to a subpage      
        tryError <- try(htmltab(paste("https://www.uniprot.org/uniprot/?query=", x[seqRun], sep=""), which=1, rm_nodata_cols=FALSE), silent=TRUE)
        
        ifelse("try-error" %in% class(tryError), readInThis <-x[seqRun], readInThis <- tryError[1,2])
        
        uniprotURL <- paste('https://www.uniprot.org/uniprot/', readInThis,".fasta", sep="")
        
        # If the uniprotID exists, fetch the data, otherwise try to find it via Uniparc
        if(url.exists(uniprotURL)){
          if(verbose) cat("found! \n")
          fastaIn <- readLines(uniprotURL, warn=FALSE)
        } else {
          x[seqRun] <- sapply(strsplit(x[seqRun],"\\."),"[",1)
          if(verbose) cat("not found, try to use UniParc ...")
          tryError <- try(htmltab(paste("https://www.uniprot.org/uniparc/?query=", x[seqRun], sep=""), which=1, rm_nodata_cols=FALSE), silent=TRUE)
          
          if(sum(is.element("Entry",colnames(tryError)))==0){
            readInThis <-x[seqRun]
          } else {
            ifelse("try-error" %in% class(tryError), readInThis <-x[seqRun], readInThis <- tryError[1,2])
          }
          
          # If Protein was found via UniParc, get the sequence from there
          uniparcURL <- paste('https://www.uniprot.org/uniparc/', readInThis, ".fasta", sep="")
          
          if(url.exists(uniparcURL)){
            if(verbose) cat("found! \n")
            fastaIn <- readLines(uniparcURL, warn=FALSE)
          } else {
            if(verbose) cat("not found... Return emtpy sequence! \n")
            fastaSeq[seqRun] <- ""
          }
        }
      }
      # Extract the Sequence
      fastaSeq[seqRun] <- paste(fastaIn[-1],collapse="")
      names(fastaSeq)[seqRun] <- fastaIn[1]
    }
  }  
  # Return the result
  class(fastaSeq) <- "fa"
  fastaSeq
}

###################################################################
###################################################################
###
###      TESTING AREA
###################################################################
###################################################################

uniprotID2Sequence.old <- function(x, dryrun=FALSE, verbose=TRUE){
  # Input formatting
  x <- as.character(x)
  
  # Prepare for dryrun
  if(dryrun){
    fastaSeq <- ""
  } else {
    # Read in the html source
    numberOfSequences <- length(x)
    if(verbose) cat("Contact UniProtDB...\n")
    fastaSeq <- vector(mode="character", length=numberOfSequences)
    for(seqRun in 1:numberOfSequences){
      if(verbose) cat("Process ID ",x[seqRun]," (",seqRun,")...", sep="")
      
      uniprotURL <- paste('https://www.uniprot.org/uniprot/', x[seqRun], sep="")
      
      if(url.exists(uniprotURL)){
        if(verbose) cat("found! \n")
        htmlIn <- readLines(uniprotURL, warn=FALSE)
        # Grep the sequence start and end postion in the vector
        seqStart <- grep("When browsing through different UniProt", htmlIn) + 1
        seqEnd <- grep("Align this entry with its isoforms.", htmlIn) - 1
        
        seqStart <- max(seqStart)
        seqEnd <- min(seqEnd)
        
      } else {
        # Do I first query, if the ID is in Uniprot or not, or if we need to go to a subpage      
        tryError <- try(htmltab(paste("https://www.uniprot.org/uniprot/?query=", x[seqRun], sep=""), which=1, rm_nodata_cols=FALSE), silent=TRUE)
        
        ifelse("try-error" %in% class(tryError), readInThis <-x[seqRun], readInThis <- tryError[1,2])
        
        uniprotURL <- paste('https://www.uniprot.org/uniprot/', readInThis, sep="")
        
        # If the uniprotID exists, fetch the data, otherwise try to find it via Uniparc
        if(url.exists(uniprotURL)){
          if(verbose) cat("found! \n")
          htmlIn <- readLines(uniprotURL, warn=FALSE)
          # Grep the sequence start and end postion in the vector
          seqStart <- grep("When browsing through different UniProt", htmlIn) + 1
          seqEnd <- grep("Align this entry with its isoforms.", htmlIn) - 1
          
        } else {
          x[seqRun] <- sapply(strsplit(x[seqRun],"\\."),"[",1)
          if(verbose) cat("not found, try to use UniParc ...")
          tryError <- try(htmltab(paste("https://www.uniprot.org/uniparc/?query=", x[seqRun], sep=""), which=1, rm_nodata_cols=FALSE), silent=TRUE)
          
          if(sum(is.element("Entry",colnames(tryError)))==0){
            readInThis <-x[seqRun]
          } else {
            ifelse("try-error" %in% class(tryError), readInThis <-x[seqRun], readInThis <- tryError[1,2])
          }
          
          # If Protein was found via UniParc, get the sequence from there
          uniparcURL <- paste('https://www.uniprot.org/uniparc/', readInThis, sep="")
          
          if(url.exists(uniparcURL)){
            if(verbose) cat("found! \n")
            htmlIn <- readLines(uniparcURL, warn=FALSE)
            seqStart1 <- grep("A search result page is subdivided", htmlIn) + 1
            seqStart2 <- grep("When browsing through different UniProt", htmlIn) + 1
            
            seqEnd1 <- grep("Align this entry with its isoforms.", htmlIn) - 1
            seqEnd2 <- grep("This entry is in your", htmlIn) - 1
            
            seqStart <- max(c(seqStart1, seqStart2))
            seqEnd <- min(c(seqEnd1, seqEnd2))
            
          } else {
            if(verbose) cat("not found... Return emtpy sequence! \n")
            fastaSeq[seqRun] <- ""
          }
        }
      }
      # Extract the Sequence
      if(seqStart>0 & seqEnd < length(htmlIn)){
        fastaSeq[seqRun] <- as.character(paste(htmlIn[seqStart:seqEnd],collapse=""))        
      } else {
        if(verbose) cat("sequence detection problem... Return emtpy sequence! \n")
        fastaSeq[seqRun] <- ""
      }
      
    }
  }  
  # Return the result
  names(fastaSeq) <- paste(">",x,sep="")
  class(fastaSeq) <- "fa"
  fastaSeq
}


###################################################################
