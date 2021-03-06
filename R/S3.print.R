#' Print fa Objects
#' 
#' Prints an \code{fa} object.
#' 
#' The print function displays a fa object
#' 
#' @name print.fa
#' @docType methods
#' @param x Object of class \code{fa}.
#' @param n Number of objects
#' @param seq.out Length of Sequences
#' @param ... Additional parameters
#' @author Daniel Fischer
#' @keywords methods print
#' @export

print.fa <- function(x, n=2, seq.out=50, ...){
  if(!is.numeric(n)) stop("The argument n has to be numeric.")
  if(n>length(x)){
    n <- length(x)
    warning("n cannot be larger than length(x). Hence, I set n <- length(x)")
  }
  X <- x[1:n]
  if(!is.null(seq.out)){
    if(!is.numeric(seq.out)) stop("The argument seq.out has to be numeric.")
    for(i in 1:n){
      X[i] <- paste(substr(X[i],1,seq.out),"...",sep="")
    }
  }
  print(X,...)
  if(n<length(x)) message("Fasta sequences ommited to print: ", length(x)-n)
} 