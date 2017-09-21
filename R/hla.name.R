#' Simplify two-position HLA allotype names 
#'
#' \code{hla.name()} takes a proper HLA name and simplifies it down to a two position abbreviation
#' to get rid of special characters
#'
#' @param allele character representing the hla name
#'
#' @return new abreviated form of the hla name that is safe to use in file names
#' 
#' @export


hla.name= function(allele){
  return(paste0(substring(allele, 1, 1),
                ".", substring(allele, 3, 4),
                ".", substring(allele, 6, 7), collapse= ""))
}
