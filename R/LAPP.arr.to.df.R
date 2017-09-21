#' Convert list of 3D arrays into a 2D data.frame
#'
#' \code{LAPP.arr.to.df()} takes in a list of array from make.features.list() and 
#' converts it into a 2D data.frame with sequences as rows for all HLA, and 
#' position-by-property features as columns
#'
#' @param features list of 3D arrays from make.features.list().
#'
#' @return a 2D data.frame
#' 
#' @export

LAPP.arr.to.df = function(features) {

  # preallocate space for 2D matrix: sequence by pos*prop features
  features.hla.total= data.frame()
  
  for(i in 1:length(features)){
    features.hla= features[[i]]
    
    features.hla= adply(features.hla, c(2, 3))
    rownames(features.hla)= paste0("F.", features.hla[,1], ".", features.hla[,2])    
    features.hla= data.frame(t(features.hla[,-c(1,2)]))
    
    features.hla$sequence= rownames(features.hla)
    features.hla$HLA= rep(names(features)[i], nrow(features.hla))
    
    features.hla.total= rbind(features.hla.total, features.hla)
  }
  return(features.hla.total)
}
