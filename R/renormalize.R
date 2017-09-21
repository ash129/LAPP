#' Normalize feature data and drop features according to how another dataset was normalized
#'
#' \code{renormalize()} takes a data.frame and normalizes all numeric columns, as well as
#' dropping some, according to how a previous dataset was normalized through normalize().
#'
#' @param data data.frame with at least one numeric column
#'
#' @param info normalizing info given from normalize()
#' 
#' @return new list with data.frame with normalized numeric columns and dropped constant numeric columns
#' and other information used to scale the data
#' 
#' @export


renormalize= function(data, info){
  
  data.classes= sapply(data, FUN= function(x) class(x))
  data.num= data[,data.classes == "numeric"]
  data.cat= data[,data.classes != "numeric"]
  
  data.new= data.num
  
  if(!is.na(info$center[1])){
    data.new= sapply(1:length(info$center), FUN= function(x) {data.new[,x] - info$center[x]})
  }
  
  if(!is.na(info$scale[1])){
    data.new= sapply(1:length(info$scale), FUN= function(x) {data.new[,x] / info$scale[x]})
  }
  
  colnames(data.new)= colnames(data.num)
  
  if(!is.na(info$drop[1])){
    data.new= data.new[,!info$drop]
  }
  
  data.new.final= cbind(data.new, data.cat)
  
  return(data.frame(data.new.final))
}