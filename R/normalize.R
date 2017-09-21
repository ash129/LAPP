#' Normalize feature data and remove uninformative features
#'
#' \code{normalize()} takes a data.frame and normalizes all numeric columns, as well as
#' dropping constant ones. 
#'
#' @param data data.frame with at least one numeric column
#'
#' @return new list with data.frame with normalized numeric columns and dropped constant numeric columns
#' and other information used to scale the data
#' 
#' @export

normalize= function(data){
  
  data.classes= sapply(data, FUN= function(x) class(x))
  data.num= data[,data.classes == "numeric"]
  data.cat= data[,data.classes != "numeric"]
  
  data.scale= scale(data.num)
  data.scale.scale= attr(data.scale, "scaled:scale")
  data.scale.center= attr(data.scale, "scaled:center")
  data.scale.drop.inds= data.scale.scale == 0
  data.scale.drop= data.scale[,!data.scale.drop.inds]
  data.scale.final= cbind(data.scale.drop, data.cat)
  
  return.list= list(data= data.frame(data.scale.final),
                    info= list(
                      center= data.scale.center,
                      scale= data.scale.scale,
                      drop= data.scale.drop.inds))
  
  return(return.list)
  
}