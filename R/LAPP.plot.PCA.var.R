#' Plot variances for LAPP PCA
#'
#' \code{LAPP.plot.PCA.var()} takes in a modified PCA object returned by LAPP.PCA() and plots the
#'  percentage of variance explained by each principal component.
#'  Uses ggplot2.
#'
#' @param pca modified PCA object returned by LAPP.PCA().
#'
#' @param top.perc top percentage of variance explained to display on graph. Default is 90\%. 
#'
#' @param n number of eigenvalues to use. (optional)
#'
#' @export

LAPP.plot.PCA.var = function(pca, top.per= 100, n= NA) {
  
  # use top percent
  if(is.na(n)){
    reduced.eigs= pca$eig[!c(pca$eig$`cumulative percentage of variance` > top.per),]
    
    # use top n
  } else {
    reduced.eigs= pca$eig[1:n,]
  }

  # upper y axis limit for plot
  upper.y= max(reduced.eigs$`percentage of variance`) * 1.1
  
  # variance plot
  pca.var.plot = ggplot() + 
    geom_bar(data= reduced.eigs, width=1, stat = "identity", col= "blue", fill= "gray80",
             aes(x = 1:nrow(reduced.eigs), y = `percentage of variance`)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) + 
    labs(title = paste0("Percentage of Variance of Principal Components\n(", nrow(pca$eig), " PCs)"), 
         x = "Principal Component Number", y = "Percentage of Variance") + 
    theme(title = element_text(face = "bold", angle = 0),
          axis.text = element_text(face = "bold", angle = 0),
          axis.line= element_blank(),
          panel.grid = element_blank(), panel.background = element_blank())
    
  
  return(pca.var.plot)
}
