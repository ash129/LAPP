#' Plot PCA contributions
#'
#' \code{LAPP.plot.PCA.con()} takes in a modified PCA object returned by LAPP.PCA() and plots the
#' variable contributions of two principal components.
#'  Uses ggplot2 and ggrepel.
#'
#' @param pca Modified PCA object returned by LAPP.PCA().
#'
#' @param dim1 Index of PC to plot along the x-axis. Defaults to 1
#'
#' @param dim2 Index of PC to plot along the y-axis. Defaults to 2
#'
#' @param top.n Number of top contributions to highlight on the plot. Defaults to 10.
#'
#' @export


LAPP.plot.PCA.con= function(pca, dim1= 1, dim2= 2, top.n= 10){

  # get highest contributions for the two PCs
  pca.var= data.frame(pca$var$coord[,c(dim1, dim2)])
  colnames(pca.var)= c("dim1", "dim2")
  pca.var$zero= rep(0, nrow(pca$var$coord))
  pca.var$combined= sqrt(pca.var$dim1^2 + pca.var$dim2^2)

  # change name for plot
  pca.var$name= row.names(pca.var)
  pca.var$name= gsub("\\.", " ", pca.var$name)
  pca.var$name= gsub("F", "P", pca.var$name)

  pca.var.top= pca.var[order(abs(pca.var$combined), decreasing= TRUE)[1:top.n],]

  # set plot boundaries
  con.max.d1=  1 # * max(abs(pca.var$dim1))
  con.max.d2=  1 # * max(abs(pca.var$dim2))

  # sequence length
  n= nchar(as.character(pca$sequence[1]))

  # make circle
  circle.n= 1000
  circle.t= seq(0, 2*pi, length.out= circle.n)
  circle.x= cos(circle.t); circle.y= sin(circle.t)
  circle= geom_path(data= data.frame(x= circle.x, y= circle.y), aes(x, y), col= "red", linetype= "dashed")
  
  # create plot
  plot.contrib= ggplot() + circle + 
    ggtitle(paste0("Feature contributions for PCA on ",
                   n, "mers")) +
    geom_segment(data= pca.var, aes(x= zero, y= zero, xend= dim1, yend= dim2),
                 col= "gray", size= 1, lty= 1, arrow= arrow(length= unit(0.03, "npc"))) +
    geom_segment(data= pca.var.top, aes(x= zero, y= zero, xend= dim1, yend= dim2),
                 col= "blue", size= 1, lty= 1, arrow= arrow(length= unit(0.03, "npc"))) +
    geom_label_repel(data= pca.var.top, mapping= aes(x= dim1, y= dim2, label= name), size=5,
                     point.padding= unit(0, "lines"), segment.size= 1, label.size= 0.5) +
    scale_x_continuous(limits=c( -con.max.d1 , con.max.d1 )) +
    scale_y_continuous(limits=c( -con.max.d2 , con.max.d2 )) +
    xlab(paste0("PC", dim1, ": ", round(pca$eig$per[dim1], digits= 4), "%")) +
    ylab(paste0("PC", dim2, ": ", round(pca$eig$per[dim2], digits= 4), "%")) +
    theme(title = element_text(face = "bold", angle = 0),
          axis.text = element_text(face = "bold", angle = 0),
          axis.line= element_blank(),
          panel.grid = element_blank(), panel.background = element_blank())


  
  return(plot.contrib)
}




