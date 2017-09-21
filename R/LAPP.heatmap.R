#' Plot or save heatmap for a matrix
#'
#' \code{LAPP.heatmap()} creates a heatmap using pheatmap() for either plotting or saving
#'
#' @param matrix data to be plotted
#'
#' @param min minimum value to be used on the heatmap
#' 
#' @param max maximum value to be used on the heatmap
#'
#' @param do.cluster boolean to determine whether the heatmaps should be clustered
#'
#' @param file.dir file for the heatmap to be plotted as a pdf onto. If left empty, the heatmap
#' is plotted to the console instead.
#'
#' @return list of pairwise distance matrices for each feature, as well as a cumulative one
#'
#' @export

LAPP.heatmap= function(data.mat, min, max, do.cluster= FALSE, file.dir= NA){
  
  # force all entries within bounds [min, max]
  data.trim= data.mat
  data.trim[which(data.trim > max)] = max
  data.trim[which(data.trim < min)] = min
  
  
  # make heatmap color scale
  heat.scale = seq(min, max, length.out= 201)
  colors= NA
  
  if(min + max == 0){
    colors= colorRampPalette(rev(brewer.pal(n = 11, name = "RdBu")))(length(heat.scale)) 
  } else {
    colors= colorRampPalette(brewer.pal(n = 9, name = "Greys"))(length(heat.scale))
  }
  
  cell.length= max(250/nrow(data.mat), 10)
  
  if(is.na(file.dir)){
    
    heatmap= pheatmap(data.trim, color = colors,
                      breaks= heat.scale, cellwidth= cell.length, cellheight= cell.length,
                      cluster_rows= do.cluster, cluster_cols= do.cluster,
                      border_color = "gray50")
    
  } else {
    
    pdf(file.dir, width=10, height=10, onefile= FALSE)
    
    pheatmap(data.trim, color = colors,
             breaks= heat.scale, cellwidth= cell.length, cellheight= cell.length,
             cluster_rows= do.cluster, cluster_cols= do.cluster,
             border_color = "gray50")
    
    dev.off()
    
  }
  
}
