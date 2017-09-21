#' Perform tSNE on HLA features
#'
#' \code{LAPP.tSNE()} takes in a data.frame from make.features.list() and performs tSNE.
#' The results are a modified form of results from Rtsne() from the Rtsne package.
#'
#' @param features data.frame object returned by make.features.list().
#'
#' @param iterations Number of iterations for tSNE. Larger datasets mean longer runtime. Default is 1000.
#'
#' @export

LAPP.tSNE = function(features, iters = 1500, perplexity= 50) {
  
  # features.hla.total has 
  # columns: n.prop*n.pos features + sequence + HLA
  # rows: total n.seq across all HLA in features
  features.hla.total= features

  # just the features
  features.data = subset(features.hla.total, select = -c(sequence, HLA))
  
  # drop any constant features with no information (useless for t-SNE)
  drop.index = apply(features.data, 2, var, na.rm = TRUE) == 0
  features.drop = features.data[, !drop.index]
  
  # manually scale features
  features.scale = scale(features.drop)
  
  # run the 2D tSNE
  set.seed(129)
  tsne = Rtsne(features.scale, dims = 2, perplexity = perplexity, verbose = TRUE, max_iter = iters, check_duplicate = FALSE)
  
  # add additional info
  tsne$sequence= features.hla.total$sequence 
  tsne$HLA = features.hla.total$HLA 
  tsne$nfeats= ncol(features.scale) # number of features used
  
  return(tsne)
}

