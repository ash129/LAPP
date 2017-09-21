#' Perform PCA on HLA features
#'
#' \code{LAPP.pca()} takes in a data.frame from LAPP.arr.to.df() and performs PCA.
#' The results are a modified form of results from PCA() from the FactoMineR package.
#' Additional information tagged onto the PCA() helps transform new data
#'
#' @param features data.frame object returned by LAPP.arr.to.df().
#'
#' @return a modified PCA object from FactoMineR using the data from \code{features}
#'
#' @export

LAPP.PCA = function(features) {

  # features.hla.total has
  # columns: n.prop*n.pos features + sequence + HLA
  # rows: total n.seq across all HLA in features
  features.hla.total= LAPP.arr.to.df(features)

  # just the features
  features.data = subset(features.hla.total, select = -c(sequence, HLA))

  # drop any constant features with no information (useless for PCA)
  drop.index = apply(features.data, 2, var, na.rm = TRUE) == 0
  features.drop = features.data[, !drop.index]

  # manually scale features
  features.scale = scale(features.drop)

  # perform PCA with FactoMineR
  pca = PCA(features.scale, scale.unit = FALSE, ncp = ncol(features.scale), graph = FALSE)

  # create loading matrix
  pca.loading = sweep(pca$var$coord, 2, sqrt(pca$eig[1:ncol(pca$var$coord), 1]), FUN = "/")

  # add additional features to output, mostly for allocating new points note: these won't be revealed by print.PCA, i.e.,
  # the function that is called to describe what the FactoMineR object contains
  pca$sequence = features.hla.total$sequence
  pca$HLA = features.hla.total$HLA
  pca$drop.inds = drop.index # index of dropped features due to no information, for new data
  pca$centering = attr(features.scale, "scaled:center") # mean used for scaling, for new data
  pca$scaling = attr(features.scale, "scaled:scale") # variance used for scaling, for new data
  pca$loading = pca.loading # loading matrix, for new data

  return(pca)
}
