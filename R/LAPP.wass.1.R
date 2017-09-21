#' Calculate the cumulative first Wasserstein metric over features of peptidomes
#'
#' \code{LAPP.wass.1()} calculates pairwise first Wasserstein metric for each of the features
#' of a peptidome dataset on all HLA, and also creates a final cumulative Wasserstein distance matrix using the
#' Euclidean distance across all metrics calculated. The function will output all distance matrices calculated.
#' If requested, the function will also save pdf files of heatmaps of all matrices to a file directory.
#'
#' @param features.df features data.frame from LAPP.arr.to.df()
#'
#' @param plot.dir file directory in which to save all plots
#'
#' @return list of pairwise distance matrices for each feature, as well as a cumulative one
#'
#' @export

LAPP.wass.1= function(features.df, plot.dir= NA){

  # just the numbers (no labels)
  features.prop.data.df= subset(features.df, select= -c(sequence, HLA))

  # features split by HLA
  features.prop.df.split= split(features.prop.data.df, features.df$HLA)

  n.feats= ncol(features.prop.data.df)

  n.hla= length(features.prop.df.split)

  # list of all comparison matrices
  comp.mat.list= list()

  # for every feature...
  for(feat.ind in 1:n.feats){

    feat.name= colnames(features.prop.data.df)[feat.ind]

    # matrix holding all of the distances for feature 'feat'
    comp.mat= matrix(NA, nrow= n.hla, ncol= n.hla)
    colnames(comp.mat)= names(features.prop.df.split)
    rownames(comp.mat)= names(features.prop.df.split)


    # for every pairwise HLA comparison...
    for(hla1.ind in 1:n.hla){
      for(hla2.ind in 1:n.hla){

        # that feature for just two HLAs
        hla1= features.prop.df.split[[hla1.ind]][, feat.ind]
        hla2= features.prop.df.split[[hla2.ind]][, feat.ind]

        hla1.len= length(hla1)
        hla2.len= length(hla2)

        # First Wasserstein's metric calculation
        # (aka Earth Movers's Distance in CS)
        comp.mat[hla1.ind, hla2.ind]= wass.1(hla1, hla2)

      }
    }

    comp.mat.list[[feat.ind]]= comp.mat

    print(paste0(c(feat.name, " distances completed"), collapse= ""))
  }

  # Euclidean average matrix
  comp.mat.list.sq= lapply(1:length(comp.mat.list), FUN= function(x) { comp.mat.list[[x]]^2 } )
  avg.mat= sqrt(Reduce("+", comp.mat.list.sq))


  # output plots, if requested
  if(!is.na(plot.dir)){

    # max distance, for color scales
    # max.d= max(sapply(comp.mat.list, max)) # true max of all feature matrices
    max.d= 2 # this is a good number, empirically from most heatmaps I've made

    # individual heatmaps
    for(feat.ind in 1:n.feats){

      feat.name= colnames(features.prop.data.df)[feat.ind]

      # feature matrix
      comp.mat= comp.mat.list[[feat.ind]]

      file.dir= paste0(plot.dir, feat.name, ".pdf")
      
      # output heatmap plot
      LAPP.heatmap(comp.mat, min= 0, max= max.d, do.cluster= TRUE, 
                   file.dir= file.dir)
      
    }

    ## cumulative heatmap

    # max distnace, for color scales
    max.avg.d= max(avg.mat) # use max distance seen here

    file.dir= paste0(plot.dir, "combined.pdf")
    
    # output heatmap plot
    LAPP.heatmap(avg.mat, min= 0, max= max.avg.d, do.cluster= TRUE, 
                 file.dir= file.dir)
    
  }


  ## return list of matrices

  return(list(ind.mats= comp.mat.list, avg.mat= avg.mat))

}

