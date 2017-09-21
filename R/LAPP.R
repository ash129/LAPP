#' LAPP
#'
#' Ligand Analysis with Physicochemical Properties (LAPP) uses position by property features of HLA ligand sequences for their statistical analysis.
#' The package includes functions to translate HLA ligand sequences into pseudo-continuous features.
#' LAPP also provides statistical tests to assess how ligand sequences from different HLA differ from one another, as well as from random non-ligand peptide sequeces.
#' The package also includes wrapper code to create PCA scatterplots and heatmaps.
#'
#' @name LAPP
#' @docType package
#' @import ggplot2 RColorBrewer ColorPalette GGally plyr pheatmap FactoMineR Rtsne nnet PRROC
NULL

#' Amino acid conversion matrix with 23 properties
#'
#' Conversion matrix used to transform n-mer ligand sequences into 23 * n position-by-property features.
#' 3 properties are physicochemical: molecular weight, surface area, hydrophobicity index, and isoelectric point.
#' 20 properties describe identity of the amino acid at a position.
#'
#' @docType data
#' @keywords datasets
#' @name convmat.23
#' @usage data(convmat.23)
#' @format A data frame with 20 rows (amino acids) and 23 columns (properties)
#' @references \url{to do}
NULL

#' Randomly selected nonamer sequences from Uniprot
#'
#' 100,000 nonamer (9mer) amino acid sequences sampled from the entire Uniprot database (October 2016).
#' These sequences serve as useful null controls for statistical analyses in LAPP. 
#'
#' @docType data
#' @keywords datasets
#' @name rand.9mers
#' @usage data(rand.9mers)
#' @format A character with 100,000 entries
NULL

