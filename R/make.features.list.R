#' Create list of feature arrays using a list of list of ligands and a conversion matrix using make.features().
#'
#' \code{make.features.list()} takes in a list of ligands from read.ligand.files() and
#' returns a list of feature arrays using make.features().
#'
#' @param ligands.list List of list of HLA allele ligand sequences created by read.lig.files().
#'
#' @param AA.tab Feature conversion matrix.
#'
#' @param n.pos Length of peptides (number of amino acids) in the list of ligands \code{ligands}
#' to convert into features. Defaults to nonamers (9).
#'
#' @return a list of a three-dimensional arrays with peptide sequence, amino acid position,
#' and amino acid property as the dimensions
#'
#' @export

make.features.list = function(ligands.list, AA.tab, n.pos) {

  features.total= lapply(1:length(ligands.list), FUN= function(x){

    start.time= proc.time()[3]
    aa= make.features(ligands.list[[x]], AA.tab, 9)
    end.time= proc.time()[3]

    print(paste(names(ligands.list)[x], "feature conversion completed in",
                round(end.time - start.time, 2), "seconds")) # message just to see progress
    return(aa)
  } )

  names(features.total)= names(ligands.list)

  return(features.total)
}
