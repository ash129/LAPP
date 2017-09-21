#' Create a converted three-dimensional feature array using a list of ligands and a conversion matrix
#'
#' \code{make.features()} takes in a list of ligands from one HLA and returns
#' a three dimensional converted feature array of the HLA's ligands using a conversion matrix.
#' The dimensions in order are sequence, position, and property.
#'
#' @param ligands list of HLA peptide sequences from one HLA allele.
#'
#' @param AA.tab feature conversion matrix.
#'
#' @param n.pos length of peptides (number of amino acids) in the list of ligands \code{ligands}
#' to convert into features. Defaults to nonamers (9).
#'
#' @return a three-dimensional array with peptide sequence, amino acid position, and amino acid property
#' as the dimensions
#'
#' @export

make.features = function(ligands, AA.tab, n.pos= 9) {

  # number of amino acid properties
  n.prop = ncol(AA.tab)

  # all n-mer sequences
  nmers = ligands[which(sapply(ligands, nchar) == n.pos)]

  # number of n-mer sequences
  n.seq = length(nmers)

  # blank array of sequence-by-position-by-property entries
  features= array(dim= c(n.seq, n.pos, n.prop), dimnames= list(nmers, 1:n.pos, colnames(AA.tab)))

  # for every AA position in the n-mer peptides...
  for (pos in 1:n.pos) {
    # for every AA property...
    for (prop in 1:n.prop) {
      # for every n-mer peptide sequence...
      for (seq in 1:n.seq) {
        # match the numeric property to the amino acid
        AA.index = match(substr(nmers[seq], pos, pos), rownames(AA.tab))
        features[seq, pos, prop] = AA.tab[AA.index, prop]
      }
    }
  }

  return(features)
}
