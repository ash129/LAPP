#' Read in all .txt files in a folder as ligands
#'
#' \code{read.ligand.files()} takes in a file directory and returns a list of lists of each HLA's ligands
#'
#' @param ligand.dir file directory with one or more files of txt files of peptide sequences.
#'      Each file is named after the HLA allele, and one peptide sequence is present per line.
#'
#' @param first.line.name whether to use the first line of the file as the name of the collection of peptides
#' in the file (default). If set to FALSE, uses the name of the file (without the ".txt" extension) instead.
#'
#' @return list of lists of ligands in ligand.dir, where each sublist is named by each file
#'
#' @export

read.ligand.files = function(ligand.dir, first.line.name= TRUE) {

  # list of all .txt file names in ligand.dir
  ligand.files = list.files(path= ligand.dir, full.names = TRUE, pattern = "\\.txt$")

  # list of lists of ligand sequences
  ligands = lapply(ligand.files, FUN= function(x) scan(x, what = "", sep = "\n"))

  if(first.line.name){
    # make name the first entry, get rid of first entries from peptides
    names.ligands = lapply(1:length(ligands), FUN= function(x) return(ligands[[x]][1]))
    ligands= lapply(1:length(ligands), FUN= function(x) return(ligands[[x]][-1]))
    names(ligands)= names.ligands
  } else {
    # drop last 4 characters ('.txt') from file names to create names
    names(ligands) = gsub(".{4}$", "", list.files(ligand.dir))
  }

  # erase duplicated ligands
  for (i in 1:length(ligands)) {
    ligands[[i]] = unique(ligands[[i]])
  }

  # erase ligands with characters that don't represent the 20 amino acids
  forbidden.LETTERS= c("B", "J", "O", "U", "X", "Z")
  valid.chars = LETTERS[!LETTERS %in% forbidden.LETTERS]
  for (i in 1:length(ligands)) {
    ligands[[i]] = ligands[[i]][which(sapply(ligands[[i]], FUN = function(x)
      sum(unlist(strsplit(x, "")) %in% valid.chars) == nchar(x)))]
  }

  return(ligands)
}
