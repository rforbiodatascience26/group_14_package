# Takes a random DNA sequence and substitute T for U (from DNA string to RNA)

#' dna_to_rna
#'
#' @param dna_sequence Random DNA sequence
#'
#' @returns A RNA sequence (DNA seq given with substitution T -> U)
#' @export
#'
#' @examples
#' dna_to_rna("ATGC")
#'
dna_to_rna <- function(dna_sequence){
  rna_sequence <- gsub("T", "U", dna_sequence)
  return(rna_sequence)
}
