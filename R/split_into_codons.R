#' Split a nucleotide sequence into codons
#'
#' Splits a nucleotide sequence into consecutive groups of three
#' nucleotides, starting at the specified position.
#'
#' @param sequence A character string containing a nucleotide sequence.
#' @param start An integer specifying the starting position for
#'   codon extraction. Defaults to 1.
#'
#' @return A character vector containing the extracted codons.
#' @export
#'
#' @examples
#' split_into_codons("ATGCCCTAA")
#' split_into_codons("AATGCCCTAA", start = 2)
split_into_codons <- function(sequence, start = 1){
  sequence_length <- nchar(sequence)
  codons <- substring(sequence,
                      first = seq(from = start, to = sequence_length-3+1, by = 3),
                      last = seq(from = 3+start-1, to = sequence_length, by = 3))
  return(codons)
}
