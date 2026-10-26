#' Generate a Random DNA Sequence
#'
#' Generates a random DNA sequence of a specified length using the four
#' standard DNA nucleotides: A, T, G, and C.
#'
#' @param sequence_length Number of nucleotides in the generated DNA sequence.
#'
#' @return A character string containing a random DNA sequence.
#'
#' @examples
#' generate_dna(10)
#'
#' @export
generate_dna <- function(sequence_length) {
  dna_bases <- sample(
    c("A", "T", "G", "C"),
    size = sequence_length,
    replace = TRUE
  )

  dna_sequence <- paste0(
    dna_bases,
    collapse = ""
  )

  return(dna_sequence)
}
