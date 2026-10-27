#' DNA to AA translator
#'
#' @param codons DNA triplet that you want to translate, use all caps
#'
#' @returns Returns the AA corresponding to the triplet input
#' @export
#'
#' @examples DNA_to_AA("ATG")
#'
DNA_to_AA <- function(input){
  AA <- paste0(codons[input], collapse = "")
  return(AA)
}
