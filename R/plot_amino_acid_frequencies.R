#' Plot frequency of amino acids across all input sequences
#'
#' Generates a bar plot displaying the frequency of each amino acid
#' across all input sequences.
#'
#' @param sequences A string or character vector of amino acid sequences.
#'
#' @return A ggplot2 object containing an amino acid frequency plot
#'
#' @import ggplot2 stringr
#'
#' @examples
#' plot_amino_acid_frequencies("RSLMPILRKFYDQRPKAKPM")
#' plot_amino_acid_frequencies(c("RSLMPILRKFYDQRPKAKPM", ...))
#'
#' @export
plot_amino_acid_frequencies <- function(sequences) {
  # Shred all sequences into a flat list of characters
  # and remove duplicates, keeping only unique amino acids
  unique_aa <- sequences |>
    stringr::str_split(
      pattern = stringr::boundary("character"),
      simplify = TRUE
    ) |>
    as.character() |>
    unique()

  # Count each occurrence of each unique amino acid across all sequences
  counts <- sapply(
    unique_aa,
    function(amino_acid) {
      stringr::str_count(
        string = sequences,
        pattern = amino_acid
      )
    }
  ) |>
    as.data.frame()

  # Tidy the 'counts' object into a ggplot2-appropriate format by
  # moving row names into a dedicated column naming columns appropriately
  colnames(counts) <- c("Counts")
  counts[["Sequence"]] <- rownames(counts)

  # Visualize the data in the 'counts' table as a bar plot
  frequency_plot <- counts |>
    ggplot2::ggplot(ggplot2::aes(x = Sequence, y = Counts, fill = Sequence)) +
    ggplot2::geom_col() +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none")

  return(frequency_plot)
}
