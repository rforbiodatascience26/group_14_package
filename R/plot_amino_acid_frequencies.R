#' Plot frequency of amino acids across all input sequences
#'
#' Generates a bar plot displaying the frequency of each amino acid
#' across all input sequences.
#'
#' @param sequence A string containing an amino acid sequence.
#'
#' @return A ggplot2 object containing an amino acid frequency plot
#'
#' @import ggplot2 stringr
#'
#' @examples
#' plot_amino_acid_frequencies("RSLMPILRKFYDQRPKAKPM")
#'
#' @export
plot_amino_acid_frequencies <- function(sequence) {
  # Shred the input sequence into a list of characters
  # and remove duplicates, keeping only unique amino acids
  unique_aa <- sequence |>
    stringr::str_split(
      pattern = stringr::boundary("character"),
      simplify = TRUE
    ) |>
    as.character() |>
    unique()

  # Count each occurrence of each unique amino acid
  counts <- sapply(
    unique_aa,
    function(amino_acid) {
      stringr::str_count(
        string = sequence,
        pattern = amino_acid
      )
    }
  ) |>
    as.data.frame()

  # Tidy the 'counts' object into a ggplot2-appropriate format by
  # moving row names into a dedicated column naming columns appropriately
  colnames(counts) <- c("Counts")
  counts[["Amino acid"]] <- rownames(counts)

  # Visualize the data in the 'counts' table as a bar plot
  frequency_plot <- counts |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = `Amino acid`,
        y = Counts,
        fill = `Amino acid`
      )
    ) +
    ggplot2::geom_col() +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none")

  return(frequency_plot)
}
