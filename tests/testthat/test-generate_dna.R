test_that("generate_dna returns a DNA sequence of the requested length", {
  dna_sequence <- generate_dna(10)

  expect_equal(
    nchar(dna_sequence),
    10
  )

  expect_true(
    grepl(
      "^[ATGC]+$",
      dna_sequence
    )
  )
})
