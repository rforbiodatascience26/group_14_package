
test_that("split_into_codons splits a sequence into codons", {
  expect_equal(
    split_into_codons("ATGCCCTAA"),
    c("ATG", "CCC", "TAA")
  )
})

test_that("split_into_codons respects the starting position", {
  expect_equal(
    split_into_codons("AATGCCCTAA", start = 2),
    c("ATG", "CCC", "TAA")
  )
})

test_that("split_into_codons handles a sequence with one codon", {
  expect_equal(
    split_into_codons("ATG"),
    "ATG"
  )
})
