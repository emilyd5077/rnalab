library(rnalab)

## Test 1
df = data.frame("id" = c('ID1','ID2') , 
                "Sequnce_DNA" = c('AAAGGGCTTCCC','AAAGGACTHCGC'))
length_seq = c(12,12)
df_sequence = cbind(length_seq, df)

test_that("Testing the calculation of Length for Sequence", {
  expect_equal(add_sequence_length(df, "Sequnce_DNA"),df_sequence)
})


## Test 2
df2 = data.frame("id" = c('ID1','ID2') , 
                "Sequnce_DNA" = c('AAAGGGCTTCCT','AAAGGACTTCGC'))
gc_content = c(0.5,0.5)
df_sequence_gc = cbind(gc_content, df2)

test_that("Testing the calculation of GC content for Sequence", {
  expect_equal(add_gc_content(df2, "Sequnce_DNA"),df_sequence_gc)
})


## Test 3

df3 = data.frame("id" = c('ID1','ID2') , 
                 "Sequnce_DNA" = c('AAAGGGCTTCCT','AAAGGACTTCGC'))

long_mono_A <- c(3,3)
long_mono_C <- c(2,1)
long_mono_G <- c(3,2)
long_mono_T <- c(2,2)

df_new <- cbind(long_mono_A,long_mono_C,long_mono_G,long_mono_T, df3)

test_that("Testing the calculation of GC content for Sequence", {
  expect_equal(add_mono_nucleotide_length(df3, "Sequnce_DNA"),df_new)
})

