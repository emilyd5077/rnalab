## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(rnalab)

## -----------------------------------------------------------------------------
## GC Content ##
add_gc_content(dnaseqs,'sequence')

## Longest mononucleotide stretch ##
add_mono_nucleotide_length(dnaseqs,'sequence')

## Length of the sequence ##
add_sequence_length(dnaseqs,'sequence')

## ----fig.height=10, fig.width=10----------------------------------------------
rnalab_hist_plot(dnaseqs, c('length', 'yield'), 100)

## ----fig.height=5, fig.width=10-----------------------------------------------
rnalab_scatterplot(dnaseqs, x = 'length', y = 'yield', fit = TRUE)

rnalab_scatterplot(dnaseqs, x = 'date', y = 'purity', fit = FALSE)

## ----eval=FALSE---------------------------------------------------------------
#  
#  runRNAapp()
#  

