pkgname <- "rnalab"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "rnalab-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('rnalab')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("add_gc_content")
### * add_gc_content

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: add_gc_content
### Title: Adding the GC content
### Aliases: add_gc_content

### ** Examples

df <- data.frame("id" = c('ID1','ID2') , "Sequnce_DNA" = c('AAAGGGCTTCCC','AGGGGGTTTCCC'))
df_new <- add_gc_content(df, 'Sequnce_DNA')



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("add_gc_content", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("add_mono_nucleotide_length")
### * add_mono_nucleotide_length

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: add_mono_nucleotide_length
### Title: Adding longest mono nucleotide length
### Aliases: add_mono_nucleotide_length

### ** Examples

df <- data.frame("id" = c('id1','id2') , "Sequnce_DNA" = c('AAAGGGCTTCCC','AGGGGGTTTCCC'))
df_new <- add_gc_content(df, 'Sequnce_DNA')



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("add_mono_nucleotide_length", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("add_sequence_length")
### * add_sequence_length

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: add_sequence_length
### Title: Add a column to the existing dataframe
### Aliases: add_sequence_length

### ** Examples

df <- data.frame("id" = c('ID1','ID2') , "Sequnce_DNA" = c('AAAGGGCTTCCC','AGGGGGTTTCCC'))
df_new <- add_sequence_length(df, 'Sequnce_DNA')



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("add_sequence_length", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dnaseqs")
### * dnaseqs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dnaseqs
### Title: Sample DNA Sequence Data
### Aliases: dnaseqs
### Keywords: datasets

### ** Examples

data(dnaseqs)
rnalab::rnalab_hist_plot(data = dnaseqs, vars = list('length', 'yield'), nbins = 100)
rnalab::rnalab_scatterplot(data = dnaseqs, x = 'length', y = 'yield', fit=TRUE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dnaseqs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rnalab_hist_plot")
### * rnalab_hist_plot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rnalab_hist_plot
### Title: Plot Histograms for input data
### Aliases: rnalab_hist_plot

### ** Examples

# Plot the distribution and summary stats for a single variable in the dnaseqs data set
rnalab_hist_plot(dnaseqs, 'length', 100)
# Plot the distribution and summary stats for multiple variables in the dnaseqs data set
rnalab_hist_plot(dnaseqs, c('length', 'yield'), 100)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rnalab_hist_plot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rnalab_scatterplot")
### * rnalab_scatterplot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rnalab_scatterplot
### Title: Plot Scatterplots for input data
### Aliases: rnalab_scatterplot

### ** Examples

# Plot a scatter plot without linear regression added (using data from the dnaseqs data set)
rnalab_scatterplot(dnaseqs, x = 'length', y = 'yield', fit = FALSE)
# Plot a scatter plot with linear regression added (using data from the dnaseqs data set)
rnalab_scatterplot(dnaseqs, x = 'length', y = 'yield', fit = TRUE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rnalab_scatterplot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
