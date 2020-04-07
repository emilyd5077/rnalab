#' Add a column to the existing dataframe
#' @description Calculate length of the sequence column
#' @usage add_sequence_length(df_sequence, name_seq_col) 
#' @param df_sequence global dataframe
#' @param name_seq_col Name of the Sequence Column
#' @export add_sequence_length add_sequence_length
#' @return df_updated
#' @examples df <- data.frame("id" = c('ID1','ID2') , "Sequnce_DNA" = c('AAAGGGCTTCCC','AGGGGGTTTCCC'))
#' df_new <- add_sequence_length(df, 'Sequnce_DNA')
add_sequence_length = function(df_sequence, name_seq_col) {
  if("length_seq" %in% colnames(df_sequence))
  {
    stop("repeat", call. = FALSE)
  }
  else {
  length_seq = nchar(as.character(df_sequence[[name_seq_col]]))
  
  df_sequence = cbind(length_seq, df_sequence)
  add_gc_content(df_sequence, name_seq_col)
  }
  return(df_sequence)
}


#' Adding the GC content
#' @description Calculate GC content of the sequence column
#' @usage add_gc_content(df_sequence, name_seq_col) 
#' @param df_sequence dataframe
#' @param name_seq_col Name of the sequence column
#' @export add_gc_content add_gc_content
#' @return A new dataframe with existing columns and added column named gc_content
#' @examples df <- data.frame("id" = c('ID1','ID2') , "Sequnce_DNA" = c('AAAGGGCTTCCC','AGGGGGTTTCCC'))
#' df_new <- add_gc_content(df, 'Sequnce_DNA')
add_gc_content = function(df_sequence, name_seq_col) {
  if("gc_content" %in% colnames(df_sequence))
  {
    stop("repeat", call. = FALSE)
  } else {
    DNAStrings <- apply(df_sequence[name_seq_col], 1, Biostrings::DNAString)
    
    Freq_DNAStrings <- lapply(DNAStrings, Biostrings::alphabetFrequency)
    
    gc_content=sapply(Freq_DNAStrings, function(x) sum(x[c("G", "C")])/sum(x[c("A", "C", "G", "T")]))
    
    df_sequence = cbind(gc_content, df_sequence)
  }
  return(df_sequence)
}


#' Adding longest mono nucleotide length
#' @description Calculate the length of the longest consecutive mono nucleotide
#' @usage add_mono_nucleotide_length(df_sequence, name_seq_col) 
#' @param df_sequence Dataframe
#' @param name_seq_col Name of the sequence column
#' @export add_mono_nucleotide_length add_mono_nucleotide_length
#' @return A new dataframe with existing columns and four added columns named - long_mono_A, long_mono_C, long_mono_G,long_mono_T
#' @examples df <- data.frame("id" = c('id1','id2') , "Sequnce_DNA" = c('AAAGGGCTTCCC','AGGGGGTTTCCC'))
#' df_new <- add_gc_content(df, 'Sequnce_DNA')
add_mono_nucleotide_length <- function(df_sequence, name_seq_col) 
{
  if("long_mono_A" %in% colnames(df_sequence))
  {
    stop("repeat", call. = FALSE)
  } else if("long_mono_C" %in% colnames(df_sequence))
  {
    stop("repeat", call. = FALSE)
  } else if("long_mono_G" %in% colnames(df_sequence))
  {
    stop("repeat", call. = FALSE)
  } else if("long_mono_T" %in% colnames(df_sequence))
  {
    stop("repeat", call. = FALSE)
  } else {
    # GC longest mononucleotide 'A'
    long_mono_A <- apply(df_sequence[name_seq_col], 1, function(x) Biostrings::longestConsecutive(x,'A'))
    
    # GC longest mononucleotide 'C'
    long_mono_C <- apply(df_sequence[name_seq_col], 1, function(x) Biostrings::longestConsecutive(x,'C'))
    
    # GC longest mononucleotide 'G'
    long_mono_G <- apply(df_sequence[name_seq_col], 1, function(x) Biostrings::longestConsecutive(x,'G'))
    
    # GC longest mononucleotide 'T'
    long_mono_T <- apply(df_sequence[name_seq_col], 1, function(x) Biostrings::longestConsecutive(x,'T'))
    df_sequence = cbind(long_mono_A, long_mono_C, long_mono_G, long_mono_T, df_sequence)
    add_gc_content(df_sequence, name_seq_col)
  }
  return(df_sequence)
}

#' remove_added_columns
#' @description Internal function to remove any new column added by the App
#' @param df_sequence dataframe
#' @return df_sequence 
remove_added_columns <- function(df_sequence)  {
df_sequence[!names(df_sequence) %in% c("long_mono_A", 
                                                  "long_mono_C", 
                                                  "long_mono_G", 
                                                  "long_mono_T",
                                                  "gc_content", 
                                                  "length_seq" )]
  
}

#' Rename the columns to the usable names for the internal functions
#' @description Internal function to rename exisiting columns
#' @param df_sequence dataframe
#' @param column_length Name of the  column
#' @param new_name New name of the column
#' @return df_sequence 
rename_column <- function(df_sequence, column_length, new_name)  {
  names(df_sequence)[names(df_sequence) == column_length] <- new_name
  return(df_sequence)
}

#' Internal function to check which properties users have chosen
#' @description Internal function to check which properties users have chosen
#' @param df_sequence dataframe
#' @return df_sequence 
available_seq_prop <- function(df_sequence) {
  seq_prop = list()
  if_seq_prop <- TRUE
  if ("long_mono_A" %in% colnames(df_sequence)) {seq_prop <- rlist::list.append( seq_prop, "long_mono_A")}
  if ("long_mono_C" %in% colnames(df_sequence)) {seq_prop <- rlist::list.append( seq_prop, "long_mono_C")}
  if ("long_mono_G" %in% colnames(df_sequence)) {seq_prop <- rlist::list.append( seq_prop, "long_mono_G")}
  if ("long_mono_T" %in% colnames(df_sequence)) {seq_prop <- rlist::list.append( seq_prop, "long_mono_T")}
  if ("gc_content" %in% colnames(df_sequence)) {seq_prop <- rlist::list.append( seq_prop, "gc_content")}
  if ("length_seq" %in% colnames(df_sequence)) {seq_prop <- rlist::list.append( seq_prop, "length_seq")}
  if(length(seq_prop) == 0 ) {if_seq_prop <- FALSE}
  return(list(if_seq_prop, seq_prop))
}

#' Internal function to filter the data
#' @description #' Internal function to filter the data
#' @param inside_data dataframe
#' @param min integer showing the min  to filter
#' @param max integer showing the max  to filter
#' @param col name of the column
#' @return inside_data 
#' @importFrom tidyr %>%
#' @importFrom dplyr filter
#' @importFrom rlang sym

filter_seq <- function(inside_data,col,max,min) {
  inside_data %>%filter(eval(rlang::sym(col)) <= max) %>%filter(eval(rlang::sym(col)) >= min)
}