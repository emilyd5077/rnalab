#' Plot Histograms for input data
#' @description Use ggplot2 to generate Histograms input data and display the summary statistics next to the plot.
#' @usage rnalab_hist_plot(data, vars, nbins)
#' @param data Dataset to use for ggplot2 plots
#' @param vars A list or vector of variables to plot as the x variable in individual histograms
#' @param nbins An integer number of bins to use for all histograms
#' @return A grid graphical object consisting of a ggplot2 histogram with printed summary statistics.
#' @export rnalab_hist_plot
#' @examples
#' # Plot the distribution and summary stats for a single variable in the dnaseqs data set
#' rnalab_hist_plot(dnaseqs, 'length', 100)
#' # Plot the distribution and summary stats for multiple variables in the dnaseqs data set
#' rnalab_hist_plot(dnaseqs, c('length', 'yield'), 100)
#' @importFrom grid textGrob
#' @importFrom gridExtra grid.arrange arrangeGrob marrangeGrob


rnalab_hist_plot = function(data, vars, nbins){

  generate_summary = function(data, var, nbins){
    
    # Verify that input variabe exists as a column name in data
    if(! var %in% colnames(data)){
      stop('Variable name does not exist in input data. Please select from existing column names.')
    }
    
    # Verify that input data is numeric
    if(is.numeric(data[[as.character(var)]])){

      # Generate summary statistics annotation
      sum_stat = summary(data[as.character(var)])
      annotation = paste(sum_stat, collapse = ' \n')

      # Plot histogram and add summary stats on the right of the plot
      plot = ggplot2::ggplot(data = data, mapping = ggplot2::aes_string(x = as.character(var))) +
        ggplot2::geom_histogram(bins = nbins) +
        ggplot2::theme_bw()

      gridExtra::arrangeGrob(plot, right = grid::textGrob(annotation))

    } else{
      grid::textGrob('Please select a numeric variable to plot')
    }
  }
  
  gridExtra::marrangeGrob(grobs = lapply(vars, function(x) generate_summary(data, x, nbins)),
                          nrow = length(vars), ncol = 1, heights=rep(20, length(vars)))

}

#' Plot Scatterplots for input data
#' @description Use ggplot2 to generate Scatterplots for input data.
#' Allows the option to plot a linear regression fit.
#' @usage rnalab_scatterplot(data, x, y, fit)
#' @param data Dataset to use for ggplot2 plots
#' @param x,y The x and y variables to be used in the aes() mapping for ggplot scatterplots
#' @param fit A boolean indicating whether a linear regression fit should be added to the ggplot scatterplot
#' @return A grid graphical object consisting of a ggplot2 jitter plot with or without a linear regression fit.
#' @export rnalab_scatterplot
#' @examples
#' # Plot a scatter plot without linear regression added (using data from the dnaseqs data set)
#' rnalab_scatterplot(dnaseqs, x = 'length', y = 'yield', fit = FALSE)
#' # Plot a scatter plot with linear regression added (using data from the dnaseqs data set)
#' rnalab_scatterplot(dnaseqs, x = 'length', y = 'yield', fit = TRUE)
#' @importFrom ggplot2 ggplot geom_jitter geom_histogram geom_line theme_bw aes aes_string
#' @importFrom grid textGrob
#' @importFrom gridExtra grid.arrange arrangeGrob marrangeGrob
#' @importFrom stats lm as.formula
#' @importFrom tidyr %>%
#' @importFrom dplyr mutate
#' @importFrom rlang .data

rnalab_scatterplot = function(data, x, y, fit=TRUE){

  plot = ggplot2::ggplot() +
    ggplot2::geom_jitter(data = data, mapping = ggplot2::aes_string(x = as.character(x), y = as.character(y))) +
    ggplot2::theme_bw()

  if(isTRUE(fit)){
    
    lm = stats::lm(formula = stats::as.formula(paste(as.character(y)," ~ ",as.character(x),collapse='')),
                   data = data)
    intercept = lm$coefficients[['(Intercept)']]
    slope = lm$coefficients[[2]]
    rsq = summary(lm)$adj.r.squared
    lm_fit = data.frame('x_fit' = data[[as.character(x)]]) %>%
      dplyr::mutate(y_fit = slope * .data$x_fit + intercept)
    
    legend_slope = paste('Slope: ', round(slope, 3), collapse='')
    legend_intercept = paste('Intercept: ', round(intercept, 3), collapse ='')
    legend_rsq = paste('R2: ', round(rsq, 3), collapse='')
    annotation = paste(legend_slope, legend_intercept, legend_rsq, sep='\n')
    
    plot = plot +
      ggplot2::geom_line(data = lm_fit, mapping = ggplot2::aes_string(x='x_fit', y='y_fit'))
    
    plot = gridExtra::grid.arrange(plot, right = grid::textGrob(annotation))
    
  }
  
  plot
  
}