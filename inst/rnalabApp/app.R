#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

ui <- fluidPage(
    shinyjs::useShinyjs(),
    
    tabsetPanel(id = "tabSwitch",
               tabPanel("Upload Data",
                        titlePanel("Use Sample Data"),
                        actionButton("sample_data", "Load Sample Data"),
                        titlePanel("Upload File"),
                        fileInput("raw_data", "Choose Different CSV File",
                                  multiple = FALSE,
                                  accept = c(".csv")),
                        titlePanel("Preview Uploaded Data"),
                        tableOutput("data_head")
               ),
               
               tabPanel(value = "add_sequence", title = "Add Sequence",
                        titlePanel("Add Sequence"),
                        sidebarLayout(
                            sidebarPanel(
                                h5("Click Below, to calculate Sequence properties:"),
                                actionButton("add_seq", "Add Sequence Properties"),
                                uiOutput("showColumns"),
                                uiOutput("add_length_ui"),
                                uiOutput("add_mono_ui"),
                                uiOutput("add_gc_ui"),
                                
                                h5("Click Below, to map existing properties:"),
                                actionButton("map_columns", "Map existing Properties:"),
                                uiOutput("update_length_ui"),
                                uiOutput("update_mono_A_ui"),
                                uiOutput("update_mono_C_ui"),
                                uiOutput("update_mono_G_ui"),
                                uiOutput("update_mono_T_ui"),
                                uiOutput("update_gc_ui"),
                                uiOutput("update_columns_ui"),
                                uiOutput("remove_seq_ui")
                                ),
                            mainPanel(
                                tableOutput("table2")
                            )
                        )
                        ),
               
               tabPanel(value = "analyse_sequence", title = "Analyse Sequence",
                        titlePanel("Analyse Sequence"),
                        sidebarLayout(
                            sidebarPanel(
                                uiOutput("select_y_axis"),
                                uiOutput("slider_length_seq_ui"),
                                uiOutput("slider_gccontent_ui"),
                                uiOutput("slider_mono_A_ui"),
                                uiOutput("slider_mono_C_ui"),
                                uiOutput("slider_mono_G_ui"),
                                uiOutput("slider_mono_T_ui")
                            ),
                            mainPanel(
                                plotOutput("seq_prop_scatter")
                                #tableOutput("seq_prop_scatter")
                            )
                        )
                        
               ),
               
               tabPanel("Histograms / Summary Stats",
                        titlePanel("Histograms / Summary Statistics"),
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("summary_vars", "Generate Summary Statistics:",
                                            choices = list("Generate summary statistics"),
                                            multiple = TRUE
                                            ),
                                numericInput("nbins", "Histogram Bins:", 
                                             value = 100, min = 1, step = 50)
                            ),
                            mainPanel(
                                plotOutput("histograms")
                            )
                        )),
               
               tabPanel("Scatterplots",
                        titlePanel("Plot Scatterplot"),
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("xvar", "X-Axis Variable:",
                                            choices = list("X-Axis Variable"),
                                            multiple = FALSE),
                                selectInput("yvar", "Y-Axis Variable:",
                                            choices = list("Y-Axis Variable"),
                                           multiple = FALSE),
                                checkboxInput("regression_yn", "Add Linear Regression Fit",
                                              FALSE)
                            ),
                            mainPanel(
                                plotOutput("comparison_plots")
                            )
                        )
               )
    )
)

server <- function(input, output, session) {
    
    ####################
    # Tab: Upload Data #
    ####################
    
    raw_data = reactiveValues(data = NULL)
    
    # Create reactive to reset input values if new data source is selected
    reset_inputs = function(){
        shinyjs::reset("regression_yn")
        shinyjs::reset("summary_vars")
        updateSelectInput(session, "xvar",
                          choices = list("X-Axis Variable"))
        updateSelectInput(session, "yvar",
                          choices = list("Y-Axis Variable"))
    }
    
    # Allow users to select package sample data
    observeEvent(input$sample_data, {
        sample_data_path = paste0(system.file("extdata", package="rnalab"), "/dnaseqs.rds", collapse="")
        raw_data$data <- readRDS(sample_data_path)
        
        # Reset all other inputs when new data set is selected
        reset_inputs()
    })
    
    # Create reactive for uploading user-provided data
    observeEvent(input$raw_data, {
        print(input$raw_data$datapath)
        raw_data$data = read.csv(input$raw_data$datapath, stringsAsFactors = FALSE)
        
        # Reset all other inputs when new data set is selected
        reset_inputs()
    })
    
    #Display head of data table
    output$data_head = renderTable({
        head(raw_data$data)
    })
    
    
    ###################################
    # Tab: Histograms / Summary Stats #
    ###################################
    
    # Create reactive for getting variables
    
    get_vars_for_summary = reactive({
        data = raw_data$data
        
        updateSelectInput(session, "summary_vars",
                          choices = colnames(data),
                          selected = input$summary_vars)
        
        updateNumericInput(session, "nbins",
                           value = input$nbins)
        
        # Only generate a plot if a variable is selected by the user
        validate(need(!is.null(input$summary_vars), message="Please select a variable to plot."))
        
        rnalab::rnalab_hist_plot(data = data, vars = input$summary_vars, nbins = input$nbins)
    })
    
    # Return the number of variables selected for plotting to determine rendered plot height
    nplots = reactive({
        data = raw_data$data

        updateSelectInput(session, "summary_vars",
                          choices = colnames(data),
                          selected = input$summary_vars)
        
        # Only generate a plot if a variable is selected by the user
        validate(need(!is.null(input$summary_vars), message="Please select a variable to plot."))
        
        length(session$input$summary_vars)
    })
    
    # Plot histograms and show summary statistics
    observe({
        output$histograms = renderPlot(
            get_vars_for_summary(),
            height = 400 * nplots()
            )
    })
    
    
    #####################
    # Tab: Scatterplots #
    #####################
    
    # Generate comparison plots

    output$comparison_plots = renderPlot({
        data = raw_data$data
        
        updateSelectInput(session, "xvar",
                          choices = colnames(data),
                          selected = input$xvar
        )
        
        updateSelectInput(session, "yvar",
                          choices = colnames(data),
                          selected = input$yvar
        )
        
        updateCheckboxInput(session, "regression_yn",
                            value = input$regreesion_yn)
        
        # Only generate plot if .csv file has been uploaded and variables have been selected:
        if(is.null(data) | input$xvar=="X-Axis Variable" | input$yvar=="Y-Axis Variable"){
            NULL
        }
        else{
            # Generate the scatterplot
            rnalab::rnalab_scatterplot(data = data, x = input$xvar, y = input$yvar, fit=input$regression_yn)
        }
    })
    
    
    ##########################
    # Tab: Sequence Analysis #
    ##########################
    
    # Dynamic rendering of button to add sequence properties
    observeEvent(input$add_seq,{
        column_names <<- colnames(raw_data$data)
        if(length(column_names) == 0) {
            showNotification("Please Load Data", type = "error", duration = 2)
        }
        else {
        column_names <<- rlist::list.append("None", column_names)
        output$showColumns <- renderUI({selectInput("column_names_seq", "Choose Column for Sequence", column_names)})
        output$add_length_ui <- renderUI({actionButton("add_length", "Add Length")})
        output$add_gc_ui <- renderUI({actionButton("add_gc", "Add GC Content")})
        output$add_mono_ui <- renderUI({actionButton("add_mono", "Add Mono Length")})
        output$remove_seq_ui <- renderUI({actionButton("remove_seq", "Reset")})
        }
    })
    
    # Dynamic rendering of button to update sequence properties
    observeEvent(input$map_columns,{
        column_names <<- colnames(raw_data$data)
        if(length(column_names) == 0) {
            showNotification("Please Load Data", type = "error", duration = 2)
        }
        else {
            column_names <<- rlist::list.append( "None", column_names)
            output$update_length_ui <- renderUI({selectInput("update_length", "Choose Length", column_names)})
            output$update_mono_A_ui <- renderUI({selectInput("update_mono_A", "Choose length of longest Mono Nucelotide A", column_names)})
            output$update_mono_C_ui <- renderUI({selectInput("update_mono_C", "Choose length of longest Mono Nucelotide C", column_names)})
            output$update_mono_G_ui <- renderUI({selectInput("update_mono_G", "Choose length of longest Mono Nucelotide G", column_names)})
            output$update_mono_T_ui <- renderUI({selectInput("update_mono_T", "Choose length of longest Mono Nucelotide T", column_names)})
            output$update_gc_ui <- renderUI({selectInput("update_gc", "Choose Column for Length", column_names)})
            output$update_columns_ui <- renderUI({actionButton("update_columns", "Update")})
            output$remove_seq_ui <- renderUI({actionButton("remove_seq", "Reset")})
        }
    })
    
    # Button to add the length
    observeEvent(input$add_length,{
        if(input$column_names_seq == 'None') {showNotification("Please select sequence column", type = "error", duration = 2)}
        else {
          tryCatch({
            raw_data$data <<- rnalab:::add_sequence_length(raw_data$data, input$column_names_seq)
            output$table2 <- renderTable(head(raw_data$data))    
          }, error = function(err)
            {
            if(grepl("repeat",err)) {
              showNotification("Column already added", type="error", duration=5)
            }
            else {
                showNotification("Doesn't seem like a sequence column, it should be like - AAACCGGTT", type="error", duration=5)
            }
          })
        }
    })
    # Button to add mono nucleotide column
    observeEvent(input$add_mono,{
        if(input$column_names_seq == 'None') {showNotification("Please select sequence column", type="error", duration=2)}
        else {
          tryCatch({
            raw_data$data <<- rnalab:::add_mono_nucleotide_length(raw_data$data, input$column_names_seq)
            output$table2 <- renderTable(head(raw_data$data)) 
          }, error = function(err) {
            if(grepl("repeat",err)) {
              showNotification("Column already added", type="error", duration=5)
            }
            else {
              showNotification("Doesn't seem like a sequence column, it should be like - AAACCGGTT", type="error", duration=5)
            }
          })
            
        }
    })
    # Button to add the gc content column
    observeEvent(input$add_gc,{
        if(input$column_names_seq == 'None') {showNotification("Please select sequence column", type = "error", duration=2)}
        else {
          tryCatch({
            raw_data$data <<- rnalab:::add_gc_content(raw_data$data, input$column_names_seq)
            output$table2 <- renderTable(head(raw_data$data))
          }, error = function(err) {
            if(grepl("repeat",err)) {
              showNotification("Column already added", type="error", duration=5)
            }
            else {
            showNotification("Please correct format column", type="error", duration=5)
            }
          }
        )
        }
    })
    
    # Button to remove the properties
    observeEvent(input$remove_seq,{
        raw_data$data <<- rnalab:::remove_added_columns(raw_data$data)
        output$table2 <- renderTable(head(raw_data$data))
        
        # Remove the add buttons
        output$showColumns <- renderUI({})
        output$add_length_ui <- renderUI({})
        output$add_gc_ui <- renderUI({})
        output$add_mono_ui <- renderUI({})
        output$remove_seq_ui <- renderUI({})
        
        # Remove the update buttons
        output$update_length_ui <- renderUI({})
        output$update_mono_A_ui <- renderUI({})
        output$update_mono_C_ui <- renderUI({})
        output$update_mono_G_ui <- renderUI({})
        output$update_mono_T_ui <- renderUI({})
        output$update_gc_ui <- renderUI({})
        output$update_columns_ui <- renderUI({})
        
    })
    
    observeEvent(input$update_columns,{
        if(!("length_seq" %in% colnames(raw_data$data)))
        {
          raw_data$data <<- rnalab:::rename_column(raw_data$data, input$update_length, new_name = "length_seq")
        }
        if(!("long_mono_A" %in% colnames(raw_data$data)))
        {
          raw_data$data <<- rnalab:::rename_column(raw_data$data, input$update_mono_A, new_name = "long_mono_A")
        }
        if(!("long_mono_C" %in% colnames(raw_data$data)))
        {
          raw_data$data <<- rnalab:::rename_column(raw_data$data, input$update_mono_C, new_name = "long_mono_C")
        }
        if(!("long_mono_G" %in% colnames(raw_data$data)))
        {
          raw_data$data <<- rnalab:::rename_column(raw_data$data, input$update_mono_G, new_name = "long_mono_G")
        }
        if(!("long_mono_T" %in% colnames(raw_data$data)))
        {
          raw_data$data <<- rnalab:::rename_column(raw_data$data, input$update_mono_T, new_name = "long_mono_T")
        }
        if(!("gc_content" %in% colnames(raw_data$data)))
        {
          raw_data$data <<- rnalab:::rename_column(raw_data$data, input$update_gc, new_name = "gc_content")
        }
        output$table2 <- renderTable(head(raw_data$data))
    })
    
    
    ####################################
    # Tab: Analyse Sequence Properties #
    ####################################
    
    # Check if there are any Sequence Properties
    
    data_seq <- reactive({raw_data$data})
    
    data_seq1 = reactive({
        column_names <<- colnames(data_seq())
        
        seq_columns <- rnalab:::available_seq_prop(data_seq())[[2]]
        inside_data <- data_seq()
        if(rnalab:::available_seq_prop(data_seq())[[1]] == TRUE) {
            if ("length_seq" %in% seq_columns) {
                min_len <- input$slider_length_seq[1]
                max_len <- input$slider_length_seq[2]
                inside_data <- rnalab:::filter_seq(inside_data,'length_seq',max_len,min_len)
            }
            
            if ("gc_content" %in% seq_columns) {
                min_gc <- input$slider_gccontent[1]
                max_gc <- input$slider_gccontent[2]
                inside_data <- rnalab:::filter_seq(inside_data,'gc_content',max_gc,min_gc)
            }
            if ("long_mono_A" %in% seq_columns) {
                min_a <- input$slider_mono_A[1]
                max_a <- input$slider_mono_A[2]
                inside_data <- rnalab:::filter_seq(inside_data,'long_mono_A',max_a,min_a)
            }
            if ("long_mono_T" %in% seq_columns) {
                min_t <- input$slider_mono_T[1]
                max_t <- input$slider_mono_T[2]   
                inside_data <- rnalab:::filter_seq(inside_data,'long_mono_T',max_t,min_t)
            }
            if ("long_mono_G" %in% seq_columns) {
                min_g <- input$slider_mono_G[1]
                max_g <- input$slider_mono_G[2]   
                inside_data <- rnalab:::filter_seq(inside_data,'long_mono_G', max_g,min_g)
            }
    
            if ("long_mono_C" %in% seq_columns) {
                min_c <- input$slider_mono_C[1]
                max_c <- input$slider_mono_C[2]    
                inside_data <- rnalab:::filter_seq(inside_data,'long_mono_C',max_c,min_c)
            }
        }
        inside_data
    })
    
    observeEvent(input$tabSwitch, {
        if (input$tabSwitch == "analyse_sequence") {
            #print(need(expr = input$add_mono, message = "Please upload dell."))
            #validate(need(input$tabSwitch == "analyse_sequence", "Please upload some."))
            column_names <<- colnames(data_seq())
            seq_columns <- rnalab:::available_seq_prop(data_seq())[[2]]
            if(rnalab:::available_seq_prop(data_seq())[[1]] == TRUE) {
                # Selection for the Y-axis of the scatter plot
                output$select_y_axis <- renderUI({selectInput("select_y", "Choose column for Y-axis", column_names)})
                
                if ("length_seq" %in% seq_columns) {    
                    output$slider_length_seq_ui <- renderUI({sliderInput("slider_length_seq", "Sequence length:",min = 0, max = 10000, value = c(0, 10000))})
                }
                if ("gc_content" %in% seq_columns) {    
                    output$slider_gccontent_ui <- renderUI({sliderInput("slider_gccontent", "GC Content:", min = 0, max = 1, value = c(.1, .5))})
                }
                if ("long_mono_A" %in% seq_columns) {
                    output$slider_mono_A_ui <- renderUI({sliderInput("slider_mono_A", "A mononucleotide longest length:",min = 5, max = 30 , value = c(0, 100))})
                }
                if ("long_mono_C" %in% seq_columns) {    
                    output$slider_mono_C_ui <- renderUI({sliderInput("slider_mono_C", "C mononucleotide longest length:",min = 0, max = 30, value = c(0, 100))})
                }
                if ("long_mono_G" %in% seq_columns) {    
                    output$slider_mono_G_ui <- renderUI({sliderInput("slider_mono_G", "G mononucleotide longest length:",min = 0, max = 30, value = c(0, 100))})
                }
                if ("long_mono_T" %in% seq_columns) {    
                    output$slider_mono_T_ui <- renderUI({sliderInput("slider_mono_T", "T mononucleotide longest length:",min = 0, max = 30, value = c(0, 100))})
                }
                #output$seq_prop_scatter <- renderPlot({ggplot2::ggplot(data_seq1()) + ggplot2::geom_point(ggplot2::aes_string(x="ensembl_id",y=input$select_y))})
            }
        }
        else {
            output$select_y_axis <- renderUI({})
            output$slider_length_seq_ui <- renderUI({})
            output$slider_gccontent_ui <- renderUI({})
            output$slider_mono_A_ui <- renderUI({})
            output$slider_mono_C_ui <- renderUI({})
            output$slider_mono_G_ui <- renderUI({})
            output$slider_mono_T_ui <- renderUI({})
            output$seq_prop_scatter <- renderUI({})
        }
    })
    observeEvent(input$select_y, ({
        output$seq_prop_scatter <- renderPlot({ggplot2::ggplot(data_seq1()) + ggplot2::geom_point(ggplot2::aes_string(x="ensembl_id",y=input$select_y))})
    }))

}

shinyApp(ui, server)