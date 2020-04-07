#' Run the rnalab Shiny App
#' 
#' @export runRNAapp
#' @importFrom shiny runApp
runRNAapp = function(){
  shiny::runApp(system.file('rnalabApp', package='rnalab'))
}
