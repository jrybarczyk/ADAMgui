#' @title GFAGtargetUi
#' @description Launch Launch GFAGtargetUi Graphical User Interface (GUI) in
#' local machine or default browser
#' @importFrom dplyr mutate filter summarize arrange
#' @importFrom ggpubr ggarrange
#' @importFrom reshape2 melt
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom data.table data.table
#' @rawNamespace import(shiny, except=c("runExample", "dataTableOutput", 
#' "renderDataTable"))
#' @import ggsignif RColorBrewer colorRamps gridExtra knitr stringr stringi
#' @import varhandle ggplot2 ggrepel testthat GO.db shinyjs ADAM 
#' @details This shiny (GUI) enables the creation of a plot that shows the
#' expression of the targets in a selected path between control and case.
#' See the vignette for more information.
#' @usage GFAGtargetUi(browser)
#' @param browser is a logical variable necessary to run the app.
#' When browser=TRUE the app is launched in your default web browser.
#' When browser=FALSE the app is launched in your local machine.
#' @return return a shiny Graphical User Interface (GUI).
#' @export
#' @author Giordano Bruno Sanches Seco <giordano.bruno@unesp.br>
#' @examples
#' #Creating input files
#' data(DiffAedes)
#' data(GeneFunctionAedes)
#' data(ResultAnalysisAedes)
#' \dontrun{
#' #Input file 1
#' write.table(DiffHs,file = "DiffAedes.txt",sep = "\t", col.names = TRUE,
#' row.names = FALSE, quote = FALSE)
#' #Input file 2
#' write.table(GeneFunctionHs,file = "GeneFunctionAedes.txt",sep = "\t",
#' col.names = TRUE, row.names = FALSE, quote = FALSE)
#' #Input file 3
#' write.table(ResultAnalysisAedes,file = "ResultAnalysisAedes.txt",sep = "\t",
#' col.names = TRUE, row.names = FALSE, quote = FALSE)
#' #Grahphical analysis
#' GFAGtargetUi(browser = TRUE) # Launch the app in your default web browser.
#' GFAGtargetUi(browser = FALSE) # Launch the app in your local machine.
#' }
GFAGtargetUi <- function(browser = TRUE) { 
  tryCatch({
    if (browser==TRUE){
      shiny::runApp(paste0(system.file("shiny",
                                       package="ADAMgui"),
                           "/app2.R"),
                    launch.browser = TRUE)
    } else {
      if (browser ==FALSE) {
        shiny::runApp(paste0(system.file("shiny", package="ADAMgui"),"/app2.R"))
      }else{
        error = print("Parameter must be TRUE or FALSE")
      }
      
    }
  }
  
  )} 
