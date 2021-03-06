library(shiny)
library(knitr)
library(readr)
library("crayon")

r <- getOption("repos")
chooseBioCmirror(ind = 0)
r <- append(BiocManager::repositories(), r)
options(repos = r)

library(sangerseqR)
library(Biostrings)
library(BiocGenerics)
library("msa")

source("hetindelFunctions.R")
source("indelForShiny.R")


ui <- fluidPage(
  
  titlePanel("Hetindel: Analyze Heterozygous Indels"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput(inputId = "archive",
                label = "Upload your .ab1 sequences together with the reference as a .zip archive:"),
      textInput(inputId = "prefix_file",
                label = "Prefix for output files",
                value = "test"),
      sliderInput(inputId = "ratio_value",
                  label = "Signal/Noise Ratio for Peak Detection:",
                  value = 0.35,
                  min = 0.05, max = 0.75, step = 0.05),
      checkboxInput(inputId = "make_chromatogram",
                    label = "Make Chromatograms",
                    value = T),
      numericInput(inputId = "beginning_start",
                   label = "Offset for matching the 5' homozygous part of the sequence to the reference",
                   value =60),
      numericInput(inputId = "homo_match_len",
                   label = "Length of the homozygous part of the sequence to match the reference",
                   value = 50),
      numericInput(inputId = "offset_3p",
                   label = "Offset at 3' end of the sequence to find matches for heterozygous part",
                   value = 80, min = 40),
      
      actionButton(inputId = "submit", label = "Submit!"),
      
      width = 4
    ),
    
    mainPanel(
      
      
      downloadButton(outputId = "download", label = "Download results"),
      
      span(textOutput(outputId = "error_txt"), style="color:red"),
      textOutput(outputId = "sequences_txt"),
      textOutput(outputId = "match_txt"),
      
      
      doc <- tags$html(
          HTML(read_file("main_content.html")),
        width = 8
                  ) 
        )
    )
)

server <- function(input, output, session) {
  
  observeEvent(input$submit, {

    outp1 <- files_fun(archive = input$archive, input$prefix_file, submit_count = input$submit)
    
    param_str <- paste0("Signal/Noise Ratio for Peak Detection: ", input$ratio_value, "\n", 
                        "Offset for matching the 5' homozygous part of the sequence to the reference: ", 
                        input$beginning_start,"\n",
                        "Length of the homozygous part of the sequence to match the reference: ", 
                        input$homo_match_len, "\n",
                        "Offset at 3' end of the sequence to find matches for heterozygous part: ", 
                        input$offset_3p, "\n")
    
    write(param_str, file=outp1[2], append=TRUE)
    
    ref_file <- list.files(path = outp1[3], pattern = "reference.txt", full.names = T, recursive = T)
    if (length(ref_file) != 1){
      output$error_txt <- renderText({outp1[[1]]})
      output$match_txt <- renderText({paste0(" ERROR: Reference file not found. Check that it has correct name.")
      })
    } else {
      input_reference <- gsub("[\r\n]", "", (read_file(ref_file)))
      input_reference <- gsub("[\n]", "", input_reference)
      
      seq_list <- list.files(path = outp1[3], pattern = ".ab1$", full.names = T, recursive = T)
      for (j in seq_list) {
        input_file <- j
        withProgress(message = paste0("Analyzing chromatogram ", j), min = 0, max = 1, value = 1, {
          outp2 <- sangerseq_function(input_file, ratio_value = input$ratio_value, 
                                      make_chromatogram = input$make_chromatogram,
                                      outp1 = outp1) })

        withProgress(message = paste0("Calculating allele shift", j), min = 0, max = 1, value = 1, {
          try(phaseShift_function(both_alleles = outp2[[2]], input_reference, beginning_start = input$beginning_start,
                                  homo_match_len = input$homo_match_len,
                                  offset_3p = input$offset_3p, 
                                  outp1 = outp1, input_file = input_file, sangerobj = outp2 [[1]]) ) })
        
      }
      
      out_zip_name <- paste0(Sys.Date(), "_", input$prefix_file, ".zip")
      
      withProgress(message = "Archiving...", min = 0, max = 1, value = 1, {
        out_archive <- zip(paste0(outp1[3], "/", out_zip_name), list.files(outp1[3], full.names = T)) })
      output$download <- downloadHandler(filename = out_zip_name, 
                                         content = function(file) {
                                           file.copy(paste0(outp1[3], "/", out_zip_name), file)
                                         },
                                         contentType = "application/zip")

      output$sequences_txt <- renderText({
        paste(out_zip_name)
      })
      
      output$match_txt <- renderText({
        "Now you can download the output files as a .zip archive "
      })  
    }
    
    session$onSessionEnded(function() { unlink(outp1[3], recursive = TRUE) } )
    
  }, once = F) 
  
}

shinyApp(ui=ui, server = server)
