# Create a server variable that we can use in the UI for a conditionalPanel
output$saved_plots_exist <- reactive({
  length(values$plots) > 0
})
outputOptions(output, 'saved_plots_exist', suspendWhenHidden = FALSE)

countNoun <- function(num, noun) {
  if (num == 1) paste0(num, " ", noun)
  else paste0(num, " ", noun, "s")
}

output$export_btn_ui <- renderUI({
  btn_text <- paste0("Download ",
                     countNoun(length(input$plots_order), "plot"),
                     " (", countNoun(export_num_pages(), "page"), ")")
  downloadButton("export_btn", btn_text)
})

# Select the plots and the order of the plots to export
output$plots_order_ui <- renderUI({
  selectizeInput("plots_order", "Plots to export (drag to reorder)",
                 choices = names(values$plots), selected = names(values$plots),
                 multiple = TRUE, options = list(
                   plugins = list('drag_drop','remove_button')))
})

# If no plots are chosen to export, don't show all the export options
observe({
  shinyjs::toggle(selector = "#exporting_plots_options, #preview_plots_options",
                  condition = length(input$plots_order) > 0)
})

# Show a dropdown to select which page to show
output$plots_select_page_ui <- renderUI({
  num_pages <- export_num_pages()
  
  # Try to remain on the same page even when the dropdown changes
  isolate({
    if (!is.null(input$plots_select_page) &&
        as.numeric(input$plots_select_page) <= num_pages) {
      selected <- input$plots_select_page
    } else {
      selected <- 1
    }
  })
  selectInput("plots_select_page", "Page to preview",
              choices = seq(num_pages), selected = selected)
})

# Calculate the number of pages to export
export_num_pages <- reactive({
  if (input$export_multiple) {
    plots_per_page <- input$export_nrow * input$export_ncol
    pages <- ceiling(length(input$plots_order) / plots_per_page)
  } else {
    pages <- length(input$plots_order)
  }
  pages
})

# print a specific page of plots (either 1 plot/page or multiple rows/cols)
export_print_page <- function(page) {
  page <- as.numeric(page)
  if (!input$export_multiple) {
    plot_name <- input$plots_order[page]
    values$plots[plot_name]
  } else {
    plots_per_page <- input$export_nrow * input$export_ncol
    idx_start <- (page - 1) * plots_per_page + 1
    idx_end <- min(length(input$plots_order), page * plots_per_page)
    if (idx_start > idx_end) {
      return()
    }
    plot_names <- input$plots_order[idx_start:idx_end]
    plots <- values$plots[plot_names]
    
    gridExtra::grid.arrange(
      grobs = plots,
      nrow = input$export_nrow,
      ncol = input$export_ncol,
      as.table = (input$export_arrangement == "byrow")
    )
  }
}

# Show a dropdown to select a plot to remove in the Export tab
output$plots_remove_ui <- renderUI({
  selectInput("plots_remove", NULL, names(values$plots))
})

# Preview a plot in the Export tab
output$plot_preview <- renderPlot({
  if (is.null(input$plots_select_page)) {
    return()
  }
  export_print_page(input$plots_select_page)
},
width = function() { plot_preview_width() },
height = function() { plot_preview_height() })

# Return the dimensions of the PDF page selected in the Export tab
pdf_page_dim <- reactive({
  if (input$export_pdf_orientation == "landscape") {
    width <- 11
    height <- 8.5
  } else if (input$export_pdf_orientation == "portrait") {
    width <- 8.5
    height <- 11
  } else {
    width <- input$export_pdf_width
    height <- input$export_pdf_height
  }
  list(width = width, height = height)
})

# Calculate the dimensions of the plot preview
plot_preview_dim <- reactive({
  # If it's PDF, the units are inches and default resolution is 72 px/inch
  if (input$export_file_type == "pdf") {
    width <- pdf_page_dim()$width * 72
    height <- pdf_page_dim()$height * 72
  } else {
    width <- input$export_file_width
    height <- input$export_file_height
  }
  
  # Keep the aspect ratio, but make the max dimensions 500
  ratio <- height/width
  if (ratio > 1) {
    height <- 500
    width <- height/ratio
  } else {
    width <- 500
    height <- ratio*width
  }
  
  list(width = width, height = height)
})
plot_preview_width <- reactive({
  plot_preview_dim()$width
})
plot_preview_height<- reactive({
  plot_preview_dim()$height
})

# Remove the currently selected plot from the saved plots list
observeEvent(input$remove_plot_btn, {
  values$plots[[input$plots_remove]] <- NULL
})

# Determine the file name of the exported plots file.
# If there's only one plot or using PDF, export it in its raw format.
# Multiple plots in non-PDF format are zipped together.
export_file_name <- reactive({
  if (export_num_pages() == 1 || input$export_file_type == "pdf") {
    paste0("export_plots", ".", input$export_file_type)
  } else {
    paste0("export_plots", ".zip")
  }
})

# Download the saved plots
output$export_btn <- downloadHandler(
  filename = function() {
    export_file_name()
  },
  content = function(file) {
    tryCatch({
      file_type <- input$export_file_type
      
      # If saving as PDF, save all pages in one file
      if (file_type == "pdf") {
        width <- pdf_page_dim()$width
        height <- pdf_page_dim()$height
        
        file_names <- "export_plots.pdf"
        grDevices::pdf(file = file_names, width = width, height = height,
                       title = file_names, onefile = TRUE)
        
        if (!input$export_multiple) {
          plots <- values$plots[input$plots_order]
          invisible <- lapply(plots, print)
        } else {
          num_pages <- export_num_pages()
          for (page in seq(num_pages)) {
            suppressMessages(print(export_print_page(page)))
          }
        }
        
        grDevices::dev.off()
      }
      # If saving as raw images, save each page as a separate file
      else {
        num_pages <- export_num_pages()
        for (page in seq(num_pages)) {
          export_print_page(page)
        }
        file_names <- lapply(seq(num_pages), function(page) { 
          file_name <- paste0("export_plots_p", page, ".", file_type)
          export_params <- list(file_name,
                                width = input$export_file_width,
                                height = input$export_file_height)
          do.call(file_type, export_params)
          print(export_print_page(page))
          grDevices::dev.off()
          file_name
        })
        file_names <- unlist(file_names)
      }
      
      # If there's a single file, download the file. If multiple files, zip
      if (length(file_names) == 1) {
        file.copy(file_names, file, overwrite = TRUE)
      } else {
        zip(file, file_names)
      }
      
      # Remove the generated files so that we don't run out of disk space :)
      
      file.remove(file_names)
    },
    error = function(err) {
      stop(err$message)
    })
  }
)