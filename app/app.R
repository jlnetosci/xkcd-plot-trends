library(shiny)
library(ggplot2)

# UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("csv_file", "Upload CSV file"),
      actionButton("generate_btn", "Generate Plot")
    ),
    mainPanel(
      div(
        align = "center",
        plotOutput("my_plot", inline=TRUE),
        br(),
        downloadButton("download_pdf", "Download as PDF"),
        downloadButton("download_svg", "Download as SVG")
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  data <- reactive({
    req(input$csv_file)
    df <- read.csv(input$csv_file$datapath, skip = 2, stringsAsFactors = FALSE)
    # Perform preprocessing steps on the data if needed
    colnames(df) <- ifelse(grepl("^X.*\\d$", colnames(df)), paste0("X", sub("X.*?(\\d)$", "\\1", colnames(df))), gsub("\\..*", "", colnames(df)))
    colnames(df)[1] <- "timeframe"
    df[df == "<1"] <- "0"
    
    df$timeframe <- as.Date(zoo::as.yearmon(df$timeframe, format = "%Y-%m"))
    df[, 2:ncol(df)] <- lapply(df[, 2:ncol(df)], as.numeric)
    
    df <- df |>
      pivot_longer(
        cols = 2:ncol(df),
        names_to = 'search_term',
        values_to = 'value'
      )
    
    df$search_term <- factor(df$search_term, levels = unique(df$search_term))
    
    return(df)  # Add this line to return the modified data frame
  })
  
  generatePlot <- eventReactive(input$generate_btn, {
    req(data())
    
    fill <- c("#4684ee", "#dc3a13", "#fabb5b", "#008000", "#886cd5")
    
    ggplot(data(), aes(x = timeframe, y = value, color = search_term)) +
      geom_line(linewidth = 1.2) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      labs(x = NULL, y = NULL) +
      ggtitle("Google Trends \nSearch Volume") +
      scale_color_manual(labels = paste("<span style='color:", fill, "'>", unique(data()$search_term), "</span>"),
                         values = fill) +
      theme(
        text = element_text(size = 14, family = "xkcd", color = "black"),
        panel.background = element_rect(fill = "white", colour = "black", linewidth = 1.2),
        axis.text.x = element_text(color = "black"),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key = element_rect(fill = "white"),
        legend.text = element_markdown()
      )
  })
  
  output$my_plot <- renderPlot({
    generatePlot()
  }, width = 707, height = 500)
  
  # Download as PDF
  output$download_pdf <- downloadHandler(
    filename = function() {
      "plot.pdf"
    },
    content = function(file) {
      ggsave(file, plot = generatePlot(), width = 14.85, height = 10.5, units = "cm", device = "pdf")
    }
  )

  # Download as SVG
  output$download_svg <- downloadHandler(
    filename = function() {
      "plot.svg"
    },
    content = function(file) {
      ggsave(file, plot = generatePlot(), width = 14.85, height = 10.5, units = "cm", device = "svg")
    }
  )
}

# Run the app
shinyApp(ui, server)
