library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)

ui= fluidPage(
  titlePanel("Cytokine Levels of APOE Genotype"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("genotype", 
                  "Select APOE Genotype:", 
                  choices = c("APOE 33", "APOE 34", "APOE 44"))
    ),
    
    mainPanel(
      plotOutput("cytokinePlot"),
      plotOutput("penultimatePlot")
    )
  )
)

server= function(input, output) {
  
  data= read_excel("/Users/shreyarege/Downloads/book2.xlsx")
  
  filteredData= reactive({
    subset(data, CellLine == input$genotype)
  })
  
  output$cytokinePlot= renderPlot({
    df= filteredData()
    
    ggplot(df, aes(x = `Concentration (uM)`, y = `IL-1 beta`, color = "IL-1 beta")) +
      geom_line() +
      geom_point() +
      geom_line(aes(y = `IL-33`, color = "IL-33")) +
      geom_point(aes(y = `IL-33`, color = "IL-33")) +
      geom_line(aes(y = `IL-18`, color = "IL-18")) +
      geom_point(aes(y = `IL-18`, color = "IL-18")) +
      geom_line(aes(y = `IL-6`, color = "IL-6")) +
      geom_point(aes(y = `IL-6`, color = "IL-6")) +
      labs(title = paste("Cytokine Levels for", input$genotype),
           x = "Concentration (µM)",
           y = "Cytokine Level") +
      scale_color_manual(name = "Cytokine",
                         values = c("IL-1 beta" = "red", 
                                    "IL-33" = "blue",
                                    "IL-18" = "green",
                                    "IL-6" = "purple")) +
      theme_minimal(base_size = 15) +
      theme(
        axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.line = element_line(color = "black"),
        plot.title = element_text(color = "black", face = "bold"),
        legend.title = element_text(color = "black"),
        legend.text = element_text(color = "black")
      )
  })
  
  output$penultimatePlot= renderPlot({
    df_long= data %>%
      pivot_longer(cols = starts_with("IL"), 
                   names_to = "Cytokine", 
                   values_to = "Level")
    
    ggplot(df_long, aes(x = `Concentration (uM)`, y = Level, color = Cytokine)) +
      geom_point() +
      geom_line() +
      facet_wrap(~ CellLine, scales = "free_y") +
      labs(title = "Accumulated Cytokine Levels of APOE Genotype",
           x = "Concentration (µM)",
           y = "Cytokine Level") +
      theme_minimal(base_size = 15) +
      theme(
        axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.line = element_line(color = "black"),
        plot.title = element_text(color = "black", face = "bold"),
        legend.title = element_text(color = "black"),
        legend.text = element_text(color = "black")
      )
  })
}

shinyApp(ui = ui, server = server)

