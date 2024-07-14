library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(zoo)
library(DT)
library(shinythemes)
library(bslib)
library(rsconnect)


# Define the UI
ui <- fluidPage(
  #theme = shinytheme("slate"),  # Apply a theme
  theme = bs_theme(preset = "vapor"),
  tags$head(
    tags$style(HTML("
      .dataTables_wrapper {
        color: #E3E3E3;
      }
      table.dataTable thead th {
        color: #E3E3E3;
      }
      table.dataTable tbody tr {
        color: #E3E3E3;
      }
      table.dataTable tfoot th {
        color: #E3E3E3;
      },
      .credits {
        display: flex;
        justify-content: center;
        align-items: center;
        margin-top: 10px;
      }
      .credits img {
        width: 20px;
        height: 20px;
        vertical-align: middle;
      }
      .credits span {
        margin-left: 5px;
        margin-right: 15px;
      }
    "))
  ),
  titlePanel("Predictive Web App for Average Temperature Using a Multi-Model Fuzzy Time Series Algorithm"),
  # Credits below the title
  tags$div(
    class = "credits",
    tags$a(href="https://www.linkedin.com/in/achmadardanip/", target="_blank",
           tags$img(src="https://upload.wikimedia.org/wikipedia/commons/c/ca/LinkedIn_logo_initials.png"),
           tags$span("Achmad Ardani Prasha")
    ),
    tags$a(href="https://www.linkedin.com/in/clavinorachmadi/", target="_blank",
           tags$img(src="https://upload.wikimedia.org/wikipedia/commons/c/ca/LinkedIn_logo_initials.png"),
           tags$span("Clavino Ourizqi Rachmadi")
    )
  ),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File",
                accept = c("text/csv", 
                           "text/comma-separated-values,text/plain", 
                           ".csv")),
      tags$hr(),
      numericInput("period", "Prediction Period (days):", min = 1, max = 30, value = 7),
      selectInput("model", "Select Model:", choices = c("Stevenson Porter", "Markov Chain"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("plot")),
        tabPanel("Prediction", DT::dataTableOutput("prediction_table")),
        tabPanel("Accuracy", tableOutput("accuracy")),
        tabPanel("Comparison Plot", plotOutput("comparison_plot"))
      )
    )
  )
)