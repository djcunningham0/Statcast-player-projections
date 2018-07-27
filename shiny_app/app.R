library(shiny)
library(rdrop2)
library(dplyr)
library(stringr)
library(readr)


# define functions --------------------------------------------------------
#' readRDS on a file in a Dropbox folder
#' modeled this after rdrop2::drop_read_csv
#' this relies on my .httr-oauth file being in the folder
dropbox_readRDS <- function(file, dest=tempdir(), ...) {
  localfile = paste0(dest, "/", basename(file))
  drop_download(file, localfile, overwrite = TRUE)
  readRDS(localfile, ...)
}

#' strips the x's and X's from a string (useful for renaming columns in marcel_df)
strip_x <- function(string) {
  string %>% 
    str_replace("x", "") %>% 
    str_replace("X", "")
}

#' strips the suffixes in marcel_df off the predicted values (e.g., *_rf)
strip_suffixes <- function(string) {
  string %>% 
    str_replace("_rf", "")
}


# preprocessing -----------------------------------------------------------
count_cols <- c("xPA", "xAB", "xH", "xX1B", "xX2B", "xX3B", "xHR", "xR", "xRBI", "xBB",
                "xIBB", "xSO", "xHBP", "xSF", "xSH", "xSB", "xCS", "xX1B_rf", "xX2B_rf",
                "xX3B_rf", "xHR_rf", "xH_rf")
rate_cols  <- c("xBA", "xOBP", "xSLG", "xOPS", "xwOBA", "xBA_rf", "xOBP_rf", "xSLG_rf",
                "xOPS_rf", "xwOBA_rf")

marcel_df <- dropbox_readRDS("/statcast_modeling_data/prediction_data/marcel_projections.rds") %>% 
  mutate_at(count_cols, round, 1) %>% 
  mutate_at(rate_cols, round, 3)

method_choices <- c("Standard Marcel projections", 
                    "Statcast-enhanced projections (random forest)")

github_url <- a(href="https://github.com/djcunningham0/Statcast-player-projections",
                "my GitHub page")
email_url  <- a(href="mailto:djcunningham0@gmail.com?subject=Statcast player projections",
                "djcunningham0@gmail.com")


# ui ----------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Statcast-Enhanced Batting Projections")
  
  ,p("Much more coming soon, but for now view or download the projections for the 2018 season.")
  ,p("View the source code at", github_url, "or email me at", email_url, "with comments or questions.")
  
  ,fluidRow(
    column(
      width=3,
      selectInput("player", "Select a player",
                  choices=c("ALL", sort(unique(marcel_df$Name))),
                  selected="ALL",
                  multiple=TRUE)
    )
    ,column(
      width=6,
      selectInput("method", "Projection method",
                  choices=method_choices,
                  selected=method_choices[2],
                  width=500)
    )
    ,column(
      width=2,
      selectInput("season", "Season",
                  choices=c(2018))
    )
  )
  
  ,downloadButton("download", "Download all projections")
  
  ,fluidRow(
    width=4,
    column(width=4, style='padding:5px')
  )
  
  ,dataTableOutput("datatable")
)


# server ------------------------------------------------------------------
server <- function(input, output) {
  
  cols <- reactive({
    cols <- c("xH", "xX1B", "xX2B", "xX3B", "xHR",
              "xBA", "xOBP", "xSLG", "xOPS", "xwOBA")
    
    if (input$method == method_choices[2]) {
      cols <- paste0(cols, "_rf")
    }
    
    cols <- c("Name", "xPA", "xAB", cols)
    cols
  })
  
  output$datatable <- renderDataTable({
    out_df <- marcel_df %>%
      filter(("ALL" %in% input$player) | (Name %in% input$player)) %>%
      select(cols())
    
    colnames(out_df) <- colnames(out_df) %>%
      strip_x() %>%
      strip_suffixes()
    
    out_df
  }, options=list(autoWidth=FALSE
                  ,order=list(length(cols())-1, 'desc')  # note, DataTable columns are 0-indexed
                  ,columnDefs=list(list(width="120px", targets=0))
                  )
  )
  
  output$download <- downloadHandler(
    filename="statcast_marcel_projections.csv",
    content=function(file) {
      write_csv(marcel_df, path=file)
    }
  )
  
}

shinyApp(ui, server)
