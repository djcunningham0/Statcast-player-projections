library(shiny)
library(shinydashboard)
library(rdrop2)
library(dplyr)
library(stringr)
library(readr)
library(lattice)
library(NISTunits)
library(lubridate)
library(GAlogger)



# start tracking ----------------------------------------------------------
source("my_google_tracking_id.R")  # just contains my tracking ID (excluded from github)
my_id <- my_google_tracking_id()
ga_set_tracking_id(my_id)
ga_set_approval(consent = TRUE)
ga_collect_pageview(page="/home", title="Home")


# define functions --------------------------------------------------------
#' readRDS on a file in a Dropbox folder
#' modeled this after rdrop2::drop_read_csv
#' this relies on my .httr-oauth file being in the folder
dropbox_readRDS <- function(file, dest=tempdir(), ...) {
  localfile = paste0(dest, "/", basename(file))
  if (!file.exists(localfile)) {
    drop_download(file, localfile, overwrite = TRUE)
  }
  readRDS(localfile, ...)
}

dropbox_read_file <- function(file, dest=tempdir(), ...) {
  require(readr)
  localfile = paste0(dest, "/", basename(file))
  if (!file.exists(localfile)) {
    drop_download(file, localfile, overwrite = TRUE)
  }
  read_file(localfile, ...)
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

# this is mostly copied from openWAR::panel.baseball, but that package caused errors when publishing the app
my.panel.baseball <- function () 
{
  bgcol <-  "darkgray"
  panel.segments(0, 0, -400, 400, col=bgcol)
  panel.segments(0, 0, 400, 400, col=bgcol)
  bw <-  2
  
  base1.x = 90 * cos(pi/4)
  base1.y = 90 * sin(pi/4)
  panel.polygon(c(base1.x, base1.x - bw, base1.x - 2 * bw, base1.x - bw), 
                c(base1.y, base1.y - bw, base1.y, base1.y + bw), 
                col=bgcol)
  
  base2.x <- 0
  base2.y <- sqrt(90^2 + 90^2)
  panel.polygon(c(-bw, base2.x, bw, base2.x), 
                c(base2.y, base2.y - bw, base2.y, base2.y + bw), 
                col=bgcol)
  
  base3.x <- 90 * cos(3 * pi/4)
  base3.y <- base1.y
  panel.polygon(c(base3.x, base3.x + bw, base3.x + 2 * bw, base3.x + bw), 
                c(base3.y, base3.y - bw, base3.y, base3.y + bw), 
                col = bgcol)
  
  x <- NULL
  rm(x)
  
  mound.y <- 60.5
  panel.curve(mound.y + sqrt(95^2 - x^2), from=base3.x - 26, to=base1.x + 26, col=bgcol)
  panel.rect(-bw, mound.y - bw/2, bw, mound.y + bw/2, col=bgcol)
  panel.polygon(c(0, -8.5/12, -8.5/12, 8.5/12, 8.5/12), 
                c(0, 8.5/12, 17/12, 17/12, 8.5/12), col=bgcol)
  
  distances <- seq(from=200, to=500, by=100)
  for (d in distances) {
    panel.curve(sqrt(d^2 - x^2), 
                from=d * cos(3 * pi/4), 
                to=d * cos(pi/4), 
                col=bgcol)
  }
  
  # draw infield details (I added this)
  panel.segments(base1.x, base1.y, base2.x, base2.y, col=bgcol)
  panel.segments(base2.x, base2.y, base3.x, base3.y, col=bgcol)
  
  r <- 10
  panel.curve(sqrt(r^2 - x^2) + mound.y, from=-r, to=r, col=bgcol)
  panel.curve(-sqrt(r^2 - x^2) + mound.y, from=-r, to=r, col=bgcol)
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

rf_probs <- dropbox_readRDS("/statcast_modeling_data/prediction_data/rf_probs.rds")

lucky_df  <- dropbox_readRDS("/statcast_modeling_data/prediction_data/current_season_wOBA_preds.rds") %>% 
  bind_rows(dropbox_readRDS("/statcast_modeling_data/prediction_data/completed_seasons_wOBA_preds.rds")) %>% 
  arrange(desc(Season), key_mlbam)

cur_season_max_date <- as.Date(dropbox_read_file("/statcast_modeling_data/data/max_date.txt"))
cur_season_max_G <- as.numeric(dropbox_read_file("/statcast_modeling_data/data/max_G.txt"))


# variables for ui and server ---------------------------------------------
method_choices <- c("Standard Marcel projections", 
                    "Statcast-enhanced projections (random forest)")

github_url <- a(href="https://github.com/djcunningham0/Statcast-player-projections",
                "my GitHub page")

email_url  <- a(href="mailto:djcunningham0@gmail.com?subject=Statcast player projections",
                "djcunningham0@gmail.com")

Spd_url <- a(href="https://www.fangraphs.com/library/offense/spd/",
             "speed scores")


# these need to match the discrete values in rf_probs
exit_velo_choices    <- list(vals=seq(50, 120, 5), default=85, step=5)
launch_angle_choices <- list(vals=seq(-40, 85, 5), default=10, step=5)
spray_angle_choices  <- list(vals=seq(-45, 45, 5), default=0, step=5)
Spd_choices          <- list(vals=seq(1, 9, 0.5), default=4.5, step=0.5)
ballpark_choices     <- sort(unique(rf_probs$home_team))


# ui ----------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title="Statcast-Enhanced Batting Projections",
                  titleWidth=400)
  
  ,dashboardSidebar(
    sidebarMenu(
      id="tabs"
      ,menuItem("Batted Ball Predictions", tabName="batted", icon=icon("baseball-ball"))
      ,menuItem("Projections", tabName="projections", icon=icon("database"))
      ,menuItem("Who's Been Lucky?", tabName="lucky", icon=icon("baseball-ball"))
      ,menuItem("Methodology", tabName="methodology")
    )
    ,textOutput("res")
    ,textOutput("test")
  )
  
  ,dashboardBody(
    
    tabItems(
      
      
      # begin batted balls tab
      tabItem(
        tabName="batted"
        ,h2("Batted Ball Predictions")
        
        ,p("Predictions are made using a random forest model (see the Methodology tab for details and", 
           github_url, "for source code).")
        
        ,sidebarPanel(
          # note: the step sizes must match the discrete values in rf_probs
          sliderInput("exit_velocity", "Exit Velocity", 
                      min=min(exit_velo_choices$vals), 
                      max=max(exit_velo_choices$vals), 
                      value=exit_velo_choices$default, 
                      step=exit_velo_choices$step)
          ,sliderInput("launch_angle", "Launch Angle", min=min(launch_angle_choices$vals), 
                       max=max(launch_angle_choices$vals), 
                       value=launch_angle_choices$default, 
                       step=launch_angle_choices$step)
          ,sliderInput("spray_angle", "Spray Angle", 
                       min=min(spray_angle_choices$vals), 
                       max=max(spray_angle_choices$vals), 
                       value=spray_angle_choices$default, 
                       step=spray_angle_choices$step)
          ,sliderInput("Spd", "Batter Speed Score", 
                       min=min(Spd_choices$vals), 
                       max=max(Spd_choices$vals), 
                       value=Spd_choices$default, 
                       step=Spd_choices$step)
          ,div(p("More about ", Spd_url), style="font-size:80%")
          ,selectInput("ballpark", "Ballpark", choices=ballpark_choices)
          
          ,actionButton("random", "Random Values", icon=icon("refresh"))
        ) # end sidebarPanel
        
        ,mainPanel(
          fluidRow(
            column(
              width=12
              ,div(tableOutput("probs"), style="font-size:120%")
              ,align="center"
            )
          )
          
          ,box(title="Spray Angle",
               collapsible=FALSE,
               plotOutput("spray_chart")
          )
          ,box(title="Launch Angle",
               collapsible=FALSE,
               plotOutput("launch_velo")
          )
        ) # end mainPanel - batted balls
      ) # end tabItem - batted balls
      
      # begin projections tab
      ,tabItem(
        tabName="projections"
        ,h2("Full Season Projections")
        # ,p("More coming soon, but for now view or download the projections for the 2018 season",
        #    "and explore hit probabilities for different batted balls.")
        
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
                        choices=sort(unique(marcel_df$Season), decreasing=TRUE))
          )
        )
        
        ,downloadButton("download", "Download projections (all seasons)")
        ,downloadButton("download_xstats", "Download xStats (all seasons)")
        
        ,fluidRow(width=4,column(width=4, style='padding:5px'))
        
        ,htmlOutput("note_2017")
        ,div("filler", style="opacity:0.0;")
        ,dataTableOutput("datatable")
      ) # end tabItem - full season projections
      
      # begin lucky/unlucky tab
      ,tabItem(
        tabName="lucky"
        
        ,div(style="display:inline-block",
             selectInput("lucky_season", "Season", 
                         choices=sort(unique(lucky_df$Season), decreasing=TRUE),
                         width="100px"))
        ,div(style="display:inline-block",
             selectInput("stat", "Statistic", 
                         choices=c("wOBA", "OPS", "OBP", "SLG", "AVG", 
                                   "H", "1B", "2B", "3B", "HR"),
                         selected="wOBA",
                         width="100px"))
        ,div(style="display:inline-block",
             numericInput("AB_cutoff", "Minimum ABs", 
                         value=200,
                         min=0,
                         width="100px"))
        
        ,htmlOutput("date_message")
        
        ,h2("Luckiest Batters")
        ,dataTableOutput("lucky_table")
        
        ,h2("Unluckiest Batters")
        ,dataTableOutput("unlucky_table")
      ) # end tabItem - lucky
      
      ,tabItem(
        tabName="methodology"
        # this HTML is created from methodology.Rmd in the main folder
        ,includeHTML("methodology.html")
      ) # end tabItem - methodology
    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage


# server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # begin batted ball predictions
  
  observeEvent(
    input$random,
    {
      ga_collect_event(event_category="Usage", event_action="randomize inputs")
      updateSliderInput(session, "exit_velocity", value=sample(exit_velo_choices$vals, 1))
      updateSliderInput(session, "launch_angle", value=sample(launch_angle_choices$vals, 1))
      updateSliderInput(session, "spray_angle", value=sample(spray_angle_choices$vals, 1))
      updateSliderInput(session, "Spd", value=sample(Spd_choices$vals, 1))
      updateSelectInput(session, "ballpark", selected=sample(ballpark_choices))
    })
  
  ## visualize the spray angle
  output$spray_chart <- renderPlot({
    r <- input$exit_velocity * 3  # radius for the arrow
    theta <- NISTdegTOradian(input$spray_angle)
    xyplot(c(-25, 525) ~ c(-325, 325),
           panel=function() {
             my.panel.baseball()
             larrows(x0=0, y0=0, x1=r*sin(theta), y1=r*cos(theta), col='blue',
                     length=.15, lwd=2)
           },
           xlab="", ylab="", scales=list(draw=FALSE))
  })
  
  ## visualize the launch angle
  output$launch_velo <- renderPlot({
    velo <- input$exit_velocity
    theta <- NISTdegTOradian(input$launch_angle)
    xyplot(c(-92, 120) ~ c(-10, 125),
           panel=function() {
             panel.abline(h=0, lty=2, alpha=.5)
             panel.abline(v=0, lty=2, alpha=.5)
             larrows(x0=0, y0=0, x1=velo*cos(theta), y1=velo*sin(theta), col='blue',
                     length=.15, lwd=2)
           },
           xlab="horizontal", ylab="vertical", scales=list(draw=FALSE))
  })
  
  ## prediction table
  output$probs <- renderTable({
    ga_collect_pageview(page="/batted", title="Batted Ball Predictions")
    
    probs <- rf_probs %>% 
      filter(launch_speed == input$exit_velocity,
             launch_angle == input$launch_angle,
             spray_angle  == input$spray_angle,
             Spd          == input$Spd,
             home_team    == input$ballpark) %>% 
      select(out, single, double, triple, home_run) %>% 
      rename(Out = out,
             Single = single,
             Double = double,
             Triple = triple,
             `Home Run` = home_run) %>% 
      as.matrix()  # so formatC works
    
    pred <- which.max(probs)
    tmp <- colnames(probs)
    probs <- paste0(formatC(probs*100, digits=1, format="f"), "%")
    names(probs) <- tmp
    probs[pred] <- paste0("<b>", probs[pred], "</b>")  # bold the prediction
    
    t(probs)
  }, align='c', sanitize.text.function=function(x){x})  # sanitize.text.function applies HTML formatting
  
  # end batted ball predictions
  
  # begin full season projections
  
  vals <- reactiveValues()
  
  observeEvent(
    input$method,
    {
      cols <- c("xH", "xX1B", "xX2B", "xX3B", "xHR",
                "xBA", "xOBP", "xSLG", "xOPS", "xwOBA")
      
      if (input$method == method_choices[2]) {
        cols <- paste0(cols, "_rf")
      }
      
      cols <- c("Name", "xPA", "xAB", cols)
      vals$cols <- cols
    }
  )
  
  output$datatable <- renderDataTable({
    ga_collect_pageview(page="/projections", title="Full Season Projections")
    
    out_df <- marcel_df %>%
      filter(("ALL" %in% input$player) | (Name %in% input$player),
             Season == input$season) %>%
      select(vals$cols)
    
    colnames(out_df) <- colnames(out_df) %>%
      strip_x() %>%
      strip_suffixes()
    
    out_df
  }, options=list(autoWidth=FALSE
                  ,order=list(length(vals$cols)-1, 'desc')  # note, DataTable columns are 0-indexed
                  ,columnDefs=list(list(width="120px", targets=0))
                  )
  )
  
  output$download <- downloadHandler(
    filename="statcast_marcel_projections.csv",
    content=function(file) {
      ga_collect_event(event_category="Download", event_action="download projections")
      write_csv(marcel_df, path=file)
    }
  )
  
  output$download_xstats <- downloadHandler(
    filename="random_forest_xstats.csv",
    content=function(file) {
      ga_collect_event(event_category="Download", event_action="download xStats")
      write_csv(lucky_df, path=file)
    }
  )
  
  observeEvent(
    list(input$season, input$method),
    {
      if (input$season == 2017 & input$method != method_choices[1]) {
        vals$note_2017 <- "<b>Note:</b> 2017 projections are based partially on 2014 observed (not predicted) stats."
      } else {
        vals$note_2017 <- NULL
      }
    }
  )
  
  output$note_2017 <- renderText(vals$note_2017)
  
  # end full season projections
  
  # begin lucky batters
  
  observeEvent(
    input$lucky_season,
    {
      vals$lucky_df <- lucky_df %>% 
        filter(Season == input$lucky_season)
      
      if ((input$lucky_season == format(Sys.Date(), "%Y")) & (cur_season_max_G < 162)) {
        vals$date_message <- paste0("<b>Note:</b> Stats are current through ", 
                                    month(cur_season_max_date), "/", day(cur_season_max_date), "/",
                                    year(cur_season_max_date), " games.")
      } else {
        vals$date_message <- NULL
      }
    }
  )
  
  output$date_message <- renderText(vals$date_message)
  
  observeEvent(
    input$stat,
    {
      vals$lucky_cols <- switch(input$stat,
                                "wOBA" = c("wOBA", "rf_wOBA", 3),
                                "OPS"  = c("OPS", "rf_OPS", 3),
                                "OBP"  = c("OBP", "rf_OBP", 3),
                                "SLG"  = c("SLG", "rf_SLG", 3),
                                "AVG"  = c("AVG", "rf_AVG", 3),
                                "H"    = c("H", "rf_H", 1),
                                "1B"   = c("X1B", "rf_single", 1),
                                "2B"   = c("X2B", "rf_double", 1),
                                "3B"   = c("X3B", "rf_triple", 1),
                                "HR"   = c("HR", "rf_home_run", 1))
    }
  )
  
  observeEvent(
    list(input$lucky_season, input$stat, input$AB_cutoff),
    {
      vals$lucky_table <- vals$lucky_df %>% 
        select(Name, G, AB, vals$lucky_cols[1:2]) %>% 
        filter(AB >= input$AB_cutoff) %>% 
        mutate_at(vals$lucky_cols[1:2], round, as.numeric(vals$lucky_cols[3])) %>% 
        mutate(Difference = !!as.name(vals$lucky_cols[1]) - !!as.name(vals$lucky_cols[2])) %>% 
        rename_at(vals$lucky_cols[2], funs(paste0("Predicted")))
    }
  )
  
  output$lucky_table <- renderDataTable({
    ga_collect_pageview(page="/lucky", title="Lucky Batters")
    
    vals$lucky_table %>% 
      arrange(desc(Difference)) %>% 
      mutate(Rank = 1:n()) %>% 
      select(Rank, everything())
  }, options=list(autoWidth=FALSE, 
                  lengthMenu=list(c(5, 10, 25, -1), c(5, 10, 25, "All")),
                  pageLength=10,
                  columnDefs=list(list(width="1px", targets=0))))
  
  output$unlucky_table <- renderDataTable({
    vals$lucky_table %>% 
      arrange(Difference) %>% 
      mutate(Rank = 1:n()) %>% 
      select(Rank, everything())
  }, options=list(autoWidth=FALSE, 
                  lengthMenu=list(c(5, 10, 25, -1), c(5, 10, 25, "All")),
                  pageLength=10,
                  columnDefs=list(list(width="1px", targets=0))))
  
  # end lucky batters
  
}

shinyApp(ui, server)
