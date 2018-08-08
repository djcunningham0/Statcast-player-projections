# Statcast-Enhanced Player Projections

This repository contains all of the code I used to create my Statcast-enhanced player projections. For a semi-complete description of my research, go to my web app (link in the next section) and check out the "Methodology" tab.

Here's a rundown of most of the directories and files in the repo:

* js/ – contains some JavaScript for formatting R Markdown files
* projections/ – contains CSV files of archived projections from other systems (only Steamer right now)
* related scripts/ – some auxiliary R scripts I wrote for related visualizations or exploration
* shiny_app/ – source code for my R Shiny app
* Statcast modeling.R – this is where I did most of my experimentation, so it might follow a logical train of thought
* data_update_utilities.R – defines some functions that are used in update_data_files.R and update_prediction_data.R
* define_functions.R – any custom functions you see in the code are probably defined in here
* methodology.Rmd – the HTML output from this file is shown in the R Shiny app
* plots.R – code for lots of scatterplots that I found useful at some point
* run_updates.sh – shell script that I schedule daily to update the data that powers the web app
* update_data_files.R – updates raw data files from Statcast and FanGraphs (triggered from run_updates.sh)
* update_prediction_data.R – updates prediction dataframes used in the web app (triggered from run_updates.sh)

# R Shiny Web App

You can explore the projections and model predictions in my web app made with R Shiny:
https://djcunningham0.shinyapps.io/statcast-enhanced-batting-projections/

Everything in there is based on a random forest model applied to Statcast batted ball data. The "Methodology" tab describes the whole process of fitting the model and building the player projections including some code examples.

I'll be adding more content and functionality on an ongoing basis.

# Presentations

## Saberseminar 2018

8/5/2018

Presentation title: "Improved Batting Projections Through Statcast Modeling"

Presentation slides: https://drive.google.com/drive/folders/1B6AZS3bKpyVbMas20Zgo29W2zdvwRziO?usp=sharing

## GLASC 2018

6/21/2018

Presentation title: "Predicting Future Hitting Performance from Statcast Batted Ball Data"

Presentation slides: https://drive.google.com/drive/folders/13maZpQ78Sl3GYZMx5L7dZn5gJ__Mcjwt?usp=sharing

# Contact Info

djcunningham0@gmail.com
