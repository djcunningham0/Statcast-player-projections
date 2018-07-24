#!/bin/bash

# make sure this runs again tomorrow
at -f run_updates.sh 5 am tomorrow

# copy data and model files to Dropbox folder that will host data for RShiny web app
# (haven't changed anything yet, but make sure they're up to date in case the script crashes)
# (rsync copies the whole directory and overwrites what's at the destination)
cd /Users/Daniel/Documents/University\ of\ Chicago/thesis/Statcast\ linear\ weights/
rsync -a --delete ./data/ ~/Dropbox/statcast_modeling_data/data/
rsync -a --delete ./models/ ~/Dropbox/statcast_modeling_data/models/
rsync -a --delete ./prediction_data/ ~/Dropbox/statcast_modeling_data/prediction_data/

# wait 2 minutes in case you're just turning on the computer and it doesn't 
# establish an internet connection right away
sleep 120

# run R scripts to update raw data, then update prediction data
Rscript update_data_files.R
Rscript update_prediction_data.R

# copy files again after updating
rsync -a --delete ./data/ ~/Dropbox/statcast_modeling_data/data/
rsync -a --delete ./models/ ~/Dropbox/statcast_modeling_data/models/
rsync -a --delete ./prediction_data/ ~/Dropbox/statcast_modeling_data/prediction_data/

