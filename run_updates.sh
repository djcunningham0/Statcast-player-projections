#!/bin/bash

# ** Make sure this script is executable! (chmod 744) **

# launchd script is in /Users/Daniel/Library/LaunchAgents

# copy data and model files to Dropbox folder that will host data for RShiny web app
# (haven't changed anything yet, but make sure they're up to date in case the script crashes)
# (rsync copies the whole directory and overwrites what's at the destination)
cd /Users/Daniel/Documents/University\ of\ Chicago/thesis/Statcast\ linear\ weights/
/usr/bin/rsync -a --delete ./data/ ~/Dropbox/statcast_modeling_data/data/
/usr/bin/rsync -a --delete ./models/ ~/Dropbox/statcast_modeling_data/models/
/usr/bin/rsync -a --delete ./prediction_data/ ~/Dropbox/statcast_modeling_data/prediction_data/

# wait 2 minutes in case you're just turning on the computer and it doesn't 
# establish an internet connection right away
sleep 120

# run R scripts to update raw data, then update prediction data
/usr/local/bin/Rscript update_data_files.R
/usr/local/bin/Rscript update_prediction_data.R

# copy files again after updating
/usr/bin/rsync -a --delete ./data/ ~/Dropbox/statcast_modeling_data/data/
/usr/bin/rsync -a --delete ./models/ ~/Dropbox/statcast_modeling_data/models/
/usr/bin/rsync -a --delete ./prediction_data/ ~/Dropbox/statcast_modeling_data/prediction_data/

