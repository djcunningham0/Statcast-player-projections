#!/bin/sh

# ** Make sure this script is executable! (chmod 744) **

# launchd script is in /Users/Daniel/Library/LaunchAgents

echo "starting at $(date)"

# change to the directory with the R scripts
dir=$(pwd)
cd /Users/Daniel/Documents/University\ of\ Chicago/thesis/Statcast\ linear\ weights/

# wait 2 minutes in case you're just turning on the computer and it doesn't 
# establish an internet connection right away
sleep 120

# I have this folder synced on iCloud, so the files might not be in local storage
# I'm using iCloudDownloader to download them to local storage: https://github.com/farnots/iCloudDownloader
# then wait 5 more minutes to give the files time to download (hopefully enough time?)
echo "downloading from iCloud at $(date)"
icd ./data/
icd ./models/
icd ./prediction_data/
sleep 300

# copy data and model files to Dropbox folder that will host data for RShiny web app
# (haven't changed anything yet, but make sure they're up to date in case the script crashes)
# (rsync copies the whole directory and overwrites what's at the destination)
echo "copying data to Dropbox at $(date)"
/usr/bin/rsync -a --delete ./data/ ~/Dropbox/statcast_modeling_data/data/
/usr/bin/rsync -a --delete ./models/ ~/Dropbox/statcast_modeling_data/models/
/usr/bin/rsync -a --delete ./prediction_data/ ~/Dropbox/statcast_modeling_data/prediction_data/

# run R scripts to update raw data, then update prediction data (and redirect output and error)
echo "running R scripts at $(date)"
/usr/local/bin/Rscript update_data_files.R >> /tmp/local.statcastdata.out 2>>/tmp/local.statcastdata.err
/usr/local/bin/Rscript update_prediction_data.R >> /tmp/local.statcastdata.out 2>>/tmp/local.statcastdata.err

# copy files again after updating
echo "downloading from iCloud (second time) at $(date)"
icd ./data/
icd ./models/
icd ./prediction_data/
sleep 300
echo "copying data to Dropbox (second time) at $(date)"
/usr/bin/rsync -a --delete ./data/ ~/Dropbox/statcast_modeling_data/data/
/usr/bin/rsync -a --delete ./models/ ~/Dropbox/statcast_modeling_data/models/
/usr/bin/rsync -a --delete ./prediction_data/ ~/Dropbox/statcast_modeling_data/prediction_data/

cd $dir
echo "done at $(date)"
