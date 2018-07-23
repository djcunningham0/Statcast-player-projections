# make sure this runs again tomorrow
at -f run_updates.sh 5 am tomorrow

# copy raw data and model files to Dropbox folder that will host data for RShiny web app
# (haven't changed anything yet, but make sure they're up to date in case the script crashes)
cd /Users/Daniel/Documents/University\ of\ Chicago/thesis/Statcast\ linear\ weights/
rsync -a --delete ./data/ ~/Dropbox/statcast_modeling_data/data/
rsync -a --delete ./models/ ~/Dropbox/statcast_modeling_data/models/

# wait 2 minutes in case you're just turning on the computer and it doesn't 
# establish an internet connection right away
sleep 120

# run update_data_files.R
Rscript update_data_files.R

# copy files again after updating
rsync -a --delete ./data/ ~/Dropbox/statcast_modeling_data/data/
rsync -a --delete ./models/ ~/Dropbox/statcast_modeling_data/models/

