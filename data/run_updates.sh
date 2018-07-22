# make sure this runs again tomorrow
at -f run_updates.sh 5 am tomorrow

# copy .rds and .csv files to Dropbox folder that will host data for RShiny web app
cd /Users/Daniel/Documents/University\ of\ Chicago/thesis/Statcast\ linear\ weights/
cp ./data/*.rds ~/Dropbox/statcast_data/
cp ./data/*.csv ~/Dropbox/statcast_data/

# wait 2 minutes in case you're just turning on the computer and it doesn't 
# establish an internet connection right away
sleep 120

# run update_data_files.R
Rscript update_data_files.R

# copy files again after updating
cp ./data/*.rds ~/Dropbox/statcast_data/
cp ./data/*.csv ~/Dropbox/statcast_data/

