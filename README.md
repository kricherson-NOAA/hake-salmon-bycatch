# hake-salmon-bycatch

Modeling bycatch of Chinook salmon in the hake fishery. Code by Philip Shirk.

Notes on files: 
Files include: 

1. "01_data_preparation.R"
    - You'll need to run through this manually before fitting any models. You'll probably have to install a number of libraries, too. 
2. "02_model_definitions.R"
    - You don't need to do anything with this script. It's sourced by all the model-fitting scripts. It just needs to be in the project/working directory.
3. "03a_run_models_k-fold_cross_validation.R"
    - This script & the next 2 (weekly & yearly cross validation) are all nearly identical. The only thing that changes from script to script is the cross validation strategy and accompanying matter. 
    - After running scripts 01 and 02, you should be able to run this script without any modifications or hand-holding. 
    - Scripts 03a-c are set up to run all models and model families at the same time (e.g. each script calculates model averages and hurdle model predictions as it goes). If you want to run subsets of models (e.g. to speed things up by spreading model fitting across multiple machines), you'll need to modify script 04_aggregate_results.R to aggregate all the results from multiple runs, calculate model averages, and calculate hurdle model estimates. If you want to do that at some point, dig through the github repository I've shared for the script "aggregate results.R", and start working off that code. 
    - I mostly gave up on comparing results with and without the shoreside hake sector. If you want to add that back in, you'll need to re-run scripts 03a to 03c. There is an argument/value in each script to set whether or not the run should include the shoreside sector or not, but you can't do with and without in a single run. You'll also need to slightly modify script 04... to handle runs that exclude the shoreside sector. Right now the script just assumes that it's included.  
4. "03b_run_models_weekly_cross_validation.R"
    - This script will be the slowest by a large margin. It fits thousands of models, and model GAM 3 alone can take 1 hr + to run (depending on the specific dataset). Other models generally fit in < 2 minutes. 
5. "03c_run_models_yearly_cross_validation.R"
6. "03d_run_models_full_data.R"
    - This script fits models to the full dataset (no cross validation). It then uses the model fits for plotting covariate marginal effects and mapping models' spatial components. This script produces many figures. There's also (unrun) code in here for looking at model residuals, creating variograms, calculating Moran's I,  and producing interactive HTML maps.
    - You might be able to simply run the whole script at once if your machine allows it.  The NOAA laptop I'm working on didn't have enough RAM to run the script from start to finish, so I would restart R to run particular models. (Some of the model fit objects are > 1 GB and so is the 2nd prediction dataset, and estimating SE of random forest model fits in the ranger package is very memory intensive (especially for binomial probability forests).)  The script saves partial results and notes where I restarted R. 
7. "04_aggregate_results.R"
    - Right now this script isn't very flexible. It assumes that you've run 03a, 03b, and 03c each with all the models (and each run only once). If you've done that, you should be able to run the whole script all at once. 
8. "05_plot_results.R"
    - Produces some images for the manuscript. You might need to fiddle with the dimensions or file type of the saved plots, but initially you can just run the whole script and look at the figures that are saved to image files.
