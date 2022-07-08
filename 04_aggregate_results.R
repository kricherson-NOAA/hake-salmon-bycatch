# the purpose of this script is to aggregate results from: 
# - 03a_run_models_k-fold_cross_validation.R
# - 03b_run_models_weekly_cross_validation.R
# - 03c_run_models_yearly_cross_validation.R
# Those scripts also save the relevant results to CSV files. This script assumes
# that you run all your models at the same time; i.e. each of the 3 scripts above
# has only a single output. If you run a script multiple times for different models,
# you'll need to combine the results manually.
{
   # Inputs: 
   #  - 'data/data_prepped.rds'
   #    From 01_data_preparation.R
   #  - results of scripts
   #		"03a_run_models_k-fold_cross_validation.R"
   # 		"03b_run_models_weekly_cross_validation.R"
   # 		"03c_run_models_yearly_cross_validation.R"
   # 	  From each of those 3 scripts:
   #  - 'results.df.haul.csv'
   #  - 'results.df.hurdle.csv'
   
   # Outputs: (all saved to base_folder: results/cv-yearly)
   #  - 'results/aggregated_results.RDS'
   #     List containing all summary information
   #         1) predicted_observed_by_haul = data.frame of haul-level observations and predictions
   # 				This is the result of joining 'data/data_prepped.rds' with the predictions in each of the 
   # 				'results.df.haul.csv' AND 'results.df.hurdle.csv' tables. 
   # 				It includes all columns from 'data/data_prepped.rds' as well as the following columns from 
   # 				the 2 results.df...csv 
   # 				- observed = observed bycatch (number Chinook)
   # 	  			- predicted = predicted bycatch
   #				- shoreside = whether or not the shoreside/midwater hake sector is included
   # 	  			- model_family = binomial, gamma, or hurdle
   # 	  			- cross_val_type = k-fold, weekly, or yearly
   # 	  			- n_years_train = number of years included in the training data
   # 	  			- n_rows_train = number of rows/hauls in the training data
   # 	  			- model = name of the model (e.g. 'glm 1')
   # 	  			- model_id = unique model fit ID
   #         2) AUCs = list of calculated AUCs
   # 				auc_total = data.frame with single AUC values calculated using all hauls from 2014-2019 (for all combinations of cross_val_type, n_years_train, shoreside, & model)
   # 				auc_year  = data.frame with AUC values calculated for each year from 2014-2019 (for all combinations of cross_val_type, n_years_train, shoreside, & model)
   # 				auc_month = data.frame with AUC values for each month of the fishing season (for all combinations of cross_val_type, n_years_train, shoreside, & model)
   # 				auc_year_month = data.frame with AUC values for each month of the fishing season in each year from 2014-2019 (for all combinations of cross_val_type, n_years_train, shoreside, & model)
   # 		  3) RMSE_gamma = list of calculated RMSE values for gamma models
   # 				rmse_total = data.frame with single RMSE values for all hauls from 2014-2019 (for all combinations of cross_val_type, n_years_train, shoreside, & model)
   # 				rmse_year  = data.frame with RMSE values calculated for each year from 2014-2019 (for all combinations of cross_val_type, n_years_train, shoreside, & model)
   # 				rmse_month = data.frame with RMSE values for each month of the fishing season (for all combinations of cross_val_type, n_years_train, shoreside, & model)
   # 				rmse_year_month = data.frame with RMSE values for each month of the fishing season in each year from 2014-2019 (for all combinations of cross_val_type, n_years_train, shoreside, & model)
   # 		  4) RMSE_hurdle = list of calculated RMSE values for hurdle models (includes hauls without any observed bycatch)
   # 				rmse_total = data.frame with single RMSE values for all hauls from 2014-2019 (for all combinations of cross_val_type, n_years_train, shoreside, & model)
   # 				rmse_year  = data.frame with RMSE values calculated for each year from 2014-2019 (for all combinations of cross_val_type, n_years_train, shoreside, & model)
   # 				rmse_month = data.frame with RMSE values for each month of the fishing season (for all combinations of cross_val_type, n_years_train, shoreside, & model)
   # 				rmse_year_month = data.frame with RMSE values for each month of the fishing season in each year from 2014-2019 (for all combinations of cross_val_type, n_years_train, shoreside, & model)
   # 		  5) bycatch_target_ratios_hurdle = data.frame with reductions in bycatch-to-target ratios with different levels of fishing effort removed (does NOT include the first week of the fishing season) (for all combinations of cross_val_type, n_years_train, shoreside, & model)
   # 		  6) bycatch_target_ratios_hurdle_w1 = data.frame with reductions in bycatch-to-target ratios with different levels of fishing effort removed (ONLY includes the first week of the fishing season) (for all combinations of cross_val_type, n_years_train, shoreside, & model)
   # 		  7) bycatch_target_ratios_binomial = data.frame with reductions in bycatch-to-target ratios with different levels of fishing effort removed (calculated using ONLY binomial model predictions) (for all combinations of cross_val_type, n_years_train, shoreside, & model)
}

library(tidyverse)

# choose where to get the results from: 
#    - either the csv files 
#    - or the lists saved by the "03_run_models..." scripts. 
# NOTE! Only the CSV option is working. I didn't bother updating all the code 
#       necessary to aggregate results from the lists b/c I changed the 
#       03a-c_run_models... scripts to save all the relevant info to the csv files. 
method <- 'csv' 
# specify the folders that hold the results
# k-fold cross validation
folder_cv.k <- file.path('results', 'cv-kfold')
# weekly cross validation
folder_cv.w <- file.path('results', 'cv-1week')
# yearly cross validation
folder_cv.y <- file.path('results', 'cv-1year')


# Overview
{
   # end goal: 
   # Long dataframe that connects each haul with the predictions of that haul from different models. 
   # i. Columns for:
   #    1) Haul id
   #       a) character
   #    2) Observed bycatch
   #       a) Numeric (not always an integer b/c of averaging across hauls)
   #    3) Predicted bycatch 
   #       a) Numeric: probability or amount
   #    4) Shoreside included?
   #       a) Factor w/ 2 values
   #    5) Binomial/Gamma/Hurdle model (hurdle = gamma prediction * binomial prediction)
   #       a) Factor w/ 3 values
   #    6) CV set
   #       a) Factor with 3 values: week, year, all
   #    7) Number of years in the training set
   #       a) Integer, with 2 to 5ish unique values
   #    8) Number of rows of training data
   #       a) Integer with many unique values
   #    9) Model name ( e.g. "glm 1" )
   #       a) Factor with 12 unique values
   
   # dataframe of full results
   fr <- data.frame(
      haul_id = NA_character_,
      observed = NA_real_,
      predicted = NA_real_,
      shoreside = factor(NA, levels = c('included', 'excluded')),
      model_family = factor(NA, levels = c('binomial', 'gamma', 'hurdle')),
      cross_val_type = factor(NA, levels = c('week', 'year', 'k-fold')),
      n_years_train = NA_integer_,
      n_rows_train = NA_integer_,
      model = factor(NA, levels = c('glm 1', 'glm 2', 
                                    'gam 1', 'gam 2', 'gam 3', 
                                    'rf 1',  'rf 2',  'rf 3', 
                                    'gbt 1', 'gbt 2',
                                    'avg 1', 'avg 2')),
      model_id = NA
   )[0,]
}

## Aggregate results -----
{
   # read in the original data used to fit the models in order to connect haul 
   # information with the predictions
   {
      # data that was produced in "01_data_preparation.R"
      dat0 <- readRDS(file = 'data/data_prepped.rds')
      
      # Don'e need to exclude any sectors here
      
      # add in a "week" column
      odat <- dat0 %>% 
         dplyr::mutate(
            week = floor(as.numeric(difftime(date, as.Date(paste0(year, '-05-14')), 
                                             units = 'weeks'))) + 1,
            # in order for all the haul_id's to match, they need to be converted 
            # to a number (and optionally back to character). 
            # I'm not entirely sure why, but it's related to savingthe results 
            # to CSV, where they are converted to number.
            haul_id = as.character(as.numeric(as.character(haul_id))),
            row_id = 1:nrow(dat0)
         )
      
      rm(dat0)
   }
   
   # get the results from CSV's 
   if(method == 'csv'){
      # get results from the k-fold CV
      r.k1 <- read.csv(file = file.path(folder_cv.k, 'results.df.haul.csv')) # this has results for all models
      r.k2 <- read.csv(file = file.path(folder_cv.k, 'results.df.hurdle.csv')) # this only has results for models that make binomial & gamma predictions (i.e. NOT RF 2 or RF 3), but it includes hauls that do NOT have any observed bycatch.
      # get results from the weekly CV
      r.w1 <- read.csv(file = file.path(folder_cv.w, 'results.df.haul.csv'))
      r.w2 <- read.csv(file = file.path(folder_cv.w, 'results.df.hurdle.csv'))
      # get results from the yearly CV
      r.y1 <- read.csv(file = file.path(folder_cv.y, 'results.df.haul.csv'))
      r.y2 <- read.csv(file = file.path(folder_cv.y, 'results.df.hurdle.csv'))
      
      # reformat and combine
      fdat <- rbind(
         # k-fold cross validation; binomial & gamma models
         r.k1 %>% 
            dplyr::mutate(
               shoreside      = factor('included', levels = c('included', 'excluded')),
               cross_val_type = factor('k-fold', levels = c('weekly', 'yearly', 'k-fold')),
               n_years_train = 18,
               n_rows_train = NA,
            ) %>% 
            dplyr::select(names(fr)),
         # k-fold cross validation; hurdle models
         r.k2 %>% 
            dplyr::mutate(
               model_family   = factor('hurdle',   levels = c('binomial', 'gamma', 'hurdle')),
               shoreside      = factor('included', levels = c('included', 'excluded')),
               cross_val_type = factor('k-fold',   levels = c('weekly', 'yearly', 'k-fold')),
               n_years_train = 18,
               n_rows_train = NA,
               model_id = paste(model_id_bin, model_id_gam, sep = '_')
            ) %>% 
            dplyr::select(names(fr)),
         
         # weekly cross validation; binomial & gamma models
         r.w1 %>% 
            dplyr::mutate(
               shoreside      = factor('included', levels = c('included', 'excluded')),
               cross_val_type = factor('weekly', levels = c('weekly', 'yearly', 'k-fold')),
               n_rows_train = NA,
            ) %>% 
            dplyr::select(names(fr)),
         # weekly cross validation; hurdle models
         r.w2 %>% 
            dplyr::mutate(
               model_family   = factor('hurdle',   levels = c('binomial', 'gamma', 'hurdle')),
               shoreside      = factor('included', levels = c('included', 'excluded')),
               cross_val_type = factor('weekly',   levels = c('weekly', 'yearly', 'k-fold')),
               n_rows_train = NA,
               model_id = paste(model_id_bin, model_id_gam, sep = '_')
            ) %>% 
            dplyr::select(names(fr)),
         
         # yearly cross validation; binomial & gamma models
         r.y1 %>% 
            dplyr::mutate(
               shoreside      = factor('included', levels = c('included', 'excluded')),
               cross_val_type = factor('yearly', levels = c('weekly', 'yearly', 'k-fold')),
               n_rows_train = NA,
            ) %>% 
            dplyr::select(names(fr)),
         # yearly cross validation; hurdle models
         r.y2 %>% 
            dplyr::mutate(
               model_family   = factor('hurdle',   levels = c('binomial', 'gamma', 'hurdle')),
               shoreside      = factor('included', levels = c('included', 'excluded')),
               cross_val_type = factor('yearly',   levels = c('weekly', 'yearly', 'k-fold')),
               n_rows_train = NA,
               model_id = paste(model_id_bin, model_id_gam, sep = '_')
            ) %>% 
            dplyr::select(names(fr))
      ) %>% 
         dplyr::mutate(haul_id = as.character(haul_id)) %>% 
         # join on original haul data
         dplyr::left_join(x = ., 
                          y = odat %>% dplyr::mutate(haul_id = as.character(haul_id)), 
                          by = 'haul_id') %>% 
         # save some memory by converting some columns to factors
         mutate(haul_id       = factor(haul_id),
                model_family  = factor(model_family),
                n_years_train = factor(n_years_train),
                model         = factor(model, levels = levels(fr$model)),
                model_id      = factor(model_id))
      
      str(fdat)
      
      # make sure that all the haul_id's matched 
      if(any( !( fdat$haul_id %in% (odat$haul_id) ) )) stop('Some haul_ids from the the model results were not found in the original dataset!!!')
   } 
   
   # get results from list objects (NOT IMPLEMENTED!)
   if(FALSE){
      
      # NOTE!!! I didn't bother updating all this code. Currently this script only 
      # works using the CSV files. In the past I used the list files b/c I had not
      # saved the haul_id's in the CSV files, so I had to go through the full results
      # lists. But it's much easier just to use the CSVs.
      
      
      # Basic idea: 
      # 1. use the original model-fitting scripts ("03_run_models...") to loop 
      #    in the same way, but instead of fitting the models, extract the observations,
      #    predictions, and any other info from the saved list. 
      #        - basically just copy and paste the for-loop structure from the 
      #          "03_run_models..." scripts and replace model-fitting functions 
      #          with code to extract relevant results from the lists.
      # 2. Add the observations and predictions, along with all the other columns
      #    outlined above, to the new output data.frame
      
      # Then with the FULL results, I can subdivide however I like and calculate
      # fit metrics like AUC and RMSE on exactly the same hauls/datasets for different models.
      
      ## First, the k-fold cv models
      {
         # read in the results list
         res <- readRDS(file = file.path(folder_cv.k, 'results.list.RDS'))
         results_list <- res$list
         
         # to get info on the models that were run: 
         dfm <- res$dataframe_model
         
         # get the train/test split index from the results list
         tt.idx <- res$train_test_index
         
         # which parts of the hurdle model were run: binomial and/or gamma 
         model_family <- unique(dfm$model_family)
         # number of folds for cross validation
         folds <- length(tt.idx[[1]]) # 5 
         # number of times to repeat the cross validation
         reps  <- length(tt.idx)      # 1
         
         # whether or not the shoreside hake sector was excluded from analyses
         exclude_shoreside_hake_sector <- FALSE
         
         # models that were fit   
         model_names  <- unique(dfm$model)
         
         
         # run the loop over the replications
         for(y in 1:reps){ # indexing with "y" to match b3.R (years)
            
            # loop over the k folds within a given repetition
            for(i in 1:folds){ # indexing with "i" to match b3.R
               
               
            } # end loop over the k folds
         } # end for loop over different replications values
         
         # re-format some things
         fr <- fr %>% 
            mutate(# haul_id = factor(haul_id, levels = unique(dat0$haul_id)),
               observed = as.numeric(observed),
               shoreside = factor(shoreside, levels = c('include', 'exclude')),
               model_family = factor(model_family, levels = c('binomial', 'gamma')),
               cross_val_type = factor(cross_val_type, levels = c('k-fold', 'week', 'year')),
               # n_years_train = as.integer(n_years_train),
               model_name = factor(model_name, levels = c('glm 1', 'glm 2', 'gam 1', 
                                                          'gam 2', 'gam 3', 'rf 1', 
                                                          'rf 2', 'rf 3', 'gbm 1')),
               model_id = factor(model_id))
         
         # save the "full" results dataframe
         # this is a really large dataframe, so I'll use data.table to speed things up
         # write.csv(fr, file = file.path('results', 'full_results.csv'), row.names = FALSE)
         # data.table::fwrite(fr, file = file.path('results', 'full_results.csv'), row.names = FALSE)
         # the RDS file (even without compressing) is 8% as large, so I'll go with that.
         saveRDS(object = fr, file = file.path('results', 'full_results_k-fold.rds')) # this is such a terrible name for a subset of the results...
         
         # remove all the stuff created here just to make sure I don't screw something up later on
         rm(list = ls()[!(ls() == 'fr')])
         fr <- fr[0,]
      }
      
      ## Next the 1-week models
      
      ## Finally the 1-year models
      
      
      # read in the predictions from each of the cross validation approaches
      {
         # predictions for weekly time series cross validation
         pre_w <- readRDS('results/full_results_week.rds') %>% 
            dplyr::mutate(haul_id = factor(haul_id, levels = unique(dat0$haul_id)),
                          n_years_train = as.character(n_years_train))
         # predictions for yearly time series cross validation
         pre_y <- readRDS('results/full_results_year.rds') %>% 
            dplyr::mutate(haul_id = factor(haul_id, levels = unique(dat0$haul_id)),
                          n_years_train = as.character(n_years_train))
         # predictions for standard k-fold cross validation (k = 5 in this case)
         pre_k <- readRDS('results/full_results_k-fold.rds') %>% 
            dplyr::mutate(haul_id = factor(haul_id, levels = unique(dat0$haul_id)))
         
         # combine them into a single dataframe
         pre0 <- rbind(
            pre_w, pre_y, pre_k
         ) #  %>% dplyr::mutate(n_years_train = factor(n_years_train))
         rm(pre_w, pre_y, pre_k)
      }
      
      # Maybe also calculate model averages again
      {
         # model average including all models
         ma_1 <- pre0 %>% 
            dplyr::group_by(cross_val_type,
                            shoreside,
                            model_family,
                            n_years_train,
                            haul_id) %>% 
            dplyr::summarise(n.models = n(),
                             predicted = mean(predicted),
                             observed = ifelse(length(unique(observed)) > 1, -99, unique(observed)),
                             n_rows_train = ifelse(length(unique(n_rows_train)) > 1, -99, unique(n_rows_train)),
                             model_name = 'mod avg',
                             model_id = NA_character_,
                             .groups = 'drop')
         ma_1 %>% with(., table(n.models, model_family))
         ma_1 %>% filter(
            observed == -99 |
               n_rows_train == -99
         )
         
         # model average EXCLUDING gam 3 (b/c gam 3 can get funky)
         ma_2 <- pre0 %>% 
            dplyr::filter( model_name != 'gam 3') %>% 
            dplyr::group_by(cross_val_type,
                            shoreside,
                            model_family,
                            n_years_train,
                            haul_id) %>% 
            dplyr::summarise(n.models = n(),
                             predicted = mean(predicted),
                             observed = ifelse(length(unique(observed)) > 1, -99, unique(observed)),
                             n_rows_train = ifelse(length(unique(n_rows_train)) > 1, -99, unique(n_rows_train)),
                             model_name = 'ma no gam3',
                             model_id = NA_character_,
                             .groups = 'drop')
         ma_2 %>% with(., table(n.models, model_family))
         ma_2 %>% filter(
            observed == -99 |
               n_rows_train == -99
         )
         
         # combine the model averages with the other model predictions
         pre <- rbind(pre0 %>% 
                         dplyr::mutate(model_name = as.character(model_name),
                                       model_id = as.character(model_id)) %>% 
                         dplyr::select(names(pre0)),
                      ma_1 %>% 
                         dplyr::mutate(model_id = NA_character_) %>% 
                         dplyr::select(names(pre0)),
                      ma_2 %>% 
                         dplyr::mutate(model_id = NA_character_) %>% 
                         dplyr::select(names(pre0))) %>%
            dplyr::mutate(model_name = factor(model_name, 
                                              levels = c('glm 1', 'glm 2', 'gam 1', 
                                                         'gam 2', 'gam 3', 'rf 1', 
                                                         'rf 2', 'rf 3', 'gbm 1', 
                                                         'mod avg', 'ma no gam3')),
                          model_id = factor(model_id)) %>% 
            dplyr::select(names(pre0))
         
         
         # add in date for each haul now b/c I'm going to use it alot
         pre$date   <- dat0$date[   match(pre$haul_id, dat0$haul_id, nomatch = 0L) ]
         pre$sector <- dat0$sector[ match(pre$haul_id, dat0$haul_id, nomatch = 0L) ]
         # might as well add in year, month, and week while I'm at it...
         pre <- pre %>%
            dplyr::mutate(year  = format(date, format = '%Y'),
                          month = format(date, format = '%m'),
                          week  = format(date, format = '%W'))
      }
   }
}

## Calculate AUC & RMSE -----
{
   # Functions to evaluate model performance
   {
      # these functions are duplicated uneccessarily from 02_model_definitions.R
      
      # Area Under Curve
      calc_AUC <- function(predicted, observed){
         
         # use a tryCatch argument so that function won't crash
         AUC <- tryCatch(expr = {
            # make sure the observation is numeric and not a factor
            obs <- as.numeric(as.character(observed))
            
            # create a uniform object structure of predictions
            predict <- ROCR::prediction(predictions = as.vector(predicted), 
                                        labels = obs)
            
            # "performance" is a ROCR function for calculating many metrics of model performance, including AUC
            round(x = unlist(slot(object = ROCR::performance(predict, "auc"), 
                                  name = "y.values")), 
                  digits = 3)
         }, error = function(e) return(NA))
         
         return(AUC)
      }
      
      # Root Mean Squared Error
      calc_RMSE <- function(predicted, 
                       observed, 
                       normalize_by = c('none', 'mean', 'range', 'IQrange', 'sd')[1]){
         
         rmse = sqrt(mean((predicted - observed)^2))
         
         switch(tolower(normalize_by),
                'none' = {
                   rmse
                },
                'mean' = {
                   rmse <- rmse / mean(observed)
                },
                'range' = {
                   diff <- max(observed) - min(observed)
                   rmse <- rmse / diff
                },
                'sd' = {
                   diff <- sd(observed)
                   rmse <- rmse / diff
                },
                'iqrange' = {
                   diff <- unname(quantile(observed, 0.75) - quantile(observed, 0.25))
                   rmse <- rmse / diff
                })
         
         rmse <- round(rmse, 3)
         return(rmse)
      }
   } 
   
   # for a fair comparison, have to exclude the first week of every year
   # because weekly time series cross validation doesn't predict the first week
   # of any given year
   week_1_haul_ids <- odat %>% 
      dplyr::filter(week == 1) %>% 
      dplyr::pull(haul_id)
   
   # calculate AUC
   {
      # single AUC value for each cross validation method
      {
         # specify the grouping for calculating AUC values
         auc_total_group <- fdat %>% 
            dplyr::filter(model_family == 'binomial' & 
                             year > 2013 & # 2014 is the first year that all models predict
                             # would need to change this if you change the number of years
                             # of training data in the time-series cross validation
                             # e.g. year >  ( 2002 + (fdat %>% filter(cross_val_type != 'k-fold') %>% pull(n_years_train) %>% as.character() %>% as.numeric() %>% max()) - 1)
                             
                             # exclude hauls that were in the first week of the fishing season
                             !(haul_id %in% week_1_haul_ids)) %>% 
            dplyr::group_by(cross_val_type, n_years_train, shoreside, model)
         
         # make sure that no individual haul is included multiple times in any single AUC calculation
         auc_total_group %>% 
            dplyr::summarise(n_each_haul = paste(unique(table(haul_id)), collapse = ','),
                             each_haul_1x = max(unique(table(haul_id))) == 1,
                             .groups = 'drop') %>% 
            pull(each_haul_1x) %>% 
            `!`() %>% 
            any() %>% 
            if(.) stop('Some hauls are used more than once in calculating AUC values!!!')
         
         # calculate the AUCs
         auc_total <- auc_total_group %>% 
            dplyr::summarise(n = n(),
                             AUC = calc_AUC(predicted = predicted, observed = observed),
                             unique_observed = paste(unique(observed), collapse = ', '),
                             .groups = 'drop')
         rm(auc_total_group)
      }
      
      # single AUC value for each cross validation method & year
      {
         # weekly time-series cross validation has the fewest hauls that are included
         # (excludes the first week of each year and also 82 hauls that reach into the 2nd week. I'm not quite sure why.)
         # specify the grouping for calculating AUC values
         auc_year_group <- fdat %>% 
            dplyr::filter(model_family == 'binomial' & 
                             year > 2013 & # 2014 is the first year that all models predict
                             # would need to change this if you change the number of years
                             # of training data in the time-series cross validation
                             # e.g. year >  ( 2002 + (fdat %>% filter(cross_val_type != 'k-fold') %>% pull(n_years_train) %>% as.character() %>% as.numeric() %>% max()) - 1)
                             
                             # exclude hauls that were in the first week of the fishing season
                             !(haul_id %in% week_1_haul_ids)) %>% 
            dplyr::group_by(cross_val_type, n_years_train, year, shoreside, model)
         
         # make sure that no individual haul is included multiple times in any single AUC calculation
         auc_year_group %>% 
            dplyr::summarise(n_each_haul = paste(unique(table(haul_id)), collapse = ','),
                             each_haul_1x = max(unique(table(haul_id))) == 1,
                             .groups = 'drop') %>% 
            pull(each_haul_1x) %>% 
            `!`() %>% 
            any() %>% 
            if(.) stop('Some hauls are used more than once in calculating AUC values!!!')
         
         # calculate the AUCs
         auc_year <- auc_year_group %>% 
            dplyr::summarise(n = n(),
                             AUC = calc_AUC(predicted = predicted, observed = observed),
                             unique_observed = paste(unique(observed), collapse = ', '),
                             .groups = 'drop')
         rm(auc_year_group)
      }
      
      # single AUC value for each cross validation method & month
      {
         # weekly time-series cross validation has the fewest hauls that are included
         # (excludes the first week of each year and also 82 hauls that reach into the 2nd week. I'm not quite sure why.)
         # specify the grouping for calculating AUC values
         auc_month_group <- fdat %>% 
            dplyr::filter(model_family == 'binomial' & 
                             year > 2013 & # 2014 is the first year that all models predict
                             # would need to change this if you change the number of years
                             # of training data in the time-series cross validation
                             # e.g. year >  ( 2002 + (fdat %>% filter(cross_val_type != 'k-fold') %>% pull(n_years_train) %>% as.character() %>% as.numeric() %>% max()) - 1)
                             
                             # exclude hauls that were in the first week of the fishing season
                             !(haul_id %in% week_1_haul_ids)) %>% 
            dplyr::group_by(cross_val_type, n_years_train, month, shoreside, model)
         
         # make sure that no individual haul is included multiple times in any single AUC calculation
         auc_month_group %>% 
            dplyr::summarise(n_each_haul = paste(unique(table(haul_id)), collapse = ','),
                             each_haul_1x = max(unique(table(haul_id))) == 1,
                             .groups = 'drop') %>% 
            pull(each_haul_1x) %>% 
            `!`() %>% 
            any() %>% 
            if(.) stop('Some hauls are used more than once in calculating AUC values!!!')
         
         # calculate the AUCs
         auc_month <- auc_month_group %>% 
            dplyr::summarise(n = n(),
                             AUC = calc_AUC(predicted = predicted, observed = observed),
                             unique_observed = paste(unique(observed), collapse = ', '),
                             .groups = 'drop')
         rm(auc_month_group)
      }
      
      # AUC by cross validation method, year, & month
      {
         # specifiy the grouping for calculating AUC values
         auc_group <- fdat %>% 
            dplyr::filter(model_family == 'binomial' & 
                             year > 2013 & # 2014 is the first year that all models predict
                             # would need to change this if you change the number of years
                             # of training data in the time-series cross validation
                             # e.g. year >  ( 2002 + (fdat %>% filter(cross_val_type != 'k-fold') %>% pull(n_years_train) %>% as.character() %>% as.numeric() %>% max()) - 1)
                             
                             # exclude hauls that were in the first week of the fishing season
                             !(haul_id %in% week_1_haul_ids)) %>% 
            dplyr::group_by(cross_val_type, n_years_train, shoreside, year, month, model)
         
         # make sure that no individual haul is included multiple times in any single AUC calculation
         auc_group %>% 
            dplyr::summarise(n_each_haul = paste(unique(table(haul_id)), collapse = ','),
                             each_haul_1x = max(unique(table(haul_id))) == 1,
                             .groups = 'drop') %>% 
            pull(each_haul_1x) %>% 
            `!`() %>% 
            any() %>% 
            if(.) stop('Some hauls are used more than once in calculating AUC values!!!')
         
         # calculate the AUCs
         aucs <- auc_group %>% 
            dplyr::summarise(n = n(),
                             AUC = calc_AUC(predicted = predicted, observed = observed),
                             unique_observed = paste(unique(observed), collapse = ', '),
                             .groups = 'drop') %>%
            dplyr::mutate(date = as.Date(paste0(year, '-', month, '-15')))
         rm(auc_group)
      }
   }
   
   # calculate RMSE
   {
      # calculate TOTAL RMSE values (grouped by cross validation type)
      {
         # specify the grouping for calculating RMSE values
         rmse_total_group <- fdat %>% 
            dplyr::filter(model_family == 'gamma' & 
                             year > 2013 & # 2014 is the first year that all models predict
                             # would need to change this if you change the number of years
                             # of training data in the time-series cross validation
                             # e.g. year >  ( 2002 + (fdat %>% filter(cross_val_type != 'k-fold') %>% pull(n_years_train) %>% as.character() %>% as.numeric() %>% max()) - 1)
                             
                             # exclude hauls that were in the first week of the fishing season
                             !(haul_id %in% week_1_haul_ids)) %>% 
            dplyr::group_by(cross_val_type, n_years_train, shoreside, model)
         
         # make sure that no individual haul is included multiple times in any single AUC calculation
         rmse_total_group %>% 
            dplyr::summarise(n_each_haul = paste(unique(table(haul_id)), collapse = ','),
                             each_haul_1x = max(unique(table(haul_id))) == 1,
                             .groups = 'drop') %>% 
            pull(each_haul_1x) %>% 
            `!`() %>% 
            any() %>% 
            if(.) stop('Some hauls are used more than once in calculating RMSE values!!!')
         
         # calculate RMSE (and normalized RMSE)
         rmse_total <- rmse_total_group %>% 
            dplyr::summarise(n = n(),
                             RMSE =          calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'none'),
                             nRMSE_mean    = calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'mean'),
                             nRMSE_sd      = calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'sd'),
                             nRMSE_range   = calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'range'),
                             nRMSE_IQrange = calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'iqrange'),
                             unique_observed = paste(unique(observed), collapse = ', '),
                             .groups = 'drop')
         rm(rmse_total_group)
      }
      
      # calculate yearly RMSE values (grouped by cross validation type & year)
      {
         # specify the grouping for calculating RMSE values
         rmse_year_group <- fdat %>% 
            dplyr::filter(model_family == 'gamma' & 
                             year > 2013 & # 2014 is the first year that all models predict
                             # would need to change this if you change the number of years
                             # of training data in the time-series cross validation
                             # e.g. year >  ( 2002 + (fdat %>% filter(cross_val_type != 'k-fold') %>% pull(n_years_train) %>% as.character() %>% as.numeric() %>% max()) - 1)
                             
                             # exclude hauls that were in the first week of the fishing season
                             !(haul_id %in% week_1_haul_ids)) %>% 
            dplyr::group_by(cross_val_type, n_years_train, year, shoreside, model)
         
         # make sure that no individual haul is included multiple times in any single AUC calculation
         rmse_year_group %>% 
            dplyr::summarise(n_each_haul = paste(unique(table(haul_id)), collapse = ','),
                             each_haul_1x = max(unique(table(haul_id))) == 1,
                             .groups = 'drop') %>% 
            pull(each_haul_1x) %>% 
            `!`() %>% 
            any() %>% 
            if(.) stop('Some hauls are used more than once in calculating RMSE values!!!')
         
         # calculate RMSE (and normalized RMSE)
         rmse_year <- rmse_year_group %>% 
            dplyr::summarise(n = n(),
                             RMSE =          calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'none'),
                             nRMSE_mean    = calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'mean'),
                             nRMSE_sd      = calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'sd'),
                             nRMSE_range   = calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'range'),
                             nRMSE_IQrange = calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'iqrange'),
                             unique_observed = paste(unique(observed), collapse = ', '),
                             .groups = 'drop')
         rm(rmse_year_group)
      }
      
      # calculate yearly RMSE values (grouped by cross validation type & month)
      {
         # specify the grouping for calculating RMSE values
         rmse_month_group <- fdat %>% 
            dplyr::filter(model_family == 'gamma' & 
                             year > 2013 & # 2014 is the first year that all models predict
                             # would need to change this if you change the number of years
                             # of training data in the time-series cross validation
                             # e.g. year >  ( 2002 + (fdat %>% filter(cross_val_type != 'k-fold') %>% pull(n_years_train) %>% as.character() %>% as.numeric() %>% max()) - 1)
                             
                             # exclude hauls that were in the first week of the fishing season
                             !(haul_id %in% week_1_haul_ids)) %>% 
            dplyr::group_by(cross_val_type, n_years_train, month, shoreside, model)
         
         # make sure that no individual haul is included multiple times in any single AUC calculation
         rmse_month_group %>% 
            dplyr::summarise(n_each_haul = paste(unique(table(haul_id)), collapse = ','),
                             each_haul_1x = max(unique(table(haul_id))) == 1,
                             .groups = 'drop') %>% 
            pull(each_haul_1x) %>% 
            `!`() %>% 
            any() %>% 
            if(.) stop('Some hauls are used more than once in calculating RMSE values!!!')
         
         # calculate RMSE (and normalized RMSE)
         rmse_month <- rmse_month_group %>% 
            dplyr::summarise(n = n(),
                             RMSE =          calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'none'),
                             nRMSE_mean    = calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'mean'),
                             nRMSE_sd      = calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'sd'),
                             nRMSE_range   = calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'range'),
                             nRMSE_IQrange = calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'iqrange'),
                             unique_observed = paste(unique(observed), collapse = ', '),
                             .groups = 'drop')
         rm(rmse_month_group)
      }
      
      # Group hauls by cross validation type, year, & month
      {
         # specify the grouping for calculating RMSE values
         rmse_group <- fdat %>% 
            dplyr::filter(model_family == 'gamma' & 
                             year > 2013 & # 2014 is the first year that all models predict
                             # would need to change this if you change the number of years
                             # of training data in the time-series cross validation
                             # e.g. year >  ( 2002 + (fdat %>% filter(cross_val_type != 'k-fold') %>% pull(n_years_train) %>% as.character() %>% as.numeric() %>% max()) - 1)
                             
                             # exclude hauls that were in the first week of the fishing season
                             !(haul_id %in% week_1_haul_ids)) %>% 
            dplyr::group_by(cross_val_type, n_years_train, shoreside, year, month, model)
         
         # make sure that no individual haul is included multiple times in any single AUC calculation
         rmse_group %>% 
            dplyr::summarise(n_each_haul = paste(unique(table(haul_id)), collapse = ','),
                             each_haul_1x = max(unique(table(haul_id))) == 1,
                             .groups = 'drop') %>% 
            pull(each_haul_1x) %>% 
            `!`() %>% 
            any() %>% 
            if(.) stop('Some hauls are used more than once in calculating AUC values!!!')
         
         # calculate RMSE (and normalized RMSE)
         rmses <- rmse_group %>% 
            dplyr::summarise(n = n(),
                             RMSE =          calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'none'),
                             nRMSE_mean    = calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'mean'),
                             nRMSE_sd      = calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'sd'),
                             nRMSE_range   = calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'range'),
                             nRMSE_IQrange = calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'iqrange'),
                             unique_observed = paste(unique(observed), collapse = ', '),
                             .groups = 'drop') %>%
            dplyr::mutate(date = as.Date(paste0(year, '-', month, '-15')))
         rm(rmse_group)
      }
   }
   
   # calculate RMSE for hurdle models (includes hauls w/o bycatch)
   {
      # calculate TOTAL RMSE values (grouped by cross validation type)
      {
         # specify the grouping for calculating RMSE values
         rmse_total_group <- fdat %>% 
            dplyr::filter(model_family == 'hurdle' & 
                             year > 2013 & # 2014 is the first year that all models predict
                             # would need to change this if you change the number of years
                             # of training data in the time-series cross validation
                             # e.g. year >  ( 2002 + (fdat %>% filter(cross_val_type != 'k-fold') %>% pull(n_years_train) %>% as.character() %>% as.numeric() %>% max()) - 1)
                             
                             # exclude hauls that were in the first week of the fishing season
                             !(haul_id %in% week_1_haul_ids)) %>% 
            dplyr::group_by(cross_val_type, n_years_train, shoreside, model)
         
         # make sure that no individual haul is included multiple times in any single AUC calculation
         rmse_total_group %>% 
            dplyr::summarise(n_each_haul = paste(unique(table(haul_id)), collapse = ','),
                             each_haul_1x = max(unique(table(haul_id))) == 1,
                             .groups = 'drop') %>% 
            pull(each_haul_1x) %>% 
            `!`() %>% 
            any() %>% 
            if(.) stop('Some hauls are used more than once in calculating RMSE values!!!')
         
         # calculate RMSE (and normalized RMSE)
         rmse_total_hurdle <- rmse_total_group %>% 
            dplyr::summarise(n = n(),
                             RMSE =          calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'none'),
                             nRMSE_mean    = calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'mean'),
                             nRMSE_sd      = calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'sd'),
                             nRMSE_range   = calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'range'),
                             nRMSE_IQrange = calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'iqrange'),
                             unique_observed = paste(unique(observed), collapse = ', '),
                             .groups = 'drop')
         rm(rmse_total_group)
      }
      
      # calculate yearly RMSE values (grouped by cross validation type & year)
      {
         # specify the grouping for calculating RMSE values
         rmse_year_group <- fdat %>% 
            dplyr::filter(model_family == 'hurdle' & 
                             year > 2013 & # 2014 is the first year that all models predict
                             # would need to change this if you change the number of years
                             # of training data in the time-series cross validation
                             # e.g. year >  ( 2002 + (fdat %>% filter(cross_val_type != 'k-fold') %>% pull(n_years_train) %>% as.character() %>% as.numeric() %>% max()) - 1)
                             
                             # exclude hauls that were in the first week of the fishing season
                             !(haul_id %in% week_1_haul_ids)) %>% 
            dplyr::group_by(cross_val_type, n_years_train, year, shoreside, model)
         
         # make sure that no individual haul is included multiple times in any single AUC calculation
         rmse_year_group %>% 
            dplyr::summarise(n_each_haul = paste(unique(table(haul_id)), collapse = ','),
                             each_haul_1x = max(unique(table(haul_id))) == 1,
                             .groups = 'drop') %>% 
            pull(each_haul_1x) %>% 
            `!`() %>% 
            any() %>% 
            if(.) stop('Some hauls are used more than once in calculating RMSE values!!!')
         
         # calculate RMSE (and normalized RMSE)
         rmse_year_hurdle <- rmse_year_group %>% 
            dplyr::summarise(n = n(),
                             RMSE =          calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'none'),
                             nRMSE_mean    = calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'mean'),
                             nRMSE_sd      = calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'sd'),
                             nRMSE_range   = calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'range'),
                             nRMSE_IQrange = calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'iqrange'),
                             unique_observed = paste(unique(observed), collapse = ', '),
                             .groups = 'drop')
         rm(rmse_year_group)
      }
      
      # calculate yearly RMSE values (grouped by cross validation type & month)
      {
         # specify the grouping for calculating RMSE values
         rmse_month_group <- fdat %>% 
            dplyr::filter(model_family == 'hurdle' & 
                             year > 2013 & # 2014 is the first year that all models predict
                             # would need to change this if you change the number of years
                             # of training data in the time-series cross validation
                             # e.g. year >  ( 2002 + (fdat %>% filter(cross_val_type != 'k-fold') %>% pull(n_years_train) %>% as.character() %>% as.numeric() %>% max()) - 1)
                             
                             # exclude hauls that were in the first week of the fishing season
                             !(haul_id %in% week_1_haul_ids)) %>% 
            dplyr::group_by(cross_val_type, n_years_train, month, shoreside, model)
         
         # make sure that no individual haul is included multiple times in any single AUC calculation
         rmse_month_group %>% 
            dplyr::summarise(n_each_haul = paste(unique(table(haul_id)), collapse = ','),
                             each_haul_1x = max(unique(table(haul_id))) == 1,
                             .groups = 'drop') %>% 
            pull(each_haul_1x) %>% 
            `!`() %>% 
            any() %>% 
            if(.) stop('Some hauls are used more than once in calculating RMSE values!!!')
         
         # calculate RMSE (and normalized RMSE)
         rmse_month_hurdle <- rmse_month_group %>% 
            dplyr::summarise(n = n(),
                             RMSE =          calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'none'),
                             nRMSE_mean    = calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'mean'),
                             nRMSE_sd      = calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'sd'),
                             nRMSE_range   = calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'range'),
                             nRMSE_IQrange = calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'iqrange'),
                             unique_observed = paste(unique(observed), collapse = ', '),
                             .groups = 'drop')
         rm(rmse_month_group)
      }
      
      # Group hauls by cross validation type, year, & month
      {
         # specify the grouping for calculating RMSE values
         rmse_group <- fdat %>% 
            dplyr::filter(model_family == 'hurdle' & 
                             year > 2013 & # 2014 is the first year that all models predict
                             # would need to change this if you change the number of years
                             # of training data in the time-series cross validation
                             # e.g. year >  ( 2002 + (fdat %>% filter(cross_val_type != 'k-fold') %>% pull(n_years_train) %>% as.character() %>% as.numeric() %>% max()) - 1)
                             
                             # exclude hauls that were in the first week of the fishing season
                             !(haul_id %in% week_1_haul_ids)) %>% 
            dplyr::group_by(cross_val_type, n_years_train, shoreside, year, month, model)
         
         # make sure that no individual haul is included multiple times in any single AUC calculation
         rmse_group %>% 
            dplyr::summarise(n_each_haul = paste(unique(table(haul_id)), collapse = ','),
                             each_haul_1x = max(unique(table(haul_id))) == 1,
                             .groups = 'drop') %>% 
            pull(each_haul_1x) %>% 
            `!`() %>% 
            any() %>% 
            if(.) stop('Some hauls are used more than once in calculating AUC values!!!')
         
         # calculate RMSE (and normalized RMSE)
         rmses_hurdle <- rmse_group %>% 
            dplyr::summarise(n = n(),
                             RMSE =          calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'none'),
                             nRMSE_mean    = calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'mean'),
                             nRMSE_sd      = calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'sd'),
                             nRMSE_range   = calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'range'),
                             nRMSE_IQrange = calc_RMSE(predicted = predicted, observed = observed, normalize_by = 'iqrange'),
                             unique_observed = paste(unique(observed), collapse = ', '),
                             .groups = 'drop') %>%
            dplyr::mutate(date = as.Date(paste0(year, '-', month, '-15')))
         rm(rmse_group)
      }
   }
}

#### Calculate reduction in bycatch-to-target ratio -----
{
   # Based on full hurdle model
   {
      # Steps: 
      # 1. After aggregating the data, subset the data to get the predicted values for each model set (all hauls from 2014-2019). 
      # 2. Calculate the overall bycatch-to-target ratio
      # 3. Identify the hauls with the highest bycatch probability
      # 4. Remove them
      # 5. Calculate the (reduced set) bycatch-to-target ratio
      # 6. Save values
      
      # double-check that "predicted" always == "chinook_n" for the gamma models
      all.equal(fdat %>% filter(model_family == 'hurdle') %>% pull(observed),
                fdat %>% filter(model_family == 'hurdle') %>% pull(chinook_n))
      
      # divide the model predictions up into sets based on the cross validation, etc.
      # specify the grouping for calculating AUC values
      btt_groups <- fdat %>% 
         dplyr::filter(model_family == 'hurdle' & 
                          year > 2013 & # 2014 is the first year that all models predict
                          # would need to change this if you change the number of years
                          # of training data in the time-series cross validation
                          # e.g. year >  ( 2002 + (fdat %>% filter(cross_val_type != 'k-fold') %>% pull(n_years_train) %>% as.character() %>% as.numeric() %>% max()) - 1)
                          
                          # exclude hauls that were in the first week of the fishing season
                          !(haul_id %in% week_1_haul_ids)) %>% 
         dplyr::group_by(cross_val_type, n_years_train, shoreside, model)
      
      # make sure that no individual haul is included multiple times
      btt_groups %>% 
         dplyr::summarise(n_each_haul = paste(unique(table(haul_id)), collapse = ','),
                          each_haul_1x = max(unique(table(haul_id))) == 1,
                          .groups = 'drop') %>% 
         pull(each_haul_1x) %>% 
         `!`() %>% 
         any() %>% 
         if(.) stop('Some hauls are used more than once in calculating AUC values!!!')
      
      # proportions to remove
      pr <- seq(from = 0.9, to = 1, by = 0.005)
      btt_list <- vector('list', length(pr))
      
      for(i in 1:length(pr)){
         
         #calculate the cutoff for each group based on a particular proportion to remove
         btt_list[[i]] <- btt_groups %>% 
            dplyr::summarise(prop_removed = 1 - pr[i],
                             pred_quantile = quantile(predicted, probs = pr[i]),
                             n_hauls = n(),
                             chinook_full = sum(chinook_n),
                             hake_full    = sum(hake_mt),
                             btt_full     = chinook_full / hake_full,
                             chinook_reduced = sum(chinook_n[ predicted < pred_quantile ]),
                             hake_reduced    = sum(hake_mt[ predicted < pred_quantile ]),
                             btt_reduced     = chinook_reduced / hake_reduced,
                             btt_relative     = btt_reduced / btt_full,
                             hake_relative    = hake_reduced / hake_full,
                             chinook_relative = chinook_reduced / chinook_full,
                             n_exluded       = sum(as.numeric( predicted > pred_quantile )),
                             unique_observed = paste(unique(observed), collapse = ', '),
                             .groups = 'drop')
      }
      
      btts <- do.call(rbind, btt_list)
      # 
      
      # reduction in bycatch-to-target ratio
      btts %>% 
         dplyr::filter(cross_val_type == 'weekly', 
                       n_years_train == '4', 
                       shoreside == 'included') %>% 
         ggplot(., mapping = aes(x = prop_removed,
                                 y = btt_relative,
                                 color = model)) + 
         theme_bw() + 
         geom_line(size = 1) + 
         xlab(label = 'Proportion Hauls Removed') + 
         ylab(label = 'Chinook (n) per mt Hake (relative to all hauls)') + 
         scale_y_continuous(breaks = seq(0.5,1,0.1), 
                            labels = scales::percent_format())+
         scale_x_continuous(breaks = seq(0,.1,0.025), 
                            labels = scales::percent_format())
      #
      
      # reduction in chinook
      btts %>% 
         dplyr::filter(cross_val_type == 'weekly', 
                       n_years_train == '4', 
                       shoreside == 'included') %>% 
         ggplot(., mapping = aes(x = prop_removed,
                                 y = chinook_reduced,
                                 color = model)) + 
         theme_bw() + 
         geom_line(size = 1) + 
         xlab(label = 'Proportion Hauls Removed') + 
         ylab(label = 'Chinook Bycatch (n)') + 
         scale_y_continuous(labels = scales::comma_format())+
         scale_x_continuous(breaks = seq(0,.1,0.025), 
                            labels = scales::percent_format())
      # reduction in hake
      btts %>% 
         dplyr::filter(cross_val_type == 'weekly', 
                       n_years_train == '4', 
                       shoreside == 'included') %>% 
         ggplot(., mapping = aes(x = prop_removed,
                                 y = hake_relative,
                                 color = model)) + 
         theme_bw() + 
         geom_line(size = 1) + 
         xlab(label = 'Proportion Hauls Removed') + 
         ylab(label = 'Hake (metric tons)') + 
         scale_y_continuous(breaks = seq(0.9,1,0.02), 
                            labels = scales::percent_format())+
         scale_x_continuous(breaks = seq(0,.1,0.025), 
                            labels = scales::percent_format())
   }
   
   # FIRST WEEK ONLY (hurdle model; excluding weekly cross validation)
   {
      # Steps: 
      # 1. After aggregating the data, subset the data to get the predicted values for each model set (all hauls from 2014-2019). 
      # 2. Calculate the overall bycatch-to-target ratio
      # 3. Identify the hauls with the highest bycatch probability
      # 4. Remove them
      # 5. Calculate the (reduced set) bycatch-to-target ratio
      # 6. Save values
      
      # double-check that "predicted" always == "chinook_n" for the gamma models
      all.equal(fdat %>% filter(model_family == 'hurdle') %>% pull(observed),
                fdat %>% filter(model_family == 'hurdle') %>% pull(chinook_n))
      
      # divide the model predictions up into sets based on the cross validation, etc.
      # specify the grouping for calculating AUC values
      btt_groups_w1 <- fdat %>% 
         dplyr::filter(model_family == 'hurdle' & 
                          year > 2013 & # 2014 is the first year that all models predict
                          # would need to change this if you change the number of years
                          # of training data in the time-series cross validation
                          # e.g. year >  ( 2002 + (fdat %>% filter(cross_val_type != 'k-fold') %>% pull(n_years_train) %>% as.character() %>% as.numeric() %>% max()) - 1)
                          
                          # ONLY INCLUDE hauls that were in the first week of the fishing season
                          (haul_id %in% week_1_haul_ids) & 
                          # exclude weekly cross val b/c it doesn't make predictions for the first week
                          cross_val_type != ('weekly')) %>% 
         dplyr::group_by(cross_val_type, n_years_train, shoreside, model)
      
      # number of hauls that we're actually dealing with: 
      btt_groups_w1 %>% pull(haul_id) %>% unique() %>% length()
      btt_groups %>% pull(haul_id) %>% unique() %>% length()
      btt_groups_w1 %>% pull(haul_id) %>% unique() %>% length() / btt_groups %>% pull(haul_id) %>% unique() %>% length()
      # 9% of hauls are in the first week.
      
      # how much of the overall bycatch is in the first week? 
      (n_bycatch_week1 <- fdat %>% 
         dplyr::filter(model_family == 'hurdle' & 
                          year > 2013 & 
                          # ONLY INCLUDE hauls that were in the first week of the fishing season
                          (haul_id %in% week_1_haul_ids) & 
                          # exclude weekly cross val b/c it doesn't make predictions for the first week
                          cross_val_type == ('yearly') & 
                          model == 'glm 1' & 
                          n_years_train == 4) %>% 
         pull(chinook_n) %>% sum())
      # bycatch the rest of the season
      (n_bycatch_not_week1 <- fdat %>% 
         dplyr::filter(model_family == 'hurdle' & 
                          year > 2013 & 
                          # ONLY INCLUDE hauls that were in the first week of the fishing season
                          !(haul_id %in% week_1_haul_ids) & 
                          # exclude weekly cross val b/c it doesn't make predictions for the first week
                          cross_val_type == ('yearly') & 
                          model == 'glm 1' & 
                          n_years_train == 4) %>% 
         pull(chinook_n) %>% sum())
      n_bycatch_week1 / n_bycatch_not_week1
      
      
      # make sure that no individual haul is included multiple times
      btt_groups_w1 %>% 
         dplyr::summarise(n_each_haul = paste(unique(table(haul_id)), collapse = ','),
                          each_haul_1x = max(unique(table(haul_id))) == 1,
                          .groups = 'drop') %>% 
         pull(each_haul_1x) %>% 
         `!`() %>% 
         any() %>% 
         if(.) stop('Some hauls are used more than once in calculating AUC values!!!')
      
      # proportions to remove
      pr <- seq(from = 0.9, to = 1, by = 0.005)
      btt_list_w1 <- vector('list', length(pr))
      
      for(i in 1:length(pr)){
         
         #calculate the cutoff for each group based on a particular proportion to remove
         btt_list_w1[[i]] <- btt_groups_w1 %>% 
            dplyr::summarise(prop_removed = 1 - pr[i],
                             pred_quantile = quantile(predicted, probs = pr[i]),
                             n_hauls = n(),
                             chinook_full = sum(chinook_n),
                             hake_full    = sum(hake_mt),
                             btt_full     = chinook_full / hake_full,
                             chinook_reduced = sum(chinook_n[ predicted < pred_quantile ]),
                             hake_reduced    = sum(hake_mt[ predicted < pred_quantile ]),
                             btt_reduced     = chinook_reduced / hake_reduced,
                             btt_relative     = btt_reduced / btt_full,
                             hake_relative    = hake_reduced / hake_full,
                             chinook_relative = chinook_reduced / chinook_full,
                             n_exluded       = sum(as.numeric( predicted > pred_quantile )),
                             unique_observed = paste(unique(observed), collapse = ', '),
                             .groups = 'drop')
      }
      
      btts_w1 <- do.call(rbind, btt_list_w1)
      # 
      
      # reduction in bycatch-to-target ratio
      btts_w1 %>% 
         dplyr::filter(cross_val_type != 'weekly', 
                       n_years_train == '4', 
                       shoreside == 'included') %>% 
         ggplot(., mapping = aes(x = prop_removed,
                                 y = btt_relative,
                                 color = model)) + 
         theme_bw() + 
         geom_line(size = 1) + 
         xlab(label = 'Proportion Hauls Removed') + 
         ylab(label = 'Chinook (n) per mt Hake (relative to all hauls)') + 
         scale_y_continuous(breaks = seq(0.5,1,0.1), 
                            labels = scales::percent_format())+
         scale_x_continuous(breaks = seq(0,.1,0.025), 
                            labels = scales::percent_format())
      #
      
      # reduction in chinook
      btts %>% 
         dplyr::filter(cross_val_type != 'weekly', 
                       n_years_train == '4', 
                       shoreside == 'included') %>% 
         ggplot(., mapping = aes(x = prop_removed,
                                 y = chinook_reduced,
                                 color = model)) + 
         theme_bw() + 
         geom_line(size = 1) + 
         xlab(label = 'Proportion Hauls Removed') + 
         ylab(label = 'Chinook Bycatch (n)') + 
         scale_y_continuous(labels = scales::comma_format())+
         scale_x_continuous(breaks = seq(0,.1,0.025), 
                            labels = scales::percent_format())
      # reduction in hake
      btts %>% 
         dplyr::filter(cross_val_type != 'weekly', 
                       n_years_train == '4', 
                       shoreside == 'included') %>% 
         ggplot(., mapping = aes(x = prop_removed,
                                 y = hake_relative,
                                 color = model)) + 
         theme_bw() + 
         geom_line(size = 1) + 
         xlab(label = 'Proportion Hauls Removed') + 
         ylab(label = 'Hake (metric tons)') + 
         scale_y_continuous(breaks = seq(0.9,1,0.02), 
                            labels = scales::percent_format())+
         scale_x_continuous(breaks = seq(0,.1,0.025), 
                            labels = scales::percent_format())
   }
   
   
   # Based ONLY on binomial models (same method as Stock et al. 2020)
   {
      # Steps: 
      # 1. After aggregating the data, subset the data to get the predicted values for each model set (all hauls from 2014-2019). 
      # 2. Calculate the overall bycatch-to-target ratio
      # 3. Identify the hauls with the highest bycatch probability
      # 4. Remove them
      # 5. Calculate the (reduced set) bycatch-to-target ratio
      # 6. Save values
      
      # double-check that "predicted" always == "chinook_n" for the gamma models
      all.equal(fdat %>% filter(model_family == 'gamma') %>% pull(observed),
                fdat %>% filter(model_family == 'gamma') %>% pull(chinook_n))
      
      # divide the model predictions up into sets based on the cross validation, etc.
      # specify the grouping for calculating AUC values
      btt_groups_b <- fdat %>% 
         dplyr::filter(model_family == 'binomial' & 
                          year > 2013 & # 2014 is the first year that all models predict
                          # would need to change this if you change the number of years
                          # of training data in the time-series cross validation
                          # e.g. year >  ( 2002 + (fdat %>% filter(cross_val_type != 'k-fold') %>% pull(n_years_train) %>% as.character() %>% as.numeric() %>% max()) - 1)
                          
                          # exclude hauls that were in the first week of the fishing season
                          !(haul_id %in% week_1_haul_ids)) %>% 
         dplyr::group_by(cross_val_type, n_years_train, shoreside, model)
      
      # make sure that no individual haul is included multiple times
      btt_groups_b %>% 
         dplyr::summarise(n_each_haul = paste(unique(table(haul_id)), collapse = ','),
                          each_haul_1x = max(unique(table(haul_id))) == 1,
                          .groups = 'drop') %>% 
         pull(each_haul_1x) %>% 
         `!`() %>% 
         any() %>% 
         if(.) stop('Some hauls are used more than once in calculating AUC values!!!')
      
      # proportions to remove
      pr <- seq(from = 0.9, to = 1, by = 0.005)
      btt_list_b <- vector('list', length(pr))
      
      for(i in 1:length(pr)){
         
         #calculate the cutoff for each group based on a particular proportion to remove
         btt_list_b[[i]] <- btt_groups_b %>% 
            dplyr::summarise(prop_removed = 1 - pr[i],
                             pred_quantile = quantile(predicted, probs = pr[i]),
                             n_hauls = n(),
                             chinook_full = sum(chinook_n),
                             hake_full    = sum(hake_mt),
                             btt_full     = chinook_full / hake_full,
                             chinook_reduced = sum(chinook_n[ predicted < pred_quantile ]),
                             hake_reduced    = sum(hake_mt[ predicted < pred_quantile ]),
                             btt_reduced     = chinook_reduced / hake_reduced,
                             btt_relative     = btt_reduced / btt_full,
                             hake_relative    = hake_reduced / hake_full,
                             chinook_relative = chinook_reduced / chinook_full,
                             n_exluded       = sum(as.numeric( predicted > pred_quantile )),
                             unique_observed = paste(unique(observed), collapse = ', '),
                             .groups = 'drop')
      }
      
      btts_b <- do.call(rbind, btt_list_b)
      # 
      
      # reduction in bycatch-to-target ratio
      btts_b %>% 
         dplyr::filter(cross_val_type == 'weekly', 
                       n_years_train == '4', 
                       shoreside == 'included') %>% 
         ggplot(., mapping = aes(x = prop_removed,
                                 y = btt_relative,
                                 color = model)) + 
         theme_bw() + 
         geom_line(size = 1) + 
         xlab(label = 'Proportion Hauls Removed') + 
         ylab(label = 'Chinook (n) per mt Hake (relative to all hauls)') + 
         scale_y_continuous(breaks = seq(0.5,1,0.1), 
                            labels = scales::percent_format())+
         scale_x_continuous(breaks = seq(0,.1,0.025), 
                            labels = scales::percent_format())
      #
      
      # reduction in chinook
      btts_b %>% 
         dplyr::filter(cross_val_type == 'weekly', 
                       n_years_train == '4', 
                       shoreside == 'included') %>% 
         ggplot(., mapping = aes(x = prop_removed,
                                 y = chinook_reduced,
                                 color = model)) + 
         theme_bw() + 
         geom_line(size = 1) + 
         xlab(label = 'Proportion Hauls Removed') + 
         ylab(label = 'Chinook Bycatch (n)') + 
         scale_y_continuous(labels = scales::comma_format())+
         scale_x_continuous(breaks = seq(0,.1,0.025), 
                            labels = scales::percent_format())
      # reduction in hake
      btts_b %>% 
         dplyr::filter(cross_val_type == 'weekly', 
                       n_years_train == '4', 
                       shoreside == 'included') %>% 
         ggplot(., mapping = aes(x = prop_removed,
                                 y = hake_relative,
                                 color = model)) + 
         theme_bw() + 
         geom_line(size = 1) + 
         xlab(label = 'Proportion Hauls Removed') + 
         ylab(label = 'Hake (metric tons)') + 
         scale_y_continuous(breaks = seq(0.9,1,0.02), 
                            labels = scales::percent_format())+
         scale_x_continuous(breaks = seq(0,.1,0.025), 
                            labels = scales::percent_format())
   }
}

# Save the results
saveRDS(object = list(
   predicted_observed_by_haul = fdat,
   AUCs = list(
      auc_total = auc_total,
      auc_year  = auc_year,
      auc_month = auc_month,
      auc_year_month = aucs
   ),
   RMSE_gamma = list(
      rmse_total = rmse_total,
      rmse_year  = rmse_year,
      rmse_month = rmse_month,
      rmse_year_month = rmses
   ),
   RMSE_hurdle = list(
      rmse_total = rmse_total_hurdle,
      rmse_year  = rmse_year_hurdle,
      rmse_month = rmse_month_hurdle,
      rmse_year_month = rmses_hurdle
   ),
   bycatch_target_ratios_hurdle = btts,
   bycatch_target_ratios_hurdle_w1 = btts_w1,
   bycatch_target_ratios_binomial = btts_b), 
   file = file.path('results', 'aggregated_results.RDS'))

# results   <- readRDS(file = file.path('results', 'aggregated_results.RDS'))
# fdat      <- results[['predicted_observed_by_haul']]
# auc_total <- results[['AUCs']][['auc_total']]
# auc_year  <- results[['AUCs']][['auc_year']]
# auc_month <- results[['AUCs']][['auc_month']]
# aucs      <- results[['AUCs']][['auc_year_month']] 
# rmse_total <- results[['RMSE_gamma']][['rmse_total']]
# rmse_year  <- results[['RMSE_gamma']][['rmse_year']]
# rmse_month <- results[['RMSE_gamma']][['rmse_month']]
# rmses      <- results[['RMSE_gamma']][['rmse_year_month']] 
# rmse_total_hurdle <- results[['RMSE_hurdle']][['rmse_total']]
# rmse_year_hurdle  <- results[['RMSE_hurdle']][['rmse_year']]
# rmse_month_hurdle <- results[['RMSE_hurdle']][['rmse_month']]
# rmses_hurdle      <- results[['RMSE_hurdle']][['rmse_year_month']] 