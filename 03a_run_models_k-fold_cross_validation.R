# Run K-fold cross validation
{
   # Inputs: 
   #  - '02_model_definitions.R'
   #  - 'data/data_prepped.rds'
   #     from 01_data_preparation.R
   
   # Outputs: (all saved to base_folder: results/cv-kfold)
   #  - 'results.list.RDS'
   #     List object with all results. 
   #     Level 1 = cross validation replicate
   #     Level 2 = cv folds
   #     Level 3 = model family (binomial or gamma)
   #     Level 4 = individual models
   #  - 'results.list rep_1.RDS'
   #     List of all results for a single cv replicate
   #     (i.e. levels 2-4 above) This is essentially just an extra file I 
   #     created to be able to start investigating results before all the models finished.
   #  - 'results.df.model.csv'
   #     Table summarizing individual models. 
   #       - model = model name
   #       - model_family = model family (binomial or gamma)
   #       - model_id = random model ID to connect this dataframe with results.df.haul
   #       - fold  = cv fold
   #       - rep   = cv replicate
   #       - AUC   = Area under the receiver-operating curve for an individual model
   #       - RMSE  = Root-mean squared error for an individual model
   #       - runtime = amount of time to run an individual model
   #       - model_converged = whether or not a linear model converged, according to mgcv package (which might not mean what you think it means. e.g. https://r.789695.n4.nabble.com/mgcv-BAM-convergence-conflicting-messages-td4692253.html)
   #       - rows_training = number of rows of data used in training
   #       - rows_testing = number of rows of data used in testing
   #       - test_n_0 = number of hauls with no bycatch in the testing data
   #       - test_n_1 = number of hauls with bycatch in the testing data
   #  - 'results.df.haul.csv'
   # 	  Table of model predictions at the haul level.
   #       - model_family = model family (binomial or gamma)
   #       - model = model name
   #       - model_id = random model ID to connect this dataframe with results.df.haul
   #       - haul_id = unique identifier for each haul
   #       - observed = number of Chinook bycatch in the haul
   #       - predicted	= model prediction
   #  - 'results.df.hurdle.csv'
   #     Table summarizing hurdle model results. 
   # 	  It is very simimlar to 'results.df.haul.csv', but it includes
   #     gamma predictions for hauls withOUT bycatch. It excludes models RF 2 & 
   #     RF 3 because they do not produce full hurdle model estimates.
   #       - haul_id = unique identifier for each haul
   #       - observed = number of Chinook bycatch in the haul
   #       - pred.bin = binomial model prediction
   #       - pred.gam = gamma model prediction
   #       - predicted	= hurdle model prediction
   #       - model_id_bin = model_id for the binomial model
   #       - model_id_gam = model_id for the gamma model
   #       - model = name of model
}

# Setting most likely to change from one run to another
# set the names & locations for saving results files (only need to change 2nd item in "base_folder")
base_folder   <- file.path('results', 'cv-kfold')
exclude_shoreside_hake_sector <- FALSE


library(tidyverse)
source('02_model_definitions.R')

# other settings that I don't need to change very often
{
   # choose which half of the hurdle model to run: binomial, gamma, or both
   model_family <- c('binomial', 'gamma')
   # number of folds for cross validation
   folds <- 5
   # number of times to repeat the cross validation
   reps  <- 1
   # only do it once for now since I can't repeat the time-series cross validation in a similar manner.
   
   # show progress in the console as you go along
   print_progress <- TRUE
   
   # models to run
   model_names  <- c('glm 1', 'glm 2', 
                     'gam 1', 'gam 2', 'gam 3', 
                     'rf 1', 'rf 2', 'rf 3', 
                     'gbt 1', 'gbt 2',
                     'avg 1', 'avg 2')
   # glm 1 = glm with linear covariates (including lat & lon)
   # glm 2 = glm with 2nd order polynomials for continuous covariates (including lat & lon)
   # gam 1 = "low" complexity gam with linear covariates & 2-d smooth over lat & lon
   # gam 2 = "medium" complexity gam with low-order smooths on covariates & 2-d smooth over lat & lon
   # gam 3 = "high" complexity gam with smooths over covariates and 3-d smooth over space and time
   # inla  = "medium" complexity model with polynomial covariates and spatial random effects via GMRF 
   # rf 1  = random forest (classification or regression)
   # rf 2  = [only classification] random forest with downsampling of over-represented class (in this case, hauls without bycatch) 
   # rf 3  = [only classification] random forest with SMOTE = oversampling of the under-represented class & downsampling of the over-represented class.
   # gbt 1 = gradient boosting trees with close to default tuning parameters
   # gbt 2 = gradient boosting trees with DART
   # avg 1 = model average of all models
   # avg 2 = model average of all models *other than* gam 3 (which doesn't always work well with gamma models)
   
   # base filename for files saved to base_folder
   base_filename <- 'results'
   # create a folder for the results
   dir.create(base_folder, showWarnings = FALSE)
   # filename for the results dataframe that's saved after each model run
   intermediate_results_filename_model <- file.path(base_folder, paste0(base_filename, '.df.model.csv'))
   intermediate_results_filename_haul  <- file.path(base_folder, paste0(base_filename, '.df.haul.csv'))
   intermediate_results_filename_hurdle <- file.path(base_folder, paste0(base_filename, '.df.hurdle.csv'))
   # filename for the list with all of the results 
   final_results_filename <- file.path(base_folder, paste0(base_filename, '.list.RDS'))
}

# read in data
{
   # Note: tidyverse can cause errors trying to read in an "sf" object without 
   # "sf" loaded. If reading in "data/data_prepped_sf.rds", first load library('sf')
   
   # data that was produced in "01_data_preparation.R"
   dat0 <- readRDS(file = 'data/data_prepped.rds')
   
   # sectors to exclude
   if(exclude_shoreside_hake_sector) {
      dat0 <- dat0 %>% 
         dplyr::filter(sector != 'Midwater Hake') %>% 
         dplyr::mutate(sector = factor(sector))
   }
   
   # set bycatch to be a binomial response (will change later for gamma models)
   dat <- dat0 %>% 
      dplyr::mutate(bycatch = as.factor(as.numeric(chinook_n > 0)),
                    haul_id = as.character(haul_id))
   
   rm(dat0)
}

# Use same environmental covariates for all models
{
   # choose environmental covariates
   # names(dat0)
   covar <- c('sector', # fishing sector
              # 'year', # have to use continuous time to predict a year into the future
              'year_f', # year as a factor
              'doy',    # day of year
              'ToD',    # time of day
              'duration',      # haul duration. included it as a covariate instead of an offset b/c I assume that long hauls will only happen when there aren't a lot of fish and that really short hauls won't have many fish, either.
              'depth_fishing', # fishing depth in meters
              'depth_bottom',  # bottom depth in meters (as reported by boats for ASHOP data; NOAA CRM bathymetry for WCGOP data)
              'slope_bottom',  # bottom slope calculated from NOAA CRM bathymetry data (and from GEBCO data for ___ hauls missing CRM data)
              'sst_anomaly',   # SST anomaly
              # 'sst_anom_lag_7',
              # 'sst_anom_lag_365',
              'cuti')
              # 'cuti_lag_7')
              # 'cuti_lag_365')
   
   # Random forest includes lat/lon as covariates instead of spatial structures
   rf.covar <- c(covar, 
                 'lat',  # latitude (projected & transformed)
                 'lon')  # longitude (projected & transformed)
}

# Split data for cross validation
{
   # index of train/test splits
   tt.idx <- list()
   for(i in 1:reps){
      tt.idx[[i]] <- caret::createFolds(y = dat[,'bycatch'], 
                                        k = folds) # k-fold cross validation
      # str(tt.idx)
      # tt.idx is a list with an element for each repetition of the k-fold cross validation.
      # Each of those primary lists contains a set of k sublists: one for each fold of the cross validation
      # Each of the k sublists contains a vector with the index values of the testing data
      # so the first set of training & testing data for the first repetition of cross validation is:
      #   test 1  = dat[ tt.idx[[1]][[1]],]
      #   train 1 = dat[-tt.idx[[1]][[1]],]
   }
   # When the response variable is categorical, caret::createFolds does the random sampling WITHIN groups,
   #  so each fold will have the same proportion bycatch as the overall dataset.
}

# also directly create data.frames to hold results
{
   # dataframe for model-level results
   results.df.model <- expand.grid(
      model = model_names, # model name
      model_family = model_family,
      model_id = NA, # random ID to connect this dataframe with results.df.haul
      fold  = 1:folds,
      rep   = 1:reps,
      AUC = NA,
      RMSE = NA,
      runtime = NA,       # amount of time to run an individual model
      model_converged = NA,
      rows_training = NA, # number of rows of data used in training
      rows_testing = NA,  # number of rows of data used in testing
      test_n_0 = NA, # number of hauls with no bycatch in the testing data
      test_n_1 = NA  # number of hauls with bycatch in the testing data
   ) %>% 
      dplyr::filter( !(model %in% c('rf 2', 'rf 3') & model_family == 'gamma'))
   
   # dataframe for haul-level observations & predictions
   #    This will hold results for calculating RMSE & AUC
   #    This will get very large, so I just save it to disk, not in memory. 
   results.df.haul <- data.frame(
      model_family = NA,
      model = NA, # model name
      model_id = NA, 
      haul_id = NA,
      observed = NA,
      predicted = NA
   )[0,]
}

# set up a list to hold all the results (including haul_id, observed, & predicted values)
results_list <- vector('list', reps)
# structure of results_list
# 1st level will group results by the cross validation replicate
# 2nd level will have the training/testing splits
# 3rd level will have model family
# 4th level will have all the models

start_time <- Sys.time()
n_total_models <- nrow(results.df.model)
n_this_model <- 1

# run the loop over the replications
for(y in 1:reps){ # indexing with "y" to match b3.R (years)
   
   # set up sub-lists within the results list
   results_list[[y]] <- vector('list', folds)
   
   # fit all the models
   # loop over the k folds within a given repetition
   for(i in 1:folds){ # indexing with "i" to match b3.R
      # set up sub-lists for each model family
      results_list[[y]][[i]] <- vector('list', 2)
      names(results_list[[y]][[i]]) <- c('binomial', 'gamma') # include both even if only running 1 of the 2
      
      # set up sub-lists for each model
      for(z in 1:2){
         # exclude rf 2 and rf 3 from gamma model list
         mn <- model_names[ 
            if(z == 2 & ('rf 2' %in% model_names | 'rf 3' %in% model_names)){
               (1:length(model_names))[ -which(model_names %in% c('rf 2', 'rf 3'))] 
            } else 1:length(model_names) ]
         results_list[[y]][[i]][[z]] <- vector('list', length(mn))
         names(results_list[[y]][[i]][[z]]) <- mn
      }
      
      # extract the training and testing data for this replicate & fold
      dat.test.index  <- tt.idx[[y]][[i]]
      dat.train.index <- (1:nrow(dat))[ -dat.test.index ]
      n.test <- length(dat.test.index)
      n.train <- length(dat.train.index)
      
      # process the data for this fold
      {
         # the point of this is to scale variables based on the training dataset rather than the full dataset
         # (except that I'm not doing this once for both the binomial and gamma components of the model, which
         #  means that I *am* still doing the transformations based on a larger dataset for the gamma models.)
         dat.train <- dat[dat.train.index,]
         dat.test  <- dat[dat.test.index,]
         
         # make sure there are no new levels of categorical covariates in the testing data. 
         # no models can make predictions to new factor levels. 
         # This problem will occur in time-series cross validation when 
         # shoreside/midwater hake sector first appears in 2011.
         for(c in covar){
            # if the covariate is not a factor, skip to the next one
            if( !class(dat.train[,c]) %in% c('factor', 'character')  ) next
            
            # get the unique factor levels in the training data
            train.unique <- unique(dat.train[,c])
            # print a warning if there are any levels in the testing dataset 
            # that are NOT in the training dataset.
            levels_to_remove <- unique(dat.test[,c])[ ! unique(dat.test[,c]) %in% train.unique ]
            if(length(levels_to_remove) >0) {
               print(paste0( 'Removing level "', 
                             levels_to_remove, 
                             '" from covariate "', 
                             c, '" in the testing dataset because ', 
                             levels_to_remove, 
                             ' are not present in training dataset.'))
            }
            
            # actually remove the levels
            dat.test <- dat.test[ dat.test[,c] %in% unique(train.unique),]
         }
         
         # combine training and testing data
         dat.ik <- rbind(dat.train, dat.test) %>% 
            # add in a covariate "time" = days since 2010 that is used for gam 3 (only in yearly cross validation)
            dplyr::mutate(time = as.numeric(date - as.Date('2010-01-01')))
         # row indexes for training and testing data
         r.train <- 1:n.train
         r.test  <- 1:n.test + n.train
         
         # Add the amount of data used to train and test the models to the results dataframe
         rows_this_loop.b <- (results.df.model$rep == y) & 
            (results.df.model$fold == i) & 
            (results.df.model$model_family == 'binomial')
         results.df.model$rows_training[ rows_this_loop.b ] <- n.train
         results.df.model$rows_testing[  rows_this_loop.b ] <- n.test
         results.df.model$test_n_0[ rows_this_loop.b ]  <- nrow(dat.test[ dat.test$bycatch == '0',])
         results.df.model$test_n_1[ rows_this_loop.b ]  <- nrow(dat.test[ dat.test$bycatch == '1',])
         
         # Neither of these should be necessary now, but I'll do them as a precaution
         dat.ik$year_f <- factor(dat.ik$year)
         dat.ik$sector <- factor(dat.ik$sector)
         
         # vector to hold new list of covariate names (after processing)
         covar_processed <- vector(mode = 'character')
         
         # go through each covariate and do transformations before fitting the model
         for(c in rf.covar){
            
            # transformations are only for continuous predictor variables, 
            # so I'll skip categorical predictor variables
            if( class(dat.ik[,c]) %in% c('factor', 'character')  ){
               covar_processed <- c(covar_processed, c)
               next
            } 
            
            # use the covar_name_df to decide whether or not to log, scale, 
            # and/or polynomialize each variable
            # was the original covariate logged?
            var_logged_logical <- lsp_lut[match(x = c,
                                                table = lsp_lut$original_name),
                                          'logged']
            # was the original covariate scaled?
            var_scaled_logical <- lsp_lut[match(x = c,
                                                table = lsp_lut$original_name),
                                          'scaled']
            # was the original covariate polynomialized?
            var_poly_logical <- lsp_lut[match(x = c,
                                              table = lsp_lut$original_name),
                                        'polyd']
            
            # if the variable is to be processed, then process it
            if(any(var_logged_logical, var_scaled_logical, var_poly_logical)){
               # calculate the new covariates
               newc <- process_covariates(covariate = dat.ik[,c], 
                                          log = var_logged_logical, 
                                          scale = var_scaled_logical, 
                                          polynomialize = var_poly_logical, 
                                          index.train = r.train)
               
               # create new column names for the processed covariate values 
               if(var_poly_logical){
                  names(newc) <- paste0(c, c('_poly1', '_poly2'))
               } else if(var_scaled_logical){
                  names(newc) <- paste0(c, '_scale')
               } else if(var_logged_logical){
                  names(newc) <- paste0(c, '_log')
               }
               
               # and the processed data back into the dataset
               dat.ik <- cbind(dat.ik, newc)
               
               # add the names into the list of new covariate names
               covar_processed <- c(covar_processed, names(newc))
            } else {
               # just in case any variables slipped through unprocessed
               covar_processed <- c(covar_processed, c) 
            }
         }
         
         # subset the data for the gamma models
         if('gamma' %in% model_family ){
            r.train.g <- r.train[which( dat.ik[r.train,'chinook_n'] > 0 )]
            r.test.g  <- r.test[ which( dat.ik[r.test, 'chinook_n'] > 0 )]
            n.train.g <- length(r.train.g)
            n.test.g  <- length(r.test.g)
            
            rows_this_loop.g <- (results.df.model$rep == y) & 
               (results.df.model$fold == i) & 
               (results.df.model$model_family == 'gamma')
            results.df.model$rows_training[ rows_this_loop.g ] <- n.train.g
            results.df.model$rows_testing[  rows_this_loop.g ] <- n.test.g
            # these should all be bycatch, but I'll calculate it anyway
            results.df.model$test_n_0[ rows_this_loop.g ]  <- sum( dat.ik$bycatch[r.test.g] == '0')
            results.df.model$test_n_1[ rows_this_loop.g ]  <- sum( dat.ik$bycatch[r.test.g] == '1')
         }
      }
      
      # fit Binomial models
      if('binomial' %in% model_family){
         # fit model 1: GLM
         if('glm 1' %in% model_names){
            # model name 
            mn <- 'glm 1'
            
            # fit the model
            results_list[[y]][[i]][['binomial']][[mn]] <- fit_m1_glm(data = dat.ik, 
                                                                     covariates  = c(covar, 'lat', 'lon'), 
                                                                     modelfamily = 'binomial', 
                                                                     rows.train  = r.train,
                                                                     rows.test   = r.test)
            
            # save the haul ID's in the list object
            results_list[[y]][[i]][['binomial']][[mn]][['haul_id']] <- dat.ik[r.test, 'haul_id']
            results_list[[y]][[i]][['binomial']][[mn]][['model_name']] <- mn
            # create a model ID to help organize results
            modelid <- paste(sample( c(letters, LETTERS, 0:9), size = 25, replace = T), collapse = '')
            results_list[[y]][[i]][['binomial']][[mn]][['model_id']] <- modelid
            
            # save results to model & haul dataframes
            {
               results.df.model[ results.df.model$model == mn & 
                                    results.df.model$model_family == 'binomial' & 
                                    results.df.model$rep  == y &
                                    results.df.model$fold == i, 
                                 c( 'model_id',
                                    'AUC', 
                                    'runtime',
                                    'model_converged')] <- c(
                                       modelid,
                                       results_list[[y]][[i]][['binomial']][[mn]]$AUC,
                                       results_list[[y]][[i]][['binomial']][[mn]]$runtime,
                                       results_list[[y]][[i]][['binomial']][[mn]]$model_converged
                                    ) # note: grouping these together into a vector forces them all to character
               write.csv(x = results.df.model %>% filter(!is.na(runtime)), 
                         file = intermediate_results_filename_model, row.names = F)
               
               # haul dataframe [append the file on disk instead of growing the file in memory]
               data.table::fwrite(x = data.frame(
                  model_family = rep('binomial', n.test),
                  model     = rep(mn, n.test), # model name
                  model_id  = rep(modelid, n.test), 
                  haul_id   = dat.ik[r.test, 'haul_id'],
                  observed  = as.numeric(as.character(dat.ik[r.test, 'bycatch'])),
                  predicted = results_list[[y]][[i]][['binomial']][[mn]]$expected
               ), file = intermediate_results_filename_haul, append = T, row.names = F)
            }
            
            # track progress
            if(print_progress){
               elapsed_time <- difftime(Sys.time(), start_time, units = 'mins')
               time_remaining <- round( as.numeric(elapsed_time) / n_this_model * (n_total_models-n_this_model))
               m_time_elapsed <- if(n_this_model == 1) round(as.numeric(elapsed_time)) else round(as.numeric(difftime(Sys.time(), m_time, units = 'mins')))
               m_time <- Sys.time()
               
               print(paste0(mn, ' : ', m_time_elapsed, 'm',
                            ' |  ', n_this_model, ' of ', n_total_models, ' : ', round( as.numeric(elapsed_time)), ' min',
                            ' |  t - ', time_remaining, ' min.'))
               n_this_model <- n_this_model + 1
            }
         }
         
         # fit model 2: GLM with polynomial predictors
         if('glm 2' %in% model_names){
            # model name 
            mn <- 'glm 2'
            
            # fit the model
            results_list[[y]][[i]][['binomial']][[mn]] <- fit_m1_glm(data = dat.ik, 
                                                                     covariates  = covar_processed, 
                                                                     modelfamily = 'binomial', 
                                                                     rows.train  = r.train,
                                                                     rows.test   = r.test)
            
            # save the haul ID's in the list object
            results_list[[y]][[i]][['binomial']][[mn]][['haul_id']] <- dat.ik[r.test, 'haul_id']
            results_list[[y]][[i]][['binomial']][[mn]][['model_name']] <- mn
            # create a model ID to help organize results
            modelid <- paste(sample( c(letters, LETTERS, 0:9), size = 25, replace = T), collapse = '')
            results_list[[y]][[i]][['binomial']][[mn]][['model_id']] <- modelid
            
            # save results to model & haul dataframes
            {
               results.df.model[ results.df.model$model == mn & 
                                    results.df.model$model_family == 'binomial' & 
                                    results.df.model$rep  == y &
                                    results.df.model$fold == i, 
                                 c( 'model_id',
                                    'AUC', 
                                    'runtime',
                                    'model_converged')] <- c(
                                       modelid,
                                       results_list[[y]][[i]][['binomial']][[mn]]$AUC,
                                       results_list[[y]][[i]][['binomial']][[mn]]$runtime,
                                       results_list[[y]][[i]][['binomial']][[mn]]$model_converged
                                    )
               write.csv(x = results.df.model %>% filter(!is.na(runtime)), 
                         file = intermediate_results_filename_model, row.names = F)
               
               # haul dataframe [append the file on disk instead of growing the file in memory]
               data.table::fwrite(x = data.frame(
                  model_family = rep('binomial', n.test),
                  model     = rep(mn, n.test), # model name
                  model_id  = rep(modelid, n.test), 
                  haul_id   = dat.ik[r.test, 'haul_id'],
                  observed  = as.numeric(as.character(dat.ik[r.test, 'bycatch'])),
                  predicted = results_list[[y]][[i]][['binomial']][[mn]]$expected
               ), file = intermediate_results_filename_haul, append = T, row.names = F)
            }
            
            # track progress
            if(print_progress){
               elapsed_time <- difftime(Sys.time(), start_time, units = 'mins')
               time_remaining <- round( as.numeric(elapsed_time) / n_this_model * (n_total_models-n_this_model))
               m_time_elapsed <- if(n_this_model == 1) round(as.numeric(elapsed_time)) else round(as.numeric(difftime(Sys.time(), m_time, units = 'mins')))
               m_time <- Sys.time()
               
               print(paste0(mn, ' : ', m_time_elapsed, 'm',
                            ' |  ', n_this_model, ' of ', n_total_models, ' : ', round( as.numeric(elapsed_time)), ' min',
                            ' |  t - ', time_remaining, ' min.'))
               n_this_model <- n_this_model + 1
            }
         }
         
         # fit model 3: GAM 1 model (linear covariates with spatial smooth)
         if('gam 1' %in% model_names){
            # model name 
            mn <- 'gam 1'
            
            # fit the model
            results_list[[y]][[i]][['binomial']][[mn]] <- fit_m2_gam1(data = dat.ik, 
                                                                      covariates  = covar, 
                                                                      modelfamily = 'binomial', 
                                                                      rows.train  = r.train,
                                                                      rows.test   = r.test)
            
            # save the haul ID's in the list object
            results_list[[y]][[i]][['binomial']][[mn]][['haul_id']] <- dat.ik[r.test, 'haul_id']
            results_list[[y]][[i]][['binomial']][[mn]][['model_name']] <- mn
            # create a model ID to help organize results
            modelid <- paste(sample( c(letters, LETTERS, 0:9), size = 25, replace = T), collapse = '')
            results_list[[y]][[i]][['binomial']][[mn]][['model_id']] <- modelid
            
            # save results to model & haul dataframes
            {
               results.df.model[ results.df.model$model == mn & 
                                    results.df.model$model_family == 'binomial' & 
                                    results.df.model$rep  == y &
                                    results.df.model$fold == i, 
                                 c( 'model_id',
                                    'AUC', 
                                    'runtime',
                                    'model_converged')] <- c(
                                       modelid,
                                       results_list[[y]][[i]][['binomial']][[mn]]$AUC,
                                       results_list[[y]][[i]][['binomial']][[mn]]$runtime,
                                       results_list[[y]][[i]][['binomial']][[mn]]$model_converged
                                    )
               write.csv(x = results.df.model %>% filter(!is.na(runtime)), 
                         file = intermediate_results_filename_model, row.names = F)
               
               # haul dataframe [append the file on disk instead of growing the file in memory]
               data.table::fwrite(x = data.frame(
                  model_family = rep('binomial', n.test),
                  model     = rep(mn, n.test), # model name
                  model_id  = rep(modelid, n.test), 
                  haul_id   = dat.ik[r.test, 'haul_id'],
                  observed  = as.numeric(as.character(dat.ik[r.test, 'bycatch'])),
                  predicted = results_list[[y]][[i]][['binomial']][[mn]]$expected
               ), file = intermediate_results_filename_haul, append = T, row.names = F)
            }
            
            # track progress
            if(print_progress){
               elapsed_time <- difftime(Sys.time(), start_time, units = 'mins')
               time_remaining <- round( as.numeric(elapsed_time) / n_this_model * (n_total_models-n_this_model))
               m_time_elapsed <- if(n_this_model == 1) round(as.numeric(elapsed_time)) else round(as.numeric(difftime(Sys.time(), m_time, units = 'mins')))
               m_time <- Sys.time()
               
               print(paste0(mn, ' : ', m_time_elapsed, 'm',
                            ' |  ', n_this_model, ' of ', n_total_models, ' : ', round( as.numeric(elapsed_time)), ' min',
                            ' |  t - ', time_remaining, ' min.'))
               n_this_model <- n_this_model + 1
            }
         }
         
         # fit model 4: GAM 2 model (low-basis-dimension smooths of covariates & spatial smooth)
         if('gam 2' %in% model_names){
            # model name 
            mn <- 'gam 2'
            
            # fit the model
            # if there's not a lot of data in the training set, this model could fail
            # so I'll use a try-catch statement so that the script keeps running. 
            # the model can be fit later by lowering the k parameter in the smoothing term (e.g. k = c(80,8) instead of k = c(100,10))
            results_list[[y]][[i]][['binomial']][[mn]] <- fit_m3_gam2(data = dat.ik, 
                                                                      covariates  = covar, 
                                                                      modelfamily = 'binomial', 
                                                                      rows.train  = r.train,
                                                                      rows.test   = r.test)
            
            # save the haul ID's in the list object
            results_list[[y]][[i]][['binomial']][[mn]][['haul_id']] <- dat.ik[r.test, 'haul_id']
            results_list[[y]][[i]][['binomial']][[mn]][['model_name']] <- mn
            # create a model ID to help organize results
            modelid <- paste(sample( c(letters, LETTERS, 0:9), size = 25, replace = T), collapse = '')
            results_list[[y]][[i]][['binomial']][[mn]][['model_id']] <- modelid
            
            # save results to model & haul dataframes
            {
               results.df.model[ results.df.model$model == mn & 
                                    results.df.model$model_family == 'binomial' & 
                                    results.df.model$rep  == y &
                                    results.df.model$fold == i, 
                                 c( 'model_id',
                                    'AUC', 
                                    'runtime',
                                    'model_converged')] <- c(
                                       modelid,
                                       results_list[[y]][[i]][['binomial']][[mn]]$AUC,
                                       results_list[[y]][[i]][['binomial']][[mn]]$runtime,
                                       results_list[[y]][[i]][['binomial']][[mn]]$model_converged
                                    )
               write.csv(x = results.df.model %>% filter(!is.na(runtime)), 
                         file = intermediate_results_filename_model, row.names = F)
               
               # haul dataframe [append the file on disk instead of growing the file in memory]
               data.table::fwrite(x = data.frame(
                  model_family = rep('binomial', n.test),
                  model     = rep(mn, n.test), # model name
                  model_id  = rep(modelid, n.test), 
                  haul_id   = dat.ik[r.test, 'haul_id'],
                  observed  = as.numeric(as.character(dat.ik[r.test, 'bycatch'])),
                  predicted = results_list[[y]][[i]][['binomial']][[mn]]$expected
               ), file = intermediate_results_filename_haul, append = T, row.names = F)
            }
            
            # track progress
            if(print_progress){
               elapsed_time <- difftime(Sys.time(), start_time, units = 'mins')
               time_remaining <- round( as.numeric(elapsed_time) / n_this_model * (n_total_models-n_this_model))
               m_time_elapsed <- if(n_this_model == 1) round(as.numeric(elapsed_time)) else round(as.numeric(difftime(Sys.time(), m_time, units = 'mins')))
               m_time <- Sys.time()
               
               print(paste0(mn, ' : ', m_time_elapsed, 'm',
                            ' |  ', n_this_model, ' of ', n_total_models, ' : ', round( as.numeric(elapsed_time)), ' min',
                            ' |  t - ', time_remaining, ' min.'))
               n_this_model <- n_this_model + 1
            }
         }
         
         # fit model 5: GAM 3 model (low-basis-dimension smooths of covariates & spatial effect changes through time)
         if('gam 3' %in% model_names){
            # model name 
            mn <- 'gam 3'
            
            # fit the model
            results_list[[y]][[i]][['binomial']][[mn]] <- fit_m4_gam3(data = dat.ik, 
                                                                      covariates  = covar, # don't use the processed covariates here
                                                                      modelfamily = 'binomial', 
                                                                      rows.train  = r.train,
                                                                      rows.test   = r.test, 
                                                                      year_type = 'factor')
            
            # save the haul ID's in the list object
            results_list[[y]][[i]][['binomial']][[mn]][['haul_id']] <- dat.ik[r.test, 'haul_id']
            results_list[[y]][[i]][['binomial']][[mn]][['model_name']] <- mn
            # create a model ID to help organize results
            modelid <- paste(sample( c(letters, LETTERS, 0:9), size = 25, replace = T), collapse = '')
            results_list[[y]][[i]][['binomial']][[mn]][['model_id']] <- modelid
            
            # save results to model & haul dataframes
            {
               results.df.model[ results.df.model$model == mn & 
                                    results.df.model$model_family == 'binomial' & 
                                    results.df.model$rep  == y &
                                    results.df.model$fold == i, 
                                 c( 'model_id',
                                    'AUC', 
                                    'runtime',
                                    'model_converged')] <- c(
                                       modelid,
                                       results_list[[y]][[i]][['binomial']][[mn]]$AUC,
                                       results_list[[y]][[i]][['binomial']][[mn]]$runtime,
                                       results_list[[y]][[i]][['binomial']][[mn]]$model_converged
                                    )
               write.csv(x = results.df.model %>% filter(!is.na(runtime)), 
                         file = intermediate_results_filename_model, row.names = F)
               
               # haul dataframe [append the file on disk instead of growing the file in memory]
               data.table::fwrite(x = data.frame(
                  model_family = rep('binomial', n.test),
                  model     = rep(mn, n.test), # model name
                  model_id  = rep(modelid, n.test), 
                  haul_id   = dat.ik[r.test, 'haul_id'],
                  observed  = as.numeric(as.character(dat.ik[r.test, 'bycatch'])),
                  predicted = results_list[[y]][[i]][['binomial']][[mn]]$expected
               ), file = intermediate_results_filename_haul, append = T, row.names = F)
            }
            
            # track progress
            if(print_progress){
               elapsed_time <- difftime(Sys.time(), start_time, units = 'mins')
               time_remaining <- round( as.numeric(elapsed_time) / n_this_model * (n_total_models-n_this_model))
               m_time_elapsed <- if(n_this_model == 1) round(as.numeric(elapsed_time)) else round(as.numeric(difftime(Sys.time(), m_time, units = 'mins')))
               m_time <- Sys.time()
               
               print(paste0(mn, ' : ', m_time_elapsed, 'm',
                            ' |  ', n_this_model, ' of ', n_total_models, ' : ', round( as.numeric(elapsed_time)), ' min',
                            ' |  t - ', time_remaining, ' min.'))
               n_this_model <- n_this_model + 1
            }
         }
         
         # fit model ...: INLA model with polynomial covariates and a single/constant GMRF
         # NOT UP TO DATE!! THIS WILL NOT RUN AS-IS.
         if('inla' %in% model_names){
            # model name 
            mn <- 'inla'
            
            # fit the model
            results_list[[y]][[i]][['binomial']][[mn]] <- fit_m5_INLA(data = dat.ik, 
                                                        covariates  = covar_processed, 
                                                        modelfamily = 'binomial', 
                                                        rows.train  = r.train,
                                                        rows.test   = r.test)
            
            # save the haul ID's in the list object
            results_list[[y]][[i]][['binomial']][[mn]][['haul_id']] <- dat.ik[r.test, 'haul_id']
            results_list[[y]][[i]][['binomial']][[mn]][['model_name']] <- mn
            # create a model ID to help organize results
            modelid <- paste(sample( c(letters, LETTERS, 0:9), size = 25, replace = T), collapse = '')
            results_list[[y]][[i]][['binomial']][[mn]][['model_id']] <- modelid
            
            # save results to model & haul dataframes
            {
               results.df.model[ results.df.model$model == mn & 
                                    results.df.model$model_family == 'binomial' & 
                                    results.df.model$rep  == y &
                                    results.df.model$fold == i, 
                                 c( 'model_id',
                                    'AUC', 
                                    'runtime',
                                    'model_converged')] <- c(
                                       modelid,
                                       results_list[[y]][[i]][['binomial']][[mn]]$AUC,
                                       results_list[[y]][[i]][['binomial']][[mn]]$runtime,
                                       results_list[[y]][[i]][['binomial']][[mn]]$model_converged
                                    )
               write.csv(x = results.df.model %>% filter(!is.na(runtime)), 
                         file = intermediate_results_filename_model, row.names = F)
               
               # haul dataframe [append the file on disk instead of growing the file in memory]
               data.table::fwrite(x = data.frame(
                  model_family = rep('binomial', n.test),
                  model     = rep(mn, n.test), # model name
                  model_id  = rep(modelid, n.test),
                  haul_id   = dat.ik[r.test, 'haul_id'],
                  observed  = as.numeric(as.character(dat.ik[r.test, 'bycatch'])),
                  predicted = results_list[[y]][[i]][['binomial']][[mn]]$expected
               ), file = intermediate_results_filename_haul, append = T, row.names = F)
            }
            
            # track progress
            if(print_progress){
               elapsed_time <- difftime(Sys.time(), start_time, units = 'mins')
               time_remaining <- round( as.numeric(elapsed_time) / n_this_model * (n_total_models-n_this_model))
               m_time_elapsed <- if(n_this_model == 1) round(as.numeric(elapsed_time)) else round(as.numeric(difftime(Sys.time(), m_time, units = 'mins')))
               m_time <- Sys.time()
               
               print(paste0(mn, ' : ', m_time_elapsed, 'm',
                            ' |  ', n_this_model, ' of ', n_total_models, ' : ', round( as.numeric(elapsed_time)), ' min',
                            ' |  t - ', time_remaining, ' min.'))
               n_this_model <- n_this_model + 1
            }
         }
         
         # fit model 6: Random Forest
         if('rf 1' %in% model_names){
            # model name 
            mn <- 'rf 1'
            
            # fit the model
            results_list[[y]][[i]][['binomial']][[mn]] <- fit_m6_rf1(data = dat.ik, 
                                                                     covariates  = rf.covar, # use the unprocessed covariates for random forests
                                                                     modelfamily = 'binomial', 
                                                                     rows.train  = r.train,
                                                                     rows.test   = r.test, 
                                                                     use.package = 'ranger')
            
            # save the haul ID's in the list object
            results_list[[y]][[i]][['binomial']][[mn]][['haul_id']] <- dat.ik[r.test, 'haul_id']
            results_list[[y]][[i]][['binomial']][[mn]][['model_name']] <- mn
            # create a model ID to help organize results
            modelid <- paste(sample( c(letters, LETTERS, 0:9), size = 25, replace = T), collapse = '')
            results_list[[y]][[i]][['binomial']][[mn]][['model_id']] <- modelid
            
            # save results to model & haul dataframes
            {
               results.df.model[ results.df.model$model == mn & 
                                    results.df.model$model_family == 'binomial' & 
                                    results.df.model$rep  == y &
                                    results.df.model$fold == i, 
                                 c( 'model_id',
                                    'AUC', 
                                    'runtime',
                                    'model_converged')] <- c(
                                       modelid,
                                       results_list[[y]][[i]][['binomial']][[mn]]$AUC,
                                       results_list[[y]][[i]][['binomial']][[mn]]$runtime,
                                       results_list[[y]][[i]][['binomial']][[mn]]$model_converged
                                    )
               write.csv(x = results.df.model %>% filter(!is.na(runtime)), 
                         file = intermediate_results_filename_model, row.names = F)
               
               # haul dataframe [append the file on disk instead of growing the file in memory]
               data.table::fwrite(x = data.frame(
                  model_family = rep('binomial', n.test),
                  model     = rep(mn, n.test), # model name
                  model_id  = rep(modelid, n.test), 
                  haul_id   = dat.ik[r.test, 'haul_id'],
                  observed  = as.numeric(as.character(dat.ik[r.test, 'bycatch'])),
                  predicted = results_list[[y]][[i]][['binomial']][[mn]]$expected
               ), file = intermediate_results_filename_haul, append = T, row.names = F)
            }
            
            # track progress
            if(print_progress){
               elapsed_time <- difftime(Sys.time(), start_time, units = 'mins')
               time_remaining <- round( as.numeric(elapsed_time) / n_this_model * (n_total_models-n_this_model))
               m_time_elapsed <- if(n_this_model == 1) round(as.numeric(elapsed_time)) else round(as.numeric(difftime(Sys.time(), m_time, units = 'mins')))
               m_time <- Sys.time()
               
               print(paste0(mn, ' : ', m_time_elapsed, 'm',
                            ' |  ', n_this_model, ' of ', n_total_models, ' : ', round( as.numeric(elapsed_time)), ' min',
                            ' |  t - ', time_remaining, ' min.'))
               n_this_model <- n_this_model + 1
            }
         }
         
         # fit model 7: Random Forest with down-sampling of the over-represented class (no bycatch)
         if('rf 2' %in% model_names){
            # model name 
            mn <- 'rf 2'
            
            # fit the model
            results_list[[y]][[i]][['binomial']][[mn]] <- fit_m7_rf2(data = dat.ik, 
                                                                     covariates  = rf.covar, # use the unprocessed covariates for random forests
                                                                     modelfamily = 'binomial', 
                                                                     rows.train  = r.train,
                                                                     rows.test   = r.test, 
                                                                     use.package = 'ranger')
            
            # save the haul ID's in the list object
            results_list[[y]][[i]][['binomial']][[mn]][['haul_id']] <- dat.ik[r.test, 'haul_id']
            results_list[[y]][[i]][['binomial']][[mn]][['model_name']] <- mn
            # create a model ID to help organize results
            modelid <- paste(sample( c(letters, LETTERS, 0:9), size = 25, replace = T), collapse = '')
            results_list[[y]][[i]][['binomial']][[mn]][['model_id']] <- modelid
            
            # save results to model & haul dataframes
            {
               results.df.model[ results.df.model$model == mn & 
                                    results.df.model$model_family == 'binomial' & 
                                    results.df.model$rep  == y &
                                    results.df.model$fold == i, 
                                 c( 'model_id',
                                    'AUC', 
                                    'runtime',
                                    'model_converged')] <- c(
                                       modelid,
                                       results_list[[y]][[i]][['binomial']][[mn]]$AUC,
                                       results_list[[y]][[i]][['binomial']][[mn]]$runtime,
                                       results_list[[y]][[i]][['binomial']][[mn]]$model_converged
                                    )
               write.csv(x = results.df.model %>% filter(!is.na(runtime)), 
                         file = intermediate_results_filename_model, row.names = F)
               
               # haul dataframe [append the file on disk instead of growing the file in memory]
               data.table::fwrite(x = data.frame(
                  model_family = rep('binomial', n.test),
                  model     = rep(mn, n.test), # model name
                  model_id  = rep(modelid, n.test), 
                  haul_id   = dat.ik[r.test, 'haul_id'],
                  observed  = as.numeric(as.character(dat.ik[r.test, 'bycatch'])),
                  predicted = results_list[[y]][[i]][['binomial']][[mn]]$expected
               ), file = intermediate_results_filename_haul, append = T, row.names = F)
            }
            
            # track progress
            if(print_progress){
               elapsed_time <- difftime(Sys.time(), start_time, units = 'mins')
               time_remaining <- round( as.numeric(elapsed_time) / n_this_model * (n_total_models-n_this_model))
               m_time_elapsed <- if(n_this_model == 1) round(as.numeric(elapsed_time)) else round(as.numeric(difftime(Sys.time(), m_time, units = 'mins')))
               m_time <- Sys.time()
               
               print(paste0(mn, ' : ', m_time_elapsed, 'm',
                            ' |  ', n_this_model, ' of ', n_total_models, ' : ', round( as.numeric(elapsed_time)), ' min',
                            ' |  t - ', time_remaining, ' min.'))
               n_this_model <- n_this_model + 1
            }
         }
         
         # fit model 8: Random Forest with SMOTE (oversampling & downsampling)
         if('rf 3' %in% model_names){
            # model name 
            mn <- 'rf 3'
            
            # fit the model
            results_list[[y]][[i]][['binomial']][[mn]] <- fit_m8_rf3(data = dat.ik, 
                                                                     covariates  = rf.covar, # use the unprocessed covariates for random forests
                                                                     modelfamily = 'binomial', 
                                                                     rows.train  = r.train,
                                                                     rows.test   = r.test, 
                                                                     use.package = 'ranger')
            
            # save the haul ID's in the list object
            results_list[[y]][[i]][['binomial']][[mn]][['haul_id']] <- dat.ik[r.test, 'haul_id']
            results_list[[y]][[i]][['binomial']][[mn]][['model_name']] <- mn
            # create a model ID to help organize results
            modelid <- paste(sample( c(letters, LETTERS, 0:9), size = 25, replace = T), collapse = '')
            results_list[[y]][[i]][['binomial']][[mn]][['model_id']] <- modelid
            
            # save results to model & haul dataframes
            {
               results.df.model[ results.df.model$model == mn & 
                                    results.df.model$model_family == 'binomial' & 
                                    results.df.model$rep  == y &
                                    results.df.model$fold == i, 
                                 c( 'model_id',
                                    'AUC', 
                                    'runtime',
                                    'model_converged')] <- c(
                                       modelid,
                                       results_list[[y]][[i]][['binomial']][[mn]]$AUC,
                                       results_list[[y]][[i]][['binomial']][[mn]]$runtime,
                                       results_list[[y]][[i]][['binomial']][[mn]]$model_converged
                                    )
               write.csv(x = results.df.model %>% filter(!is.na(runtime)), 
                         file = intermediate_results_filename_model, row.names = F)
               
               # haul dataframe [append the file on disk instead of growing the file in memory]
               data.table::fwrite(x = data.frame(
                  model_family = rep('binomial', n.test),
                  model     = rep(mn, n.test), # model name
                  model_id  = rep(modelid, n.test), 
                  haul_id   = dat.ik[r.test, 'haul_id'],
                  observed  = as.numeric(as.character(dat.ik[r.test, 'bycatch'])),
                  predicted = results_list[[y]][[i]][['binomial']][[mn]]$expected
               ), file = intermediate_results_filename_haul, append = T, row.names = F)
            }
            
            # track progress
            if(print_progress){
               elapsed_time <- difftime(Sys.time(), start_time, units = 'mins')
               time_remaining <- round( as.numeric(elapsed_time) / n_this_model * (n_total_models-n_this_model))
               m_time_elapsed <- if(n_this_model == 1) round(as.numeric(elapsed_time)) else round(as.numeric(difftime(Sys.time(), m_time, units = 'mins')))
               m_time <- Sys.time()
               
               print(paste0(mn, ' : ', m_time_elapsed, 'm',
                            ' |  ', n_this_model, ' of ', n_total_models, ' : ', round( as.numeric(elapsed_time)), ' min',
                            ' |  t - ', time_remaining, ' min.'))
               n_this_model <- n_this_model + 1
            }
         }
         
         # fit model 9: Gradient Boosting Machine
         if('gbt 1' %in% model_names){
            # model name 
            mn <- 'gbt 1'
            
            # fit the model
            results_list[[y]][[i]][['binomial']][[mn]] <- fit_m9_gbt1(data = dat.ik, 
                                                                      covariates  = rf.covar, # use the unprocessed covariates for random forests
                                                                      modelfamily = 'binomial', 
                                                                      rows.train  = r.train,
                                                                      rows.test   = r.test, 
                                                                      use.package = 'xgboost')
            
            # save the haul ID's in the list object
            results_list[[y]][[i]][['binomial']][[mn]][['haul_id']] <- dat.ik[r.test, 'haul_id']
            results_list[[y]][[i]][['binomial']][[mn]][['model_name']] <- mn
            # create a model ID to help organize results
            modelid <- paste(sample( c(letters, LETTERS, 0:9), size = 25, replace = T), collapse = '')
            results_list[[y]][[i]][['binomial']][[mn]][['model_id']] <- modelid
            
            # save results to model & haul dataframes
            {
               results.df.model[ results.df.model$model == mn & 
                                    results.df.model$model_family == 'binomial' & 
                                    results.df.model$rep  == y &
                                    results.df.model$fold == i, 
                                 c( 'model_id',
                                    'AUC', 
                                    'runtime',
                                    'model_converged')] <- c(
                                       modelid,
                                       results_list[[y]][[i]][['binomial']][[mn]]$AUC,
                                       results_list[[y]][[i]][['binomial']][[mn]]$runtime,
                                       results_list[[y]][[i]][['binomial']][[mn]]$model_converged
                                    )
               write.csv(x = results.df.model %>% filter(!is.na(runtime)), 
                         file = intermediate_results_filename_model, row.names = F)
               
               # haul dataframe [append the file on disk instead of growing the file in memory]
               data.table::fwrite(x = data.frame(
                  model_family = rep('binomial', n.test),
                  model     = rep(mn, n.test), # model name
                  model_id  = rep(modelid, n.test), 
                  haul_id   = dat.ik[r.test, 'haul_id'],
                  observed  = as.numeric(as.character(dat.ik[r.test, 'bycatch'])),
                  predicted = results_list[[y]][[i]][['binomial']][[mn]]$expected
               ), file = intermediate_results_filename_haul, append = T, row.names = F)
            }
            
            # track progress
            if(print_progress){
               elapsed_time <- difftime(Sys.time(), start_time, units = 'mins')
               time_remaining <- round( as.numeric(elapsed_time) / n_this_model * (n_total_models-n_this_model))
               m_time_elapsed <- if(n_this_model == 1) round(as.numeric(elapsed_time)) else round(as.numeric(difftime(Sys.time(), m_time, units = 'mins')))
               m_time <- Sys.time()
               
               print(paste0(mn, ' : ', m_time_elapsed, 'm',
                            ' |  ', n_this_model, ' of ', n_total_models, ' : ', round( as.numeric(elapsed_time)), ' min',
                            ' |  t - ', time_remaining, ' min.'))
               n_this_model <- n_this_model + 1
            }
         }
         
         # fit model 10: Gradient Boosting Machine using DART
         if('gbt 2' %in% model_names){
            # model name 
            mn <- 'gbt 2'
            
            # fit the model
            results_list[[y]][[i]][['binomial']][[mn]] <- fit_m10_gbt2(data = dat.ik, 
                                                                       covariates  = rf.covar, # use the unprocessed covariates for random forests
                                                                       modelfamily = 'binomial', 
                                                                       rows.train  = r.train,
                                                                       rows.test   = r.test)
            
            # save the haul ID's in the list object
            results_list[[y]][[i]][['binomial']][[mn]][['haul_id']] <- dat.ik[r.test, 'haul_id']
            results_list[[y]][[i]][['binomial']][[mn]][['model_name']] <- mn
            # create a model ID to help organize results
            modelid <- paste(sample( c(letters, LETTERS, 0:9), size = 25, replace = T), collapse = '')
            results_list[[y]][[i]][['binomial']][[mn]][['model_id']] <- modelid
            
            # save results to model & haul dataframes
            {
               results.df.model[ results.df.model$model == mn & 
                                    results.df.model$model_family == 'binomial' & 
                                    results.df.model$rep  == y &
                                    results.df.model$fold == i, 
                                 c( 'model_id',
                                    'AUC', 
                                    'runtime',
                                    'model_converged')] <- c(
                                       modelid,
                                       results_list[[y]][[i]][['binomial']][[mn]]$AUC,
                                       results_list[[y]][[i]][['binomial']][[mn]]$runtime,
                                       results_list[[y]][[i]][['binomial']][[mn]]$model_converged
                                    )
               write.csv(x = results.df.model %>% filter(!is.na(runtime)), 
                         file = intermediate_results_filename_model, row.names = F)
               
               # haul dataframe [append the file on disk instead of growing the file in memory]
               data.table::fwrite(x = data.frame(
                  model_family = rep('binomial', n.test),
                  model     = rep(mn, n.test), # model name
                  model_id  = rep(modelid, n.test), 
                  haul_id   = dat.ik[r.test, 'haul_id'],
                  observed  = as.numeric(as.character(dat.ik[r.test, 'bycatch'])),
                  predicted = results_list[[y]][[i]][['binomial']][[mn]]$expected
               ), file = intermediate_results_filename_haul, append = T, row.names = F)
            }
            
            # track progress
            if(print_progress){
               elapsed_time <- difftime(Sys.time(), start_time, units = 'mins')
               time_remaining <- round( as.numeric(elapsed_time) / n_this_model * (n_total_models-n_this_model))
               m_time_elapsed <- if(n_this_model == 1) round(as.numeric(elapsed_time)) else round(as.numeric(difftime(Sys.time(), m_time, units = 'mins')))
               m_time <- Sys.time()
               
               print(paste0(mn, ' : ', m_time_elapsed, 'm',
                            ' |  ', n_this_model, ' of ', n_total_models, ' : ', round( as.numeric(elapsed_time)), ' min',
                            ' |  t - ', time_remaining, ' min.'))
               n_this_model <- n_this_model + 1
            }
         }
         
         # calculate model average(s)
         if( ('avg 1' %in% model_names) | ('avg 2' %in% model_names) ){
            
            # get the predictions for each model that was run
            exp_list <- lapply(results_list[[y]][[i]][['binomial']], function(x){
               if( length(x$expected == n.test) ){
                  data.frame(
                     model    = rep(unique(x$model_name), n.test),
                     haul_id  = x$haul_id,
                     expected = x$expected,
                     order = 1:n.test # the group_by below can change the order of rows, so I'll add this to be able to easily revert to the original order
                  )
               }
            })
            
            # add into a long dataframe
            long_df <- do.call(rbind, exp_list)
            
            # calculate model averages
            avg1 <- long_df %>% 
               dplyr::group_by(haul_id) %>% 
               dplyr::summarise(n_models_included = n(),
                                expected = mean(expected, na.rm = T),
                                order = unique(order)) %>% 
               dplyr::arrange(order) # revert to original order
            
            avg2 <- long_df %>% 
               dplyr::filter(model != 'gam 3') %>% 
               dplyr::group_by(haul_id) %>% 
               dplyr::summarise(n_models_included = n(),
                                expected = mean(expected, na.rm = T),
                                order = unique(order)) %>% 
               dplyr::arrange(order)
            
            # calculate AUC values
            avg1.auc <- calc_AUC(predicted = avg1$expected, 
                                 observed = dat.ik[ r.test, 'bycatch'])
            avg2.auc <- calc_AUC(predicted = avg2$expected, 
                                 observed = dat.ik[ r.test, 'bycatch'])
            
            # add results to the list
            if( ('avg 1' %in% model_names) ){
               modelid1 <- paste(sample( c(letters, LETTERS, 0:9), size = 25, replace = T), collapse = '')
               results_list[[y]][[i]][['binomial']][['avg 1']] <- list(
                  AUC = avg1.auc,
                  observed = dat.ik[ match(avg1$haul_id, dat.ik$haul_id), 'bycatch'],
                  expected = avg1$expected,
                  haul_id  = avg1$haul_id,
                  model_name = 'avg 1',
                  model_id = modelid1
               )
            }
            if( ('avg 2' %in% model_names) ){
               modelid2 <- paste(sample( c(letters, LETTERS, 0:9), size = 25, replace = T), collapse = '')
               results_list[[y]][[i]][['binomial']][['avg 2']] <- list(
                  AUC      = avg2.auc,
                  observed = dat.ik[ match(avg1$haul_id, dat.ik$haul_id), 'bycatch'],
                  expected = avg2$expected,
                  haul_id  = avg2$haul_id,
                  model_name = 'avg 2',
                  model_id = modelid2
               )
            }
            
            # save results to model & haul dataframes
            if( ('avg 1' %in% model_names) ){
               mn <- 'avg 1'
               results.df.model[ results.df.model$model == mn & 
                                    results.df.model$model_family == 'binomial' & 
                                    results.df.model$rep  == y &
                                    results.df.model$fold == i, 
                                 c( 'model_id',
                                    'AUC',
                                    'runtime')] <- c(
                                       modelid1,
                                       avg1.auc,
                                       '0'
                                    )
               write.csv(x = results.df.model %>% filter(!is.na(runtime)), 
                         file = intermediate_results_filename_model, row.names = F)
               
               # haul dataframe [append the file on disk instead of growing the file in memory]
               data.table::fwrite(x = data.frame(
                  model_family = rep('binomial', n.test),
                  model     = rep(mn, n.test), # model name
                  model_id  = rep(modelid1, n.test), 
                  haul_id   = dat.ik[r.test, 'haul_id'],
                  observed  = as.numeric(as.character(dat.ik[r.test, 'bycatch'])),
                  predicted = avg1$expected
               ), file = intermediate_results_filename_haul, append = T, row.names = F)
            }
            if( ('avg 2' %in% model_names) ){
               mn <- 'avg 2'
               results.df.model[ results.df.model$model == mn & 
                                    results.df.model$model_family == 'binomial' & 
                                    results.df.model$rep  == y &
                                    results.df.model$fold == i, 
                                 c( 'model_id',
                                    'AUC',
                                    'runtime')] <- c(
                                       modelid2,
                                       avg2.auc,
                                       '0'
                                    )
               write.csv(x = results.df.model %>% filter(!is.na(runtime)), 
                         file = intermediate_results_filename_model, row.names = F)
               
               # haul dataframe [append the file on disk instead of growing the file in memory]
               data.table::fwrite(x = data.frame(
                  model_family = rep('binomial', n.test),
                  model     = rep(mn, n.test), # model name
                  model_id  = rep(modelid2, n.test), 
                  haul_id   = dat.ik[r.test, 'haul_id'],
                  observed  = as.numeric(as.character(dat.ik[r.test, 'bycatch'])),
                  predicted = avg2$expected
               ), file = intermediate_results_filename_haul, append = T, row.names = F)
            }            
            
            # track progress
            if(print_progress){
               if('avg 1' %in% model_names & 'avg 2' %in% model_names) {
                  n_this_model <- n_this_model + 1
                  mn <- 'avg 1 & 2'
               }
               elapsed_time <- difftime(Sys.time(), start_time, units = 'mins')
               time_remaining <- round( as.numeric(elapsed_time) / n_this_model * (n_total_models-n_this_model))
               m_time_elapsed <- if(n_this_model == 1) round(as.numeric(elapsed_time)) else round(as.numeric(difftime(Sys.time(), m_time, units = 'mins')))
               m_time <- Sys.time()
               
               print(paste0(mn, ' : ', m_time_elapsed, 'm',
                            ' |  ', n_this_model, ' of ', n_total_models, ' : ', round( as.numeric(elapsed_time)), ' min',
                            ' |  t - ', time_remaining, ' min.'))
               n_this_model <- n_this_model + 1
            }
            
            # remove objects that were created just in case there's a problem later which would prevent one of these objects from being re-created and replaced (and then an old value could maybe get repeated. Presumably any error that would prevent the object from being over-written would also stop the script, but I'm trying to be careful.)
            rm(list = c('exp_list', 'long_df', 'avg1', 'avg2'))
         }
      }
      
      
      # fit gamma models
      if('gamma' %in% model_family){
         
         # change the response variable
         dat.ik$bycatch <- dat.ik$chinook_n
         
         # fit model 1: GLM
         if('glm 1' %in% model_names){
            # model name 
            mn <- 'glm 1'
            
            # fit the model
            results_list[[y]][[i]][['gamma']][[mn]] <- fit_m1_glm(data = dat.ik, 
                                                                  covariates  = c(covar, 'lat', 'lon'), 
                                                                  modelfamily = 'gamma', 
                                                                  prediction.data = dat.ik[r.test,], # get predictions for same hauls as the binomial models
                                                                  rows.train  = r.train.g,
                                                                  rows.test   = r.test.g)
            
            # save the haul ID's in the list object
            results_list[[y]][[i]][['gamma']][[mn]][['haul_id']] <- dat.ik[r.test.g, 'haul_id']
            results_list[[y]][[i]][['gamma']][[mn]][['model_name']] <- mn
            # create a model ID to help organize results
            modelid <- paste(sample( c(letters, LETTERS, 0:9), size = 25, replace = T), collapse = '')
            results_list[[y]][[i]][['gamma']][[mn]][['model_id']] <- modelid
            
            # save results to model & haul dataframes
            {
               results.df.model[ results.df.model$model == mn & 
                                    results.df.model$model_family == 'gamma' & 
                                    results.df.model$rep  == y &
                                    results.df.model$fold == i, 
                                 c( 'model_id',
                                    'RMSE', 
                                    'runtime',
                                    'model_converged')] <- c(
                                       modelid,
                                       results_list[[y]][[i]][['gamma']][[mn]]$RMSE,
                                       results_list[[y]][[i]][['gamma']][[mn]]$runtime,
                                       results_list[[y]][[i]][['gamma']][[mn]]$model_converged
                                    )
               write.csv(x = results.df.model %>% filter(!is.na(runtime)), 
                         file = intermediate_results_filename_model, row.names = F)
               
               # haul dataframe [append the file on disk instead of growing the file in memory]
               data.table::fwrite(x = data.frame(
                  model_family = rep('gamma', n.test.g),
                  model     = rep(mn, n.test.g), # model name
                  model_id  = rep(modelid, n.test.g), 
                  haul_id   = dat.ik[r.test.g, 'haul_id'],
                  observed  = as.numeric(as.character(dat.ik[r.test.g, 'bycatch'])),
                  predicted = results_list[[y]][[i]][['gamma']][[mn]]$expected
               ), file = intermediate_results_filename_haul, append = T, row.names = F)
            }
            
            # track progress
            if(print_progress){
               elapsed_time <- difftime(Sys.time(), start_time, units = 'mins')
               time_remaining <- round( as.numeric(elapsed_time) / n_this_model * (n_total_models-n_this_model))
               m_time_elapsed <- if(n_this_model == 1) round(as.numeric(elapsed_time)) else round(as.numeric(difftime(Sys.time(), m_time, units = 'mins')))
               m_time <- Sys.time()
               
               print(paste0(mn, ' : ', m_time_elapsed, 'm',
                            ' |  ', n_this_model, ' of ', n_total_models, ' : ', round( as.numeric(elapsed_time)), ' min',
                            ' |  t - ', time_remaining, ' min.'))
               n_this_model <- n_this_model + 1
            }
         }
         
         # fit model 2: GLM with polynomial predictors
         if('glm 2' %in% model_names){
            # model name 
            mn <- 'glm 2'
            
            # fit the model
            results_list[[y]][[i]][['gamma']][[mn]] <- fit_m1_glm(data = dat.ik,
                                                                  covariates  = covar_processed, 
                                                                  modelfamily = 'gamma', 
                                                                  prediction.data = dat.ik[r.test,], # get predictions for same hauls as the binomial models
                                                                  rows.train  = r.train.g,
                                                                  rows.test   = r.test.g)
            
            # save the haul ID's in the list object
            results_list[[y]][[i]][['gamma']][[mn]][['haul_id']] <- dat.ik[r.test.g, 'haul_id']
            results_list[[y]][[i]][['gamma']][[mn]][['model_name']] <- mn
            # create a model ID to help organize results
            modelid <- paste(sample( c(letters, LETTERS, 0:9), size = 25, replace = T), collapse = '')
            results_list[[y]][[i]][['gamma']][[mn]][['model_id']] <- modelid
            
            # save results to model & haul dataframes
            {
               results.df.model[ results.df.model$model == mn & 
                                    results.df.model$model_family == 'gamma' & 
                                    results.df.model$rep  == y &
                                    results.df.model$fold == i, 
                                 c( 'model_id',
                                    'RMSE', 
                                    'runtime',
                                    'model_converged')] <- c(
                                       modelid,
                                       results_list[[y]][[i]][['gamma']][[mn]]$RMSE,
                                       results_list[[y]][[i]][['gamma']][[mn]]$runtime,
                                       results_list[[y]][[i]][['gamma']][[mn]]$model_converged
                                    )
               write.csv(x = results.df.model %>% filter(!is.na(runtime)), 
                         file = intermediate_results_filename_model, row.names = F)
               
               # haul dataframe [append the file on disk instead of growing the file in memory]
               data.table::fwrite(x = data.frame(
                  model_family = rep('gamma', n.test.g),
                  model     = rep(mn, n.test.g), # model name
                  model_id  = rep(modelid, n.test.g), 
                  haul_id   = dat.ik[r.test.g, 'haul_id'],
                  observed  = as.numeric(as.character(dat.ik[r.test.g, 'bycatch'])),
                  predicted = results_list[[y]][[i]][['gamma']][[mn]]$expected
               ), file = intermediate_results_filename_haul, append = T, row.names = F)
            }
            
            # track progress
            if(print_progress){
               elapsed_time <- difftime(Sys.time(), start_time, units = 'mins')
               time_remaining <- round( as.numeric(elapsed_time) / n_this_model * (n_total_models-n_this_model))
               m_time_elapsed <- if(n_this_model == 1) round(as.numeric(elapsed_time)) else round(as.numeric(difftime(Sys.time(), m_time, units = 'mins')))
               m_time <- Sys.time()
               
               print(paste0(mn, ' : ', m_time_elapsed, 'm',
                            ' |  ', n_this_model, ' of ', n_total_models, ' : ', round( as.numeric(elapsed_time)), ' min',
                            ' |  t - ', time_remaining, ' min.'))
               n_this_model <- n_this_model + 1
            }
         }
         
         # fit model 3: GAM 1 model (linear covariates with spatial smooth)
         if('gam 1' %in% model_names){
            # model name 
            mn <- 'gam 1'
            
            # fit the model
            results_list[[y]][[i]][['gamma']][[mn]] <- fit_m2_gam1(data = dat.ik, 
                                                                   covariates  = covar, 
                                                                   modelfamily = 'gamma', 
                                                                   prediction.data = dat.ik[r.test,], # get predictions for same hauls as the binomial models
                                                                   rows.train  = r.train.g,
                                                                   rows.test   = r.test.g)
            
            # save the haul ID's in the list object
            results_list[[y]][[i]][['gamma']][[mn]][['haul_id']] <- dat.ik[r.test.g, 'haul_id']
            results_list[[y]][[i]][['gamma']][[mn]][['model_name']] <- mn
            # create a model ID to help organize results
            modelid <- paste(sample( c(letters, LETTERS, 0:9), size = 25, replace = T), collapse = '')
            results_list[[y]][[i]][['gamma']][[mn]][['model_id']] <- modelid
            
            # save results to model & haul dataframes
            {
               results.df.model[ results.df.model$model == mn & 
                                    results.df.model$model_family == 'gamma' & 
                                    results.df.model$rep  == y &
                                    results.df.model$fold == i, 
                                 c( 'model_id',
                                    'RMSE', 
                                    'runtime',
                                    'model_converged')] <- c(
                                       modelid,
                                       results_list[[y]][[i]][['gamma']][[mn]]$RMSE,
                                       results_list[[y]][[i]][['gamma']][[mn]]$runtime,
                                       results_list[[y]][[i]][['gamma']][[mn]]$model_converged
                                    )
               write.csv(x = results.df.model %>% filter(!is.na(runtime)), 
                         file = intermediate_results_filename_model, row.names = F)
               
               # haul dataframe [append the file on disk instead of growing the file in memory]
               data.table::fwrite(x = data.frame(
                  model_family = rep('gamma', n.test.g),
                  model     = rep(mn, n.test.g), # model name
                  model_id  = rep(modelid, n.test.g), 
                  haul_id   = dat.ik[r.test.g, 'haul_id'],
                  observed  = as.numeric(as.character(dat.ik[r.test.g, 'bycatch'])),
                  predicted = results_list[[y]][[i]][['gamma']][[mn]]$expected
               ), file = intermediate_results_filename_haul, append = T, row.names = F)
            }
            
            # track progress
            if(print_progress){
               elapsed_time <- difftime(Sys.time(), start_time, units = 'mins')
               time_remaining <- round( as.numeric(elapsed_time) / n_this_model * (n_total_models-n_this_model))
               m_time_elapsed <- if(n_this_model == 1) round(as.numeric(elapsed_time)) else round(as.numeric(difftime(Sys.time(), m_time, units = 'mins')))
               m_time <- Sys.time()
               
               print(paste0(mn, ' : ', m_time_elapsed, 'm',
                            ' |  ', n_this_model, ' of ', n_total_models, ' : ', round( as.numeric(elapsed_time)), ' min',
                            ' |  t - ', time_remaining, ' min.'))
               n_this_model <- n_this_model + 1
            }
         }
         
         # fit model 4: GAM 2 model (low-basis-dimension smooths of covariates & spatial smooth)
         if('gam 2' %in% model_names){
            # model name 
            mn <- 'gam 2'
            
            # fit the model
            # if there's not a lot of data in the training set, this model could fail
            # so I'll use a try-catch statement so that the script keeps running. 
            # the model can be fit later by lowering the k parameter in the smoothing term (e.g. k = c(80,8) instead of k = c(100,10))
            results_list[[y]][[i]][['gamma']][[mn]] <- fit_m3_gam2(data = dat.ik, 
                                                                      covariates  = covar, 
                                                                      modelfamily = 'gamma', 
                                                                   prediction.data = dat.ik[r.test,], # get predictions for same hauls as the binomial models
                                                                   rows.train  = r.train.g,
                                                                      rows.test   = r.test.g)
            
            # save the haul ID's in the list object
            results_list[[y]][[i]][['gamma']][[mn]][['haul_id']] <- dat.ik[r.test.g, 'haul_id']
            results_list[[y]][[i]][['gamma']][[mn]][['model_name']] <- mn
            # create a model ID to help organize results
            modelid <- paste(sample( c(letters, LETTERS, 0:9), size = 25, replace = T), collapse = '')
            results_list[[y]][[i]][['gamma']][[mn]][['model_id']] <- modelid
            
            # save results to model & haul dataframes
            {
               results.df.model[ results.df.model$model == mn & 
                                    results.df.model$model_family == 'gamma' & 
                                    results.df.model$rep  == y &
                                    results.df.model$fold == i, 
                                 c( 'model_id',
                                    'RMSE', 
                                    'runtime',
                                    'model_converged')] <- c(
                                       modelid,
                                       results_list[[y]][[i]][['gamma']][[mn]]$RMSE,
                                       results_list[[y]][[i]][['gamma']][[mn]]$runtime,
                                       results_list[[y]][[i]][['gamma']][[mn]]$model_converged
                                    )
               write.csv(x = results.df.model %>% filter(!is.na(runtime)), 
                         file = intermediate_results_filename_model, row.names = F)
               
               # haul dataframe [append the file on disk instead of growing the file in memory]
               data.table::fwrite(x = data.frame(
                  model_family = rep('gamma', n.test.g),
                  model     = rep(mn, n.test.g), # model name
                  model_id  = rep(modelid, n.test.g), 
                  haul_id   = dat.ik[r.test.g, 'haul_id'],
                  observed  = as.numeric(as.character(dat.ik[r.test.g, 'bycatch'])),
                  predicted = results_list[[y]][[i]][['gamma']][[mn]]$expected
               ), file = intermediate_results_filename_haul, append = T, row.names = F)
            }
            
            # track progress
            if(print_progress){
               elapsed_time <- difftime(Sys.time(), start_time, units = 'mins')
               time_remaining <- round( as.numeric(elapsed_time) / n_this_model * (n_total_models-n_this_model))
               m_time_elapsed <- if(n_this_model == 1) round(as.numeric(elapsed_time)) else round(as.numeric(difftime(Sys.time(), m_time, units = 'mins')))
               m_time <- Sys.time()
               
               print(paste0(mn, ' : ', m_time_elapsed, 'm',
                            ' |  ', n_this_model, ' of ', n_total_models, ' : ', round( as.numeric(elapsed_time)), ' min',
                            ' |  t - ', time_remaining, ' min.'))
               n_this_model <- n_this_model + 1
            }
         }
         
         # fit model 5: GAM 3 model (low-basis-dimension smooths of covariates & spatial effect changes through time)
         if('gam 3' %in% model_names){
            # model name 
            mn <- 'gam 3'
            
            # fit the model
            results_list[[y]][[i]][['gamma']][[mn]] <- fit_m4_gam3(data = dat.ik, 
                                                                      covariates  = covar, # don't use the processed covariates here
                                                                      modelfamily = 'gamma', 
                                                                   prediction.data = dat.ik[r.test,], # get predictions for same hauls as the binomial models
                                                                   rows.train  = r.train.g,
                                                                      rows.test   = r.test.g, 
                                                                      year_type = 'factor')
            
            # save the haul ID's in the list object
            results_list[[y]][[i]][['gamma']][[mn]][['haul_id']] <- dat.ik[r.test.g, 'haul_id']
            results_list[[y]][[i]][['gamma']][[mn]][['model_name']] <- mn
            # create a model ID to help organize results
            modelid <- paste(sample( c(letters, LETTERS, 0:9), size = 25, replace = T), collapse = '')
            results_list[[y]][[i]][['gamma']][[mn]][['model_id']] <- modelid
            
            # save results to model & haul dataframes
            {
               results.df.model[ results.df.model$model == mn & 
                                    results.df.model$model_family == 'gamma' & 
                                    results.df.model$rep  == y &
                                    results.df.model$fold == i, 
                                 c( 'model_id',
                                    'RMSE', 
                                    'runtime',
                                    'model_converged')] <- c(
                                       modelid,
                                       results_list[[y]][[i]][['gamma']][[mn]]$RMSE,
                                       results_list[[y]][[i]][['gamma']][[mn]]$runtime,
                                       results_list[[y]][[i]][['gamma']][[mn]]$model_converged
                                    )
               write.csv(x = results.df.model %>% filter(!is.na(runtime)), 
                         file = intermediate_results_filename_model, row.names = F)
               
               # haul dataframe [append the file on disk instead of growing the file in memory]
               data.table::fwrite(x = data.frame(
                  model_family = rep('gamma', n.test.g),
                  model     = rep(mn, n.test.g), # model name
                  model_id  = rep(modelid, n.test.g), 
                  haul_id   = dat.ik[r.test.g, 'haul_id'],
                  observed  = as.numeric(as.character(dat.ik[r.test.g, 'bycatch'])),
                  predicted = results_list[[y]][[i]][['gamma']][[mn]]$expected
               ), file = intermediate_results_filename_haul, append = T, row.names = F)
            }
            
            # track progress
            if(print_progress){
               elapsed_time <- difftime(Sys.time(), start_time, units = 'mins')
               time_remaining <- round( as.numeric(elapsed_time) / n_this_model * (n_total_models-n_this_model))
               m_time_elapsed <- if(n_this_model == 1) round(as.numeric(elapsed_time)) else round(as.numeric(difftime(Sys.time(), m_time, units = 'mins')))
               m_time <- Sys.time()
               
               print(paste0(mn, ' : ', m_time_elapsed, 'm',
                            ' |  ', n_this_model, ' of ', n_total_models, ' : ', round( as.numeric(elapsed_time)), ' min',
                            ' |  t - ', time_remaining, ' min.'))
               n_this_model <- n_this_model + 1
            }
         }
         
         # fit model ...: INLA model with polynomial covariates and a single/constant GMRF
         # NOT UP TO DATE!! THIS WILL NOT RUN AS-IS.
         if('inla' %in% model_names){
            # model name 
            mn <- 'inla'
            
            # fit the model
            results_list[[y]][[i]][['gamma']][[mn]] <- fit_m5_INLA(data = dat.ik, 
                                                                      covariates  = covar_processed, 
                                                                      modelfamily = 'gamma', 
                                                                   prediction.data = dat.ik[r.test,], # get predictions for same hauls as the binomial models
                                                                   rows.train  = r.train.g,
                                                                      rows.test   = r.test.g)
            
            # save the haul ID's in the list object
            results_list[[y]][[i]][['gamma']][[mn]][['haul_id']] <- dat.ik[r.test.g, 'haul_id']
            results_list[[y]][[i]][['gamma']][[mn]][['model_name']] <- mn
            # create a model ID to help organize results
            modelid <- paste(sample( c(letters, LETTERS, 0:9), size = 25, replace = T), collapse = '')
            results_list[[y]][[i]][['gamma']][[mn]][['model_id']] <- modelid
            
            # save results to model & haul dataframes
            {
               results.df.model[ results.df.model$model == mn & 
                                    results.df.model$model_family == 'gamma' & 
                                    results.df.model$rep  == y &
                                    results.df.model$fold == i, 
                                 c( 'model_id',
                                    'RMSE', 
                                    'runtime',
                                    'model_converged')] <- c(
                                       modelid,
                                       results_list[[y]][[i]][['gamma']][[mn]]$RMSE,
                                       results_list[[y]][[i]][['gamma']][[mn]]$runtime,
                                       results_list[[y]][[i]][['gamma']][[mn]]$model_converged
                                    )
               write.csv(x = results.df.model %>% filter(!is.na(runtime)), 
                         file = intermediate_results_filename_model, row.names = F)
               
               # haul dataframe [append the file on disk instead of growing the file in memory]
               data.table::fwrite(x = data.frame(
                  model_family = rep('gamma', n.test.g),
                  model     = rep(mn, n.test.g), # model name
                  model_id  = rep(modelid, n.test.g), 
                  haul_id   = dat.ik[r.test.g, 'haul_id'],
                  observed  = as.numeric(as.character(dat.ik[r.test.g, 'bycatch'])),
                  predicted = results_list[[y]][[i]][['gamma']][[mn]]$expected
               ), file = intermediate_results_filename_haul, append = T, row.names = F)
            }
            
            # track progress
            if(print_progress){
               elapsed_time <- difftime(Sys.time(), start_time, units = 'mins')
               time_remaining <- round( as.numeric(elapsed_time) / n_this_model * (n_total_models-n_this_model))
               m_time_elapsed <- if(n_this_model == 1) round(as.numeric(elapsed_time)) else round(as.numeric(difftime(Sys.time(), m_time, units = 'mins')))
               m_time <- Sys.time()
               
               print(paste0(mn, ' : ', m_time_elapsed, 'm',
                            ' |  ', n_this_model, ' of ', n_total_models, ' : ', round( as.numeric(elapsed_time)), ' min',
                            ' |  t - ', time_remaining, ' min.'))
               n_this_model <- n_this_model + 1
            }
         }
         
         # fit model 6: Random Forest
         if('rf 1' %in% model_names){
            # model name 
            mn <- 'rf 1'
            
            # fit the model
            results_list[[y]][[i]][['gamma']][[mn]] <- fit_m6_rf1(data = dat.ik, 
                                                                     covariates  = rf.covar, # use the unprocessed covariates for random forests
                                                                     modelfamily = 'gamma', 
                                                                  prediction.data = dat.ik[r.test,], # get predictions for same hauls as the binomial models
                                                                  rows.train  = r.train.g,
                                                                     rows.test   = r.test.g, 
                                                                     use.package = 'ranger')
            
            # save the haul ID's in the list object
            results_list[[y]][[i]][['gamma']][[mn]][['haul_id']] <- dat.ik[r.test.g, 'haul_id']
            results_list[[y]][[i]][['gamma']][[mn]][['model_name']] <- mn
            # create a model ID to help organize results
            modelid <- paste(sample( c(letters, LETTERS, 0:9), size = 25, replace = T), collapse = '')
            results_list[[y]][[i]][['gamma']][[mn]][['model_id']] <- modelid
            
            # save results to model & haul dataframes
            {
               results.df.model[ results.df.model$model == mn & 
                                    results.df.model$model_family == 'gamma' & 
                                    results.df.model$rep  == y &
                                    results.df.model$fold == i, 
                                 c( 'model_id',
                                    'RMSE', 
                                    'runtime',
                                    'model_converged')] <- c(
                                       modelid,
                                       results_list[[y]][[i]][['gamma']][[mn]]$RMSE,
                                       results_list[[y]][[i]][['gamma']][[mn]]$runtime,
                                       results_list[[y]][[i]][['gamma']][[mn]]$model_converged
                                    )
               write.csv(x = results.df.model %>% filter(!is.na(runtime)), 
                         file = intermediate_results_filename_model, row.names = F)
               
               # haul dataframe [append the file on disk instead of growing the file in memory]
               data.table::fwrite(x = data.frame(
                  model_family = rep('gamma', n.test.g),
                  model     = rep(mn, n.test.g), # model name
                  model_id  = rep(modelid, n.test.g), 
                  haul_id   = dat.ik[r.test.g, 'haul_id'],
                  observed  = as.numeric(as.character(dat.ik[r.test.g, 'bycatch'])),
                  predicted = results_list[[y]][[i]][['gamma']][[mn]]$expected
               ), file = intermediate_results_filename_haul, append = T, row.names = F)
            }
            
            # track progress
            if(print_progress){
               elapsed_time <- difftime(Sys.time(), start_time, units = 'mins')
               time_remaining <- round( as.numeric(elapsed_time) / n_this_model * (n_total_models-n_this_model))
               m_time_elapsed <- if(n_this_model == 1) round(as.numeric(elapsed_time)) else round(as.numeric(difftime(Sys.time(), m_time, units = 'mins')))
               m_time <- Sys.time()
               
               print(paste0(mn, ' : ', m_time_elapsed, 'm',
                            ' |  ', n_this_model, ' of ', n_total_models, ' : ', round( as.numeric(elapsed_time)), ' min',
                            ' |  t - ', time_remaining, ' min.'))
               n_this_model <- n_this_model + 1
            }
         }
         
         
         # fit model 9: Gradient Boosting Machine
         if('gbt 1' %in% model_names){
            # model name 
            mn <- 'gbt 1'
            
            # fit the model
            results_list[[y]][[i]][['gamma']][[mn]] <- fit_m9_gbt1(data = dat.ik, 
                                                                   covariates  = rf.covar, # use the unprocessed covariates for random forests
                                                                   modelfamily = 'gamma', 
                                                                   prediction.data = dat.ik[r.test,], # get predictions for same hauls as the binomial models
                                                                   rows.train  = r.train.g,
                                                                   rows.test   = r.test.g, 
                                                                   use.package = 'xgboost')
            
            # save the haul ID's in the list object
            results_list[[y]][[i]][['gamma']][[mn]][['haul_id']] <- dat.ik[r.test.g, 'haul_id']
            results_list[[y]][[i]][['gamma']][[mn]][['model_name']] <- mn
            # create a model ID to help organize results
            modelid <- paste(sample( c(letters, LETTERS, 0:9), size = 25, replace = T), collapse = '')
            results_list[[y]][[i]][['gamma']][[mn]][['model_id']] <- modelid
            
            # save results to model & haul dataframes
            {
               results.df.model[ results.df.model$model == mn & 
                                    results.df.model$model_family == 'gamma' & 
                                    results.df.model$rep  == y &
                                    results.df.model$fold == i, 
                                 c( 'model_id',
                                    'RMSE', 
                                    'runtime',
                                    'model_converged')] <- c(
                                       modelid,
                                       results_list[[y]][[i]][['gamma']][[mn]]$RMSE,
                                       results_list[[y]][[i]][['gamma']][[mn]]$runtime,
                                       results_list[[y]][[i]][['gamma']][[mn]]$model_converged
                                    )
               write.csv(x = results.df.model %>% filter(!is.na(runtime)), 
                         file = intermediate_results_filename_model, row.names = F)
               
               # haul dataframe [append the file on disk instead of growing the file in memory]
               data.table::fwrite(x = data.frame(
                  model_family = rep('gamma', n.test.g),
                  model     = rep(mn, n.test.g), # model name
                  model_id  = rep(modelid, n.test.g), 
                  haul_id   = dat.ik[r.test.g, 'haul_id'],
                  observed  = as.numeric(as.character(dat.ik[r.test.g, 'bycatch'])),
                  predicted = results_list[[y]][[i]][['gamma']][[mn]]$expected
               ), file = intermediate_results_filename_haul, append = T, row.names = F)
            }
            
            # track progress
            if(print_progress){
               elapsed_time <- difftime(Sys.time(), start_time, units = 'mins')
               time_remaining <- round( as.numeric(elapsed_time) / n_this_model * (n_total_models-n_this_model))
               m_time_elapsed <- if(n_this_model == 1) round(as.numeric(elapsed_time)) else round(as.numeric(difftime(Sys.time(), m_time, units = 'mins')))
               m_time <- Sys.time()
               
               print(paste0(mn, ' : ', m_time_elapsed, 'm',
                            ' |  ', n_this_model, ' of ', n_total_models, ' : ', round( as.numeric(elapsed_time)), ' min',
                            ' |  t - ', time_remaining, ' min.'))
               n_this_model <- n_this_model + 1
            }
         }
         
         # fit model 10: Gradient Boosting Machine using DART
         if('gbt 2' %in% model_names){
            # model name 
            mn <- 'gbt 2'
            
            # fit the model
            results_list[[y]][[i]][['gamma']][[mn]] <- fit_m10_gbt2(data = dat.ik,
                                                                    covariates  = rf.covar, # use the unprocessed covariates for random forests
                                                                    modelfamily = 'gamma', 
                                                                    prediction.data = dat.ik[r.test,], # get predictions for same hauls as the binomial models
                                                                    rows.train  = r.train.g,
                                                                    rows.test   = r.test.g)
            
            # save the haul ID's in the list object
            results_list[[y]][[i]][['gamma']][[mn]][['haul_id']] <- dat.ik[r.test.g, 'haul_id']
            results_list[[y]][[i]][['gamma']][[mn]][['model_name']] <- mn
            # create a model ID to help organize results
            modelid <- paste(sample( c(letters, LETTERS, 0:9), size = 25, replace = T), collapse = '')
            results_list[[y]][[i]][['gamma']][[mn]][['model_id']] <- modelid
            
            # save results to model & haul dataframes
            {
               results.df.model[ results.df.model$model == mn & 
                                    results.df.model$model_family == 'gamma' & 
                                    results.df.model$rep  == y &
                                    results.df.model$fold == i, 
                                 c( 'model_id',
                                    'RMSE', 
                                    'runtime',
                                    'model_converged')] <- c(
                                       modelid,
                                       results_list[[y]][[i]][['gamma']][[mn]]$RMSE,
                                       results_list[[y]][[i]][['gamma']][[mn]]$runtime,
                                       results_list[[y]][[i]][['gamma']][[mn]]$model_converged
                                    )
               write.csv(x = results.df.model %>% filter(!is.na(runtime)), 
                         file = intermediate_results_filename_model, row.names = F)
               
               # haul dataframe [append the file on disk instead of growing the file in memory]
               data.table::fwrite(x = data.frame(
                  model_family = rep('gamma', n.test.g),
                  model     = rep(mn, n.test.g), # model name
                  model_id  = rep(modelid, n.test.g), 
                  haul_id   = dat.ik[r.test.g, 'haul_id'],
                  observed  = as.numeric(as.character(dat.ik[r.test.g, 'bycatch'])),
                  predicted = results_list[[y]][[i]][['gamma']][[mn]]$expected
               ), file = intermediate_results_filename_haul, append = T, row.names = F)
            }
            
            # track progress
            if(print_progress){
               elapsed_time <- difftime(Sys.time(), start_time, units = 'mins')
               time_remaining <- round( as.numeric(elapsed_time) / n_this_model * (n_total_models-n_this_model))
               m_time_elapsed <- if(n_this_model == 1) round(as.numeric(elapsed_time)) else round(as.numeric(difftime(Sys.time(), m_time, units = 'mins')))
               m_time <- Sys.time()
               
               print(paste0(mn, ' : ', m_time_elapsed, 'm',
                            ' |  ', n_this_model, ' of ', n_total_models, ' : ', round( as.numeric(elapsed_time)), ' min',
                            ' |  t - ', time_remaining, ' min.'))
               n_this_model <- n_this_model + 1
            }
         }
         
         # calculate model average(s)
         if( ('avg 1' %in% model_names) | ('avg 2' %in% model_names) ){
            
            # I have to average 2 sets of data for the gamma models: the test dataset and the prediction dataset
            # Test dataset
            {
               # get the predictions for each model that was run
               exp_list <- lapply(results_list[[y]][[i]][['gamma']], function(x){
                  if( length(x$expected) == n.test.g ){
                     data.frame(
                        model    = rep(unique(x$model_name), n.test.g),
                        haul_id  = x$haul_id,
                        expected = x$expected,
                        order = 1:n.test.g # the group_by below can change the order of rows, so I'll add this to be able to easily revert to the original order
                     )
                  }
               })
               
               # add into a long dataframe
               long_df <- do.call(rbind, exp_list)
               
               # calculate model averages
               avg1 <- long_df %>% 
                  dplyr::group_by(haul_id) %>% 
                  dplyr::summarise(n_models_included = n(),
                                   expected = mean(expected, na.rm = T),
                                   order = unique(order)) %>% 
                  dplyr::arrange(order) # revert to original order
               
               avg2 <- long_df %>% 
                  dplyr::filter(model != 'gam 3') %>% 
                  dplyr::group_by(haul_id) %>% 
                  dplyr::summarise(n_models_included = n(),
                                   expected = mean(expected, na.rm = T),
                                   order = unique(order)) %>% 
                  dplyr::arrange(order)
               
               # calculate RMSE values
               avg1.rmse <- calc_RMSE(predicted = avg1$expected, 
                                      observed = dat.ik[ r.test.g, 'bycatch'])
               avg2.rmse <- calc_RMSE(predicted = avg2$expected, 
                                      observed = dat.ik[ r.test.g, 'bycatch'])
               
               # add results to the list
               if( ('avg 1' %in% model_names) ){
                  modelid1 <- paste(sample( c(letters, LETTERS, 0:9), size = 25, replace = T), collapse = '')
                  results_list[[y]][[i]][['gamma']][['avg 1']] <- list(
                     RMSE     = avg1.rmse,
                     observed = dat.ik[ match(avg1$haul_id, dat.ik$haul_id), 'bycatch'], # all.equal(dat.ik[ match(avg1$haul_id, dat.ik$haul_id), 'bycatch'], dat.ik[ r.test.g, 'bycatch'])
                     expected = avg1$expected,
                     haul_id  = avg1$haul_id,
                     model_name = 'avg 1',
                     model_id = modelid1
                  )
               }
               if( ('avg 2' %in% model_names) ){
                  modelid2 <- paste(sample( c(letters, LETTERS, 0:9), size = 25, replace = T), collapse = '')
                  results_list[[y]][[i]][['gamma']][['avg 2']] <- list(
                     RMSE     = avg2.rmse,
                     observed = dat.ik[ match(avg1$haul_id, dat.ik$haul_id), 'bycatch'],
                     expected = avg2$expected,
                     haul_id  = avg2$haul_id,
                     model_name = 'avg 2',
                     model_id = modelid2
                  )
               }
               
               # save results to model & haul dataframes
               if( ('avg 1' %in% model_names) ){
                  mn <- 'avg 1'
                  results.df.model[ results.df.model$model == mn & 
                                       results.df.model$model_family == 'gamma' & 
                                       results.df.model$rep  == y &
                                       results.df.model$fold == i, 
                                    c( 'model_id',
                                       'RMSE',
                                       'runtime')] <- c(
                                          modelid1,
                                          avg1.rmse,
                                          '0'
                                       )
                  write.csv(x = results.df.model %>% filter(!is.na(runtime)), file = intermediate_results_filename_model, row.names = F)
                  
                  # haul dataframe [append the file on disk instead of growing the file in memory]
                  data.table::fwrite(x = data.frame(
                     model_family = rep('gamma', n.test.g),
                     model     = rep(mn, n.test.g), # model name
                     model_id  = rep(modelid1, n.test.g), 
                     haul_id   = dat.ik[r.test.g, 'haul_id'],
                     observed  = as.numeric(as.character(dat.ik[r.test.g, 'bycatch'])),
                     predicted = avg1$expected
                  ), file = intermediate_results_filename_haul, append = T, row.names = F)
               }
               if( ('avg 2' %in% model_names) ){
                  mn <- 'avg 2'
                  results.df.model[ results.df.model$model == mn & 
                                       results.df.model$model_family == 'gamma' & 
                                       results.df.model$rep  == y &
                                       results.df.model$fold == i, 
                                    c( 'model_id',
                                       'RMSE',
                                       'runtime')] <- c(
                                          modelid2,
                                          avg2.rmse,
                                          '0'
                                       )
                  write.csv(x = results.df.model %>% filter(!is.na(runtime)), file = intermediate_results_filename_model, row.names = F)
                  
                  # haul dataframe [append the file on disk instead of growing the file in memory]
                  data.table::fwrite(x = data.frame(
                     model_family = rep('gamma', n.test.g),
                     model     = rep(mn, n.test.g), # model name
                     model_id  = rep(modelid2, n.test.g), 
                     haul_id   = dat.ik[r.test.g, 'haul_id'],
                     observed  = as.numeric(as.character(dat.ik[r.test.g, 'bycatch'])),
                     predicted = avg2$expected
                  ), file = intermediate_results_filename_haul, append = T, row.names = F)
               }
            }
            
            # Prediction dataset
            {
               # right now this whole script is assuming that gamma models will also make predictions for the full test dataset
               # get the predictions for each model that was run
               exp_list2 <- lapply(results_list[[y]][[i]][['gamma']], function(x){
                  if( length(x$predictions$predictions) == n.test ){
                     data.frame(
                        model    = rep(unique(x$model_name), n.test),
                        haul_id  = x$predictions$haul_id,
                        expected = x$predictions$predictions,
                        order = 1:n.test # the group_by below can change the order of rows, so I'll add this to be able to easily revert to the original order
                     )
                  }
               })
               
               # add into a long dataframe
               long_df2 <- do.call(rbind, exp_list2)
               
               # calculate model averages
               havg1 <- long_df2 %>% 
                  dplyr::group_by(haul_id) %>% 
                  dplyr::summarise(n_models_included = n(),
                                   expected = mean(expected, na.rm = T),
                                   order = unique(order)) %>% 
                  dplyr::arrange(order) # revert to original order
               
               havg2 <- long_df2 %>% 
                  dplyr::filter(model != 'gam 3') %>% 
                  dplyr::group_by(haul_id) %>% 
                  dplyr::summarise(n_models_included = n(),
                                   expected = mean(expected, na.rm = T),
                                   order = unique(order)) %>% 
                  dplyr::arrange(order)
               
               # add results to the list
               if( ('avg 1' %in% model_names) ){
                  results_list[[y]][[i]][['gamma']][['avg 1']][['predictions']] <- list(
                     predictions = havg1$expected,
                     haul_id     = havg1$haul_id
                  )
               }
               if( ('avg 2' %in% model_names) ){
                  results_list[[y]][[i]][['gamma']][['avg 2']][['predictions']] <- list(
                     predictions = havg2$expected,
                     haul_id     = havg2$haul_id
                  )
               }
            }
            
            # track progress
            if(print_progress){
               if('avg 1' %in% model_names & 'avg 2' %in% model_names) {
                  n_this_model <- n_this_model + 1
                  mn <- 'avg 1 & 2'
               }
               elapsed_time <- difftime(Sys.time(), start_time, units = 'mins')
               time_remaining <- round( as.numeric(elapsed_time) / n_this_model * (n_total_models-n_this_model))
               m_time_elapsed <- if(n_this_model == 1) round(as.numeric(elapsed_time)) else round(as.numeric(difftime(Sys.time(), m_time, units = 'mins')))
               m_time <- Sys.time()
               
               print(paste0(mn, ' : ', m_time_elapsed, 'm',
                            ' |  ', n_this_model, ' of ', n_total_models, ' : ', round( as.numeric(elapsed_time)), ' min',
                            ' |  t - ', time_remaining, ' min.'))
               n_this_model <- n_this_model + 1
            }
            
            # remove objects that were created just in case there's a problem later which would prevent one of these objects from being re-created and replaced (and then an old value could maybe get repeated. Presumably any error that would prevent the object from being over-written would also stop the script, but I'm trying to be careful.)
            rm(list = c('exp_list', 'long_df', 'avg1', 'avg2'))
         }
      }
      
      
      # combine binomial & gamma models
      if(('binomial' %in% model_family) & 
         ('gamma' %in% model_family)){
         
         # for each model:
         # - get the binomial predictions
         # - get the gamma predictions
         # - multiply predictions together
         
         # empty list to hold results
         hurdle_list <- list()
         
         for(mn in model_names){
            
            # skip rf 2 and rf 3 b/c they're not used for gamma models
            if(mn %in% c('rf 2', 'rf 3')) next
            
            # make sure the haul id's line up for the binomial and gamma models
            if( isTRUE(all.equal(results_list[[y]][[i]][['binomial']][[mn]][['haul_id']],
                          results_list[[y]][[i]][['gamma']][[mn]][['predictions']][['haul_id']])) & 
                isTRUE(all.equal(results_list[[y]][[i]][['binomial']][[mn]][['haul_id']],
                          dat.ik[r.test,'haul_id'])) ){
               
               # create a dataframe of relevant info and add it to the list
               hurdle_list[[mn]] <- data.frame(
                  haul_id  = dat.ik[r.test, 'haul_id'],
                  observed = dat.ik[r.test, 'bycatch'],
                  # get the binomial predictions
                  pred.bin = results_list[[y]][[i]][['binomial']][[mn]][['expected']],
                  # get the gamma predictions
                  pred.gam = results_list[[y]][[i]][['gamma']][[mn]][['predictions']][['predictions']]
               ) %>% 
                  dplyr::mutate(
                     predicted = pred.bin * pred.gam, # multiple binomial & gamma predictions to get hurdle predictions
                     model_id_bin = results_list[[y]][[i]][['binomial']][[mn]][['model_id']],
                     model_id_gam = results_list[[y]][[i]][['gamma']][[mn]][['model_id']],
                     model = mn
                  )
               
            } else print(paste0('haul_id did not line up between binomial and gamma models for model ', 
                                mn, ' in fold ', i, ' of replicate ', y, '. Hurdle model output will not be calculated.'))
            
         }
         
         # combine results into a dataframe
         hurdle_df <- do.call(rbind, hurdle_list)
         # lapply(hurdle_list, FUN = names)
         
         # save the dataframe to the list of results
         results_list[[y]][[i]][['hurdle_df']] <- hurdle_df
         
         # save to hurdle csv [append the file on disk instead of growing the dataframe in memory]
         data.table::fwrite(x = hurdle_df, 
                            file = intermediate_results_filename_hurdle, 
                            append = T, 
                            row.names = F)
         rm(hurdle_df)
      }
   }
   
   saveRDS(object = list(dataframe_model = results.df.model, 
                         # dataframe_haul  = results.df.haul, # not saved in memory
                         # dataframe_hurdle = results.df.hurdle, # not saved in memory
                         list      = results_list[[y]],
                         train_test_index = tt.idx[[y]]),
           file = paste0(tools::file_path_sans_ext(final_results_filename), ' rep_', y, '.RDS'))
} # end for loop over different years_in_training values

# save all the results
saveRDS(object = list(dataframe_model = results.df.model, 
                      # dataframe_haul  = results.df.haul, # not saved in memory
                      # dataframe_hurdle = results.df.hurdle, # not saved in memory
                      list = results_list,
                      train_test_index = tt.idx), 
        file = final_results_filename)