# The purpose of this script is to fit models to the full dataset (no cross validation). 
# These models will then be used for plotting covariate marginal effects and 
# mapping models' spatial components. 
{
   # Inputs: 
   #  - '02_model_definitions.R'
   #  - 'data/data_prepped.rds'
   #     from 01_data_preparation.R
   #  - 'data/prediction_grid_list.rds'
   #     from 01_data_preparation.R
   #  - uses rnaturalearth to download some shapefiles
   
   
   # Outputs: 
   #  - lots of plots and some temporary storage files, all of which are saved to
   #  	 base_folder: 'results/cv-none/'
}

# Setting most likely to change from one run to another
# set the names & locations for saving results files (only need to change 2nd item in "base_folder")
base_folder   <- file.path('results', 'cv-none')
exclude_shoreside_hake_sector <- FALSE

library(tidyverse)
source('02_model_definitions.R')

# read in the data
{
   # other things that I don't need to change from one run to the next
   {
      # choose which half of the hurdle model to run: binomial, gamma, or both
      model_family <- c('binomial', 'gamma')
      
      # show progress in the console as you go along
      print_progress <- TRUE
      
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
      
      # create a folder for the results
      dir.create(base_folder, showWarnings = FALSE)
   }
   
   # read in data
   {
      # data for the models that was produced in "01_data_preparation.R"
      dat0 <- readRDS(file = 'data/data_prepped.rds')
      
      # sectors to exclude
      if(exclude_shoreside_hake_sector) {
         dat0 <- dat0 %>% 
            dplyr::filter(sector != 'Midwater Hake') %>% 
            dplyr::mutate(sector = factor(sector))
      }
      
      # set bycatch to be a binomial response (will change later for gamma models)
      dat <- dat0 %>% 
         dplyr::mutate(bycatch = as.factor(as.numeric(chinook_n > 0)))
      
      # The projection I used for the projected coordinates
      # proj_crs <- dat0$proj_crs_proj4string[1]
      rm(dat0)
      
      # read in the data for making maps of model predictions 
      pgl <- readRDS('data/prediction_grid_list.rds')
      
      # grid for making a single map of predicted values
      grid1 <- pgl$grid1
      # grid for plotting models' spatial components through time
      grid2 <- pgl$grid2
      
      # pgl$transformations = includes information necessary to transform new covariate values before model fitting.
      rm(pgl)
   }
   
   # Use same environmental covariates for all models
   {
      # choose environmental covariates
      # names(dat0)
      covar <- c('sector',
                 # 'year', # have to use continuous time to predict a year into the future
                 'year_f',
                 'ToD',
                 'doy',
                 'duration',   
                 'depth_fishing',
                 'depth_bottom',
                 'slope_bottom',
                 'sst_anomaly',
                 # 'sst_anom_lag_7',
                 # 'sst_anom_lag_365',
                 # 'cuti_lag_7')
                 # 'cuti_lag_365')
                 'cuti')
      
      # Random forest includes lat/lon as covariates instead of spatial structures
      rf.covar <- c(covar, 
                    'lat',  # latitude (projected)
                    'lon')  # longitude (projected)
      
      # function to process the covariates (log, scale, and/or polynomialize them)
      # This is very similar to the "process_covariates" function in "02_model_definitions.R", but the other version 
      # assumes that the values used to calculate the mean & SD of a covariate are a subsample of the "covariate" 
      # argument. This version allows you to pass an entirely different vector of values via the "reference_covariate"
      # argument. 
      process_covariates <- function(covariate,
                                     reference_covariate, # the scale & polynomial will be based on these values
                                     log = FALSE,
                                     scale = FALSE,
                                     polynomialize = FALSE,
                                     polynomial_degree = 2){
         if(!class(covariate) %in% c('integer', 'numeric', 'POSIXct', 'POSIXt', 'Date')) stop('Covariate must be numeric or similar (e.g. integer, Date, etc.)')
         # Function arguments
         # covariate (numeric) = vector of covariate values
         # reference_covariate (numeric) = vector of values on which to base the scale & polynomials
         # log (logical) = whether or not to log the covariate
         # scale (logical) = whether or not to scale the covariate
         # polynomialize (logical) = whether or not to convert the covariate to a 2nd order polynomial
         # index.train (numeric) = index identifying the training data. scale & polynomial values are calculated ONLY using the training data, then applied to all covariate values.
         
         # rename covariate to make it a little easier
         x <- covariate
         y <- reference_covariate
         
         # log covariate
         if(log) {
            x <- log(x)
            y <- log(y)
         }
         
         # scale covariate (based only on training data)
         if(scale){
            y <- scale(y)
            yscale <- attributes(y)
            x <- scale(x, 
                       center = yscale$`scaled:center`, 
                       scale  = yscale$`scaled:scale`)
         }
         
         # polynomialize covariate (based only on training data)
         if(polynomialize){
            y <- poly(y, 
                      degree = polynomial_degree)
            x <- stats:::predict.poly(y, as.vector(x))
         }
         
         # return the transformed covariate as a dataframe (possibly multiple columns if polynomialize == TRUE)
         return(as.data.frame(x))
      }
   }
   
   # process the data
   {
      # vector to hold new list of covariate names (after processing)
      covar_processed <- vector(mode = 'character')
      
      # go through each covariate and do transformations before fitting the model
      for(c in rf.covar){
         
         # these plots are only for continuous predictor variables, so I'll skip categorical predictor variables
         if( class(dat[,c]) %in% c('factor', 'character')  ){
            covar_processed <- c(covar_processed, c)
            next
         } 
         
         # use the lsp_lut to decide whether or not to log, scale, and/or polynomialize each variable
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
            newc <- process_covariates(covariate = dat[,c], 
                                       reference_covariate = dat[,c], # all rows. 1:length(dat[,c])
                                       log = var_logged_logical, 
                                       scale = var_scaled_logical, 
                                       polynomialize = var_poly_logical) # use all the values
            
            # create new column names for the processed covariate values 
            if(var_poly_logical){
               names(newc) <- paste0(c, c('_poly1', '_poly2'))
            } else if(var_scaled_logical){
               names(newc) <- paste0(c, '_scale')
            } else if(var_logged_logical){
               names(newc) <- paste0(c, '_log')
            }
            
            # and the processed data back into the dataset
            dat <- cbind(dat, newc)
            
            # add the names into the list of new covariate names
            covar_processed <- c(covar_processed, names(newc))
         } else {
            # just in case any variables slipped through unprocessed
            covar_processed <- c(covar_processed, c) 
         }
      }
      
      # create datasets for the binomial and gamma models
      bdat <- dat
      gdat <- dat %>% 
         dplyr::filter(chinook_n > 0) %>% 
         dplyr::mutate(bycatch = chinook_n)
   }
   
   # process the prediction data frame for mapping
   {
      # filter out locations over land
      pdat <- grid1 %>%
         # filter out areas over land or in very shallow (< 10 m) water
         dplyr::filter(depth_bottom > 10) %>%
         dplyr::select(dplyr::all_of(covar), 'lon', 'lat', 'lat_proj', 'lon_proj', 'lat_unproj', 'lon_unproj') %>%
         as.data.frame() %>% 
         na.omit()
      
      # remove grid 1 to save memory
      rm(grid1)

      # vector to hold new list of covariate names (after processing)
      cp2 <- vector(mode = 'character')

      # go through each covariate and do transformations
      for(c in rf.covar){

         # these plots are only for continuous predictor variables, so I'll skip categorical predictor variables
         if( class(pdat[,c]) %in% c('factor', 'character')  ){
            cp2 <- c(cp2, c)
            next
         }
         
         # use the lsp_lut to decide whether or not to log, scale, and/or polynomialize each variable
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
            newc <- process_covariates(covariate = pdat[,c],
                                       reference_covariate = dat[,c],
                                       log = var_logged_logical,
                                       scale = var_scaled_logical,
                                       polynomialize = var_poly_logical) # use all the values

            # create new column names for the processed covariate values
            if(var_poly_logical){
               names(newc) <- paste0(c, c('_poly1', '_poly2'))
            } else if(var_scaled_logical){
               names(newc) <- paste0(c, '_scale')
            } else if(var_logged_logical){
               names(newc) <- paste0(c, '_log')
            }

            # and the processed data back into the dataset
            pdat <- cbind(pdat, newc)

            # add the names into the list of new covariate names
            cp2 <- c(cp2, names(newc))
         } else {
            # just in case any variables slipped through unprocessed
            cp2 <- c(cp2, c)
         }
      }
      
      # repreat for grid 2
      # filter out locations over land
      pdat2 <- grid2 %>%
         # filter out areas over land or in very shallow (< 10 m) water
         dplyr::filter(depth_bottom > 10) %>%
         ## until I re-run 01_data_preparation, I need to make sure that "doy" is numeric
         dplyr::mutate(doy = as.numeric(doy)) %>% 
         ######
         dplyr::select(dplyr::all_of(covar), 'lon', 'lat', 'lat_proj', 'lon_proj', 'lat_unproj', 'lon_unproj') %>%
         as.data.frame() %>% 
         na.omit()
      
      # remove grid 2 to save memory
      rm(grid2) 
      
      # vector to hold new list of covariate names (after processing)
      cp3 <- vector(mode = 'character')
      
      # go through each covariate and do transformations
      for(c in rf.covar){
         
         # these plots are only for continuous predictor variables, so I'll skip categorical predictor variables
         if( class(pdat[,c]) %in% c('factor', 'character')  ){
            cp3 <- c(cp3, c)
            next
         }
         
         # use the lsp_lut to decide whether or not to log, scale, and/or polynomialize each variable
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
            newc <- process_covariates(covariate = pdat2[,c],
                                       reference_covariate = dat[,c],
                                       log = var_logged_logical,
                                       scale = var_scaled_logical,
                                       polynomialize = var_poly_logical) # use all the values
            
            # create new column names for the processed covariate values
            if(var_poly_logical){
               names(newc) <- paste0(c, c('_poly1', '_poly2'))
            } else if(var_scaled_logical){
               names(newc) <- paste0(c, '_scale')
            } else if(var_logged_logical){
               names(newc) <- paste0(c, '_log')
            }
            
            # and the processed data back into the dataset
            pdat2 <- cbind(pdat2, newc)
            
            # add the names into the list of new covariate names
            cp3 <- c(cp3, names(newc))
         } else {
            # just in case any variables slipped through unprocessed
            cp3 <- c(cp3, c)
         }
      }
   }

   # create datasets for plotting covariate marginal effects
   {
      # vector of covariate names
      plot_covars <- c(rf.covar,
                       paste0(rf.covar[3:12], '_poly1'),
                       paste0(rf.covar[3:12], '_poly2'))

      # create an empty list to hold the results
      prediction_dfs <- vector('list', length = length(plot_covars))
      names(prediction_dfs) <- plot_covars

      # loop over the covariates and create data.frames for each one
      # where the covariate of interest varies over it's whole range and all other covariates
      # are held constant at average values
      for(i in 1:length(plot_covars)){
         # get the covariate name
         covar_name  <- plot_covars[i]

         # If the covariate is a factor, just use all the unique values
         if(covar_name %in% c('sector', 'year_f')){

            newdat <- data.frame(
               sector = if(covar_name == 'sector') levels(dat$sector) else factor(unique(pdat$sector), levels = levels(dat$sector)), # pdat has the most common sector
               year_f = if(covar_name == 'year_f') levels(dat$year_f) else factor('2018', levels = levels(dat$year_f)),
               ToD = mean(dat$ToD),
               ToD_poly1 = 0,
               ToD_poly2 = 0,
               doy       = mean(dat$doy),
               doy_poly1 = 0,
               doy_poly2 = 0,
               duration       = mean(dat$duration),
               duration_poly1 = 0,
               duration_poly2 = 0,
               depth_fishing       = mean(dat$depth_fishing),
               depth_fishing_poly1 = 0,
               depth_fishing_poly2 = 0,
               depth_bottom       = mean(dat$depth_bottom),
               depth_bottom_poly1 = 0,
               depth_bottom_poly2 = 0,
               slope_bottom       = mean(dat$slope_bottom),
               slope_bottom_poly1 = 0,
               slope_bottom_poly2 = 0,
               sst_anomaly = mean(dat$sst_anomaly),
               sst_anomaly_poly1 = 0,
               sst_anomaly_poly2 = 0,
               cuti       = mean(dat$cuti),
               cuti_poly1 = 0,
               cuti_poly2 = 0,
               lat = mean(dat$lat),
               lon = mean(dat$lon),
               lat_poly1 = 0,
               lat_poly2 = 0,
               lon_poly1 = 0,
               lon_poly2 = 0
            )

            # save it to the list
            prediction_dfs[[covar_name]] <- newdat
         } else if(covar_name %in% c(covar, 'lat', 'lon')){
            # continuous predictors when they are NOT processed (e.g. when in models that use raw covariates)
            newdat <- data.frame(
               sector = if(covar_name == 'sector') levels(dat$sector) else factor(unique(pdat$sector), levels = levels(dat$sector)), # pdat has the most common sector
               year_f = if(covar_name == 'year_f') levels(dat$year_f) else factor('2018', levels = levels(dat$year_f)),
               ToD = mean(dat$ToD),
               ToD_poly1 = 0,
               ToD_poly2 = 0,
               doy       = rep(mean(dat$doy), 100),
               doy_poly1 = 0,
               doy_poly2 = 0,
               duration       = mean(dat$duration),
               duration_poly1 = 0,
               duration_poly2 = 0,
               depth_fishing       = mean(dat$depth_fishing),
               depth_fishing_poly1 = 0,
               depth_fishing_poly2 = 0,
               depth_bottom       = mean(dat$depth_bottom),
               depth_bottom_poly1 = 0,
               depth_bottom_poly2 = 0,
               slope_bottom       = mean(dat$slope_bottom),
               slope_bottom_poly1 = 0,
               slope_bottom_poly2 = 0,
               sst_anomaly = mean(dat$sst_anomaly),
               sst_anomaly_poly1 = 0,
               sst_anomaly_poly2 = 0,
               cuti       = mean(dat$cuti),
               cuti_poly1 = 0,
               cuti_poly2 = 0,
               lat = mean(dat$lat),
               lon = mean(dat$lon),
               lat_poly1 = 0,
               lat_poly2 = 0,
               lon_poly1 = 0,
               lon_poly2 = 0
            )

            # get the range of the variable from the original data
            var_range <- range(dat[,covar_name])
            # create a sequence over the range for model predictions
            newdat[,covar_name] <- seq(from = var_range[1], to = var_range[2], length.out = 100)
            
            # save it to the list
            prediction_dfs[[covar_name]] <- newdat
         } else {
            # continous covariates that ARE processed

            # I can also skip any covariates that end with "poly2" because polynomial predictors need to be considered together
            if( grepl(pattern = '.+_poly2$', x = covar_name) ) next

            # same dataframe as before
            newdat <- data.frame(
               sector = if(covar_name == 'sector') levels(dat$sector) else factor(unique(pdat$sector), levels = levels(dat$sector)), # pdat has the most common sector
               year_f = if(covar_name == 'year_f') levels(dat$year_f) else factor('2018', levels = levels(dat$year_f)),
               ToD = mean(dat$ToD),
               ToD_poly1 = 0,
               ToD_poly2 = 0,
               doy       = rep(mean(dat$doy), 100),
               doy_poly1 = 0,
               doy_poly2 = 0,
               duration       = mean(dat$duration),
               duration_poly1 = 0,
               duration_poly2 = 0,
               depth_fishing       = mean(dat$depth_fishing),
               depth_fishing_poly1 = 0,
               depth_fishing_poly2 = 0,
               depth_bottom       = mean(dat$depth_bottom),
               depth_bottom_poly1 = 0,
               depth_bottom_poly2 = 0,
               slope_bottom       = mean(dat$slope_bottom),
               slope_bottom_poly1 = 0,
               slope_bottom_poly2 = 0,
               sst_anomaly = mean(dat$sst_anomaly),
               sst_anomaly_poly1 = 0,
               sst_anomaly_poly2 = 0,
               cuti       = mean(dat$cuti),
               cuti_poly1 = 0,
               cuti_poly2 = 0,
               latitude = mean(dat$latitude),
               longitude = mean(dat$longitude),
               lat = mean(dat$lat),
               lon = mean(dat$lon),
               lat_poly1 = 0,
               lat_poly2 = 0,
               lon_poly1 = 0,
               lon_poly2 = 0
            )

            # get the original, raw covariate name (not scaled, logged, or polynomialized)
            var_name <- lsp_lut[match(x = covar_name,
                                      table = lsp_lut$final_name),
                                'original_name']
            # get the range of the variable from the original data
            var_range <- range(dat[,var_name])

            # create a sequence over the range for model predictions
            new_var   <- seq(from = var_range[1], to = var_range[2], length.out = 100)

            # add in the new variable data
            newdat[,var_name] <- new_var
            # process the new variable data
            pnv <- process_covariates(covariate = new_var,
                                      reference_covariate = dat[, var_name],
                                      log   = lsp_lut[match(x = covar_name, table = lsp_lut$final_name), 'logged'],
                                      scale = lsp_lut[match(x = covar_name, table = lsp_lut$final_name), 'scaled'],
                                      polynomialize = lsp_lut[match(x = covar_name, table = lsp_lut$final_name), 'polyd'])
            # add in the processed new variable data
            newdat[,covar_name] <- pnv[,1]
            newdat[,lsp_lut[match(x = covar_name, table = lsp_lut$final_name), 'pair_name']] <- pnv[,2]
            
            # save it to the list
            prediction_dfs[[covar_name]] <- newdat
         }
      }
   }

   # create a list to save the dataframes to create marginal effect plots
   marginal_effect_list_b <- marginal_effect_list_g <- vector('list', length(model_names))
   names(marginal_effect_list_b) <- names(marginal_effect_list_g) <- model_names
}

# some plotting things
{
   # get land overlay outlines
   states  <- rnaturalearth::ne_download(scale = 'large',
                                         type = 'states', 
                                         category = 'cultural', 
                                         returnclass = 'sf') %>% 
      subset(., admin %in% c('United States of America', 'Canada', 'Mexico'))  %>% 
      sf::st_transform(x = ., crs = 4326)
   
   # get river outlines
   rivers <- rnaturalearth::ne_download(scale = 10, 
                                        type = 'rivers_lake_centerlines', 
                                        category = 'physical', 
                                        returnclass = 'sf') %>% 
      sf::st_transform(x = ., crs = 4326) %>% 
      sf::st_intersection(x = ., y = states)
   
   # get 200m bathymetry line
   # bath200 <- rnaturalearth::ne_download(scale = 10, 
   #                                       type = 'bathymetry_K_200',
   #                                       category = 'physical', 
   #                                       returnclass = 'sf') %>% 
   #    sf::st_transform(x = ., crs = 4326)
   
   # create a function to plot maps of model predictions
   plot_covariates_gg <- function(data,  # dataframe (must have "lat" and "lon" columns)
                                  field, # column name of data to be plotted
                                  legend.title = NULL,
                                  colormap = 'viridis',
                                  legend.color.range = NULL, # to set a range for the legend
                                  transformation = 'identity',# "log1p",
                                  legend.scale.breaks = NULL,
                                  facet_field = NULL,
                                  facet_rows = 3,
                                  remove.NAs = TRUE,
                                  lon.range = c(-127.4, -123.7), # range(pdat$lon_unproj)
                                  lat.range = c(40.8, 48.2),     # range(pdat$lat_unproj)
                                  land_vector = 'states',
                                  rivers_vector = 'rivers'){
      
      # make sure "data" is not an sf object or something else that alters plotting: 
      data <- as.data.frame(data)
      
      # possibly remove NA's
      if(remove.NAs) data <- data[!is.na(data[,field]),]
      
      # Do some conversions to prep things
      # can also get a new x/y range from: range(A.pred.grid.all$x)
      # convert the character vector into the appropriate format for dplyr
      column <- dplyr::ensym(field)
      # I can then reference the column names inside of dplyr functions by 
      # adding a !! before the "column" object
      if( is.null(legend.title) ) legend.title <- field
      if( is.null(legend.color.range) ) legend.color.range <- range(data[,field])
      
      # create base plot components
      {
         (plot_base <- ggplot() + 
             theme_bw()+
             xlab(label = 'Longitude')+
             ylab(label = 'Latitude')+
             # coord_sf(xlim = x.range+360, ylim = y.range)+
             theme(axis.text.x = element_text(angle = 90, 
                                              vjust = 1, 
                                              hjust = 0.5),
                   plot.margin = ggplot2::margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, unit = 'cm')))
         
         if(is.character(land_vector))    land_vector  <- get(land_vector)
         if(is.character(rivers_vector)) rivers_vector <- get(rivers_vector)
         plot_land   <- geom_sf(data = land_vector, 
                                inherit.aes = F) #  + coord_sf(xlim = x.range, ylim = y.range)
         plot_rivers <- geom_sf(data = rivers_vector, 
                                color = 'darkblue', 
                                alpha = 0.25, 
                                inherit.aes = F) #  + coord_sf(xlim = x.range+360, ylim = y.range)
      }
      
      # plot numeric data
      if( is.numeric( data[,field]) ) {
         plot.dat <- data
         
         plt <- plot_base +
            geom_tile(data = plot.dat,
                      aes(x = lon,
                          y = lat,
                          fill = !!column,
                          color= !!column)) +
            scale_fill_viridis_c(option = colormap, 
                                 limits = legend.color.range, 
                                 trans = transformation,
                                 breaks = {if(transformation == 'identity') 
                                    waiver() else if(transformation == 'log1p') 
                                       legend.scale.breaks else if(transformation == 'log10')
                                          scales::log_breaks()} 
            )+
            scale_color_viridis_c(option = colormap, 
                                  limits = legend.color.range,
                                  trans = transformation,
                                  breaks = {if(transformation == 'identity') 
                                     waiver() else if(transformation == 'log1p') 
                                        legend.scale.breaks else if(transformation == 'log10')
                                           scales::log_breaks()} 
            )+
            labs(colour = legend.title,
                 fill   = legend.title) +
            plot_land +
            plot_rivers +
            coord_sf(xlim = lon.range, 
                     ylim = lat.range)
      }
      
      # plot factor data
      if( is.factor( data[,field]) ){
         plot.dat <- data
         
         plt <- plot_base +
            geom_tile(data = plot.dat,
                      aes(x = lon,
                          y = lat,
                          fill  = !!column,
                          color = !!column)) +
            scale_fill_viridis_d(option = colormap, 
                                 limits = legend.color.range,
                                 breaks = {if(transformation == 'identity') 
                                    waiver() else if(transformation == 'log1p') 
                                       legend.scale.breaks else if(transformation == 'log10')
                                          scales::log_breaks()} 
            )+
            scale_color_viridis_d(option = colormap, 
                                  limits = legend.color.range,
                                  breaks = {if(transformation == 'identity') 
                                     waiver() else if(transformation == 'log1p') 
                                        legend.scale.breaks else if(transformation == 'log10')
                                           scales::log_breaks()} 
            )+
            labs(colour = legend.title,
                 fill   = legend.title) +
            plot_land +
            plot_rivers +
            coord_sf(xlim = lon.range, 
                     ylim = lat.range)
      }
      
      if( !is.null(facet_field) ){
         fc <- dplyr::ensym(facet_field)
         plt <- plt + facet_wrap(facets = vars(!!fc), nrow = facet_rows)
      }
      
      return(plt)
   }
}

# Functions to calculate model performance
# Currently these are not in use
{
   # I don't actually use these functions currently, but you can use them (or parts of them)
   # to investigate model fit in more detail.
   
   # function for calculating residuals & making some plots
   calculate_residuals <- function(m1_model = NULL, 
                                   m1_data.fit = NULL, 
                                   m1_data.test = NULL,
                                   m2_observed = NULL,
                                   m2_expected = NULL,
                                   model.family = 'binomial'){
      # this function is set up to calculate residuals from a gam model using: 
      #  - the model fit object (m1_model)
      #  - the dataset used to fit the model (m1_data.fit)
      #  - the testing dataset (optional) (m1_data.test)
      # alternatively, it will calculate residuals from:
      #  - observed values (fit/testing & training combined) (m2_observed)
      #  - expected values (fit/testing & training combined) (m2_expected)
      
      if( !is.null(m1_model) & !is.null(m1_data.fit) ){
         print('Using "m1_model", "m1_data.fit", and "m1_data.test" to calculate residuals.')
         m1 <- TRUE
         m2 <- FALSE
      } 
      if( any(is.null(m1_model), is.null(m1_data.fit)) & !is.null(m2_observed) & !is.null(m2_expected)){
         print('Using "observed" and "expected" to calculate residuals.')
         m1 <- FALSE
         m2 <- TRUE
         observed <- m2_observed
         expected <- m2_expected
      }
      if( any(is.null(m1_model), is.null(m1_data.fit)) & any(is.null(m2_observed), is.null(m2_expected)) ) stop('This function requires either all three m1 parameters or both m2 parameters.')
      
      require(tidyverse)
      require(mgcv)
      
      if(m1){
         model.type <- if('gam' %in% class(m1_model)) 'gam' else if('randomForest' %in% class(m1_model)) 'rf' else  stop('function only accepts models of class "gam" or "randomForest".')
         
         # get the observed & expected values
         # observed values
         observed.fitted <- as.numeric(as.character(m1_data.fit$bycatch))
         if(!is.null(m1_data.test)){
            observed.test   <- as.numeric(as.character(m1_data.test$bycatch))
            observed <- as.vector(c(observed.fitted, observed.test))
         } else observed <- observed.fitted
         
         # fitted/expected values
         # I don't really need to distinguish between model types or use the "switch" function b/c "predict()" already does that, and these residuals don't really make sense for random forests anyway...
         expected.fitted <- switch (model.type,
                                    'gam' = mgcv::predict.gam(m1_model,
                                                              newdata = m1_data.fit,
                                                              type = 'response'),
                                    'rf' = randomForest:::predict.randomForest(m1_model,
                                                                               newdata = m1_data.fit))
         if(!is.null(m1_data.test)){
            expected.test   <- switch (model.type,
                                       'gam' = mgcv::predict.gam(m1_model,
                                                                 newdata = m1_data.test,
                                                                 type = 'response'),
                                       'rf' = randomForest:::predict.randomForest(m1_model,
                                                                                  newdata = m1_data.test))
            expected <- as.vector(c(expected.fitted, expected.test))
         } else expected <- as.vector(expected.fitted)
         # 3 ways of getting the predicted values: 
         # all.equal(m1_model$fitted.values, 
         #           as.vector(predict(m1_model, type = 'response')), 
         #           exp(as.vector(m1_model.matrix(as.formula(formula.glm), data = m1_data.fit) %*% matrix(m1_model$coefficients, ncol = 1))))
         # exp() b/c of log link. Could also use m1_model$family$linkinv()
      }
      
      # variance Function of gamma distribution
      # NOTE: Pearson residuals in R are scaled by the VARIANCE FUNCTION, not the variance.
      #     https://stats.stackexchange.com/a/509484/45889
      # The variance function defines the variance as a function of the mean. 
      #     https://en.wikipedia.org/wiki/Variance_function#Example_%E2%80%93_Gamma
      #     https://www.sfu.ca/sasdoc/sashtml/insight/chap39/sect4.htm
      # for the gamma distribution, the variance function is: 
      # V(mu) = mu^2
      # VarY <- expected^2 
      
      
      # VarY <- m1_model$family$variance(expected)
      
      # variance of binomial distribution = n*p*(1-p)
      # VarY <- 1 * expected * (1-expected)
      
      VarY <- switch (model.family,
                      'binomial' = 1 * expected * (1-expected),
                      'gamma'    = expected^2 )
      
      
      # Pearson residuals
      resid.pearson  <- (observed - expected) / sqrt(VarY)
      # double-check that my manual calculations are the same as what R would give
      # all.equal(resid(m1_model, 'pearson'), as.vector(resid.pearson[1:nrow(m1_data.fit)]))
      # [need to do manual calculations for R-INLA models]
      
      
      
      # Deviance Residuals
      # https://stats.stackexchange.com/a/468664/45889
      #      d_i = (-1)^(y_i + 1) * sqrt( -2 * (y_i * ln(pihat_i) + (1 - y_i) * ln(1 - pihat_i)))
      # and here: https://stats.stackexchange.com/a/92413/45889 
      residuals.deviance.logistic <- function(observed., expected., n){
         (-1)^(observed. + 1) * sqrt( -2 * (observed. * log(expected.) + (1 - observed.) * log(1 - expected.)))
      }
      residuals.deviance.gamma <- function(observed., expected.){
         # "Recall that the ith deviance residual is defined as the square root of the contribution of the 
         #  ith observation to the deviance multiplied by the sign of the ordinary residual." - Introduction to Linear Regression Analysis, 5th Edition by Douglas C. Montgomery
         # deviance equation from: https://www.groups.ma.tum.de/fileadmin/w00ccg/statistics/czado/lec8.pdf
         # and same equation here: http://www.stat.tugraz.at/courses/files/GLMSlidesHSE.pdf (slide 99)
         # I'm not including shape (1/phi) parameter b/c that's a constant. But I DO need to include the 2 in order to get the same answers that R gives.
         as.vector( sign(observed. - expected.) * sqrt( 2 * abs(log(observed./expected.) - ((observed. - expected.)/expected.))) )
      }
      # all.equal(resid(m1_model, type = 'deviance'), residuals.deviance.gamma(observed = observed.fitted, expected = expected.fitted))
      
      # calculate the deviance residuals for fit & test data
      resid.deviance <- switch (model.family,
                                'binomial' = residuals.deviance.logistic(observed. = observed, # deviance residuals
                                                                         expected. = expected, 
                                                                         n = 1),
                                'gamma'    =  residuals.deviance.gamma(observed. = observed, # deviance residuals
                                                                       expected. = expected))
      
      # Randomized Quantile Residuals
      # try another version that incorporates randomized quantile residuals
      #  Quantile residuals are based on the idea of inverting the estimated distribution function for each 
      #    observation to obtain exactly standard normal residuals. In the case of discrete distributions, 
      #    such as the binomial and Poisson, some randomization is introduced to produce continuous normal 
      #    residuals. Quantile residuals are the residuals of choice for generalized linear models in large 
      #    dispersion situations when the deviance and Pearson residuals can be grossly non-normal. Quantile 
      #    residuals are the only useful residuals for binomial or Poisson data when the response takes on 
      #    only a small number of distinct values.
      # http://ugrad.stat.ubc.ca/R/library/statmod/html/qresiduals.html
      
      {
         # code for calculating Randomized Quantile Residuals. 
         # from https://github.com/eric-pedersen/mgcv-esa-workshop/blob/master/code_snippets/quantile_resid.R
         #This code is modified for D.L. Miller's dsm package for distance sampling, from
         #the rqgam.check function. The code is designed to extract randomized quantile 
         #residuals from GAMs, using the family definitions in mgcv. Note statmod only
         #supports RQ residuals for the following families: Tweedie, Poisson, Gaussian,  Any errors are due to Eric Pedersen
         library(statmod) #This has functions for randomized quantile residuals
         rqresiduals = function (gam.obj) {
            if(!"gam" %in% attr(gam.obj,"class")){
               stop('"gam.obj has to be of class "gam"')
            }
            if (!grepl("^Tweedie|^Negative Binomial|^poisson|^binomial|^gaussian|^Gamma|^inverse.gaussian",
                       gam.obj$family$family)){
               stop(paste("family " , gam.obj$family$family, 
                          " is not currently supported by the statmod library, 
                 and any randomized quantile residuals would be inaccurate.",
                          sep=""))
            }
            if (grepl("^Tweedie", gam.obj$family$family)) {
               if (is.null(environment(gam.obj$family$variance)$p)) {
                  p.val <- gam.obj$family$getTheta(TRUE)
                  environment(gam.obj$family$variance)$p <- p.val
               }
               qres <- qres.tweedie(gam.obj)
            }
            else if (grepl("^Negative Binomial", gam.obj$family$family)) {
               if ("extended.family" %in% class(gam.obj$family)) {
                  gam.obj$theta <- gam.obj$family$getTheta(TRUE)
               }
               else {
                  gam.obj$theta <- gam.obj$family$getTheta()
               }
               qres <- qres.nbinom(gam.obj)
            }
            else {
               qres <- qresid(gam.obj)
            }
            return(qres)
         }
      }
      
      if(m1) if('gam' %in% class(m1_model)) resid.rqr <- rqresiduals(m1_model) else resid.rqr <- NULL else resid.rqr <- NULL
      # I'd have to manually incorporate randomized quantile residuals for the INLA models. I think the coding would be very easy, 
      #   it's just a matter of being sure that I know what I'm doing. I think the idea would be to use the parameter of the gamma 
      #   distribution that the model outputs (mean and shape/dispersion) and use those with the dgamma function to get the 
      #   probability of observing such a value. Then use that probability to get a random value from the std. normal dist??
      
      # Plot binned residuals with arm::binnedplot()
      {
         # Binned Raw Residuals plot
         resid.raw <- observed - expected
         arm::binnedplot(x = expected, # expected (i.e. fitted) values
                         y = resid.raw, 
                         main = 'Binned Raw Residuals') # raw residual (observed - expected) values
         binned.res.raw <- recordPlot()
         
         # A plot in which the gray lines indicate plus and minus 2 standard-error bounds, 
         # within which one would expect about 95% of the binned residuals to fall, if the m1_model were actually true.
         
         
         
         # Binned Pearson Residuals plot
         arm::binnedplot(x = expected, # expected values
                         y = resid.pearson, 
                         main = 'Binned Pearson Residuals')     # pearson/standardized residuals
         binned.res.pearson <- recordPlot()
         
         # Binned Deviance Residuals plot
         arm::binnedplot(x = expected, # expected values
                         y = resid.deviance, 
                         main = 'Binned Deviance Residuals') 
         binned.res.deviance <- recordPlot()
      }
      
      # plots of residuals vs. fitted values
      {
         # "Thus, plotting the deviance residuals on a normal probability scale and versus fitted 
         #   values is a logical diagnostic. When plotting deviance residuals versus fitted values, 
         #   it is customary to transform the fitted values to a constant information scale."
         #   "For binomial responses, use 2*sin^(–1)(pi)."
         #   "For gamma responses, use 2*ln(expected)."
         # Introduction to Linear Regression Analysis, 5th Edition by Douglas C. Montgomery
         
         # transformed fitted values
         tfit <- switch(model.family,
                        'binomial' = as.vector(2*asin(expected)),
                        'gamma' = as.vector(2 * log(expected)))
         tfit.fitted <- switch(model.family,
                               'binomial' = as.vector(2*asin(expected.fitted)),
                               'gamma' = as.vector(2 * log(expected.fitted)))
         xlabel <- switch(model.family,
                          'binomial' = '2 * arcsin(Fitted value)',
                          'gamma' = '2 * log(Fitted value)')
         
         # Pearson residual plot
         (plot_fit_res_pearson <- ggplot(data = data.frame(
            fitted = tfit,
            pearson_residual = resid.pearson
         ), aes(x = fitted, y = pearson_residual)) + 
               theme_bw()+
               geom_point() + 
               geom_smooth(method = 'loess', 
                           formula = formula('y~x'), # use loess smooth b/c the default gam smooth throws an error when I make these plots inside another function (but work fine when I just make the plots...)
                           se = FALSE) + # don't use SE b/c it fails in testing of binomial models
               xlab(label = xlabel) + 
               ylab(label = 'Pearson Residual'))
         
         # Deviance residual plot
         (plot_fit_res_deviance <- ggplot(data = data.frame(
            fitted = tfit,
            deviance_residual = resid.deviance
         ), aes(x = fitted, y = deviance_residual)) + 
               theme_bw()+
               geom_point() + 
               geom_smooth(method = 'loess', 
                           formula = formula('y~x'), # use loess smooth b/c the default gam smooth throws an error when I make these plots inside another function (but work fine when I just make the plots...)
                           se = FALSE) + # don't use SE b/c it fails in testing of binomial models
               xlab(label = xlabel) + 
               ylab(label = 'Deviance Residual'))
         
         # Randomized Quantile Residuals
         if(!is.null(resid.rqr)) {
            (plot_fit_res_rqr <- ggplot(data = data.frame(
               fitted = tfit.fitted,
               randomized_quantile_residuals = resid.rqr
            ), aes(x = fitted, y = randomized_quantile_residuals)) + 
               theme_bw() + 
               geom_point() + 
               geom_smooth(method = 'loess', 
                           formula = formula('y~x'), # use loess smooth b/c the default gam smooth throws an error when I make these plots inside another function (but work fine when I just make the plots...)
                           se = FALSE) + # don't use SE b/c it fails in testing of binomial models
               xlab(label = xlabel) + 
               ylab(label = 'Randomized Quantile Residual'))
         } else plot_fit_res_rqr <- NULL
      }
      
      return(
         list(
            residuals.raw = resid.raw,
            residuals.pearson = resid.pearson,
            residuals.deviance = resid.deviance,
            residuals.randomized.quantile = resid.rqr,
            residuals.binned.plot.raw = binned.res.raw,
            residuals.binned.plot.pearson = binned.res.pearson,
            residuals.binned.plot.deviance = binned.res.deviance,
            fitted_vs_resid_plot.pearson = plot_fit_res_pearson,
            fitted_vs_resid_plot.deviance = plot_fit_res_deviance,
            fitted_vs_resid_plot.RQR = plot_fit_res_rqr
         )
      )
   }
   
   # function for making variograms
   # note: should use projected coordinates for this function even if the model was fit to unprojected coordinates
   my_variogram <- function(residuals,
                            longitude_projected, 
                            latitude_projected, 
                            cressie = TRUE, # scale output to be between 0 and 1
                            width = 1000,
                            x_label = 'Distance (km)',
                            y_label = 'Scaled Variance',
                            add_sample_size = FALSE,
                            include_directional_variogram = TRUE,
                            remove_values_over = 1){
      # Variogram output: np    = the number of point pairs for this estimate;  
      #                   dist  = the average distance of all point pairs considered for this estimate
      #                   gamma = the actual sample variogram estimate
      #                   dir.hor = the horizontal direction
      #                   dir.ver = the vertical direction 
      #                   id    = the combined id pair
      
      # make sure to exclude all NA's and INF residual values
      goodresindx <- as.vector(which( !is.na(residuals) & !is.infinite(residuals)))
      
      # put the residuals and lon lat data into a data.frame
      vgdat <- data.frame(res  = residuals[goodresindx],
                          Xdeg = longitude_projected[goodresindx], 
                          Ydeg = latitude_projected[goodresindx])
      # set the coordinates
      sp::coordinates(vgdat) <- c("Xdeg", "Ydeg")
      # set the projection so that the gstat:variogram function knows that the data are projected and doesn't try to use great-circle distances
      sp::proj4string(vgdat) <- sp::CRS(projargs = '+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
      
      
      vg <- gstat::variogram(object = res ~ Xdeg + Ydeg, 
                             data   = vgdat,
                             cressie = cressie, # this changes the value of "gamma" in the output to be between 0 and 1
                             width   = width)
      
      vgplot <- ggplot(data = vg, 
                       aes(x = dist/1000, 
                           y = gamma))
      if(add_sample_size){
         max.np <- max(vg$np)
         max.g  <- max(vg$gamma)
         multiplier <- (max.g / max.np)
         
         vgplot <- vgplot + 
            geom_col(aes(x = dist/1000, y = np * multiplier), color = 'grey80') + 
            scale_y_continuous(name = y_label,
                               sec.axis = sec_axis(~ . / multiplier, 
                                                   name = "Sample Size"))
      }
      
      vgplot <- vgplot + 
         theme_bw() +
         geom_point() + 
         geom_smooth() + # method = "gam", formula = y ~ s(x, bs = "cs"), colour = "black"
         xlab(x_label) + 
         ylab(y_label)
      
      
      # create output
      out <- list(
         'variogram_data' = vg,
         'variogram_plot' = vgplot
      )
      
      
      if(include_directional_variogram){
         vgd <- gstat::variogram(object = res ~ 1, 
                                 alpha = c(0, 45, 90, 135),
                                 vgdat,
                                 cressie = cressie,
                                 width = width)
         
         if(cressie){
            nover <- vgd %>% dplyr::filter(gamma > 1) %>% nrow()
            vgd <- vgd %>% 
               dplyr::filter(gamma <= remove_values_over)
            print(paste(nover, 'points removed because gamma value > ', remove_values_over, '.'))
         }
         
         # make the base plot
         vgdplot <- ggplot(data = vgd, 
                           aes(x = dist/1000, 
                               y = gamma))
         # possibly add in the sample size to the background
         if(add_sample_size){
            max.npd <- max(vgd$np)
            max.gd  <- max(vgd$gamma)
            multiplierd <- (max.gd / max.npd)
            
            vgdplot <- vgdplot + 
               geom_col(aes(x = dist/1000, y = np * multiplierd), color = 'grey80') + 
               scale_y_continuous(name = y_label,
                                  sec.axis = sec_axis(~ . / multiplier, 
                                                      name = "Sample Size"))
         }
         
         vgdplot <- vgdplot + 
            theme_bw() +
            geom_point() + 
            geom_smooth() + # method = "gam", formula = y ~ s(x, bs = "cs"), colour = "black"
            xlab(x_label) + 
            ylab(y_label) + 
            facet_wrap(~dir.hor)
         
         
         # geom_blank(data = data.frame(dist = 0, 
         #                                 gamma = 0)) +
         
         out[['directional_variogram_data']] <- vgd
         out[['directional_variogram_plot']] <- vgdplot
      }
      
      return(out)
   }
   
   # calculate Moran's I 
   #   NOTE: this requires matrix inversion, so you can't do it on huge datasets
   #         there's a subsample_proportion argument to calculate Moran's I on a 
   #         subset of the data. 
   calculate_Morans_I <- function(residuals, 
                                  lon, 
                                  lat, 
                                  seed = 3, 
                                  subsample_proportion = .5,
                                  use.parallel = FALSE){
      # I think I'll try following this method: 
      # https://rstudio-pubs-static.s3.amazonaws.com/278910_3ebade4ad6a14f8f9ac6e05eg16g5a21.html
      
      
      # unfortunately, this computer only has 16GB of RAM, so it can't do this 
      # calculation for all the data, and running it in parallel didn't help.
      # so I'll calculate Moran's I for a subsample
      set.seed(seed)
      MI.samp <- sample(x = 1:length(residuals),
                        size = round(subsample_proportion * length(residuals)),
                        replace = F)
      
      # based on: https://stats.idre.ucla.edu/r/faq/how-can-i-calculate-morans-i-in-r/
      # Step 1: create a matrix of weights (in this case inverse of distance)
      dist.mat <- if(use.parallel){
         as.matrix(parallelDist::parallelDist(x = matrix(data = c(lon[ MI.samp ], lat[ MI.samp ]), nrow = length(MI.samp), ncol = 2),
                                              method = 'euclidean', 
                                              threads = parallel::detectCores() - 1))
         
         # as.matrix(parallelDist::parallelDist(x = as.matrix(dat[c(fit.id, test.id), c('lon', 'lat')]),
         #                                      method = 'euclidean', 
         #                                      threads = parallel::detectCores() - 1))
      } else {
         as.matrix(dist(matrix(data = c(lon[ MI.samp ], lat[ MI.samp ]), nrow = length(MI.samp), ncol = 2)))
         # as.matrix(dist(dat[c(fit.id, test.id), 
         #                    c('lon', 'lat')]))
      }
      # there are points with the same coordinates, but if I divide by 0 it causes errors
      # so I need to set a minimum distance
      # min(dist.mat.g1[ dist.mat.g1 > 0 ])
      dist.mat[ dist.mat < 1 ] <- 1 # In reality no 2 hauls will have been less than 1 meter apart.
      
      # inverse distance
      dist.mat.inv <- 1/dist.mat
      diag(dist.mat.inv) <- 0
      
      # calculate Moran's I
      (MI <- ape::Moran.I(x = residuals[ MI.samp ],
                          weight = dist.mat.inv)) # ; beepr::beep()
      # "The null hypothesis of correlation is tested assuming normality of I 
      #   under this null hypothesis. If the observed value of I is significantly 
      #   greater than the expected value, then the values of x are positively 
      #   autocorrelated, whereas if Iobserved < Iexpected, this will indicate 
      #   negative autocorrelation."
      
      # For some alternative methods for calculating Moran's I, see: 
      # https://cran.r-project.org/web/packages/Irescale/vignettes/rectifiedI.html
      # package spdep
      # Note that p-values for Moran's I really need to be simulated
      # some places seem to think that I NEED to use projected coordinates, rather than lat/lon for Moran's I. 
      
      
      # with package spdep, which uses monte carlo simulations for the p-value. (Much more appropriate)
      # NOTE: package spdep can cause problems with raster and gstat. I've re-installed all those packages. 
      # spdep::moran.test(x     = res.fit.g1[ (fit.id %in% MI.samp.g1) ], listw = spdep::mat2listw(dist.mat.g1), randomisation = FALSE)
      # (MI.g1.mc <- spdep::moran.mc(x     = res.fit.g1[ (fit.id %in% MI.samp.g1) ],
      #                              listw = spdep::mat2listw(dist.mat.g1),
      #                              nsim  = 100)); beepr::beep()
      # plot(MI.g1.mc, main="", las=1)  
      
      interpretation <- ifelse(test = (MI$p.value < 0.05), 
                               yes = ifelse(test = (MI$observed > MI$expected), 
                                            yes = 'Residuals are positively auto-correlated.', 
                                            no = 'Residuals are negatively auto-correlated.'), 
                               no = 'Accept null hypothesis of no spatial autocorrelation.')
      
      return(list(
         observed = MI$observed,
         expected = MI$expected,
         sd       = MI$sd,
         p.value  = MI$p.value,
         interpretation = interpretation
      )
      )
   }
   
   # function to plot the effects of individual covariates
   create_fixed_effect_plot_list <- function(m1_model = NULL, 
                                             m2_fixed_effect_beta_matrix = NULL,
                                             dataset,
                                             covariates,
                                             prediction_column_in_pred.dat,
                                             m1_quantregForestmodel = NULL){
      # m1_model = 'gam' model fit
      # m2_fixed_effect_beta_matrix = matrix of samples of estimated fixed effect betas from the model posterior (for INLA model)
      # dataset = data used to fit the model
      # covariates = all covariates that were included in the model fitting 
      # prediction_column_in_pred.dat = the name of the column that has the model predictions. This function uses "prediction_column_in_pred.dat" to select Lat & Lon values for the fixed effect plots
      
      # this function requires several items to be in the global environment:
      #  - pre.dat = dataframe of covariate values used for predictions (in this case I use the average of some of the covariate values for these fixed effect plots)
      #  - dat0 = the data used to fit the models, but dat0 has the scaling info (mean & sd) of covariates that is lost in the "dat" dataset 
      #  - covar_name_df = a dataset of covariate names, abbreviations, and transformations
      
      # whether to make calculations based on a gam model (m1) or an INLA model (m2)
      if( !is.null(m1_model) ){
         m1 <- TRUE
         m2 <- FALSE
         m1_class <- if('gam' %in% class(m1_model)) 'gam' else if('randomForest' %in% class(m1_model)) 'rf' else stop('This function only accepts GAM and random Forest models at this point.')
      }
      if( !is.null(m2_fixed_effect_beta_matrix) ) {
         m1 <- FALSE
         m2 <- TRUE
         m1_class <- NA
      }
      
      # create an empty list to hold the results
      fixed_effect_plot_list <- list()
      
      # the model has lat & lon in the dataset, then I'll need to choose a lat & lon for these plots
      # I'll make predictions for the location where the predicted probability of bycatch is closest to 50%
      # hopefully that way the effect sizes will be worthwhile plotting (and not all essentially 0 or 1, which might happen if the spatial effect swamps the covariates)
      i.50 <- which(abs(pred.dat[,prediction_column_in_pred.dat] - 0.5) == min( abs(pred.dat[,prediction_column_in_pred.dat] - 0.5), 
                                                                                na.rm = T))[1] # just in case there's more than 1
      
      # names of covariates to include for model predictions (just include anything that it might use...)
      other_covars <- c('year_f', 'year', 'lon', 'lon_proj', 'lat', 'lat_proj')
      covar_to_include <- c(covariates, other_covars[ !(other_covars %in% covariates) ])
      
      # loop over the covariates
      for(i in 1:length(covariates)){
         # get the covariate name
         covar_name  <- covariates[i]
         
         # these plots are only for continuous predictor variables, so I'll skip categorical predictor variables
         if( is.factor(dataset[,covar_name]) ) next
         # I can also skip any covariates that end with "p2" because polynomial predictors need to be considered together
         if( grepl(pattern = '.+_p2$', x = covar_name) ) next
         
         # create a vector of value across the range of the covariage values (this gets a little complicated if the covariate was logged, scaled, and/or polynomialized)
         if(! (m1 & m1_class == 'rf')){
            # get the original, raw covariate name (not scaled, logged, or polynomialized)
            var_name <- covar_name_df[match(x = covar_name,
                                            table = covar_name_df$final_name),
                                      'original_name']
            # was the original covariate logged?
            var_logged_logical <- covar_name_df[match(x = covar_name,
                                                      table = covar_name_df$final_name),
                                                'logged']
            # was the original covariate scaled?
            var_scaled_logical <- covar_name_df[match(x = covar_name,
                                                      table = covar_name_df$final_name),
                                                'scaled']
            # if it was scaled, get the original scaled data (so that I have the mean and SD)
            if(var_scaled_logical){
               var_scale <- dat0[, covar_name_df[match(x = covar_name,
                                                       table = covar_name_df$final_name),
                                                 'scaled_name'] ] 
            } 
            # was the original covariate polynomialized?
            var_poly_logical <- covar_name_df[match(x = covar_name,
                                                    table = covar_name_df$final_name),
                                              'polyd']
            # if it was polynomialized, get the original polynomial object (so that I can use it for converting the new values)
            if(var_poly_logical){
               var_poly  <- covariate_polynomials[[
                  covar_name_df[ match(x = covar_name,
                                       table = covar_name_df$final_name),
                                 'polynomial_object_name' ]
               ]]
            }
            
            # get the range of the variable from the original data
            var_range <- if(var_name == 'bottom_slope') range(dat0[,'bottom_slope_GEBCO']) else range(dat0[,var_name])
            # create a sequence over the range for model predictions
            new_var   <- seq(from = var_range[1], to = var_range[2], length.out = 500)
            # log the new datapoints if original variable was logged
            if(var_logged_logical) new_var_l <- log(new_var)
            # scale new datapoints if original variable was scaled
            if(var_scaled_logical){
               new_var_s <- scale(x = if(var_logged_logical) new_var_l else new_var,
                                  center = attr(x = var_scale, 'scaled:center'), 
                                  scale  = attr(x = var_scale, 'scaled:scale'))
            }
            
            # polynomialize the new datapoints if the original variable was polynomialized
            if(var_poly_logical){
               # convert to orthogonal polynomial 
               new_var_poly <- predict(object  = var_poly,
                                       newdata = as.vector(new_var_s)) # all the variables that are polynomialized are also scaled, so I can just use var_s here
               new_var_p1 <- new_var_poly[,1]
               new_var_p2 <- new_var_poly[,2]
            }
         } else {
            # for the random forest, things are a little simpler
            
            # get the range of the variable from the original data
            var_range <- range(dat[,covar_name])
            # create a sequence over the range for model predictions
            new_var   <- seq(from = var_range[1], to = var_range[2], length.out = 500)
            
            # set these so that I can use the same if/else statement below
            var_poly_logical <- var_scaled_logical <- var_logged_logical <- FALSE
            # and set the var_name for plotting
            var_name <- covar_name
         }
         
         # create a new dataframe for predicting the model
         # use mean values for everything other than variable of interest
         # use full range for variable of interest
         newdat <- data.frame(
            sector = factor(unique(pred.dat$sector)[1], levels = levels(dataset$sector)),
            year   = unique(pred.dat$year),
            year_f = unique(pred.dat$year_f),
            doy    = unique(pred.dat$doy),
            doy_p1 = unique(pred.dat$doy_p1),
            doy_p2 = unique(pred.dat$doy_p2),
            duration = unique(pred.dat$duration),
            # use the most common fishing depth value [could just use 0 for the mean value...]
            fishing_depth_m = mean(dat$fishing_depth_m, na.rm = T), # unique(pred.dat$depth_fish_m)[ as.vector(table(pred.dat$depth_fish_m)) == max(as.vector(table(pred.dat$depth_fish_m))) ],
            depth_fish_p1 = unique(pred.dat$depth_fish_p1)[ as.vector(table(pred.dat$depth_fish_p1)) == max(as.vector(table(pred.dat$depth_fish_p1))) ],
            depth_fish_p2 = unique(pred.dat$depth_fish_p2)[ as.vector(table(pred.dat$depth_fish_p2)) == max(as.vector(table(pred.dat$depth_fish_p2))) ],
            # after scaling, 0 is the mean value for these variables
            depth           = mean(dat$depth, na.rm = T), # unique(pred.dat$depth_bottom_m)[ as.vector(table(pred.dat$depth_bottom_m)) == max(as.vector(table(pred.dat$depth_bottom_m))) ], 
            depth_bottom_p1 = rep(0, 500), 
            depth_bottom_p2 = 0, 
            bottom_slope = mean(dat$bottom_slope_GEBCO, na.rm = T), # unique(pred.dat$slope_bottom)[ as.vector(table(pred.dat$slope_bottom)) == max(as.vector(table(pred.dat$slope_bottom))) ], 
            slope_bottom_p1 = 0, # mean(pred.dat$slope_bottom_p1),
            slope_bottom_p2 = 0, # mean(pred.dat$slope_bottom_p2),
            sst    = mean(dat$sst), # unique(pred.dat$sst)[ as.vector(table(pred.dat$sst)) == max(as.vector(table(pred.dat$sst))) ][1], 
            sst_p1 = 0, # mean(pred.dat$sst_p1, na.rm = T),
            sst_p2 = 0, # mean(pred.dat$sst_p2, na.rm = T),
            sst_anomaly    = mean(dat$sst_anomaly, na.rm = T), # unique(pred.dat$sst_anomaly)[ as.vector(table(pred.dat$sst_anomaly)) == max(as.vector(table(pred.dat$sst_anomaly))) ][1], 
            sst_anomaly_p1 = 0, # mean(pred.dat$sst_anomaly_p1, na.rm = T), # hist(pred.dat$sst_anomaly_p1)
            sst_anomaly_p2 = 0, # mean(pred.dat$sst_anomaly_p2, na.rm = T), # hist(pred.dat$sst_anomaly_p2)
            cuti           = mean(dat$cuti, na.rm =T),
            cuti_p1 = 0, # mean(pred.dat$cuti_p1),
            cuti_p2 = 0, # mean(pred.dat$cuti_p2)
            lon      = pred.dat$lon[ i.50 ],
            lon_proj = pred.dat$lon_proj[ i.50 ],
            lat      = pred.dat$lat[ i.50 ],
            lat_proj = pred.dat$lat_proj[ i.50 ]
         )
         
         # make sure the covariates are in the same order
         newdat <- newdat[,covar_to_include]
         # attr(terms.formula(fit.g2$formula), 'term.labels')[ !grepl(pattern = '^s\\(', x = attr(terms.formula(fit.g2$formula), 'term.labels'))]
         
         # fill in the new values for the prediction
         if(var_poly_logical){
            newdat[, covar_name] <- new_var_p1
            newdat[, covar_name_df[match(x = covar_name,
                                         table = covar_name_df$final_name),
                                   'pair_name'] ] <- new_var_p2
         } else if(var_scaled_logical){
            newdat[, covar_name] <- new_var_s
         } else if(var_logged_logical){
            newdat[, covar_name] <- new_var_l
         } else newdat[, covar_name] <- new_var
         
         
         # predict new values
         if(m1){
            if('gam' %in% class(m1_model)){
               preds <- mgcv::predict.gam(object = m1_model, 
                                          newdata = newdat, 
                                          type = 'link', 
                                          se.fit = TRUE) # can't predict on the response scale if I'm going to +/- SE to get CI [it WILL give SE on the response scale, but I can't use those to correctly calculate confidence limits]
               
               # add the predictions into the newdat object
               newdat$z_link     <- preds$fit
               newdat$z_link_lcl <- preds$fit - 2 * preds$se.fit
               newdat$z_link_ucl <- preds$fit + 2 * preds$se.fit
               
               # convert to response scale
               newdat$zp    <- m1_model$family$linkinv(newdat$z_link)
               newdat$z_lcl <- m1_model$family$linkinv(newdat$z_link_lcl)
               newdat$z_ucl <- m1_model$family$linkinv(newdat$z_link_ucl)
               newdat[,var_name] <- new_var
            }
            
            if('randomForest' %in% class(m1_model)){
               newdat$zp <- randomForest:::predict.randomForest(object = m1_model, 
                                                                newdata = newdat)
               
               if(is.null(m1_quantregForestmodel)) warning('User must supply "m1_quantregForestmodel" to get an estimate of uncertainty for a random forest model.\nCreating plots without an estimate of uncertainty...')
               if(!is.null(m1_quantregForestmodel)){
                  # get some sort of SD
                  newdat$z.sd <- quantregForest:::predict.quantregForest(object = m1_quantregForestmodel,
                                                                         newdata = newdat,
                                                                         what   = sd)
                  # There's no link function for random forests, so perhaps this is somewhat of a relevant estimate for confidence limits?
                  newdat$z_lcl <- pmax(0, newdat$zp - 2 * newdat$z.sd)
                  newdat$z_ucl <- newdat$zp + 2 * newdat$z.sd
               } else {
                  # if there's no quantregForestmodel supplied, then just assign the confidence limits the same value as the mean estimate
                  newdat$z_lcl <- newdat$z_ucl <- newdat$zp
               }
            }
         }
         if(m2){
            # prep the data for an R-INLA analysis
            # make a model matrix with the reduced dataset
            mm.pred2 <- model.matrix( as.formula(paste ('~', paste(covariates, collapse = '+') )), 
                                      data = newdat[, covariates])
            # colnames(mm.pred2) <- c('Intercept', colnames(mm.fit))
            
            # predict the new values by multiplying the coefficients and covariates
            # preds <- mm.pred2 %*% t(m2_fixed_effect_beta_matrix)
            preds <- mm.pred2 %*% t(m2_fixed_effect_beta_matrix)
            
            # add the predictions into the newdat object
            newdat$z_link     <- apply(X = preds, MARGIN = 1, FUN = mean)
            newdat$z_link_lcl <- apply(X = preds, MARGIN = 1, FUN = quantile, probs = 0.025)
            newdat$z_link_ucl <- apply(X = preds, MARGIN = 1, FUN = quantile, probs = 0.975)
            
            # convert to response scale
            newdat$zp    <- exp(newdat$z_link) # INLA::inla.link.logit
            newdat$z_lcl <- exp(newdat$z_link_lcl)
            newdat$z_ucl <- exp(newdat$z_link_ucl)
            newdat[,var_name] <- new_var
         }
         
         # add in lon & lat to the data.frame of variable info
         if(! 'lon' %in% covar_name_df$original_name) covar_name_df <- rbind(covar_name_df,
                                                                             data.frame('original_name' = c('lon','lon_proj'),
                                                                                        'logged' = c(FALSE, FALSE),
                                                                                        'scaled' = c(FALSE, FALSE),
                                                                                        'scaled_name' = c(NA,NA),
                                                                                        'polyd' = c(FALSE,FALSE),
                                                                                        'polynomial_object_name' = c(NA,NA),
                                                                                        'final_name' = c('lon','lon_proj'),
                                                                                        'pair_name' = c(NA,NA),
                                                                                        'pretty_name' = c('Longitude','Longitude (projected)'),
                                                                                        'plot_label' = c('Longitude', 'Longitude (projected)')))
         if(! 'lat' %in% covar_name_df$original_name) covar_name_df <- rbind(covar_name_df,
                                                                             data.frame('original_name' = c('lat','lat_proj'),
                                                                                        'logged' = c(FALSE,FALSE),
                                                                                        'scaled' = c(FALSE,FALSE),
                                                                                        'scaled_name' = c(NA,NA),
                                                                                        'polyd' = c(FALSE,FALSE),
                                                                                        'polynomial_object_name' = c(NA,NA),
                                                                                        'final_name' = c('lat','lat_proj'),
                                                                                        'pair_name' = c(NA,NA),
                                                                                        'pretty_name' = c('Latitude','Latitude (projected)'),
                                                                                        'plot_label' = c('Latitude', 'Latitude (projected)')))
         
         # get some prettier names for the covariate
         plot_name <- covar_name_df[match(x = covar_name,
                                          table = if(!(m1 & 'randomForest' %in% class(m1_model))) covar_name_df$final_name else covar_name_df$original_name),
                                    'pretty_name']
         plot_label_text <- covar_name_df[match(x = covar_name,
                                                table = if(!(m1 & 'randomForest' %in% class(m1_model))) covar_name_df$final_name else covar_name_df$original_name),
                                          'plot_label']
         
         # plot covariate
         (fixed_effect_plot_list[[
            plot_name
         ]] <- ggplot(newdat, aes_string(x = var_name, y = 'zp')) +
               theme_bw() + 
               geom_polygon(data = na.omit(data.frame(x = c(newdat[,var_name], 
                                                            rev(newdat[,var_name])),
                                                      y = c(newdat$z_lcl,
                                                            rev(newdat$z_ucl)))),
                            mapping = aes(x = x, y = y),
                            fill = 'grey90')+
               geom_line()+
               coord_cartesian(ylim = c(0, ifelse(test = max(newdat$z_ucl, na.rm = T) > 1.5*max(newdat$zp, na.rm = T), 
                                                  yes = 1.5*max(newdat$zp, na.rm = T), 
                                                  no = max(newdat$z_ucl, na.rm = T))))+
               xlab(label = parse(text = plot_label_text)) +
               ylab(label = 'Effect size'))
      }
      # names(fixed_effect_plot_list)
      # fixed_effect_plot_list[[1]]
      # fixed_effect_plot_list[[2]]
      # # more on confidence intervals on GAMS
      # https://fromthebottomoftheheap.net/2016/12/15/simultaneous-interval-revisited/
      return(fixed_effect_plot_list)
   }
   
   # function to save leaflet plots as self-contained html files
   save_leaflet <- function(plot, file){
      full_path <- paste0(getwd(), '/', file)
      mapview::mapshot(x = plot, url = full_path)
      ## htmlwidgets::saveWidget(lf.g1m, file = paste0(getwd(),"/figures/leaflet/gamma_model_predictions_g1.html"))
      # htmlwidgets:::pandoc_self_contained_html(input  = full_path, 
      #                                          output = full_path)
      unlink(paste0( dirname(full_path),
                     '/',
                     tools::file_path_sans_ext(basename(full_path)),
                     '_files'), 
             recursive = T)
   }
   
   
   # save a synced leaflet map: https://github.com/r-spatial/mapview/issues/35
   save_leafsync <- function (tags, file, selfcontained = T){
      libdir <- file.path(# dirname(file), 
         paste(tools::file_path_sans_ext(basename(file)), 
               "_files", sep = ""))
      
      htmltools::save_html(tags, file = file) # , libdir = libdir)
      if (selfcontained) {
         if (!htmlwidgets:::pandoc_available()) {
            stop("Saving a widget with selfcontained = TRUE requires pandoc. For details see:\n", 
                 "https://github.com/rstudio/rmarkdown/blob/master/PANDOC.md")
         }
         htmlwidgets:::pandoc_self_contained_html(file, file)
         unlink(libdir, recursive = TRUE)
      }
      return(file)
   }
   
   # function for using the sf package to reproject coordinates
   # reproject <- function(x, y, from_crs, to_crs){
   #    sfo <- sf::st_as_sf(data.frame(x = x, y = y), 
   #                        coords = c('x', 'y'),
   #                        crs = from_crs, 
   #                        remove = F) %>% 
   #       sf::st_transform(crs = to_crs)
   #    return(as.data.frame(
   #       sf::st_coordinates(sfo)
   #    ))
   # }
   
}

# set common color ranges for plotting maps from different models
{
   # binomial model
   plot.mean.range.b <- c(0,1)
   plot.sd.range.b <- c(0,0.5)
   # gamma model
   plot.mean.range.g <- c(0,200)
   plot.mean.legend.scale.breaks.g <- c(0,10,100,200)
   plot.sd.range.g <- c(0,100)
   plot.sd.legend.scale.breaks.g <- c(0,10,100)
   # hurdle model
   plot.mean.range.h <- c(0,100)
   plot.mean.legend.scale.breaks.h <- c(0,10,100)
}


##### 1. Fit GLM 1 (linear predictors) #####
{
   # covariates for model 1
   covar.1 <- c(covar, 'lat', 'lon')
   
   # fit a binomial model
   fit.b1 <- fit_m1_glm(data = bdat, 
                        covariates  = covar.1, 
                        modelfamily = 'binomial', 
                        rows.train  = 1:nrow(bdat),
                        rows.test   = NULL, 
                        save.model.fit = TRUE)$model
   # summary(fit.b1)
   
   # fit a gamma model
   fit.g1 <- fit_m1_glm(data = gdat, 
                        covariates  = covar.1, 
                        modelfamily = 'gamma', 
                        rows.train  = 1:nrow(gdat),
                        rows.test   = NULL, 
                        save.model.fit = TRUE)$model
   # summary(fit.g1)
   
   # example (not run) of other model assessment metrics
   if(FALSE){
      # visualize model parameter estimates
      {
         # define the categorical covariates
         categorical_covs <- c('(Intercept)',
                               'sector',
                               'sectorMothership',
                               'sectorMidwater Hake',
                               'sectorCatcher Processor',
                               paste0('year_f', 2002:2019))
         # extract covariate coefficient estimates from the model
         b1.tidy <- broom::tidy(fit.b1,
                                parametric = TRUE,
                                conf.int = TRUE,
                                conf.level = 0.95) %>%
            # split up parameters that are standardized & those that are not, because they're on vastly different scales
            mutate(poly = grepl(pattern = '.+_p', x = term),
                   concat = ifelse(term %in% categorical_covs, 'categorical', 'linear'))

         # plot parameter estimates
         (b1.fixed.par.plot <- ggplot(b1.tidy,
                                      aes(x = term,
                                          y = estimate,
                                          ymin = conf.low,
                                          ymax = conf.high)) +
               theme_bw() +
               geom_pointrange() +
               facet_wrap(facets = vars(concat), scales = 'free') +
               theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)))
      }
      
      # model assessment
      {
         # mgcViz package moel assessment
         Viz.b1 <- mgcViz::getViz(fit.b1)
         o <- mgcViz::check.gamViz(Viz.b1,
                                   a.qq = list(method = 'auto', # "tunif", # "tnorm",
                                               a.cipoly = list(fill = "light blue")),
                                   a.res = list(size = 0.5),
                                   a.hist = list(bins = 30))

         # Calculate model residuals
         resid_list_b1 <- calculate_residuals(m1_model    = fit.b1,
                                              m1_data.fit  = dat[, c('bycatch', covar.b1)],
                                              model.family = model_family)

         # plot Variograms
         variogram.b1 <- my_variogram(residuals = res.b1,
                                         longitude_projected = dat[, 'lon_proj'],
                                         latitude_projected  = dat[, 'lat_proj'],
                                         cressie = TRUE,
                                         width = 1000,
                                         x_label = 'Distance (km)',
                                         add_sample_size = T,
                                         include_directional_variogram = T)
         variogram.b1[['variogram_plot']]
         variogram.b1[['directional_variogram_plot']]

         # Moran's I (using only a subset of the data b/c this requires matrix inversion)
         MI.b1 <- calculate_Morans_I(residuals = resid_list_b1$residuals.deviance,
                                     lon = dat[, 'lon_proj'],
                                     lat = dat[, 'lat_proj'],
                                     seed = 3,
                                     subsample_proportion = .2)
      }
   }
   
   # binomial model predictions
   {
      # make predictions using grid1 prediction dataset
      predictions.b1.response <- mgcv::predict.gam(fit.b1, 
                                                   newdata = pdat[,covar.1],
                                                   type = 'response', 
                                                   se.fit = T)
      # make predictions using grid2 prediction dataset
      predictions.b1.response2 <- mgcv::predict.gam(fit.b1, 
                                                   newdata = pdat2[,covar.1],
                                                   type = 'response', 
                                                   se.fit = T)
      
      # add the predicted values into the pdat dataset
      pdat$z.pred.b1         <- predictions.b1.response$fit    # fit.b1$family$linkinv(predictions.b.glm.link$fit) is the same as:         predict(fit.b1, newdata = pdat[,c('YEAR.f', covar)], type = 'response')
      pdat$z.pred.b1.sd      <- predictions.b1.response$se.fit # fit.b1$family$linkinv(predictions.b.glm.link$se.fit) is NOT THE SAME as:  predict(fit.b1, newdata = pdat[,c('YEAR.f', covar)], type = 'response', se.fit=T)$se.fit
      
      # 2nd prediction dataset
      pdat2$z.pred.b1         <- predictions.b1.response2$fit    # fit.b1$family$linkinv(predictions.b.glm.link$fit) is the same as:         predict(fit.b1, newdata = pdat[,c('YEAR.f', covar)], type = 'response')
      pdat2$z.pred.b1.sd      <- predictions.b1.response2$se.fit # fit.b1$family$linkinv(predictions.b.glm.link$se.fit) is NOT THE SAME as:  predict(fit.b1, newdata = pdat[,c('YEAR.f', covar)], type = 'response', se.fit=T)$se.fit
      
      # model name for plot legends
      mn <- 'GLM 1'
      
      # plot model predictions
      (plot.b1.mean <- plot_covariates_gg(data = pdat %>%
                                             dplyr::mutate(lat = lat_unproj,
                                                           lon = lon_unproj),
                                          field = 'z.pred.b1',
                                          legend.title = paste0(mn, '\nPredicted\nProbability\nof Bycatch'),
                                          colormap = 'viridis',
                                          legend.color.range = plot.mean.range.b))
      ggsave(filename = file.path(base_folder, paste0('binomial_model_predictions_b1.png')), plot = plot.b1.mean, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.b1.mean)
      
      # plot SD of model predictions
      (plot.b1.sd <- plot_covariates_gg(data = pdat %>%
                                           dplyr::mutate(lat = lat_unproj,
                                                         lon = lon_unproj),
                                        field = 'z.pred.b1.sd',
                                        legend.title = paste0(mn, '\nSD of\nPredicted\nBycatch\nProbability'),
                                        legend.color.range = plot.sd.range.b,
                                        colormap = 'magma'))
      ggsave(filename = file.path(base_folder, paste0('binomial_model_predictions_b1_sd.png')), plot = plot.b1.sd, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.b1.sd)
      
      # interactive plot
      if(FALSE){
         # first convert the data.frame to a raster
         b1.mean.r <- raster::rasterFromXYZ(xyz = pdat  %>%
                                               dplyr::mutate(lat = lat_unproj,
                                                             lon = lon_unproj) %>%
                                               dplyr::select(lon, lat, z.pred.b1),
                                            crs = sp::CRS('+init=EPSG:4326'))
         b1.sd.r   <- raster::rasterFromXYZ(xyz = pdat  %>%
                                               dplyr::mutate(lat = lat_unproj,
                                                             lon = lon_unproj) %>%
                                               dplyr::select(lon, lat, z.pred.b1.sd),
                                            crs = sp::CRS('+init=EPSG:4326'))

         # leaflet uses epgs:3857, so I need to project to that first
         # (leaflet will do it automatically, but it can screw up the color palate, so I'll do it first)
         # to get rid of errors see: https://github.com/rstudio/leaflet/issues/224
         b1.mean.rl <- leaflet::projectRasterForLeaflet(b1.mean.r, method = 'bilinear')
         b1.sd.rl   <- leaflet::projectRasterForLeaflet(b1.sd.r, method = 'bilinear')
         # the warnings actually come from raster package: raster::projectRaster(from = haul_count_r, crs = sp::CRS('+init=EPSG:3857'))

         # first create a color palatte function
         pal.b1m <- leaflet::colorNumeric(palette  = 'viridis',
                                          domain   = range(na.omit(raster::values(b1.mean.rl))),
                                          na.color = "transparent")
         pal.b1sd <- leaflet::colorNumeric(palette  = 'magma',
                                           domain   = range(na.omit(raster::values(b1.sd.rl))),
                                           na.color = "transparent")

         # then create the plot
         plot_height_pix <- 900
         lf.b1m <- leaflet::leaflet(height = plot_height_pix) %>%
            leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>%
            leaflet::addRasterImage(x = b1.mean.rl,
                                    colors = pal.b1m,
                                    project = FALSE,
                                    opacity = 0.9,
                                    group = "Bycatch",
                                    layerId = "Bycatch") %>%
            leafem::addImageQuery(x = b1.mean.rl,
                                  digits = 4,
                                  position = 'topleft',
                                  project = TRUE,
                                  layerId = "Bycatch") %>%
            leafem::addMouseCoordinates() %>%
            leaflet::addLegend(position = 'topright',
                               title = paste0(mn, '</br>Predicted</br>Bycatch</br>Probability'), # need HTML coding for line breaks </br> , not R \n
                               pal = pal.b1m,
                               values = raster::values(b1.mean.rl))
         # lf.b1m
         ## save to standalone .html
         save_leaflet(plot = lf.b1m,
                      file = file.path(base_folder, 'leaflet', 'binomial_model_predictions_b1_mean.html'))

         lf.b1sd <- leaflet::leaflet(height = plot_height_pix) %>%
            leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>%
            leaflet::addRasterImage(x = b1.sd.rl,
                                    colors = pal.b1sd,
                                    project = FALSE,
                                    opacity = 0.9,
                                    group = "Bycatch_SD",
                                    layerId = "Bycatch_SD") %>%
            leafem::addImageQuery(x = b1.sd.rl,
                                  digits = 6,
                                  position = 'topleft',
                                  project = TRUE,
                                  layerId = "Bycatch_SD") %>%
            leafem::addMouseCoordinates() %>%
            leaflet::addLegend(position = 'topright',
                               title = paste0(mn, '</br>SD of</br>Predicted</br>Bycatch</br>Probability'), # need HTML coding for line breaks </br> , not R \n
                               pal = pal.b1sd,
                               values = raster::values(b1.sd.rl))
         # lf.b1sd
         save_leaflet(plot = lf.b1sd,
                      file = file.path(base_folder, 'leaflet', 'binomial_model_predictions_b1_sd.html'))

         # mean and SD together
         # leafsync::sync(lf.b1m, lf.b1sd)
         # save_leafsync(tags = leafsync::sync(lf.b1m, lf.b1sd),
         #               file = paste0(getwd(), '/figures/leaflet/binomial_model_predictions_b1_mean_and_sd_', short_long, '_',
         #                             ifelse(use_projected, 'projected', 'unprojected'), '.html'))
      }
   }
   
   # gamma model predictions
   {
      # make predictions using grid1 prediction dataset
      predictions.g1.response <- mgcv::predict.gam(fit.g1, 
                                                   newdata = pdat[,covar.1],
                                                   type = 'response', 
                                                   se.fit = T)
      # make predictions using grid2 prediction dataset
      predictions.g1.response2 <- mgcv::predict.gam(fit.g1, 
                                                    newdata = pdat2[,covar.1],
                                                    type = 'response', 
                                                    se.fit = T)
      
      # add the predicted values into the pdat dataset
      pdat$z.pred.g1         <- predictions.g1.response$fit 
      pdat$z.pred.g1.sd      <- predictions.g1.response$se.fit
      
      # 2nd prediction dataset
      pdat2$z.pred.g1         <- predictions.g1.response2$fit 
      pdat2$z.pred.g1.sd      <- predictions.g1.response2$se.fit
      
      # plot model predictions
      (plot.g1.mean <- plot_covariates_gg(data = pdat %>%
                                             dplyr::mutate(lat = lat_unproj,
                                                           lon = lon_unproj),
                                          field = 'z.pred.g1',
                                          legend.title = paste0(mn, '\nPredicted\nNumber\nof Bycatch'),
                                          colormap = 'viridis',
                                          legend.color.range = plot.mean.range.g, 
                                          transformation = 'log1p', 
                                          legend.scale.breaks = plot.mean.legend.scale.breaks.g))
      ggsave(filename = file.path(base_folder, paste0('gamma_model_predictions_g1.png')), plot = plot.g1.mean, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.g1.mean)
      
      # plot SD of model predictions
      (plot.g1.sd <- plot_covariates_gg(data = pdat %>%
                                           dplyr::mutate(lat = lat_unproj,
                                                         lon = lon_unproj),
                                        field = 'z.pred.g1.sd',
                                        legend.title = paste0(mn, '\nSD of\nPredicted\nBycatch'), 
                                        legend.color.range = plot.sd.range.g,
                                        transformation = 'log1p', 
                                        legend.scale.breaks = plot.sd.legend.scale.breaks.g,
                                        colormap = 'magma'))
      ggsave(filename = file.path(base_folder, paste0('gamma_model_predictions_g1_sd.png')), plot = plot.g1.sd, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.g1.sd)
      
      # # interactive plot
      if(FALSE){
         # first convert the data.frame to a raster
         g1.mean.r <- raster::rasterFromXYZ(xyz = pdat  %>%
                                               dplyr::mutate(lat = lat_unproj,
                                                             lon = lon_unproj) %>%
                                               dplyr::select(lon, lat, z.pred.g1),
                                            crs = sp::CRS('+init=EPSG:4326'))
         g1.sd.r   <- raster::rasterFromXYZ(xyz = pdat  %>%
                                               dplyr::mutate(lat = lat_unproj,
                                                             lon = lon_unproj) %>%
                                               dplyr::select(lon, lat, z.pred.g1.sd),
                                            crs = sp::CRS('+init=EPSG:4326'))

         # leaflet uses epgs:3857, so I need to project to that first
         # (leaflet will do it automatically, but it can screw up the color palate, so I'll do it first)
         # to get rid of errors see: https://github.com/rstudio/leaflet/issues/224
         g1.mean.rl <- leaflet::projectRasterForLeaflet(g1.mean.r, method = 'bilinear')
         g1.sd.rl   <- leaflet::projectRasterForLeaflet(g1.sd.r, method = 'bilinear')
         # the warnings actually come from raster package: raster::projectRaster(from = haul_count_r, crs = sp::CRS('+init=EPSG:3857'))

         # first create a color palatte function
         pal.g1m <- leaflet::colorNumeric(palette  = 'viridis',
                                          domain   = range(na.omit(raster::values(g1.mean.rl))),
                                          na.color = "transparent")
         pal.g1sd <- leaflet::colorNumeric(palette  = 'magma',
                                           domain   = range(na.omit(raster::values(g1.sd.rl))),
                                           na.color = "transparent")

         # then create the plot
         plot_height_pix <- 900
         lf.g1m <- leaflet::leaflet(height = plot_height_pix) %>%
            leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>%
            leaflet::addRasterImage(x = g1.mean.rl,
                                    colors = pal.g1m,
                                    project = FALSE,
                                    opacity = 0.9,
                                    group = "Bycatch",
                                    layerId = "Bycatch") %>%
            leafem::addImageQuery(x = g1.mean.rl,
                                  digits = 4,
                                  position = 'topleft',
                                  project = TRUE,
                                  layerId = "Bycatch") %>%
            leafem::addMouseCoordinates() %>%
            leaflet::addLegend(position = 'topright',
                               title = paste0(mn, '</br>Predicted</br>Bycatch</br>Probability' ), # need HTML coding for line breaks </br> , not R \n
                               pal = pal.g1m,
                               values = raster::values(g1.mean.rl))
         # lf.g1m
         ## save to standalone .html
         save_leaflet(plot = lf.g1m,
                      file = file.path(base_folder, 'leaflet', 'binomial_model_predictions_g1_mean.html'))

         lf.g1sd <- leaflet::leaflet(height = plot_height_pix) %>%
            leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>%
            leaflet::addRasterImage(x = g1.sd.rl,
                                    colors = pal.g1sd,
                                    project = FALSE,
                                    opacity = 0.9,
                                    group = "Bycatch_SD",
                                    layerId = "Bycatch_SD") %>%
            leafem::addImageQuery(x = g1.sd.rl,
                                  digits = 6,
                                  position = 'topleft',
                                  project = TRUE,
                                  layerId = "Bycatch_SD") %>%
            leafem::addMouseCoordinates() %>%
            leaflet::addLegend(position = 'topright',
                               title = paste0(mn, '</br>SD of</br>Predicted</br>Bycatch</br>Probability' ), # need HTML coding for line breaks </br> , not R \n
                               pal = pal.g1sd,
                               values = raster::values(g1.sd.rl))
         # lf.g1sd
         save_leaflet(plot = lf.g1sd,
                      file = file.path(base_folder, 'leaflet', 'binomial_model_predictions_g1_sd.html'))

         # mean and SD together
         # leafsync::sync(lf.g1m, lf.g1sd)
         # save_leafsync(tags = leafsync::sync(lf.g1m, lf.g1sd),
         #               file = paste0(getwd(), '/figures/leaflet/binomial_model_predictions_g1_mean_and_sd_', short_long, '_',
         #                             ifelse(use_projected, 'projected', 'unprojected'), '.html'))
      }
   }
   
   # combined hurdle model predictions
   {
      pdat <- pdat %>% 
         dplyr::mutate(z.pred.h1 = z.pred.b1 * z.pred.g1)
      pdat2 <- pdat2 %>% 
         dplyr::mutate(z.pred.h1 = z.pred.b1 * z.pred.g1)
      
      # plot model predictions
      (plot.h1.mean <- plot_covariates_gg(data = pdat %>%
                                             dplyr::mutate(lat = lat_unproj,
                                                           lon = lon_unproj),
                                          field = 'z.pred.h1',
                                          legend.title = paste0(mn, '\nPredicted\nNumber\nof Bycatch'),
                                          colormap = 'viridis',
                                          legend.color.range = plot.mean.range.h, 
                                          transformation = 'log1p', 
                                          legend.scale.breaks = plot.mean.legend.scale.breaks.h))
      ggsave(filename = file.path(base_folder, paste0('hurdle_model_predictions_h1.png')), plot = plot.h1.mean, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.h1.mean)
      
      # plot of spatial effect through time
      # just plot 1 year b/c the spatial component is the same every year. 
      plot.h1.space <- plot_covariates_gg(data = pdat2 %>%
                                             dplyr::mutate(lat = lat_unproj,
                                                           lon = lon_unproj) %>% 
                                             dplyr::filter(year_f == '2014'),
                                          field = 'z.pred.h1',
                                          legend.title = paste0(mn, '\nPredicted\nNumber\nof Bycatch'),
                                          colormap = 'viridis',
                                          legend.color.range = plot.mean.range.h, 
                                          transformation = 'log1p', 
                                          legend.scale.breaks = plot.mean.legend.scale.breaks.h,
                                          facet_field = 'year_f', 
                                          facet_rows = 3)
      ggsave(filename = file.path(base_folder, paste0('m1_spatial_predictions.png')), plot = plot.h1.space, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.h1.space)
   }
   
   # Create datasets for marginal effects plots of fixed effects
   {
      # create empty sublists for each covariate
      marginal_effect_list_b[[tolower(mn)]] <- vector('list', length(covar.1))
      names(marginal_effect_list_b[[tolower(mn)]]) <- covar.1
      
      marginal_effect_list_g[[tolower(mn)]] <- vector('list', length(covar.1))
      names(marginal_effect_list_g[[tolower(mn)]]) <- covar.1
      
      # loop over covariates
      for(c in 1:length(covar.1)){
         
         # get the dataframe for the model predictions
         ndb <- ndg <- prediction_dfs[[covar.1[c]]]
         
         # predict new values
         preds.b <- mgcv:::predict.gam(object = fit.b1, 
                                       newdata = ndb,
                                       type = 'link', 
                                       se.fit = TRUE) # can't predict on the response scale if I'm going to +/- SE to get CI [it WILL give SE on the response scale, but I can't use those to correctly calculate confidence limits]
         # gamma model
         preds.g <- mgcv:::predict.gam(object = fit.g1, 
                                       newdata = ndg,
                                       type = 'link', 
                                       se.fit = TRUE) # can't predict on the response scale if I'm going to +/- SE to get CI [it WILL give SE on the response scale, but I can't use those to correctly calculate confidence limits]
         
         # Get the predictions on the link scale
         z_link_b     <- preds.b$fit
         z_link_lcl_b <- preds.b$fit - 2 * preds.b$se.fit
         z_link_ucl_b <- preds.b$fit + 2 * preds.b$se.fit
         
         z_link_g     <- preds.g$fit
         z_link_lcl_g <- preds.g$fit - 2 * preds.g$se.fit
         z_link_ucl_g <- preds.g$fit + 2 * preds.g$se.fit
         
         
         # convert to response scale
         ndb$bycatch     <- fit.b1$family$linkinv(z_link_b)
         ndb$bycatch_lcl <- fit.b1$family$linkinv(z_link_lcl_b)
         ndb$bycatch_ucl <- fit.b1$family$linkinv(z_link_ucl_b)
         ndb$model <- mn
         ndb$covariate <- covar.1[c]
         
         # convert to response scale
         ndg$bycatch     <- fit.g1$family$linkinv(z_link_g)
         ndg$bycatch_lcl <- fit.g1$family$linkinv(z_link_lcl_g)
         ndg$bycatch_ucl <- fit.g1$family$linkinv(z_link_ucl_g)
         ndg$model <- mn
         ndg$covariate <- covar.1[c]
         
         # save the dataframe
         marginal_effect_list_b[[tolower(mn)]][[covar.1[c]]] <- ndb
         marginal_effect_list_g[[tolower(mn)]][[covar.1[c]]] <- ndg
      }
   }
}

##### 2. Fit GLM 2 (polynomial predictors) #####
{
   # use the processed covariates for this model
   covar.2 <- c(covar_processed)
   
   # fit the binomial model
   fit.b2 <- fit_m1_glm(data = bdat, 
                        covariates  = covar.2, 
                        modelfamily = 'binomial', 
                        rows.train  = 1:nrow(bdat),
                        rows.test   = NULL, 
                        save.model.fit = TRUE)$model
   # summary(fit.b2)
   
   # fit the gamma model
   fit.g2 <- fit_m1_glm(data = gdat, 
                        covariates  = covar.2, 
                        modelfamily = 'gamma', 
                        rows.train  = 1:nrow(gdat),
                        rows.test   = NULL, 
                        save.model.fit = TRUE)$model
   # summary(fit.g2)
   
   # binomial plot predictions
   {
      predictions.b2.response <- mgcv::predict.gam(fit.b2, 
                                                   newdata = pdat[,covar.2],
                                                   type = 'response', 
                                                   se.fit = T)
      # make predictions using grid2 prediction dataset
      predictions.b2.response2 <- mgcv::predict.gam(fit.b2, 
                                                    newdata = pdat2[,covar.2],
                                                    type = 'response', 
                                                    se.fit = T)
      
      pdat$z.pred.b2    <- predictions.b2.response$fit    
      pdat$z.pred.b2.sd <- predictions.b2.response$se.fit
      pdat2$z.pred.b2    <- predictions.b2.response2$fit    
      pdat2$z.pred.b2.sd <- predictions.b2.response2$se.fit
      
      # model name for plot legends
      mn <- 'GLM 2'
      
      # plot model predictions
      (plot.b2.mean <- plot_covariates_gg(data = pdat %>% 
                                             dplyr::mutate(lat = lat_unproj,
                                                           lon = lon_unproj), 
                                          field = 'z.pred.b2', 
                                          legend.title = paste0(mn, '\nPredicted\nProbability\nof Bycatch'),
                                          colormap = 'viridis',
                                          legend.color.range = plot.mean.range.b))
      ggsave(filename = file.path(base_folder, paste0('binomial_model_predictions_b2.png')), plot = plot.b2.mean, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.b2.mean)
      
      # plot SD of model predictions
      (plot.b2.sd <- plot_covariates_gg(data = pdat %>% 
                                           dplyr::mutate(lat = lat_unproj,
                                                         lon = lon_unproj), 
                                        field = 'z.pred.b2.sd', 
                                        legend.title = paste0(mn, '\nSD of\nPredicted\nBycatch\nProbability'),
                                        legend.color.range = plot.sd.range.b,
                                        colormap = 'magma'))
      ggsave(filename = file.path(base_folder, paste0('binomial_model_predictions_b2_sd.png')), plot = plot.b2.sd, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.b2.sd)
      
      # interactive plot
      if(FALSE){
         # first convert the data.frame to a raster
         b2.mean.r <- raster::rasterFromXYZ(xyz = pdat  %>% 
                                               dplyr::mutate(lat = lat_unproj,
                                                             lon = lon_unproj) %>% 
                                               dplyr::select(lon, lat, z.pred.b2),    
                                            crs = sp::CRS('+init=EPSG:4326'))
         b2.sd.r   <- raster::rasterFromXYZ(xyz = pdat  %>% 
                                               dplyr::mutate(lat = lat_unproj,
                                                             lon = lon_unproj) %>% 
                                               dplyr::select(lon, lat, z.pred.b2.sd),
                                            crs = sp::CRS('+init=EPSG:4326'))
         
         # leaflet uses epgs:3857, so I need to project to that first 
         # (leaflet will do it automatically, but it can screw up the color palate, so I'll do it first)
         # to get rid of errors see: https://github.com/rstudio/leaflet/issues/224
         b2.mean.rl <- leaflet::projectRasterForLeaflet(b2.mean.r, method = 'bilinear')
         b2.sd.rl   <- leaflet::projectRasterForLeaflet(b2.sd.r, method = 'bilinear')
         # the warnings actually come from raster package: raster::projectRaster(from = haul_count_r, crs = sp::CRS('+init=EPSG:3857'))
         
         # first create a color palatte function
         pal.b2m <- leaflet::colorNumeric(palette  = 'viridis', 
                                          domain   = range(na.omit(raster::values(b2.mean.rl))), 
                                          na.color = "transparent")
         pal.b2sd <- leaflet::colorNumeric(palette  = 'magma', 
                                           domain   = range(na.omit(raster::values(b2.sd.rl))), 
                                           na.color = "transparent")
         
         # then create the plot
         plot_height_pix <- 900
         lf.b2m <- leaflet::leaflet(height = plot_height_pix) %>% 
            leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>% 
            leaflet::addRasterImage(x = b2.mean.rl, 
                                    colors = pal.b2m,
                                    project = FALSE, 
                                    opacity = 0.9,
                                    group = "Bycatch",
                                    layerId = "Bycatch") %>%
            leafem::addImageQuery(x = b2.mean.rl, 
                                  digits = 4,
                                  position = 'topleft',
                                  project = TRUE,
                                  layerId = "Bycatch") %>%
            leafem::addMouseCoordinates() %>%
            leaflet::addLegend(position = 'topright', 
                               title = paste0(mn, '</br>Predicted</br>Bycatch</br>Probability'), # need HTML coding for line breaks </br> , not R \n
                               pal = pal.b2m, 
                               values = raster::values(b2.mean.rl))
         # lf.b2m
         ## save to standalone .html
         save_leaflet(plot = lf.b2m, 
                      file = file.path(base_folder, 'leaflet', 'binomial_model_predictions_b2_mean.html'))
         
         lf.b2sd <- leaflet::leaflet(height = plot_height_pix) %>% 
            leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>% 
            leaflet::addRasterImage(x = b2.sd.rl, 
                                    colors = pal.b2sd,
                                    project = FALSE, 
                                    opacity = 0.9,
                                    group = "Bycatch_SD",
                                    layerId = "Bycatch_SD") %>%
            leafem::addImageQuery(x = b2.sd.rl, 
                                  digits = 6,
                                  position = 'topleft',
                                  project = TRUE,
                                  layerId = "Bycatch_SD") %>%
            leafem::addMouseCoordinates() %>%
            leaflet::addLegend(position = 'topright', 
                               title = paste0(mn, '</br>SD of</br>Predicted</br>Bycatch</br>Probability'), # need HTML coding for line breaks </br> , not R \n
                               pal = pal.b2sd, 
                               values = raster::values(b2.sd.rl))
         # lf.b2sd
         save_leaflet(plot = lf.b2sd, 
                      file = file.path(base_folder, 'leaflet', 'binomial_model_predictions_b2_sd.html'))
         
         # mean and SD together
         # leafsync::sync(lf.b2m, lf.b2sd)
         # save_leafsync(tags = leafsync::sync(lf.b2m, lf.b2sd), 
         #               file = paste0(getwd(), '/figures/leaflet/binomial_model_predictions_b2_mean_and_sd_', short_long, '_',
         #                             ifelse(use_projected, 'projected', 'unprojected'), '.html'))
      }
   }
   
   # gamma model predictions
   {
      predictions.g2.response <- mgcv::predict.gam(fit.g2, 
                                                   newdata = pdat[,covar.2],
                                                   type = 'response', 
                                                   se.fit = T)
      # make predictions using grid2 prediction dataset
      predictions.g2.response2 <- mgcv::predict.gam(fit.g2, 
                                                    newdata = pdat2[,covar.2],
                                                    type = 'response', 
                                                    se.fit = T)
      
      pdat$z.pred.g2         <- predictions.g2.response$fit    # fit.g2$family$linkinv(predictions.b.glm.link$fit) is the same as:         predict(fit.g2, newdata = pdat[,c('YEAR.f', covar)], type = 'response')
      pdat$z.pred.g2.sd      <- predictions.g2.response$se.fit # fit.g2$family$linkinv(predictions.b.glm.link$se.fit) is NOT THE SAME as:  predict(fit.g2, newdata = pdat[,c('YEAR.f', covar)], type = 'response', se.fit=T)$se.fit
      
      # 2nd prediction dataset
      pdat2$z.pred.g2         <- predictions.g2.response2$fit 
      pdat2$z.pred.g2.sd      <- predictions.g2.response2$se.fit
      
      # plot model predictions
      (plot.g2.mean <- plot_covariates_gg(data = pdat %>%
                                             dplyr::mutate(lat = lat_unproj,
                                                           lon = lon_unproj),
                                          field = 'z.pred.g2',
                                          legend.title = paste0(mn, '\nPredicted\nNumber\nof Bycatch'),
                                          colormap = 'viridis',
                                          legend.color.range = plot.mean.range.g, 
                                          transformation = 'log1p', 
                                          legend.scale.breaks = plot.mean.legend.scale.breaks.g))
      ggsave(filename = file.path(base_folder, paste0('gamma_model_predictions_g2.png')), plot = plot.g2.mean, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.g2.mean)
      
      # plot SD of model predictions
      (plot.g2.sd <- plot_covariates_gg(data = pdat %>%
                                           dplyr::mutate(lat = lat_unproj,
                                                         lon = lon_unproj),
                                        field = 'z.pred.g2.sd',
                                        legend.title = paste0(mn, '\nSD of\nPredicted\nBycatch'), 
                                        legend.color.range = plot.sd.range.g,
                                        transformation = 'log1p', 
                                        legend.scale.breaks = plot.sd.legend.scale.breaks.g,
                                        colormap = 'magma'))
      ggsave(filename = file.path(base_folder, paste0('gamma_model_predictions_g2_sd.png')), plot = plot.g2.sd, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.g2.sd)
      
      # interactive plot
      if(FALSE){
         # first convert the data.frame to a raster
         g2.mean.r <- raster::rasterFromXYZ(xyz = pdat  %>% 
                                               dplyr::mutate(lat = lat_unproj,
                                                             lon = lon_unproj) %>% 
                                               dplyr::rowwise() %>% 
                                               dplyr::mutate(z.pred.g2 = ifelse(z.pred.g2 > plot.mean.range.g[2], plot.mean.range.g[2], z.pred.g2)) %>% 
                                               dplyr::ungroup() %>% 
                                               dplyr::select(lon, lat, z.pred.g2),    
                                            crs = sp::CRS('+init=EPSG:4326'))
         g2.sd.r   <- raster::rasterFromXYZ(xyz = pdat  %>% 
                                               dplyr::mutate(lat = lat_unproj,
                                                             lon = lon_unproj) %>% 
                                               dplyr::rowwise() %>% 
                                               dplyr::mutate(z.pred.g2.sd = ifelse(z.pred.g2.sd > plot.sd.range.g[2], plot.sd.range.g[2], z.pred.g2.sd)) %>% 
                                               dplyr::ungroup() %>% 
                                               dplyr::select(lon, lat, z.pred.g2.sd),
                                            crs = sp::CRS('+init=EPSG:4326'))
         
         # leaflet uses epgs:3857, so I need to project to that first 
         # (leaflet will do it automatically, but it can screw up the color palate, so I'll do it first)
         # to get rid of errors see: https://github.com/rstudio/leaflet/issues/224
         g2.mean.rl <- leaflet::projectRasterForLeaflet(g2.mean.r, method = 'bilinear')
         g2.sd.rl   <- leaflet::projectRasterForLeaflet(g2.sd.r, method = 'bilinear')
         # the warnings actually come from raster package: raster::projectRaster(from = haul_count_r, crs = sp::CRS('+init=EPSG:3857'))
         
         # first create a color palatte function
         pal.g2m <- leaflet::colorNumeric(palette  = 'viridis', 
                                          domain   = range(na.omit(raster::values(g2.mean.rl))), 
                                          na.color = "transparent")
         pal.g2sd <- leaflet::colorNumeric(palette  = 'magma', 
                                           domain   = range(na.omit(raster::values(g2.sd.rl))), 
                                           na.color = "transparent")
         
         # then create the plot
         plot_height_pix <- 900
         lf.g2m <- leaflet::leaflet(height = plot_height_pix) %>% 
            leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>% 
            leaflet::addRasterImage(x = g2.mean.rl, 
                                    colors = pal.g2m,
                                    project = FALSE, 
                                    opacity = 0.9,
                                    group = "Bycatch",
                                    layerId = "Bycatch") %>%
            leafem::addImageQuery(x = g2.mean.rl, 
                                  digits = 4,
                                  position = 'topleft',
                                  project = TRUE,
                                  layerId = "Bycatch") %>%
            leafem::addMouseCoordinates() %>%
            leaflet::addLegend(position = 'topright', 
                               title =  paste0(mn, '</br>Predicted</br>Bycatch'), # need HTML coding for line breaks </br> , not R \n
                               pal = pal.g2m, 
                               values = raster::values(g2.mean.rl))
         # lf.g2m
         ## save to standalone .html
         save_leaflet(plot = lf.g2m, 
                      file = file.path(base_folder, 'leaflet', 'gamma_model_predictions_g2_mean.html'))
         
         lf.g2sd <- leaflet::leaflet(height = plot_height_pix) %>% 
            leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>% 
            leaflet::addRasterImage(x = g2.sd.rl, 
                                    colors = pal.g2sd,
                                    project = FALSE, 
                                    opacity = 0.9,
                                    group = "Bycatch_SD",
                                    layerId = "Bycatch_SD") %>%
            leafem::addImageQuery(x = g2.sd.rl, 
                                  digits = 6,
                                  position = 'topleft',
                                  project = TRUE,
                                  layerId = "Bycatch_SD") %>%
            leafem::addMouseCoordinates() %>%
            leaflet::addLegend(position = 'topright', 
                               title = paste0(mn, '</br>SD of</br>Predicted</br>Bycatch'), # need HTML coding for line breaks </br> , not R \n
                               pal = pal.g2sd, 
                               values = raster::values(g2.sd.rl))
         # lf.g2sd
         save_leaflet(plot = lf.g2sd, 
                      file = file.path(base_folder, 'leaflet', 'gamma_model_predictions_g2_sd.html'))
         
         # mean and SD together
         # leafsync::sync(lf.g2m, lf.g2sd)
         # save_leafsync(tags = leafsync::sync(lf.g2m, lf.g2sd), 
         #               file = paste0(getwd(), '/figures/leaflet/gamma_model_predictions_g2_mean_and_sd_', short_long, '_',
         #                             ifelse(use_projected, 'projected', 'unprojected'), '.html'))
      }
   }
   
   # combined hurdle model predictions
   {
      pdat <- pdat %>% 
         dplyr::mutate(z.pred.h2 = z.pred.b2 * z.pred.g2)
      pdat2 <- pdat2 %>% 
         dplyr::mutate(z.pred.h2 = z.pred.b2 * z.pred.g2)
      
      # plot model predictions
      (plot.h2.mean <- plot_covariates_gg(data = pdat %>%
                                             dplyr::mutate(lat = lat_unproj,
                                                           lon = lon_unproj),
                                          field = 'z.pred.h2',
                                          legend.title = paste0(mn, '\nPredicted\nNumber\nof Bycatch'),
                                          colormap = 'viridis',
                                          legend.color.range = plot.mean.range.h, 
                                          transformation = 'log1p', 
                                          legend.scale.breaks = plot.mean.legend.scale.breaks.h))
      ggsave(filename = file.path(base_folder, paste0('hurdle_model_predictions_h2.png')), plot = plot.h2.mean, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.h2.mean)
      
      # plot of spatial effect through time
      # just plot 1 year b/c the spatial component is the same every year. 
      plot.h2.space <- plot_covariates_gg(data = pdat2 %>%
                                             dplyr::mutate(lat = lat_unproj,
                                                           lon = lon_unproj) %>% 
                                             dplyr::filter(year_f == '2014'),
                                          field = 'z.pred.h2',
                                          legend.title = paste0(mn, '\nPredicted\nNumber\nof Bycatch'),
                                          colormap = 'viridis',
                                          legend.color.range = plot.mean.range.h, 
                                          transformation = 'log1p', 
                                          legend.scale.breaks = plot.mean.legend.scale.breaks.h,
                                          facet_field = 'year_f', 
                                          facet_rows = 3)
      ggsave(filename = file.path(base_folder, paste0('m2_spatial_predictions.png')), plot = plot.h2.space, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.h2.space)
   }
   
   # Datasets for marginal effects plots of fixed effects
   {
      # This model includes quadratic terms, which don't get their own plot. 
      # They're incorporated in with the linear component of the same covariate.
      # so first I'll get the names of the covariates excluding the quadratic terms
      c.2 <- covar.2[ !grepl(pattern = '.+_poly2', covar.2)]
      # I'll add a space in the list for each of them
      marginal_effect_list_b[[tolower(mn)]] <- vector('list', length(c.2))
      names(marginal_effect_list_b[[tolower(mn)]]) <- c.2
      marginal_effect_list_g[[tolower(mn)]] <- vector('list', length(c.2))
      names(marginal_effect_list_g[[tolower(mn)]]) <- c.2
      
      for(c in 1:length(c.2)){
         # I can also skip any covariates that end with "poly2" because polynomial predictors need to be considered together
         if( grepl(pattern = '.+_poly2$', x = c.2[c]) ) next
         
         # get the dataframe for the model predictions
         ndb <- ndg <- prediction_dfs[[c.2[c]]]
         
         # predict new values
         preds.b <- mgcv:::predict.gam(object = fit.b2, 
                                       newdata = ndb,
                                       type = 'link', 
                                       se.fit = TRUE) # can't predict on the response scale if I'm going to +/- SE to get CI [it WILL give SE on the response scale, but I can't use those to correctly calculate confidence limits]
         # gamma model
         preds.g <- mgcv:::predict.gam(object = fit.g2, 
                                       newdata = ndg,
                                       type = 'link', 
                                       se.fit = TRUE) # can't predict on the response scale if I'm going to +/- SE to get CI [it WILL give SE on the response scale, but I can't use those to correctly calculate confidence limits]
         
         # Get the predictions on the link scale
         z_link_b     <- preds.b$fit
         z_link_lcl_b <- preds.b$fit - 2 * preds.b$se.fit
         z_link_ucl_b <- preds.b$fit + 2 * preds.b$se.fit
         
         z_link_g     <- preds.g$fit
         z_link_lcl_g <- preds.g$fit - 2 * preds.g$se.fit
         z_link_ucl_g <- preds.g$fit + 2 * preds.g$se.fit
         
         
         # convert to response scale
         ndb$bycatch     <- fit.b2$family$linkinv(z_link_b)
         ndb$bycatch_lcl <- fit.b2$family$linkinv(z_link_lcl_b)
         ndb$bycatch_ucl <- fit.b2$family$linkinv(z_link_ucl_b)
         ndb$model <- mn
         
         # convert to response scale
         ndg$bycatch     <- fit.g2$family$linkinv(z_link_g)
         ndg$bycatch_lcl <- fit.g2$family$linkinv(z_link_lcl_g)
         ndg$bycatch_ucl <- fit.g2$family$linkinv(z_link_ucl_g)
         ndg$model <- mn
         
         # get the name of the covariate (but not "poly"). It would've been easier to just use "sub()"...
         cv <- ifelse(grepl('.+_poly1', c.2[c] ), 
                      lsp_lut[ lsp_lut$final_name == c.2[c], 'original_name' ], 
                      c.2[c])
         ndb$covariate <- ndg$covariate <- cv
            
         
         # save the dataframe
         marginal_effect_list_b[[tolower(mn)]][[cv]] <- ndb
         marginal_effect_list_g[[tolower(mn)]][[cv]] <- ndg
      }
   }
}

##### 3. Fit GAM 1 (linear covariates with spatial smooth) #####
{
   # covariates for model fitting
   covar.3 <- covar
   # covariates needed to use the model fit to make predictions
   c.3 <- c(covar.3, 'lat', 'lon')
   
   # fit binomial model
   fit.b3 <- fit_m2_gam1(data = bdat, # needs to have columns named 'lat' and 'lon'
                         covariates  = covar.3, 
                         modelfamily = 'binomial', 
                         rows.train  = 1:nrow(bdat),
                         rows.test   = NULL, 
                         save.model.fit = TRUE)$model
   # summary(fit.b3)
   
   # fit gamma model
   fit.g3 <- fit_m2_gam1(data = gdat, # needs to have columns named 'lat' and 'lon'
                         covariates  = covar.3, 
                         modelfamily = 'gamma', 
                         rows.train  = 1:nrow(gdat),
                         rows.test   = NULL, 
                         save.model.fit = TRUE)$model
   # summary(fit.g3)
   
  
   # Can plot just the spatial smooths using mgcv package, but I'll do it manually 
   # below and include the effect of year to make things more comparable with tree
   # based models
   if(FALSE){
      # using the mgcviz package
      b3.plot.smooth.mgcViz_proj0 <- plot(mgcViz::sm(o = Viz.b3, select = 1)) +
         mgcViz::l_fitRaster() +
         # mgcViz::l_fitContour() +
         mgcViz::l_points() + coord_equal()

      # un-project the data to lat/lon for better plotting
      b3ps.dat_proj <- b3.plot.smooth.mgcViz_proj0$data$fit %>%
         sf::st_as_sf(coords = c('x', 'y'),
                      crs = proj_crs,
                      remove = F) %>%
         mutate(lon_proj = x,
                lat_proj = y)
      b3ps.dat <- b3ps.dat_proj %>%
         sf::st_transform(crs = 4326)
      b3ps.dat$lon <- as.vector(sf::st_coordinates(b3ps.dat)[,1])
      b3ps.dat$lat <- as.vector(sf::st_coordinates(b3ps.dat)[,2])

      # instead I'll just rotate the graph and do a lazy plotting hack
      b3ps.dat_proj_rot <- maptools::elide(sf::as_Spatial(b3ps.dat_proj),
                                           rotate = -90) %>%
         sf::st_as_sf()
      b3ps.dat_proj_rot$lon_proj_rot <- sf::st_coordinates(b3ps.dat_proj_rot)[,1]
      b3ps.dat_proj_rot$lat_proj_rot <- sf::st_coordinates(b3ps.dat_proj_rot)[,2]

      (b3.plot.smooth.mgcViz_proj <-  ggplot(data = na.omit(b3ps.dat_proj_rot)) +
            theme_classic() +
            geom_point(aes(x = lon_proj_rot,
                           y = lat_proj_rot,
                           color = z),
                       size = 5,
                       shape = 15) +
            scale_color_gradient2(mid = '#fffdde',
                                  midpoint = 0,
                                  na.value = 'white')+
            ggtitle(label = '') +
            xlab('') +
            ylab('') +
            labs(color = "Spatial\nSmooth\nEffect Size\n(logit scale)")+
            theme(axis.text = element_blank(), axis.ticks = element_blank()))
      (b3.plot.smooth.mgcViz_proj.sd <-  ggplot(data = (b3ps.dat_proj_rot)) +
            theme_classic() +
            geom_point(aes(x = lon_proj_rot,
                           y = lat_proj_rot,
                           color = se),
                       size = 5,
                       shape = 15) +
            scale_color_gradient2(mid = '#fffdde',
                                  midpoint = 0,
                                  na.value = 'white')+
            ggtitle(label = '') +
            xlab('') +
            ylab('') +
            labs(color = "SE Spatial\nSmooth\nEffect Size\n(logit scale)")+
            theme(axis.text = element_blank(), axis.ticks = element_blank()))

      # plot the field using my plot_covariates_gg() function
      # heatmap
      hmpd <- plot(fit.b3, scheme = 2)
      # convert to a list of dfs instead of a list of lists
      hmpd_2 <- list()
      lis <- hmpd[[1]]
      grid = expand.grid(lon = lis$x, # I think I'll need to un-project this before plotting...
                         lat = lis$y) # I think I'll need to un-project this before plotting...
      grid$mean  = lis$fit
      grid$se    = lis$se
      grid$include = !lis$exclude

      hmpd_2[[1]] <- grid
      smooths.b3_proj <- dplyr::bind_rows(hmpd_2) %>%  # .id = "column_label" can be used to add a column with the list name
         sf::st_as_sf(coords = c('lon', 'lat'),
                      crs = proj_crs,
                      remove = F)
      # un-project it to lat/lon
      smooths.b3 <- smooths.b3_proj %>%
         sf::st_as_sf(crs = 4326)

      # rotate the smooth for a hack to plot it in a more pleasant manner
      smooths.b3_proj_rot <- maptools::elide(sf::as_Spatial(smooths.b3_proj),
                                             rotate = -90,
                                             center = c(mean(range(smooths.b3_proj$lon, na.rm = T)),
                                                        mean(range(smooths.b3_proj$lat, na.rm = T)))) %>%
         sf::st_as_sf()
      smooths.b3_proj_rot$lon_proj_rot <- sf::st_coordinates(smooths.b3_proj_rot)[,1]
      smooths.b3_proj_rot$lat_proj_rot <- sf::st_coordinates(smooths.b3_proj_rot)[,2]

      states_proj_rot <- states_proj %>%
         sf::st_crop(y = sf::st_bbox(smooths.b3_proj)) %>%
         sf::as_Spatial(.) %>%
         maptools::elide(rotate = -90,
                         center = c(mean(range(smooths.b3_proj$lon, na.rm = T)),
                                    mean(range(smooths.b3_proj$lat, na.rm = T)))) %>%
         sf::st_as_sf()

      # plot the smooths
      (b3.plot.smooth.mean <- ggplot(data = na.omit(smooths.b3_proj_rot)) +
            theme_classic() +
            geom_point(aes(x = lon_proj_rot,
                           y = lat_proj_rot,
                           color = mean),
                       size = 5,
                       shape = 15) +
            # geom_sf(data = states_proj_rot) +
            scale_color_viridis_c(option = 'viridis') +
            # scale_color_gradient2(mid = '#fffdde',
            #                       midpoint = 0,
            #                       na.value = 'white')+
            ggtitle(label = '') +
            xlab('') +
            ylab('') +
            coord_equal()+
            labs(color = "Spatial\nSmooth\nEffect Size\n(logit scale)")+
            theme(axis.text = element_blank(), axis.ticks = element_blank()))

      (b3.plot.smooth.sd <- ggplot(data = na.omit(smooths.b3_proj_rot)) +
            theme_classic() +
            geom_point(aes(x = lon_proj_rot,
                           y = lat_proj_rot,
                           color = se),
                       size = 5,
                       shape = 15) +
            scale_color_viridis_c(option = 'magma')+
            ggtitle(label = '') +
            xlab('') +
            ylab('') +
            coord_equal()+
            labs(color = "SE Spatial\nSmooth\nEffect Size\n(logit scale)")+
            theme(axis.text = element_blank(), axis.ticks = element_blank()))
   }
   
   # binomial model predictions
   {
      # make predictions using grid1 prediction dataset
      predictions.b3.response <- mgcv::predict.gam(fit.b3, 
                                                   newdata = pdat[,c.3],
                                                   type = 'response', 
                                                   se.fit = T)
      # make predictions using grid2 prediction dataset
      predictions.b3.response2 <- mgcv::predict.gam(fit.b3, 
                                                   newdata = pdat2[,c.3],
                                                   type = 'response', 
                                                   se.fit = T)
      
      # add the predicted values into the pdat dataset
      pdat$z.pred.b3    <- predictions.b3.response$fit    
      pdat$z.pred.b3.sd <- predictions.b3.response$se.fit
      # 2nd prediction dataset
      pdat2$z.pred.b3    <- predictions.b3.response2$fit    
      pdat2$z.pred.b3.sd <- predictions.b3.response2$se.fit
      
      # model name for plot legends
      mn <- 'GAM 1'
      
      # plot model predictions
      (plot.b3.mean <- plot_covariates_gg(data = pdat %>% 
                                             dplyr::mutate(lat = lat_unproj,
                                                           lon = lon_unproj), 
                                          field = 'z.pred.b3', 
                                          legend.title = paste0(mn, '\nPredicted\nProbability\nof Bycatch'),
                                          colormap = 'viridis',
                                          legend.color.range = plot.mean.range.b))
      ggsave(filename = file.path(base_folder, paste0('binomial_model_predictions_b3.png')), plot = plot.b3.mean, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.b3.mean)
      
      # plot SD of model predictions
      (plot.b3.sd <- plot_covariates_gg(data = pdat %>% 
                                           dplyr::mutate(lat = lat_unproj,
                                                         lon = lon_unproj), 
                                        field = 'z.pred.b3.sd', 
                                        legend.title = paste0(mn, '\nSD of\nPredicted\nBycatch\nProbability'), 
                                        legend.color.range = plot.sd.range.b,
                                        colormap = 'magma'))
      ggsave(filename = file.path(base_folder, paste0('binomial_model_predictions_b3_sd.png')), plot = plot.b3.sd, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.b3.sd)
      
      # interactive plot
      if(FALSE){
         # first convert the data.frame to a raster
         b3.mean.r <- raster::rasterFromXYZ(xyz = pdat  %>% 
                                               dplyr::mutate(lat = lat_unproj,
                                                             lon = lon_unproj) %>% 
                                               dplyr::select(lon, lat, z.pred.b3),    
                                            crs = sp::CRS('+init=EPSG:4326'))
         b3.sd.r   <- raster::rasterFromXYZ(xyz = pdat  %>% 
                                               dplyr::mutate(lat = lat_unproj,
                                                             lon = lon_unproj) %>% 
                                               dplyr::select(lon, lat, z.pred.b3.sd),
                                            crs = sp::CRS('+init=EPSG:4326'))
         
         # leaflet uses epgs:3857, so I need to project to that first 
         # (leaflet will do it automatically, but it can screw up the color palate, so I'll do it first)
         # to get rid of errors see: https://github.com/rstudio/leaflet/issues/224
         b3.mean.rl <- leaflet::projectRasterForLeaflet(b3.mean.r, method = 'bilinear')
         b3.sd.rl   <- leaflet::projectRasterForLeaflet(b3.sd.r, method = 'bilinear')
         # the warnings actually come from raster package: raster::projectRaster(from = haul_count_r, crs = sp::CRS('+init=EPSG:3857'))
         
         # first create a color palatte function
         pal.b3m <- leaflet::colorNumeric(palette  = 'viridis', 
                                          domain   = range(na.omit(raster::values(b3.mean.rl))), 
                                          na.color = "transparent")
         pal.b3sd <- leaflet::colorNumeric(palette  = 'magma', 
                                           domain   = range(na.omit(raster::values(b3.sd.rl))), 
                                           na.color = "transparent")
         
         # then create the plot
         plot_height_pix <- 900
         lf.b3m <- leaflet::leaflet(height = plot_height_pix) %>% 
            leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>% 
            leaflet::addRasterImage(x = b3.mean.rl, 
                                    colors = pal.b3m,
                                    project = FALSE, 
                                    opacity = 0.9,
                                    group = "Bycatch",
                                    layerId = "Bycatch") %>%
            leafem::addImageQuery(x = b3.mean.rl, 
                                  digits = 4,
                                  position = 'topleft',
                                  project = TRUE,
                                  layerId = "Bycatch") %>%
            leafem::addMouseCoordinates() %>%
            leaflet::addLegend(position = 'topright', 
                               title = paste0(mn, '</br>Predicted</br>Bycatch</br>Probability'), # need HTML coding for line breaks </br> , not R \n
                               pal = pal.b3m, 
                               values = raster::values(b3.mean.rl))
         # lf.b3m
         ## save to standalone .html
         save_leaflet(plot = lf.b3m, 
                      file = file.path(base_folder, 'leaflet', 'binomial_model_predictions_b3_mean.html'))
         
         lf.b3sd <- leaflet::leaflet(height = plot_height_pix) %>% 
            leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>% 
            leaflet::addRasterImage(x = b3.sd.rl, 
                                    colors = pal.b3sd,
                                    project = FALSE, 
                                    opacity = 0.9,
                                    group = "Bycatch_SD",
                                    layerId = "Bycatch_SD") %>%
            leafem::addImageQuery(x = b3.sd.rl, 
                                  digits = 6,
                                  position = 'topleft',
                                  project = TRUE,
                                  layerId = "Bycatch_SD") %>%
            leafem::addMouseCoordinates() %>%
            leaflet::addLegend(position = 'topright', 
                               title = paste0(mn, '</br>SD of</br>Predicted</br>Bycatch</br>Probability'), # need HTML coding for line breaks </br> , not R \n
                               pal = pal.b3sd, 
                               values = raster::values(b3.sd.rl))
         # lf.b3sd
         save_leaflet(plot = lf.b3sd, 
                      file = file.path(base_folder, 'leaflet', 'binomial_model_predictions_b3_sd.html'))
         
         # mean and SD together
         # leafsync::sync(lf.b3m, lf.b3sd)
         # save_leafsync(tags = leafsync::sync(lf.b3m, lf.b3sd), 
         #               file = paste0(getwd(), '/figures/leaflet/binomial_model_predictions_b3_mean_and_sd_', short_long, '_',
         #                             ifelse(use_projected, 'projected', 'unprojected'), '.html'))
      }
   }
   
   # gamma model predictions
   {
      # make predictions using grid1 prediction dataset
      predictions.g3.response <- mgcv::predict.gam(fit.g3, 
                                                   newdata = pdat[,c.3],
                                                   type = 'response', 
                                                   se.fit = T)
      # make predictions using grid1 prediction dataset
      predictions.g3.response2 <- mgcv::predict.gam(fit.g3, 
                                                    newdata = pdat2[,c.3],
                                                    type = 'response', 
                                                    se.fit = T)
      
      # add the predicted values into the pdat dataset
      pdat$z.pred.g3    <- predictions.g3.response$fit    
      pdat$z.pred.g3.sd <- predictions.g3.response$se.fit
      # 2nd prediction dataset
      pdat2$z.pred.g3    <- predictions.g3.response2$fit    
      pdat2$z.pred.g3.sd <- predictions.g3.response2$se.fit
      
      # plot model predictions
      (plot.g3.mean <- plot_covariates_gg(data = pdat %>% 
                                             dplyr::mutate(lat = lat_unproj,
                                                           lon = lon_unproj), 
                                          field = 'z.pred.g3', 
                                          legend.title = paste0(mn, '\nPredicted\nNumber\nof Bycatch'),
                                          colormap = 'viridis',
                                          legend.color.range = plot.mean.range.g, 
                                          transformation = 'log1p', 
                                          legend.scale.breaks = plot.mean.legend.scale.breaks.g))
      ggsave(filename = file.path(base_folder, paste0('gamma_model_predictions_g3.png')), plot = plot.g3.mean, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.g3.mean)
      
      # plot SD of model predictions
      (plot.g3.sd <- plot_covariates_gg(data = pdat %>% 
                                           dplyr::mutate(lat = lat_unproj,
                                                         lon = lon_unproj), 
                                        field = 'z.pred.g3.sd', 
                                        legend.title = paste0(mn, '\nSD of\nPredicted\nBycatch'), 
                                        legend.color.range = plot.sd.range.g,
                                        transformation = 'log1p', 
                                        legend.scale.breaks = plot.sd.legend.scale.breaks.g,
                                        colormap = 'magma'))
      ggsave(filename = file.path(base_folder, paste0('gamma_model_predictions_g3_sd.png')), plot = plot.g3.sd, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.g3.sd)
      
      # interactive plot
      if(FALSE){
         # first convert the data.frame to a raster
         g3.mean.r <- raster::rasterFromXYZ(xyz = pdat  %>% 
                                               dplyr::mutate(lat = lat_unproj,
                                                             lon = lon_unproj) %>% 
                                               dplyr::rowwise() %>% 
                                               dplyr::mutate(z.pred.g3 = ifelse(z.pred.g3 > plot.mean.range.g[2], plot.mean.range.g[2], z.pred.g3)) %>% 
                                               dplyr::ungroup() %>% 
                                               dplyr::select(lon, lat, z.pred.g3),    
                                            crs = sp::CRS('+init=EPSG:4326'))
         g3.sd.r   <- raster::rasterFromXYZ(xyz = pdat  %>% 
                                               dplyr::mutate(lat = lat_unproj,
                                                             lon = lon_unproj) %>% 
                                               dplyr::rowwise() %>% 
                                               dplyr::mutate(z.pred.g3.sd = ifelse(z.pred.g3.sd > plot.sd.range.g[2], plot.sd.range.g[2], z.pred.g3.sd)) %>% 
                                               dplyr::ungroup() %>% 
                                               dplyr::select(lon, lat, z.pred.g3.sd),
                                            crs = sp::CRS('+init=EPSG:4326'))
         
         # leaflet uses epgs:3857, so I need to project to that first 
         # (leaflet will do it automatically, but it can screw up the color palate, so I'll do it first)
         # to get rid of errors see: https://github.com/rstudio/leaflet/issues/224
         g3.mean.rl <- leaflet::projectRasterForLeaflet(g3.mean.r, method = 'bilinear')
         g3.sd.rl   <- leaflet::projectRasterForLeaflet(g3.sd.r, method = 'bilinear')
         # the warnings actually come from raster package: raster::projectRaster(from = haul_count_r, crs = sp::CRS('+init=EPSG:3857'))
         
         # first create a color palatte function
         pal.g3m <- leaflet::colorNumeric(palette  = 'viridis', 
                                          domain   =  range(na.omit(raster::values(g3.mean.rl))), 
                                          na.color = "transparent")
         pal.g3sd <- leaflet::colorNumeric(palette  = 'magma', 
                                           domain   = range(na.omit(raster::values(g3.sd.rl))), 
                                           na.color = "transparent")
         
         # then create the plot
         plot_height_pix <- 900
         lf.g3m <- leaflet::leaflet(height = plot_height_pix) %>% 
            leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>% 
            leaflet::addRasterImage(x = g3.mean.rl, 
                                    colors = pal.g3m,
                                    project = FALSE, 
                                    opacity = 0.9,
                                    group = "Bycatch",
                                    layerId = "Bycatch") %>%
            leafem::addImageQuery(x = g3.mean.rl, 
                                  digits = 4,
                                  position = 'topleft',
                                  project = TRUE,
                                  layerId = "Bycatch") %>%
            leafem::addMouseCoordinates() %>%
            leaflet::addLegend(position = 'topright', 
                               title = paste0(mn,'</br>Predicted</br>Bycatch'), # need HTML coding for line breaks </br> , not R \n
                               pal = pal.g3m, 
                               values = raster::values(g3.mean.rl))
         # lf.g3m
         ## save to standalone .html
         save_leaflet(plot = lf.g3m, 
                      file = file.path(base_folder, 'leaflet', 'gamma_model_predictions_g3_mean.html'))
         
         lf.g3sd <- leaflet::leaflet(height = plot_height_pix) %>% 
            leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>% 
            leaflet::addRasterImage(x = g3.sd.rl, 
                                    colors = pal.g3sd,
                                    project = FALSE, 
                                    opacity = 0.9,
                                    group = "Bycatch_SD",
                                    layerId = "Bycatch_SD") %>%
            leafem::addImageQuery(x = g3.sd.rl, 
                                  digits = 6,
                                  position = 'topleft',
                                  project = TRUE,
                                  layerId = "Bycatch_SD") %>%
            leafem::addMouseCoordinates() %>%
            leaflet::addLegend(position = 'topright', 
                               title = paste0(mn,'</br>SD of</br>Predicted</br>Bycatch'), # need HTML coding for line breaks </br> , not R \n
                               pal = pal.g3sd, 
                               values = raster::values(g3.sd.rl))
         # lf.g3sd
         save_leaflet(plot = lf.g3sd, 
                      file = file.path(base_folder, 'leaflet', 'gamma_model_predictions_g3_sd.html'))
         
         # mean and SD together
         # leafsync::sync(lf.g3m, lf.g3sd)
         # save_leafsync(tags = leafsync::sync(lf.g3m, lf.g3sd), 
         #               file = paste0(getwd(), '/figures/leaflet/gamma_model_predictions_g3_mean_and_sd_', short_long, '_',
         #                             ifelse(use_projected, 'projected', 'unprojected'), '.html'))
      }
   }
   
   # combined hurdle model predictions
   {
      pdat <- pdat %>% 
         dplyr::mutate(z.pred.h3 = z.pred.b3 * z.pred.g3)
      pdat2 <- pdat2 %>% 
         dplyr::mutate(z.pred.h3 = z.pred.b3 * z.pred.g3)
      
      # plot model predictions
      (plot.h3.mean <- plot_covariates_gg(data = pdat %>%
                                             dplyr::mutate(lat = lat_unproj,
                                                           lon = lon_unproj),
                                          field = 'z.pred.h3',
                                          legend.title = paste0(mn, '\nPredicted\nNumber\nof Bycatch'),
                                          colormap = 'viridis',
                                          legend.color.range = plot.mean.range.h, 
                                          transformation = 'log1p', 
                                          legend.scale.breaks = plot.mean.legend.scale.breaks.h))
      ggsave(filename = file.path(base_folder, paste0('hurdle_model_predictions_h3.png')), plot = plot.h3.mean, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.h3.mean)
      
      # plot of spatial effect through time
      # just plot 1 year b/c the spatial component is the same every year. 
      plot.h3.space <- plot_covariates_gg(data = pdat2 %>%
                                             dplyr::mutate(lat = lat_unproj,
                                                           lon = lon_unproj) %>% 
                                             dplyr::filter(year_f == '2014'),
                                          field = 'z.pred.h3',
                                          legend.title = paste0(mn, '\nPredicted\nNumber\nof Bycatch'),
                                          colormap = 'viridis',
                                          legend.color.range = plot.mean.range.h, 
                                          transformation = 'log1p', 
                                          legend.scale.breaks = plot.mean.legend.scale.breaks.h,
                                          facet_field = 'year_f', 
                                          facet_rows = 3)
      ggsave(filename = file.path(base_folder, paste0('m3_spatial_predictions.png')), plot = plot.h3.space, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.h3.space)
   }
   
   # Datasets for marginal effects plots of fixed effects
   {
      # add a space in the list for each of them
      marginal_effect_list_b[[tolower(mn)]] <- vector('list', length(c.3))
      names(marginal_effect_list_b[[tolower(mn)]]) <- c.3
      marginal_effect_list_g[[tolower(mn)]] <- vector('list', length(c.3))
      names(marginal_effect_list_g[[tolower(mn)]]) <- c.3
      
      
      for(c in 1:length(c.3)){
         # I can also skip any covariates that end with "poly2" because polynomial predictors need to be considered together
         if( grepl(pattern = '.+_poly2$', x = c.3[c]) ) next
         
         # get the dataframe for the model predictions
         ndb <- ndg <- prediction_dfs[[c.3[c]]]
         
         # predict new values
         preds.b <- mgcv:::predict.gam(object = fit.b3, 
                                       newdata = ndb,
                                       type = 'link', 
                                       se.fit = TRUE) # can't predict on the response scale if I'm going to +/- SE to get CI [it WILL give SE on the response scale, but I can't use those to correctly calculate confidence limits]
         # gamma model
         preds.g <- mgcv:::predict.gam(object = fit.g3, 
                                       newdata = ndg,
                                       type = 'link', 
                                       se.fit = TRUE) # can't predict on the response scale if I'm going to +/- SE to get CI [it WILL give SE on the response scale, but I can't use those to correctly calculate confidence limits]
         
         # Get the predictions on the link scale
         z_link_b     <- preds.b$fit
         z_link_lcl_b <- preds.b$fit - 2 * preds.b$se.fit
         z_link_ucl_b <- preds.b$fit + 2 * preds.b$se.fit
         
         z_link_g     <- preds.g$fit
         z_link_lcl_g <- preds.g$fit - 2 * preds.g$se.fit
         z_link_ucl_g <- preds.g$fit + 2 * preds.g$se.fit
         
         
         # convert to response scale
         ndb$bycatch     <- fit.b3$family$linkinv(z_link_b)
         ndb$bycatch_lcl <- fit.b3$family$linkinv(z_link_lcl_b)
         ndb$bycatch_ucl <- fit.b3$family$linkinv(z_link_ucl_b)
         ndb$model <- mn
         
         # convert to response scale
         ndg$bycatch     <- fit.g3$family$linkinv(z_link_g)
         ndg$bycatch_lcl <- fit.g3$family$linkinv(z_link_lcl_g)
         ndg$bycatch_ucl <- fit.g3$family$linkinv(z_link_ucl_g)
         ndg$model <- mn
         
         # get the name of the covariate (but not "poly"). It would've been easier to just use "sub()"...
         ndb$covariate <- 
            ndg$covariate <- 
            ifelse(grepl('.+_poly1', c.3[c] ), 
                   lsp_lut[ lsp_lut$final_name == c.3[c], 'original_name' ], 
                   c.3[c])
         
         # save the dataframe
         marginal_effect_list_b[[tolower(mn)]][[c.3[c]]] <- ndb
         marginal_effect_list_g[[tolower(mn)]][[c.3[c]]] <- ndg
      }
   }
}

##### 4. Fit GAM 2 (low-basis-dimension smooths of covariates & spatial smooth) #####
{
   # covariates for the model-fitting function
   covar.4 <- covar
   # covariates needed to use the model fit to make predictions
   c.4 <- c(covar.4, 'lat', 'lon')
   
   # fit the binomial model
   fit.b4 <- fit_m3_gam2(data = bdat, 
                         covariates  = covar.4, 
                         modelfamily = 'binomial', 
                         rows.train  = 1:nrow(bdat),
                         rows.test   = NULL, 
                         save.model.fit = TRUE)$model
   # summary(fit.b4)
   
   # fit the gamma model
   fit.g4 <- fit_m3_gam2(data = gdat, 
                         covariates  = covar.4, 
                         modelfamily = 'gamma', 
                         rows.train  = 1:nrow(gdat),
                         rows.test   = NULL, 
                         save.model.fit = TRUE)$model
   # summary(fit.g4)
   
   # Can plot just the spatial smooths using mgcv package, but I'll do it manually 
   # below and include the effect of year to make things more comparable with tree
   # based models
   if(FALSE){
      # using the mgcviz package
      b4.plot.smooth.mgcViz_proj0 <- plot(mgcViz::sm(o = Viz.b4, select = 1)) +
         mgcViz::l_fitRaster() +
         # mgcViz::l_fitContour() +
         mgcViz::l_points() + coord_equal()

      # un-project the data to lat/lon for better plotting
      b4ps.dat_proj <- b4.plot.smooth.mgcViz_proj0$data$fit %>%
         sf::st_as_sf(coords = c('x', 'y'),
                      crs = proj_crs,
                      remove = F) %>%
         mutate(lon_proj = x,
                lat_proj = y)
      b4ps.dat <- b4ps.dat_proj %>%
         sf::st_transform(crs = 4326)
      b4ps.dat$lon <- as.vector(sf::st_coordinates(b4ps.dat)[,1])
      b4ps.dat$lat <- as.vector(sf::st_coordinates(b4ps.dat)[,2])

      # instead I'll just rotate the graph and do a lazy plotting hack
      b4ps.dat_proj_rot <- maptools::elide(sf::as_Spatial(b4ps.dat_proj),
                                           rotate = -90) %>%
         sf::st_as_sf()
      b4ps.dat_proj_rot$lon_proj_rot <- sf::st_coordinates(b4ps.dat_proj_rot)[,1]
      b4ps.dat_proj_rot$lat_proj_rot <- sf::st_coordinates(b4ps.dat_proj_rot)[,2]

      (b4.plot.smooth.mgcViz_proj <-  ggplot(data = na.omit(b4ps.dat_proj_rot)) +
            theme_classic() +
            geom_point(aes(x = lon_proj_rot,
                           y = lat_proj_rot,
                           color = z),
                       size = 5,
                       shape = 15) +
            scale_color_gradient2(mid = '#fffdde',
                                  midpoint = 0,
                                  na.value = 'white')+
            ggtitle(label = '') +
            xlab('') +
            ylab('') +
            labs(color = "Spatial\nSmooth\nEffect Size\n(logit scale)")+
            theme(axis.text = element_blank(), axis.ticks = element_blank()))
      (b4.plot.smooth.mgcViz_proj.sd <-  ggplot(data = (b4ps.dat_proj_rot)) +
            theme_classic() +
            geom_point(aes(x = lon_proj_rot,
                           y = lat_proj_rot,
                           color = se),
                       size = 5,
                       shape = 15) +
            scale_color_gradient2(mid = '#fffdde',
                                  midpoint = 0,
                                  na.value = 'white')+
            ggtitle(label = '') +
            xlab('') +
            ylab('') +
            labs(color = "SE Spatial\nSmooth\nEffect Size\n(logit scale)")+
            theme(axis.text = element_blank(), axis.ticks = element_blank()))

      # plot the field using my plot_covariates_gg() function
      # heatmap
      hmpd <- plot(fit.b4, scheme = 2)
      # convert to a list of dfs instead of a list of lists
      hmpd_2 <- list()
      lis <- hmpd[[1]]
      grid = expand.grid(lon = lis$x, # I think I'll need to un-project this before plotting...
                         lat = lis$y) # I think I'll need to un-project this before plotting...
      grid$mean  = lis$fit
      grid$se    = lis$se
      grid$include = !lis$exclude

      hmpd_2[[1]] <- grid
      smooths.b4_proj <- dplyr::bind_rows(hmpd_2) %>%  # .id = "column_label" can be used to add a column with the list name
         sf::st_as_sf(coords = c('lon', 'lat'),
                      crs = proj_crs,
                      remove = F)
      # un-project it to lat/lon
      smooths.b4 <- smooths.b4_proj %>%
         sf::st_as_sf(crs = 4326)

      # rotate the smooth for a hack to plot it in a more pleasant manner
      smooths.b4_proj_rot <- maptools::elide(sf::as_Spatial(smooths.b4_proj),
                                             rotate = -90,
                                             center = c(mean(range(smooths.b4_proj$lon, na.rm = T)),
                                                        mean(range(smooths.b4_proj$lat, na.rm = T)))) %>%
         sf::st_as_sf()
      smooths.b4_proj_rot$lon_proj_rot <- sf::st_coordinates(smooths.b4_proj_rot)[,1]
      smooths.b4_proj_rot$lat_proj_rot <- sf::st_coordinates(smooths.b4_proj_rot)[,2]

      # not working...
      # states_proj_rot <- states %>% 
      #    sf::st_transform(crs = proj_crs) %>% 
      #    sf::st_crop(y = sf::st_bbox(smooths.b4_proj)) %>%
      #    sf::as_Spatial(.) %>%
      #    maptools::elide(rotate = -90,
      #                    center = c(mean(range(smooths.b4_proj$lon, na.rm = T)),
      #                               mean(range(smooths.b4_proj$lat, na.rm = T)))) %>%
      #    sf::st_as_sf()

      # plot the smooths
      (b4.plot.smooth.mean <- ggplot(data = na.omit(smooths.b4_proj_rot)) +
            theme_classic() +
            geom_point(aes(x = lon_proj_rot,
                           y = lat_proj_rot,
                           color = mean),
                       size = 5,
                       shape = 15) +
            # geom_sf(data = states_proj_rot) +
            scale_color_viridis_c(option = 'viridis') +
            # scale_color_gradient2(mid = '#fffdde',
            #                       midpoint = 0,
            #                       na.value = 'white')+
            ggtitle(label = '') +
            xlab('') +
            ylab('') +
            coord_equal()+
            labs(color = "Spatial\nSmooth\nEffect Size\n(logit scale)")+
            theme(axis.text = element_blank(), axis.ticks = element_blank()))

      (b4.plot.smooth.sd <- ggplot(data = na.omit(smooths.b4_proj_rot)) +
            theme_classic() +
            geom_point(aes(x = lon_proj_rot,
                           y = lat_proj_rot,
                           color = se),
                       size = 5,
                       shape = 15) +
            scale_color_viridis_c(option = 'magma')+
            ggtitle(label = '') +
            xlab('') +
            ylab('') +
            coord_equal()+
            labs(color = "SE Spatial\nSmooth\nEffect Size\n(logit scale)")+
            theme(axis.text = element_blank(), axis.ticks = element_blank()))
   }
   
   # binomial model predictions
   {
      # make predictions using grid1 prediction dataset
      predictions.b4.response <- mgcv::predict.gam(fit.b4, 
                                                   newdata = pdat[,c.4],
                                                   type = 'response', 
                                                   se.fit = T)
      # make predictions using grid2 prediction dataset
      predictions.b4.response2 <- mgcv::predict.gam(fit.b4, 
                                                    newdata = pdat2[,c.4],
                                                    type = 'response', 
                                                    se.fit = T)
      
      # add the predicted values into the pdat dataset
      pdat$z.pred.b4    <- predictions.b4.response$fit    
      pdat$z.pred.b4.sd <- predictions.b4.response$se.fit
      # 2nd prediction dataset
      pdat2$z.pred.b4    <- predictions.b4.response2$fit    
      pdat2$z.pred.b4.sd <- predictions.b4.response2$se.fit
      
      # model name for plot legends
      mn <- 'GAM 2'
      
      # plot model predictions
      (plot.b4.mean <- plot_covariates_gg(data = pdat %>% 
                                             dplyr::mutate(lat = lat_unproj,
                                                           lon = lon_unproj), 
                                          field = 'z.pred.b4', 
                                          legend.title = paste0(mn, '\nPredicted\nProbability\nof Bycatch'),
                                          colormap = 'viridis',
                                          legend.color.range = plot.mean.range.b))
      ggsave(filename = file.path(base_folder, paste0('binomial_model_predictions_b4.png')), plot = plot.b4.mean, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.b4.mean)
      
      # plot SD of model predictions
      (plot.b4.sd <- plot_covariates_gg(data = pdat %>% 
                                           dplyr::mutate(lat = lat_unproj,
                                                         lon = lon_unproj), 
                                        field = 'z.pred.b4.sd', 
                                        legend.title = paste0(mn, '\nSD of\nPredicted\nBycatch\nProbability'), 
                                        legend.color.range = plot.sd.range.b,
                                        colormap = 'magma'))
      ggsave(filename = file.path(base_folder, paste0('binomial_model_predictions_b4_sd.png')), plot = plot.b4.sd, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.b4.sd)
      
      # interactive plot
      if(FALSE){
         # first convert the data.frame to a raster
         b4.mean.r <- raster::rasterFromXYZ(xyz = pdat  %>% 
                                               dplyr::mutate(lat = lat_unproj,
                                                             lon = lon_unproj) %>% 
                                               dplyr::select(lon, lat, z.pred.b4),    
                                            crs = sp::CRS('+init=EPSG:4326'))
         b4.sd.r   <- raster::rasterFromXYZ(xyz = pdat  %>% 
                                               dplyr::mutate(lat = lat_unproj,
                                                             lon = lon_unproj) %>% 
                                               dplyr::select(lon, lat, z.pred.b4.sd),
                                            crs = sp::CRS('+init=EPSG:4326'))
         
         # leaflet uses epgs:3857, so I need to project to that first 
         # (leaflet will do it automatically, but it can screw up the color palate, so I'll do it first)
         # to get rid of errors see: https://github.com/rstudio/leaflet/issues/224
         b4.mean.rl <- leaflet::projectRasterForLeaflet(b4.mean.r, method = 'bilinear')
         b4.sd.rl   <- leaflet::projectRasterForLeaflet(b4.sd.r, method = 'bilinear')
         # the warnings actually come from raster package: raster::projectRaster(from = haul_count_r, crs = sp::CRS('+init=EPSG:3857'))
         
         # first create a color palatte function
         pal.b4m <- leaflet::colorNumeric(palette  = 'viridis', 
                                          domain   = range(na.omit(raster::values(b4.mean.rl))), 
                                          na.color = "transparent")
         pal.b4sd <- leaflet::colorNumeric(palette  = 'magma', 
                                           domain   = range(na.omit(raster::values(b4.sd.rl))), 
                                           na.color = "transparent")
         
         # then create the plot
         plot_height_pix <- 900
         lf.b4m <- leaflet::leaflet(height = plot_height_pix) %>% 
            leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>% 
            leaflet::addRasterImage(x = b4.mean.rl, 
                                    colors = pal.b4m,
                                    project = FALSE, 
                                    opacity = 0.9,
                                    group = "Bycatch",
                                    layerId = "Bycatch") %>%
            leafem::addImageQuery(x = b4.mean.rl, 
                                  digits = 4,
                                  position = 'topleft',
                                  project = TRUE,
                                  layerId = "Bycatch") %>%
            leafem::addMouseCoordinates() %>%
            leaflet::addLegend(position = 'topright', 
                               title = paste0(mn, '</br>Predicted</br>Bycatch</br>Probability'), # need HTML coding for line breaks </br> , not R \n
                               pal = pal.b4m, 
                               values = raster::values(b4.mean.rl))
         # lf.b4m
         ## save to standalone .html
         save_leaflet(plot = lf.b4m, 
                      file = file.path(base_folder, 'leaflet', 'binomial_model_predictions_b4_mean.html'))
         
         lf.b4sd <- leaflet::leaflet(height = plot_height_pix) %>% 
            leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>% 
            leaflet::addRasterImage(x = b4.sd.rl, 
                                    colors = pal.b4sd,
                                    project = FALSE, 
                                    opacity = 0.9,
                                    group = "Bycatch_SD",
                                    layerId = "Bycatch_SD") %>%
            leafem::addImageQuery(x = b4.sd.rl, 
                                  digits = 6,
                                  position = 'topleft',
                                  project = TRUE,
                                  layerId = "Bycatch_SD") %>%
            leafem::addMouseCoordinates() %>%
            leaflet::addLegend(position = 'topright', 
                               title = paste0(mn, '</br>SD of</br>Predicted</br>Bycatch</br>Probability'), # need HTML coding for line breaks </br> , not R \n
                               pal = pal.b4sd, 
                               values = raster::values(b4.sd.rl))
         # lf.b4sd
         save_leaflet(plot = lf.b4sd, 
                      file = file.path(base_folder, 'leaflet', 'binomial_model_predictions_b4_sd.html'))
         
         # mean and SD together
         # leafsync::sync(lf.b4m, lf.b4sd)
         # save_leafsync(tags = leafsync::sync(lf.b4m, lf.b4sd), 
         #               file = paste0(getwd(), '/figures/leaflet/binomial_model_predictions_b4_mean_and_sd_', short_long, '_',
         #                             ifelse(use_projected, 'projected', 'unprojected'), '.html'))
      }
   }
   
   # gamma model predictions
   {
      # make predictions using grid1 prediction dataset
      predictions.g4.response <- mgcv::predict.gam(fit.g4, 
                                                   newdata = pdat[,c.4],
                                                   type = 'response', 
                                                   se.fit = T)
      # make predictions using grid1 prediction dataset
      predictions.g4.response2 <- mgcv::predict.gam(fit.g4, 
                                                    newdata = pdat2[,c.4],
                                                    type = 'response', 
                                                    se.fit = T)
      
      # add the predicted values into the pdat dataset
      pdat$z.pred.g4    <- predictions.g4.response$fit    
      pdat$z.pred.g4.sd <- predictions.g4.response$se.fit
      # 2nd prediction dataset
      pdat2$z.pred.g4    <- predictions.g4.response2$fit    
      pdat2$z.pred.g4.sd <- predictions.g4.response2$se.fit
      
      # plot model predictions
      (plot.g4.mean <- plot_covariates_gg(data = pdat %>% 
                                             dplyr::mutate(lat = lat_unproj,
                                                           lon = lon_unproj), 
                                          field = 'z.pred.g4', 
                                          legend.title = paste0(mn, '\nPredicted\nNumber\nof Bycatch'),
                                          colormap = 'viridis',
                                          legend.color.range = plot.mean.range.g, 
                                          transformation = 'log1p', 
                                          legend.scale.breaks = plot.mean.legend.scale.breaks.g))
      ggsave(filename = file.path(base_folder, paste0('gamma_model_predictions_g4.png')), plot = plot.g4.mean, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.g4.mean)
      
      # plot SD of model predictions
      (plot.g4.sd <- plot_covariates_gg(data = pdat %>% 
                                           dplyr::mutate(lat = lat_unproj,
                                                         lon = lon_unproj), 
                                        field = 'z.pred.g4.sd', 
                                        legend.title = paste0(mn, '\nSD of\nPredicted\nBycatch'), 
                                        legend.color.range = plot.sd.range.g,
                                        transformation = 'log1p', 
                                        legend.scale.breaks = plot.sd.legend.scale.breaks.g,
                                        colormap = 'magma'))
      ggsave(filename = file.path(base_folder, paste0('gamma_model_predictions_g4_sd.png')), plot = plot.g4.sd, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.g4.sd)
      
      # interactive plot
      if(FALSE){
         # first convert the data.frame to a raster
         g4.mean.r <- raster::rasterFromXYZ(xyz = pdat  %>% 
                                               dplyr::mutate(lat = lat_unproj,
                                                             lon = lon_unproj) %>% 
                                               dplyr::rowwise() %>% 
                                               dplyr::mutate(z.pred.g4 = ifelse(z.pred.g4 > plot.mean.range.g[2], plot.mean.range.g[2], z.pred.g4)) %>% 
                                               dplyr::ungroup() %>% 
                                               dplyr::select(lon, lat, z.pred.g4),    
                                            crs = sp::CRS('+init=EPSG:4326'))
         g4.sd.r   <- raster::rasterFromXYZ(xyz = pdat  %>% 
                                               dplyr::mutate(lat = lat_unproj,
                                                             lon = lon_unproj) %>% 
                                               dplyr::rowwise() %>% 
                                               dplyr::mutate(z.pred.g4.sd = ifelse(z.pred.g4.sd > plot.sd.range.g[2], plot.sd.range.g[2], z.pred.g4.sd)) %>% 
                                               dplyr::ungroup() %>% 
                                               dplyr::select(lon, lat, z.pred.g4.sd),
                                            crs = sp::CRS('+init=EPSG:4326'))
         
         # leaflet uses epgs:3857, so I need to project to that first 
         # (leaflet will do it automatically, but it can screw up the color palate, so I'll do it first)
         # to get rid of errors see: https://github.com/rstudio/leaflet/issues/224
         g4.mean.rl <- leaflet::projectRasterForLeaflet(g4.mean.r, method = 'bilinear')
         g4.sd.rl   <- leaflet::projectRasterForLeaflet(g4.sd.r, method = 'bilinear')
         # the warnings actually come from raster package: raster::projectRaster(from = haul_count_r, crs = sp::CRS('+init=EPSG:3857'))
         
         # first create a color palatte function
         pal.g4m <- leaflet::colorNumeric(palette  = 'viridis', 
                                          domain   =  range(na.omit(raster::values(g4.mean.rl))), 
                                          na.color = "transparent")
         pal.g4sd <- leaflet::colorNumeric(palette  = 'magma', 
                                           domain   = range(na.omit(raster::values(g4.sd.rl))), 
                                           na.color = "transparent")
         
         # then create the plot
         plot_height_pix <- 900
         lf.g4m <- leaflet::leaflet(height = plot_height_pix) %>% 
            leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>% 
            leaflet::addRasterImage(x = g4.mean.rl, 
                                    colors = pal.g4m,
                                    project = FALSE, 
                                    opacity = 0.9,
                                    group = "Bycatch",
                                    layerId = "Bycatch") %>%
            leafem::addImageQuery(x = g4.mean.rl, 
                                  digits = 4,
                                  position = 'topleft',
                                  project = TRUE,
                                  layerId = "Bycatch") %>%
            leafem::addMouseCoordinates() %>%
            leaflet::addLegend(position = 'topright', 
                               title = paste0(mn,'</br>Predicted</br>Bycatch'), # need HTML coding for line breaks </br> , not R \n
                               pal = pal.g4m, 
                               values = raster::values(g4.mean.rl))
         # lf.g4m
         ## save to standalone .html
         save_leaflet(plot = lf.g4m, 
                      file = file.path(base_folder, 'leaflet', 'gamma_model_predictions_g4_mean.html'))
         
         lf.g4sd <- leaflet::leaflet(height = plot_height_pix) %>% 
            leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>% 
            leaflet::addRasterImage(x = g4.sd.rl, 
                                    colors = pal.g4sd,
                                    project = FALSE, 
                                    opacity = 0.9,
                                    group = "Bycatch_SD",
                                    layerId = "Bycatch_SD") %>%
            leafem::addImageQuery(x = g4.sd.rl, 
                                  digits = 6,
                                  position = 'topleft',
                                  project = TRUE,
                                  layerId = "Bycatch_SD") %>%
            leafem::addMouseCoordinates() %>%
            leaflet::addLegend(position = 'topright', 
                               title = paste0(mn,'</br>SD of</br>Predicted</br>Bycatch'), # need HTML coding for line breaks </br> , not R \n
                               pal = pal.g4sd, 
                               values = raster::values(g4.sd.rl))
         # lf.g4sd
         save_leaflet(plot = lf.g4sd, 
                      file = file.path(base_folder, 'leaflet', 'gamma_model_predictions_g4_sd.html'))
         
         # mean and SD together
         # leafsync::sync(lf.g4m, lf.g4sd)
         # save_leafsync(tags = leafsync::sync(lf.g4m, lf.g4sd), 
         #               file = paste0(getwd(), '/figures/leaflet/gamma_model_predictions_g4_mean_and_sd_', short_long, '_',
         #                             ifelse(use_projected, 'projected', 'unprojected'), '.html'))
      }
   }
   
   # combined hurdle model predictions
   {
      pdat <- pdat %>% 
         dplyr::mutate(z.pred.h4 = z.pred.b4 * z.pred.g4)
      pdat2 <- pdat2 %>% 
         dplyr::mutate(z.pred.h4 = z.pred.b4 * z.pred.g4)
      
      # plot model predictions
      (plot.h4.mean <- plot_covariates_gg(data = pdat %>%
                                             dplyr::mutate(lat = lat_unproj,
                                                           lon = lon_unproj),
                                          field = 'z.pred.h4',
                                          legend.title = paste0(mn, '\nPredicted\nNumber\nof Bycatch'),
                                          colormap = 'viridis',
                                          legend.color.range = plot.mean.range.h, 
                                          transformation = 'log1p', 
                                          legend.scale.breaks = plot.mean.legend.scale.breaks.h))
      ggsave(filename = file.path(base_folder, paste0('hurdle_model_predictions_h4.png')), plot = plot.h4.mean, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.h4.mean)
      
      # plot of spatial effect through time
      # just plot 1 year b/c the spatial component is the same every year. 
      plot.h4.space <- plot_covariates_gg(data = pdat2 %>%
                                             dplyr::mutate(lat = lat_unproj,
                                                           lon = lon_unproj) %>% 
                                             dplyr::filter(year_f == '2014'),
                                          field = 'z.pred.h4',
                                          legend.title = paste0(mn, '\nPredicted\nNumber\nof Bycatch'),
                                          colormap = 'viridis',
                                          legend.color.range = plot.mean.range.h, 
                                          transformation = 'log1p', 
                                          legend.scale.breaks = plot.mean.legend.scale.breaks.h,
                                          facet_field = 'year_f', 
                                          facet_rows = 3)
      ggsave(filename = file.path(base_folder, paste0('m4_spatial_predictions.png')), plot = plot.h4.space, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.h4.space)
   }
   
   # Datasets for marginal effects plots of fixed effects
   {
      # add a space in the list for each of them
      marginal_effect_list_b[[tolower(mn)]]        <- vector('list', length(c.4))
      names(marginal_effect_list_b[[tolower(mn)]]) <- c.4
      marginal_effect_list_g[[tolower(mn)]]        <- vector('list', length(c.4))
      names(marginal_effect_list_g[[tolower(mn)]]) <- c.4
      
      
      for(c in 1:length(c.4)){
         # I can also skip any covariates that end with "poly2" because polynomial predictors need to be considered together
         if( grepl(pattern = '.+_poly2$', x = c.4[c]) ) next
         
         # get the dataframe for the model predictions
         ndb <- ndg <- prediction_dfs[[c.4[c]]]
         
         # predict new values
         preds.b <- mgcv:::predict.gam(object = fit.b4, 
                                       newdata = ndb,
                                       type = 'link', 
                                       se.fit = TRUE) # can't predict on the response scale if I'm going to +/- SE to get CI [it WILL give SE on the response scale, but I can't use those to correctly calculate confidence limits]
         # gamma model
         preds.g <- mgcv:::predict.gam(object = fit.g4, 
                                       newdata = ndg,
                                       type = 'link', 
                                       se.fit = TRUE) # can't predict on the response scale if I'm going to +/- SE to get CI [it WILL give SE on the response scale, but I can't use those to correctly calculate confidence limits]
         
         # Get the predictions on the link scale
         z_link_b     <- preds.b$fit
         z_link_lcl_b <- preds.b$fit - 2 * preds.b$se.fit
         z_link_ucl_b <- preds.b$fit + 2 * preds.b$se.fit
         
         z_link_g     <- preds.g$fit
         z_link_lcl_g <- preds.g$fit - 2 * preds.g$se.fit
         z_link_ucl_g <- preds.g$fit + 2 * preds.g$se.fit
         
         
         # convert to response scale
         ndb$bycatch     <- fit.b4$family$linkinv(z_link_b)
         ndb$bycatch_lcl <- fit.b4$family$linkinv(z_link_lcl_b)
         ndb$bycatch_ucl <- fit.b4$family$linkinv(z_link_ucl_b)
         ndb$model <- mn
         
         # convert to response scale
         ndg$bycatch     <- fit.g4$family$linkinv(z_link_g)
         ndg$bycatch_lcl <- fit.g4$family$linkinv(z_link_lcl_g)
         ndg$bycatch_ucl <- fit.g4$family$linkinv(z_link_ucl_g)
         ndg$model <- mn
         
         # get the name of the covariate (but not "poly"). It would've been easier to just use "sub()"...
         ndb$covariate <- 
            ndg$covariate <- 
            ifelse(grepl('.+_poly1', c.4[c] ), 
                   lsp_lut[ lsp_lut$final_name == c.4[c], 'original_name' ], 
                   c.4[c])
         
         # save the dataframe
         marginal_effect_list_b[[tolower(mn)]][[c.4[c]]] <- ndb
         marginal_effect_list_g[[tolower(mn)]][[c.4[c]]] <- ndg
      }
   }
}

##### 5. GAM 3 (low-basis-dimension smooths of covariates & spatial effect changes through time) #####
{
   # covariates for the model-fitting function
   covar.5 <- covar
   # covariates needed to use the model fit to make predictions
   c.5 <- c(covar.5, 'lat', 'lon') # this needs to include year_f
   
   # fit the binomial model
   fit.b5 <- fit_m4_gam3(data = bdat, 
                         covariates  = covar.5, 
                         modelfamily = 'binomial', 
                         rows.train  = 1:nrow(bdat),
                         rows.test   = NULL, 
                         year_type = 'factor',
                         save.model.fit = TRUE)$model
   # notify me when this model finishes b/c it takes a long time to fit.
   beepr::beep()
   # summary(fit.b5)
   # save the model so that I can just reload it in the future instead of refitting it.
   saveRDS(fit.b5, file.path(base_folder, paste0('covariate_effects_model_g5.RDS')))
   
   # fit the gamma model
   fit.g5 <- fit_m4_gam3(data = gdat,
                         covariates  = covar.5, 
                         modelfamily = 'gamma', 
                         rows.train  = 1:nrow(gdat),
                         rows.test   = NULL, 
                         year_type = 'factor',
                         save.model.fit = TRUE)$model
   # notify me when this model finishes b/c it takes a long time to fit.
   beepr::beep()
   # summary(fit.g5)
   # save the model so that I can just reload it in the future instead of refitting it.
   saveRDS(fit.g5, file.path(base_folder, paste0('covariate_effects_model_g5.RDS')))
   
   # compare fixed effect parameter estimates to models 1-5
   {
      categorical_covs <- c('(Intercept)',
                            'sector',
                            'sectorMothership',
                            'sectorMidwater Hake',
                            'sectorShoreside',
                            'sectorCatcher Processor',
                            paste0('year_f', 2002:2019))
      
      # create a dataframe of the linear model parameter estimates
      b12345.tidy <- bind_rows(
         # model b1
         broom::tidy(fit.b1, 
                     parametric = TRUE, 
                     conf.int = TRUE, 
                     conf.level = 0.95) %>% 
            mutate(poly = grepl(pattern = '.+_poly', x = term),
                   concat = ifelse(term %in% categorical_covs, 'categorical', 'spline'),
                   model = model_names[1],
                   family = 'binomial'),
         # model g1
         broom::tidy(fit.g1, 
                     parametric = TRUE, 
                     conf.int = TRUE, 
                     conf.level = 0.95) %>% 
            mutate(poly = grepl(pattern = '.+_poly', x = term),
                   concat = ifelse(term %in% categorical_covs, 'categorical', 'spline'),
                   model = model_names[1],
                   family = 'gamma'),
         # model b2
         broom::tidy(fit.b2, 
                     parametric = TRUE, 
                     conf.int = TRUE, 
                     conf.level = 0.95) %>% 
            mutate(poly = grepl(pattern = '.+_poly', x = term),
                   concat = ifelse(term %in% categorical_covs, 'categorical', 'quadratic'),
                   model = model_names[2],
                   family = 'binomial'),
         # model g2
         broom::tidy(fit.g2, 
                     parametric = TRUE, 
                     conf.int = TRUE, 
                     conf.level = 0.95) %>% 
            mutate(poly = grepl(pattern = '.+_poly', x = term),
                   concat = ifelse(term %in% categorical_covs, 'categorical', 'quadratic'),
                   model = model_names[2],
                   family = 'gamma'),
         # model b3
         broom::tidy(fit.b3, 
                     parametric = TRUE, 
                     conf.int = TRUE, 
                     conf.level = 0.95) %>% 
            mutate(poly = grepl(pattern = '.+_poly', x = term),
                   concat = ifelse(term %in% categorical_covs, 'categorical', 'spline'),
                   model = model_names[3],
                   family = 'binomial'),
         # model g3
         broom::tidy(fit.g3, 
                     parametric = TRUE, 
                     conf.int = TRUE, 
                     conf.level = 0.95) %>% 
            mutate(poly = grepl(pattern = '.+_poly', x = term),
                   concat = ifelse(term %in% categorical_covs, 'categorical', 'spline'),
                   model = model_names[3],
                   family = 'gamma'),
         # model b4
         broom::tidy(fit.b4, 
                     parametric = TRUE, 
                     conf.int = TRUE, 
                     conf.level = 0.95) %>% 
            mutate(poly = grepl(pattern = '.+_poly', x = term),
                   concat = ifelse(term %in% categorical_covs, 'categorical', 'spline'),
                   model = model_names[4],
                   family = 'binomial'),
         # model g4
         broom::tidy(fit.g4, 
                     parametric = TRUE, 
                     conf.int = TRUE, 
                     conf.level = 0.95) %>% 
            mutate(poly = grepl(pattern = '.+_poly', x = term),
                   concat = ifelse(term %in% categorical_covs, 'categorical', 'spline'),
                   model = model_names[4],
                   family = 'gamma'),
         # model b5
         broom::tidy(fit.b5, 
                     parametric = TRUE, 
                     conf.int = TRUE, 
                     conf.level = 0.95) %>% 
            mutate(poly = grepl(pattern = '.+_poly', x = term),
                   concat = ifelse(term %in% categorical_covs, 'categorical', 'spline'),
                   model = model_names[5],
                   family = 'binomial'),
         # model g5
         broom::tidy(fit.g5, 
                     parametric = TRUE, 
                     conf.int = TRUE, 
                     conf.level = 0.95) %>% 
            mutate(poly = grepl(pattern = '.+_poly', x = term),
                   concat = ifelse(term %in% categorical_covs, 'categorical', 'spline'),
                   model = model_names[5],
                   family = 'gamma')
      )
      
      # plot parameter estimates
      (b12345comp.plot.fixed.pars <- ggplot(b12345.tidy, 
                                            aes(x = term,
                                                y = estimate, 
                                                ymin = conf.low, 
                                                ymax = conf.high,
                                                color = model,
                                                shape = family)) + 
            theme_bw() + 
            geom_pointrange(position = position_dodge(width = 0.4)) + 
            facet_wrap(facets = vars(concat), scales = 'free') +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)))
   }
   
   # binomial model predictions
   {
      # make predictions using grid1 prediction dataset
      predictions.b5.response <- mgcv::predict.gam(fit.b5, 
                                                   newdata = pdat[,c.5],
                                                   type = 'response', 
                                                   se.fit = T)
      
      # make predictions using grid2 prediction dataset
      predictions.b5.response2 <- mgcv::predict.gam(fit.b5, 
                                                    newdata = pdat2[,c.5],
                                                    type = 'response', 
                                                    se.fit = T)
      
      # add the predicted values into the pdat dataset
      pdat$z.pred.b5         <- predictions.b5.response$fit    # fit.b5$family$linkinv(predictions.b.glm.link$fit) is the same as:         predict(fit.b5, newdata = pred.dat[,c('YEAR.f', covar)], type = 'response')
      pdat$z.pred.b5.sd      <- predictions.b5.response$se.fit # fit.b5$family$linkinv(predictions.b.glm.link$se.fit) is NOT THE SAME as:  predict(fit.b5, newdata = pred.dat[,c('YEAR.f', covar)], type = 'response', se.fit=T)$se.fit
      
      # 2nd prediction dataset
      pdat2$z.pred.b5    <- predictions.b5.response2$fit
      pdat2$z.pred.b5.sd <- predictions.b5.response2$se.fit
      
      # model name for plot legends
      mn <- 'GAM 3'
      
      # plot model predictions
      (plot.b5.mean <- plot_covariates_gg(data = pdat %>% 
                                             dplyr::mutate(lat = lat_unproj,
                                                           lon = lon_unproj), 
                                          field = 'z.pred.b5', 
                                          legend.title = paste0(mn, '\nPredicted\nProbability\nof Bycatch'),
                                          colormap = 'viridis',
                                          legend.color.range = plot.mean.range.b))
      ggsave(filename = file.path(base_folder, paste0('binomial_model_predictions_b5.png')), plot = plot.b5.mean, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.b5.mean)
      
      # plot SD of model predictions
      (plot.b5.sd <- plot_covariates_gg(data = pdat %>% 
                                           dplyr::mutate(lat = lat_unproj,
                                                         lon = lon_unproj), 
                                        field = 'z.pred.b5.sd', 
                                        legend.title = paste0(mn, '\nSD of\nPredicted\nBycatch\nProbability'), 
                                        legend.color.range = plot.sd.range.b,
                                        colormap = 'magma'))
      ggsave(filename = file.path(base_folder, paste0('binomial_model_predictions_b5_sd.png')), plot = plot.b5.sd, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.b5.sd)
      
      # interactive plot
      if(FALSE){
         # first convert the data.frame to a raster
         b5.mean.r <- raster::rasterFromXYZ(xyz = pdat  %>% 
                                               dplyr::mutate(lat = lat_unproj,
                                                             lon = lon_unproj) %>% 
                                               dplyr::select(lon, lat, z.pred.b5),    
                                            crs = sp::CRS('+init=EPSG:4326'))
         b5.sd.r   <- raster::rasterFromXYZ(xyz = pdat  %>% 
                                               dplyr::mutate(lat = lat_unproj,
                                                             lon = lon_unproj) %>% 
                                               dplyr::select(lon, lat, z.pred.b5.sd),
                                            crs = sp::CRS('+init=EPSG:4326'))
         
         # leaflet uses epgs:3857, so I need to project to that first 
         # (leaflet will do it automatically, but it can screw up the color palate, so I'll do it first)
         # to get rid of errors see: https://github.com/rstudio/leaflet/issues/224
         b5.mean.rl <- leaflet::projectRasterForLeaflet(b5.mean.r, method = 'bilinear')
         b5.sd.rl   <- leaflet::projectRasterForLeaflet(b5.sd.r, method = 'bilinear')
         # the warnings actually come from raster package: raster::projectRaster(from = haul_count_r, crs = sp::CRS('+init=EPSG:3857'))
         
         # first create a color palatte function
         pal.b5m <- leaflet::colorNumeric(palette  = 'viridis', 
                                          domain   = range(na.omit(raster::values(b5.mean.rl))), 
                                          na.color = "transparent")
         pal.b5sd <- leaflet::colorNumeric(palette  = 'magma', 
                                           domain   = range(na.omit(raster::values(b5.sd.rl))), 
                                           na.color = "transparent")
         
         # then create the plot
         plot_height_pix <- 900
         lf.b5m <- leaflet::leaflet(height = plot_height_pix) %>% 
            leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>% 
            leaflet::addRasterImage(x = b5.mean.rl, 
                                    colors = pal.b5m,
                                    project = FALSE, 
                                    opacity = 0.9,
                                    group = "Bycatch",
                                    layerId = "Bycatch") %>%
            leafem::addImageQuery(x = b5.mean.rl, 
                                  digits = 4,
                                  position = 'topleft',
                                  project = TRUE,
                                  layerId = "Bycatch") %>%
            leafem::addMouseCoordinates() %>%
            leaflet::addLegend(position = 'topright', 
                               title = paste0(mn, '</br>Predicted</br>Bycatch</br>Probability'), # need HTML coding for line breaks </br> , not R \n
                               pal = pal.b5m, 
                               values = raster::values(b5.mean.rl))
         # lf.b5m
         ## save to standalone .html
         save_leaflet(plot = lf.b5m, 
                      file = file.path(base_folder, 'leaflet', 'binomial_model_predictions_b5_mean.html'))
         
         lf.b5sd <- leaflet::leaflet(height = plot_height_pix) %>% 
            leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>% 
            leaflet::addRasterImage(x = b5.sd.rl, 
                                    colors = pal.b5sd,
                                    project = FALSE, 
                                    opacity = 0.9,
                                    group = "Bycatch_SD",
                                    layerId = "Bycatch_SD") %>%
            leafem::addImageQuery(x = b5.sd.rl, 
                                  digits = 6,
                                  position = 'topleft',
                                  project = TRUE,
                                  layerId = "Bycatch_SD") %>%
            leafem::addMouseCoordinates() %>%
            leaflet::addLegend(position = 'topright', 
                               title = paste0(mn, '</br>SD of</br>Predicted</br>Bycatch</br>Probability'), # need HTML coding for line breaks </br> , not R \n
                               pal = pal.b5sd, 
                               values = raster::values(b5.sd.rl))
         # lf.b5sd
         save_leaflet(plot = lf.b5sd, 
                      file = file.path(base_folder, 'leaflet', 'binomial_model_predictions_b5_sd.html'))
         
         # mean and SD together
         # leafsync::sync(lf.b5m, lf.b5sd)
         # save_leafsync(tags = leafsync::sync(lf.b5m, lf.b5sd), 
         #               file = paste0(getwd(), '/figures/leaflet/binomial_model_predictions_b5_mean_and_sd_', short_long, '_',
         #                             ifelse(use_projected, 'projected', 'unprojected'), '.html'))
      }
   }
   
   # gamma model predictions
   {
      # make predictions using grid1 prediction dataset
      predictions.g5.response <- mgcv::predict.gam(fit.g5, 
                                                   newdata = pdat[,c.5],
                                                   type = 'response', 
                                                   se.fit = T)
      # make predictions using grid1 prediction dataset
      predictions.g5.response2 <- mgcv::predict.gam(fit.g5, 
                                                    newdata = pdat2[,c.5],
                                                    type = 'response', 
                                                    se.fit = T)
      
      # add the predicted values into the pdat dataset
      pdat$z.pred.g5    <- predictions.g5.response$fit    
      pdat$z.pred.g5.sd <- predictions.g5.response$se.fit
      # 2nd prediction dataset
      pdat2$z.pred.g5    <- predictions.g5.response2$fit    
      pdat2$z.pred.g5.sd <- predictions.g5.response2$se.fit
      # It took so long to run model GAM 3 & make predictions from it that it might be worth saving to avoid rerunning very often
      # saveRDS(object = list(pdat = pdat, pdat2 = pdat2), file = file.path(base_folder, 'temp_prediction_df.RDS'))
      
      # plot model predictions
      (plot.g5.mean <- plot_covariates_gg(data = pdat %>% 
                                             dplyr::mutate(lat = lat_unproj,
                                                           lon = lon_unproj), 
                                          field = 'z.pred.g5', 
                                          legend.title = paste0(mn, '\nPredicted\nNumber\nof Bycatch'),
                                          colormap = 'viridis',
                                          legend.color.range = plot.mean.range.g, 
                                          transformation = 'log1p', 
                                          legend.scale.breaks = plot.mean.legend.scale.breaks.g))
      ggsave(filename = file.path(base_folder, paste0('gamma_model_predictions_g5.png')), plot = plot.g5.mean, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.g5.mean)
      
      # plot SD of model predictions
      (plot.g5.sd <- plot_covariates_gg(data = pdat %>% 
                                           dplyr::mutate(lat = lat_unproj,
                                                         lon = lon_unproj), 
                                        field = 'z.pred.g5.sd', 
                                        legend.title = paste0(mn, '\nSD of\nPredicted\nBycatch'), 
                                        legend.color.range = plot.sd.range.g,
                                        transformation = 'log1p', 
                                        legend.scale.breaks = plot.sd.legend.scale.breaks.g,
                                        colormap = 'magma'))
      ggsave(filename = file.path(base_folder, paste0('gamma_model_predictions_g5_sd.png')), plot = plot.g5.sd, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.g5.sd)
      
      # interactive plot
      if(FALSE){
         # first convert the data.frame to a raster
         g5.mean.r <- raster::rasterFromXYZ(xyz = pdat  %>% 
                                               dplyr::mutate(lat = lat_unproj,
                                                             lon = lon_unproj) %>% 
                                               dplyr::rowwise() %>% 
                                               dplyr::mutate(z.pred.g5 = ifelse(z.pred.g5 > plot.mean.range.g[2], plot.mean.range.g[2], z.pred.g5)) %>% 
                                               dplyr::ungroup() %>% 
                                               dplyr::select(lon, lat, z.pred.g5),    
                                            crs = sp::CRS('+init=EPSG:4326'))
         g5.sd.r   <- raster::rasterFromXYZ(xyz = pdat  %>% 
                                               dplyr::mutate(lat = lat_unproj,
                                                             lon = lon_unproj) %>% 
                                               dplyr::rowwise() %>% 
                                               dplyr::mutate(z.pred.g5.sd = ifelse(z.pred.g5.sd > plot.sd.range.g[2], plot.sd.range.g[2], z.pred.g5.sd)) %>% 
                                               dplyr::ungroup() %>% 
                                               dplyr::select(lon, lat, z.pred.g5.sd),
                                            crs = sp::CRS('+init=EPSG:4326'))
         
         # leaflet uses epgs:3857, so I need to project to that first 
         # (leaflet will do it automatically, but it can screw up the color palate, so I'll do it first)
         # to get rid of errors see: https://github.com/rstudio/leaflet/issues/224
         g5.mean.rl <- leaflet::projectRasterForLeaflet(g5.mean.r, method = 'bilinear')
         g5.sd.rl   <- leaflet::projectRasterForLeaflet(g5.sd.r, method = 'bilinear')
         # the warnings actually come from raster package: raster::projectRaster(from = haul_count_r, crs = sp::CRS('+init=EPSG:3857'))
         
         # first create a color palatte function
         pal.g5m <- leaflet::colorNumeric(palette  = 'viridis', 
                                          domain   =  range(na.omit(raster::values(g5.mean.rl))), 
                                          na.color = "transparent")
         pal.g5sd <- leaflet::colorNumeric(palette  = 'magma', 
                                           domain   = range(na.omit(raster::values(g5.sd.rl))), 
                                           na.color = "transparent")
         
         # then create the plot
         plot_height_pix <- 900
         lf.g5m <- leaflet::leaflet(height = plot_height_pix) %>% 
            leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>% 
            leaflet::addRasterImage(x = g5.mean.rl, 
                                    colors = pal.g5m,
                                    project = FALSE, 
                                    opacity = 0.9,
                                    group = "Bycatch",
                                    layerId = "Bycatch") %>%
            leafem::addImageQuery(x = g5.mean.rl, 
                                  digits = 4,
                                  position = 'topleft',
                                  project = TRUE,
                                  layerId = "Bycatch") %>%
            leafem::addMouseCoordinates() %>%
            leaflet::addLegend(position = 'topright', 
                               title = paste0(mn,'</br>Predicted</br>Bycatch'), # need HTML coding for line breaks </br> , not R \n
                               pal = pal.g5m, 
                               values = raster::values(g5.mean.rl))
         # lf.g5m
         ## save to standalone .html
         save_leaflet(plot = lf.g5m, 
                      file = file.path(base_folder, 'leaflet', 'gamma_model_predictions_g5_mean.html'))
         
         lf.g5sd <- leaflet::leaflet(height = plot_height_pix) %>% 
            leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>% 
            leaflet::addRasterImage(x = g5.sd.rl, 
                                    colors = pal.g5sd,
                                    project = FALSE, 
                                    opacity = 0.9,
                                    group = "Bycatch_SD",
                                    layerId = "Bycatch_SD") %>%
            leafem::addImageQuery(x = g5.sd.rl, 
                                  digits = 6,
                                  position = 'topleft',
                                  project = TRUE,
                                  layerId = "Bycatch_SD") %>%
            leafem::addMouseCoordinates() %>%
            leaflet::addLegend(position = 'topright', 
                               title = paste0(mn,'</br>SD of</br>Predicted</br>Bycatch'), # need HTML coding for line breaks </br> , not R \n
                               pal = pal.g5sd, 
                               values = raster::values(g5.sd.rl))
         # lf.g5sd
         save_leaflet(plot = lf.g5sd, 
                      file = file.path(base_folder, 'leaflet', 'gamma_model_predictions_g5_sd.html'))
         
         # mean and SD together
         # leafsync::sync(lf.g5m, lf.g5sd)
         # save_leafsync(tags = leafsync::sync(lf.g5m, lf.g5sd), 
         #               file = paste0(getwd(), '/figures/leaflet/gamma_model_predictions_g5_mean_and_sd_', short_long, '_',
         #                             ifelse(use_projected, 'projected', 'unprojected'), '.html'))
      }
   }
   
   # combined hurdle model predictions
   {
      pdat <- pdat %>% 
         dplyr::mutate(z.pred.h5 = z.pred.b5 * z.pred.g5)
      pdat2 <- pdat2 %>% 
         dplyr::mutate(z.pred.h5 = z.pred.b5 * z.pred.g5)
      
      # plot model predictions
      (plot.h5.mean <- plot_covariates_gg(data = pdat %>%
                                             dplyr::mutate(lat = lat_unproj,
                                                           lon = lon_unproj),
                                          field = 'z.pred.h5',
                                          legend.title = paste0(mn, '\nPredicted\nNumber\nof Bycatch'),
                                          colormap = 'viridis',
                                          legend.color.range = plot.mean.range.h, 
                                          transformation = 'log1p', 
                                          legend.scale.breaks = plot.mean.legend.scale.breaks.h))
      ggsave(filename = file.path(base_folder, paste0('hurdle_model_predictions_h5.png')), plot = plot.h5.mean, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.h5.mean)
      
      # plot of spatial effect through time
      # This one does change every year, so I'll plot more years 
      plot.h5.space <- plot_covariates_gg(data = pdat2 %>%
                                             dplyr::mutate(lat = lat_unproj,
                                                           lon = lon_unproj) %>% 
                                             dplyr::filter(year_f %in% c(2002:2019)),
                                          field = 'z.pred.h5',
                                          legend.title = paste0(mn, '\nPredicted\nNumber\nof Bycatch'),
                                          colormap = 'viridis',
                                          legend.color.range = plot.mean.range.h, 
                                          transformation = 'log1p', 
                                          legend.scale.breaks = plot.mean.legend.scale.breaks.h,
                                          facet_field = 'year_f', 
                                          facet_rows = 3)
      ggsave(filename = file.path(base_folder, paste0('m5_spatial_predictions.png')), plot = plot.h5.space, width = 7.5, height = 9, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.h5.space)
   }
   
   # Is there any interaction between year and location? 
   {
      pdat2 %>% 
         dplyr::group_by(lat, lon) %>% 
         dplyr::summarise(range.b = diff(range(z.pred.b5)),
                          range.g = diff(range(z.pred.g5)),
                          range.h = diff(range(z.pred.h5)),
                          .groups = 'drop') %>% 
         ggplot(., aes(x = range.g)) + geom_histogram(bins = 100)
      
      pdat2 %>% 
         dplyr::group_by(lat, lon) %>% 
         dplyr::summarise(sd.b = sd(z.pred.b5),
                          sd.g = sd(z.pred.g5),
                          sd.h = sd(z.pred.h5),
                          .groups = 'drop') %>% 
         ggplot(., aes(x = sd.g)) + geom_histogram(bins = 100)
      
      
      pdat2 %>% 
         dplyr::group_by(lat, lon) %>% 
         dplyr::summarise(sd.b = sd(z.pred.b5),
                          sd.g = sd(z.pred.g5),
                          sd.h = sd(z.pred.h5),
                          .groups = 'drop') %>% 
         dplyr::filter(sd.g > 1e5) %>% nrow()
      pdat2 %>% 
         dplyr::group_by(lat, lon) %>% 
         dplyr::summarise(sd.b = sd(z.pred.b5),
                          sd.g = sd(z.pred.g5),
                          sd.h = sd(z.pred.h5),
                          .groups = 'drop') %>% 
         dplyr::filter(sd.g < 1e4) %>% nrow()
      
         
      # can also map the SD to show areas that change the most from year to year
      ggsave(filename = file.path(base_folder, paste0('m5_spatial_SD_binomial_only.png')), 
             plot = plot_covariates_gg(data = pdat2 %>% 
                                          dplyr::mutate(lat = lat_unproj,
                                                        lon = lon_unproj) %>% 
                                          dplyr::group_by(lat, lon) %>% 
                                          dplyr::summarise(sd = sd(z.pred.b5), 
                                                           .groups = 'drop'),
                                       field = 'sd',
                                       legend.title = paste0(mn, '\nSD\n(across years)\nin Predicted\nProbability\nof Bycatch'),
                                       colormap = 'viridis'), 
             width = 4.5, height = 8, units = 'in')
      # gamma model
      ggsave(filename = file.path(base_folder, paste0('m5_spatial_SD_gamma_only.png')), 
             plot = plot_covariates_gg(data = pdat2 %>% 
                                          dplyr::mutate(lat = lat_unproj,
                                                        lon = lon_unproj) %>% 
                                          dplyr::group_by(lat, lon) %>% 
                                          dplyr::summarise(sd = sd(z.pred.g5), 
                                                           .groups = 'drop'),
                                       field = 'sd',
                                       legend.title = paste0(mn, '\nSD\n(across years)\nin Predicted\nBycatch'),
                                       colormap = 'viridis', 
                                       legend.color.range = plot.mean.range.h, 
                                       transformation = 'log1p', 
                                       legend.scale.breaks = plot.mean.legend.scale.breaks.h), 
             width = 4.5, height = 8, units = 'in')
      # hurdle model
      ggsave(filename = file.path(base_folder, paste0('m5_spatial_SD_hurdle_model.png')), 
             plot = plot_covariates_gg(data = pdat2 %>% 
                                          dplyr::mutate(lat = lat_unproj,
                                                        lon = lon_unproj) %>% 
                                          dplyr::group_by(lat, lon) %>% 
                                          dplyr::summarise(sd = sd(z.pred.h5), 
                                                           .groups = 'drop'),
                                       field = 'sd',
                                       legend.title = paste0(mn, '\nSD\n(across years)\nin Predicted\nBycatch'),
                                       colormap = 'viridis', 
                                       legend.color.range = plot.mean.range.h, 
                                       transformation = 'log1p', 
                                       legend.scale.breaks = plot.mean.legend.scale.breaks.h), 
             width = 4.5, height = 8, units = 'in')
   }
   
   # Datasets for marginal effects plots of fixed effects
   {
      # add a space in the list for each of them
      marginal_effect_list_b[[tolower(mn)]] <- vector('list', length(c.5))
      names(marginal_effect_list_b[[tolower(mn)]]) <- c.5
      marginal_effect_list_g[[tolower(mn)]] <- vector('list', length(c.5))
      names(marginal_effect_list_g[[tolower(mn)]]) <- c.5
      
      for(c in 1:length(c.5)){
         # I can also skip any covariates that end with "poly2" because polynomial predictors need to be considered together
         if( grepl(pattern = '.+_poly2$', x = c.5[c]) ) next
         
         # get the dataframe for the model predictions
         ndb <- ndg <- prediction_dfs[[c.5[c]]]
         
         # predict new values
         preds.b <- mgcv:::predict.gam(object = fit.b5, 
                                       newdata = ndb,
                                       type = 'link', 
                                       se.fit = TRUE) # can't predict on the response scale if I'm going to +/- SE to get CI [it WILL give SE on the response scale, but I can't use those to correctly calculate confidence limits]
         # gamma model
         preds.g <- mgcv:::predict.gam(object = fit.g5, 
                                       newdata = ndg,
                                       type = 'link', 
                                       se.fit = TRUE) # can't predict on the response scale if I'm going to +/- SE to get CI [it WILL give SE on the response scale, but I can't use those to correctly calculate confidence limits]
         
         # Get the predictions on the link scale
         z_link_b     <- preds.b$fit
         z_link_lcl_b <- preds.b$fit - 2 * preds.b$se.fit
         z_link_ucl_b <- preds.b$fit + 2 * preds.b$se.fit
         
         z_link_g     <- preds.g$fit
         z_link_lcl_g <- preds.g$fit - 2 * preds.g$se.fit
         z_link_ucl_g <- preds.g$fit + 2 * preds.g$se.fit
         
         
         # convert to response scale
         ndb$bycatch     <- fit.b5$family$linkinv(z_link_b)
         ndb$bycatch_lcl <- fit.b5$family$linkinv(z_link_lcl_b)
         ndb$bycatch_ucl <- fit.b5$family$linkinv(z_link_ucl_b)
         ndb$model <- mn
         
         # convert to response scale
         ndg$bycatch     <- fit.g5$family$linkinv(z_link_g)
         ndg$bycatch_lcl <- fit.g5$family$linkinv(z_link_lcl_g)
         ndg$bycatch_ucl <- fit.g5$family$linkinv(z_link_ucl_g)
         ndg$model <- mn
         
         # get the name of the covariate (but not "poly"). It would've been easier to just use "sub()"...
         ndb$covariate <- 
            ndg$covariate <- 
            ifelse(grepl('.+_poly1', c.5[c] ), 
                   lsp_lut[ lsp_lut$final_name == c.5[c], 'original_name' ], 
                   c.5[c])
         
         # save the dataframe
         marginal_effect_list_b[[tolower(mn)]][[c.5[c]]] <- ndb
         marginal_effect_list_g[[tolower(mn)]][[c.5[c]]] <- ndg
      }
   }
}

# predicting SD from random forest models using the 'ranger' package requires very large amounts of memory. 
# The only way I've gotten it to run on this laptop is to restart R and only run the RF model
# So I'll save everything up to here before restarting R
{
   # check how we're doing on memory
   data.frame('object' = ls()) %>% 
      dplyr::mutate(size_unit = object %>%sapply(. %>% get() %>% object.size %>% format(., unit = 'auto')),
                    size = as.numeric(sapply(strsplit(size_unit, split = ' '), FUN = function(x) x[1])),
                    unit = factor(sapply(strsplit(size_unit, split = ' '), FUN = function(x) x[2]), levels = c('Gb', 'Mb', 'Kb', 'bytes'))) %>% 
      dplyr::arrange(unit, dplyr::desc(size)) %>% 
      dplyr::select(-size_unit)
   
   # option 1 
   # remove a few items to free memory
   rm(list = ls()[grepl(pattern = 'fit\\.+.', x = ls())])
   rm(list = ls()[grepl(pattern = 'predictions\\.+.', x = ls())])
   
   # option 2 
   # save the important output: pdat, pdat2, marginal_effect_list_b & marginal_effect_list_g
   saveRDS(object = list(pdat = pdat,
                         pdat2 = pdat2,
                         marginal_effect_list_b = marginal_effect_list_b,
                         marginal_effect_list_g = marginal_effect_list_g), 
           file.path(base_folder, paste0('temp_model_1-5_output.RDS')))
   
   # option 3
   # save the whole working space and sort out which items I actually want later on
   # save.image(file = file.path(base_folder, paste0('workspace_image_after_models_1-5.RDS')))
   # load(file.path(base_folder, paste0('workspace_image_after_models_1-5.RDS')))
}

#####
# Restart R & run all code before "1. Fit GLM 1"
#####

##### 6. Fit RF BASE #####
{
   # covariates for the model-fitting function
   covar.6 <- rf.covar
   # covariates needed to use the model fit to make predictions
   c.6 <- rf.covar
   
   # fit binomial model
   fit.b6 <- fit_m6_rf1(data = bdat,
                        covariates  = covar.6,
                        modelfamily = 'binomial',
                        rows.train  = 1:nrow(bdat),
                        rows.test   = 1,
                        use.package = 'ranger',
                        keep.inbag = TRUE, # necessary for estimating SE
                        save.model.fit = TRUE)$model

   # fit the gamma model
   fit.g6 <- fit_m6_rf1(data = gdat,
                        covariates  = covar.6,
                        modelfamily = 'gamma',
                        rows.train  = 1:nrow(gdat),
                        rows.test   = 1,
                        use.package = 'ranger',
                        keep.inbag = TRUE, # necessary for estimating SE
                        quantile.forest = TRUE, # necessary for estimating quantiles
                        save.model.fit = TRUE)$model
   
   # binomial model predictions
   {
      # make predictions using grid1 prediction dataset
      pdat$z.pred.b6 <- ranger:::predict.ranger(object = fit.b6, 
                                                data = pdat[,c.6], 
                                                type = 'response')$predictions[, 2] # take the 2nd column for the binomial predictions
      # I'm not sure what the best way to get estimates of SD is for binomial random forest models. 
      # the help documents for ranger:::predict.ranger and quantregForest::quantregForest both say that SE is only estimated for regression forests, but 
      # then there are tutorials like this: https://d3amtssd1tejdt.cloudfront.net/2018/26693/1/GeoMLA_README_thengl.pdf
      # and this: https://github.com/thengl/GeoMLA/blob/master/RF_vs_kriging/R/RF_uncertainty_maps.R
      # that use ranger:::predict.ranger to get the SE for categorical variables...
      # for more background on CI of random forests: 
      # https://github.com/imbs-hl/ranger/issues/136 
      # Option 1 = use package ranger. (slow and will probably run out of memory)
      # Option 2 = use package ranger, but split prediction dataset up into smaller pieces to hopefully avoid running out of memory (even sloooowwwwer)
      # Option 3 = re-run the model as a regression tree forest using quantreforest
      
      # option 1
      if(TRUE){
         pdat$z.pred.b6.sd <- ranger:::predict.ranger(object = fit.b6,
                                                      data = pdat[,c.6],
                                                      type = 'se')$se[,2]
      }
      
      # option 2
      if(FALSE){
         # create empty column for the data
         pdat$z.pred.b6.sd <- NA
         
         # number of rows to included in each loop
         rows_per_loop <- 100
         # number of loops
         nloops <- ceiling(nrow(pdat) / rows_per_loop)
         
         # set up a for loop timer
         counter <- 1
         pb <- pbapply::startpb(0, nloops)
         on.exit(pbapply::closepb(pb))
         
         for(l in 1:nloops){
            # get the rows to be included in this loop
            rows_this_loop <- (1:rows_per_loop + ((l-1)*rows_per_loop))
            # make sure all the rows are actually the dataset
            rows_this_loop <- rows_this_loop[rows_this_loop < nrow(pdat)]
            
            # calculate the SE
            out <- ranger:::predict.ranger(object = fit.b6, 
                                           data = pdat[ rows_this_loop ,c.6],
                                           type = 'se')
            # add SE back into the dataset
            pdat$z.pred.b6.sd[rows_this_loop] <- out$se[,2] # use the 2nd column for binomial model
            
            # increment the for loop timer
            pbapply::setpb(pb, counter)
            counter <- counter + 1
         }
      }
         
      # option 3 (quantregforest)
      if(FALSE){
         # estimate the SE using the quantreg package (and assuming response is continuous, i.e. fitting a regression tree)
         fit.qrf6 <- quantregForest::quantregForest(x = bdat[, covar.6], # training data predictors
                                                    y = as.numeric(as.character(bdat[, 'bycatch'])), # training data response
                                                    # "the response has to be continuous. Binary or count responses are not allowed"
                                                    # xtest = dat[, covar.g6], # test data predictors [same as training data in this case]
                                                    # ytest = as.numeric(as.character(dat[, 'bycatch'])), # test data response [same as training data in this case]
                                                    mtry = 3,           # Number of variables randomly sampled as candidates at each split. Note that the default values are different for classification (sqrt(p) where p is number of variables in x) and regression (p/3)
                                                    ntree = 1500,       # Number of trees to grow. This should not be set to too small a number, to ensure that every input row gets predicted at least a few times.
                                                    importance = FALSE, # Should importance of predictors be assessed?
                                                    do.trace = 250,     # print updates every 250 trees
                                                    keep.forest = TRUE)
         pdat$z.pred.b6.sd <- quantregForest:::predict.quantregForest(object = fit.qrf6,
                                                                      newdata = pdat[,c.6],
                                                                      what = sd)
      }
      
      # make predictions using grid2 prediction dataset
      pdat2$z.pred.b6 <- ranger:::predict.ranger(object = fit.b6, 
                                                data = pdat2[,c.6], 
                                                type = 'response')$predictions[, 2] # take the 2nd column for the binomial predictions
      
      # the NOAA laptop can't handle methods 1 or 2. I didn't calculate SE for grid 2
      {
         # option 1
         if(FALSE){
            out <- ranger:::predict.ranger(object = fit.b6,
                                           data = pdat2[,c.6],
                                           type = 'se')
            pdat2$z.pred.b6.sd <- out$se[,2]
         }
         
         # option 2
         if(FALSE){
            # create empty column for the data
            pdat2$z.pred.b6.sd <- NA
            
            # number of rows to included in each loop
            rows_per_loop <- 100
            # number of loops
            nloops <- ceiling(nrow(pdat2) / rows_per_loop)
            
            # set up a for loop timer
            counter <- 1
            pb <- pbapply::startpb(0, nloops)
            on.exit(pbapply::closepb(pb))
            
            for(l in 1:nloops){
               # get the rows to be included in this loop
               rows_this_loop <- (1:rows_per_loop + ((l-1)*rows_per_loop))
               # make sure all the rows are actually the dataset
               rows_this_loop <- rows_this_loop[rows_this_loop < nrow(pdat2)]
               
               # calculate the SE
               out <- ranger:::predict.ranger(object = fit.b6, 
                                              data = pdat2[ rows_this_loop ,c.6],
                                              type = 'se')
               # add SE back into the dataset
               pdat2$z.pred.b6.sd[rows_this_loop] <- out$se[,2] # use the 2nd column for binomial model
               
               # increment the for loop timer
               pbapply::setpb(pb, counter)
               counter <- counter + 1
            }
         }
         
         # option 3 (quantregforest)
         if(FALSE){
            # estimate the SE using the quantreg package (and assuming response is continuous, i.e. fitting a regression tree)
            fit.qrf6 <- quantregForest::quantregForest(x = bdat[, covar.6], # training data predictors
                                                       y = as.numeric(as.character(bdat[, 'bycatch'])), # training data response
                                                       # "the response has to be continuous. Binary or count responses are not allowed"
                                                       # xtest = dat[, covar.g6], # test data predictors [same as training data in this case]
                                                       # ytest = as.numeric(as.character(dat[, 'bycatch'])), # test data response [same as training data in this case]
                                                       mtry = 3,           # Number of variables randomly sampled as candidates at each split. Note that the default values are different for classification (sqrt(p) where p is number of variables in x) and regression (p/3)
                                                       ntree = 1500,       # Number of trees to grow. This should not be set to too small a number, to ensure that every input row gets predicted at least a few times.
                                                       importance = FALSE, # Should importance of predictors be assessed?
                                                       do.trace = 250,     # print updates every 250 trees
                                                       keep.forest = TRUE)
            pdat2$z.pred.b6.sd <- quantregForest:::predict.quantregForest(object = fit.qrf6,
                                                                          newdata = pdat2[,c.6],
                                                                          what = sd)
         }
      }
      
      # model name for plot legends
      mn <- 'RF 1'
      
      # plot model predictions
      (plot.b6.mean <- plot_covariates_gg(data = pdat %>% 
                                             dplyr::mutate(lat = lat_unproj,
                                                           lon = lon_unproj), 
                                          field = 'z.pred.b6', 
                                          legend.title = paste0(mn, '\nPredicted\nProbability\nof Bycatch'),
                                          colormap = 'viridis',
                                          legend.color.range = plot.mean.range.b))
      ggsave(filename = file.path(base_folder, paste0('binomial_model_predictions_b6.png')), plot = plot.b6.mean, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.b6.mean)
      
      # plot SD of model predictions
      if(!is.null(pdat$z.pred.b6.sd)){
         (plot.b6.sd <- plot_covariates_gg(data = pdat %>%
                                              dplyr::mutate(lat = lat_unproj,
                                                            lon = lon_unproj),
                                           field = 'z.pred.b6.sd',
                                           legend.title = paste0(mn, '\nSD of\nPredicted\nBycatch\nProbability'),
                                           legend.color.range = plot.sd.range.b,
                                           colormap = 'magma'))
         ggsave(filename = file.path(base_folder, paste0('binomial_model_predictions_b6_sd.png')), plot = plot.b6.sd, width = 4.5, height = 8, units = 'in')
         # remove plot from RAM to save memory
         rm(plot.b6.sd)
      }
      
      # interactive plot
      if(FALSE){
         # first convert the data.frame to a raster
         b6.mean.r <- raster::rasterFromXYZ(xyz = pdat  %>% 
                                               dplyr::mutate(lat = lat_unproj,
                                                             lon = lon_unproj) %>% 
                                               dplyr::select(lon, lat, z.pred.b6),    
                                            crs = sp::CRS('+init=EPSG:4326'))
         # b6.sd.r   <- raster::rasterFromXYZ(xyz = pdat  %>% 
         #                                       dplyr::mutate(lat = lat_unproj,
         #                                                     lon = lon_unproj) %>% 
         #                                       dplyr::select(lon, lat, z.pred.b6.sd),
         #                                    crs = sp::CRS('+init=EPSG:4326'))
         
         # leaflet uses epgs:3857, so I need to project to that first 
         # (leaflet will do it automatically, but it can screw up the color palate, so I'll do it first)
         # to get rid of errors see: https://github.com/rstudio/leaflet/issues/224
         b6.mean.rl <- leaflet::projectRasterForLeaflet(b6.mean.r, method = 'bilinear')
         # b6.sd.rl   <- leaflet::projectRasterForLeaflet(b6.sd.r, method = 'bilinear')
         # the warnings actually come from raster package: raster::projectRaster(from = haul_count_r, crs = sp::CRS('+init=EPSG:3857'))
         
         # first create a color palatte function
         pal.b6m <- leaflet::colorNumeric(palette  = 'viridis', 
                                          domain   = range(na.omit(raster::values(b6.mean.rl))), 
                                          na.color = "transparent")
         # pal.b6sd <- leaflet::colorNumeric(palette  = 'magma', 
         #                                   domain   = range(na.omit(raster::values(b6.sd.rl))), 
         #                                   na.color = "transparent")
         
         # then create the plot
         plot_height_pix <- 900
         lf.b6m <- leaflet::leaflet(height = plot_height_pix) %>% 
            leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>% 
            leaflet::addRasterImage(x = b6.mean.rl, 
                                    colors = pal.b6m,
                                    project = FALSE, 
                                    opacity = 0.9,
                                    group = "Bycatch",
                                    layerId = "Bycatch") %>%
            leafem::addImageQuery(x = b6.mean.rl, 
                                  digits = 4,
                                  position = 'topleft',
                                  project = TRUE,
                                  layerId = "Bycatch") %>%
            leafem::addMouseCoordinates() %>%
            leaflet::addLegend(position = 'topright', 
                               title = paste0(mn, '</br>Predicted</br>Bycatch</br>Probability'), # need HTML coding for line breaks </br> , not R \n
                               pal = pal.b6m, 
                               values = raster::values(b6.mean.rl))
         # lf.b6m
         ## save to standalone .html
         save_leaflet(plot = lf.b6m, 
                      file = file.path(base_folder, 'leaflet', 'binomial_model_predictions_b6_mean.html'))
         
         # lf.b6sd <- leaflet::leaflet(height = plot_height_pix) %>% 
         #    leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>% 
         #    leaflet::addRasterImage(x = b6.sd.rl, 
         #                            colors = pal.b6sd,
         #                            project = FALSE, 
         #                            opacity = 0.9,
         #                            group = "Bycatch_SD",
         #                            layerId = "Bycatch_SD") %>%
         #    leafem::addImageQuery(x = b6.sd.rl, 
         #                          digits = 6,
         #                          position = 'topleft',
         #                          project = TRUE,
         #                          layerId = "Bycatch_SD") %>%
         #    leafem::addMouseCoordinates() %>%
         #    leaflet::addLegend(position = 'topright', 
         #                       title = paste0(mn, '</br>SD of</br>Predicted</br>Bycatch</br>Probability'), # need HTML coding for line breaks </br> , not R \n
         #                       pal = pal.b6sd, 
         #                       values = raster::values(b6.sd.rl))
         # # lf.b6sd
         # save_leaflet(plot = lf.b6sd, 
         #              file = file.path(base_folder, 'leaflet', 'binomial_model_predictions_b6_sd.html'))
         
         # mean and SD together
         # leafsync::sync(lf.b6m, lf.b6sd)
         # save_leafsync(tags = leafsync::sync(lf.b6m, lf.b6sd), 
         #               file = paste0(getwd(), '/figures/leaflet/binomial_model_predictions_b6_mean_and_sd_', short_long, '_',
         #                             ifelse(use_projected, 'projected', 'unprojected'), '.html'))
      }
   }
   
   # gamma model predictions
   {
      # make predictions using grid1 prediction dataset
      pdat$z.pred.g6 <- ranger:::predict.ranger(object = fit.g6, 
                                                data = pdat[,c.6], 
                                                type = 'response')$predictions
      # can estimate SD for regression trees
      pdat$z.pred.g6.sd <- ranger:::predict.ranger(object = fit.g6,
                                                   data = pdat[,c.6],
                                                   type = 'se',
                                                   verbose = TRUE)$se
       
      # make predictions using grid2 prediction dataset
      pdat2$z.pred.g6 <- ranger:::predict.ranger(object = fit.g6, 
                                                 data = pdat2[,c.6], 
                                                 type = 'response')$predictions
      
      # can estimate SD for regression trees
      if(FALSE){
         pdat2$z.pred.g6.sd <- ranger:::predict.ranger(object = fit.g6,
                                                       data = pdat2[,c.6],
                                                       type = 'se',
                                                       verbose = TRUE)$se
      }
      
      # plot model predictions
      (plot.g6.mean <- plot_covariates_gg(data = pdat %>% 
                                             dplyr::mutate(lat = lat_unproj,
                                                           lon = lon_unproj), 
                                          field = 'z.pred.g6', 
                                          legend.title = paste0(mn, '\nPredicted\nNumber\nof Bycatch'),
                                          colormap = 'viridis',
                                          legend.color.range = plot.mean.range.g, 
                                          transformation = 'log1p', 
                                          legend.scale.breaks = plot.mean.legend.scale.breaks.g))
      ggsave(filename = file.path(base_folder, paste0('gamma_model_predictions_g6.png')), plot = plot.g6.mean, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.g6.mean)
      
      # plot SD of model predictions
      (plot.g6.sd <- plot_covariates_gg(data = pdat %>% 
                                           dplyr::mutate(lat = lat_unproj,
                                                         lon = lon_unproj), 
                                        field = 'z.pred.g6.sd', 
                                        legend.title = paste0(mn, '\nSD of\nPredicted\nBycatch'), 
                                        legend.color.range = plot.sd.range.g,
                                        transformation = 'log1p', 
                                        legend.scale.breaks = plot.sd.legend.scale.breaks.g,
                                        colormap = 'magma'))
      ggsave(filename = file.path(base_folder, paste0('gamma_model_predictions_g6_sd.png')), plot = plot.g6.sd, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.g6.sd)
      
      # interactive plot
      if(FALSE){
         # first convert the data.frame to a raster
         g6.mean.r <- raster::rasterFromXYZ(xyz = pdat  %>% 
                                               dplyr::mutate(lat = lat_unproj,
                                                             lon = lon_unproj) %>% 
                                               dplyr::rowwise() %>% 
                                               dplyr::mutate(z.pred.g6 = ifelse(z.pred.g6 > plot.mean.range.g[2], plot.mean.range.g[2], z.pred.g6)) %>% 
                                               dplyr::ungroup() %>% 
                                               dplyr::select(lon, lat, z.pred.g6),    
                                            crs = sp::CRS('+init=EPSG:4326'))
         g6.sd.r   <- raster::rasterFromXYZ(xyz = pdat  %>% 
                                               dplyr::mutate(lat = lat_unproj,
                                                             lon = lon_unproj) %>% 
                                               dplyr::rowwise() %>% 
                                               dplyr::mutate(z.pred.g6.sd = ifelse(z.pred.g6.sd > plot.sd.range.g[2], plot.sd.range.g[2], z.pred.g6.sd)) %>% 
                                               dplyr::ungroup() %>% 
                                               dplyr::select(lon, lat, z.pred.g6.sd),
                                            crs = sp::CRS('+init=EPSG:4326'))
         
         # leaflet uses epgs:3857, so I need to project to that first 
         # (leaflet will do it automatically, but it can screw up the color palate, so I'll do it first)
         # to get rid of errors see: https://github.com/rstudio/leaflet/issues/224
         g6.mean.rl <- leaflet::projectRasterForLeaflet(g6.mean.r, method = 'bilinear')
         g6.sd.rl   <- leaflet::projectRasterForLeaflet(g6.sd.r, method = 'bilinear')
         # the warnings actually come from raster package: raster::projectRaster(from = haul_count_r, crs = sp::CRS('+init=EPSG:3857'))
         
         # first create a color palatte function
         pal.g6m <- leaflet::colorNumeric(palette  = 'viridis', 
                                          domain   =  range(na.omit(raster::values(g6.mean.rl))), 
                                          na.color = "transparent")
         pal.g6sd <- leaflet::colorNumeric(palette  = 'magma', 
                                           domain   = range(na.omit(raster::values(g6.sd.rl))), 
                                           na.color = "transparent")
         
         # then create the plot
         plot_height_pix <- 900
         lf.g6m <- leaflet::leaflet(height = plot_height_pix) %>% 
            leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>% 
            leaflet::addRasterImage(x = g6.mean.rl, 
                                    colors = pal.g6m,
                                    project = FALSE, 
                                    opacity = 0.9,
                                    group = "Bycatch",
                                    layerId = "Bycatch") %>%
            leafem::addImageQuery(x = g6.mean.rl, 
                                  digits = 4,
                                  position = 'topleft',
                                  project = TRUE,
                                  layerId = "Bycatch") %>%
            leafem::addMouseCoordinates() %>%
            leaflet::addLegend(position = 'topright', 
                               title = paste0(mn,'</br>Predicted</br>Bycatch'), # need HTML coding for line breaks </br> , not R \n
                               pal = pal.g6m, 
                               values = raster::values(g6.mean.rl))
         # lf.g6m
         ## save to standalone .html
         save_leaflet(plot = lf.g6m, 
                      file = file.path(base_folder, 'leaflet', 'gamma_model_predictions_g6_mean.html'))
         
         lf.g6sd <- leaflet::leaflet(height = plot_height_pix) %>% 
            leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>% 
            leaflet::addRasterImage(x = g6.sd.rl, 
                                    colors = pal.g6sd,
                                    project = FALSE, 
                                    opacity = 0.9,
                                    group = "Bycatch_SD",
                                    layerId = "Bycatch_SD") %>%
            leafem::addImageQuery(x = g6.sd.rl, 
                                  digits = 6,
                                  position = 'topleft',
                                  project = TRUE,
                                  layerId = "Bycatch_SD") %>%
            leafem::addMouseCoordinates() %>%
            leaflet::addLegend(position = 'topright', 
                               title = paste0(mn,'</br>SD of</br>Predicted</br>Bycatch'), # need HTML coding for line breaks </br> , not R \n
                               pal = pal.g6sd, 
                               values = raster::values(g6.sd.rl))
         # lf.g6sd
         save_leaflet(plot = lf.g6sd, 
                      file = file.path(base_folder, 'leaflet', 'gamma_model_predictions_g6_sd.html'))
         
         # mean and SD together
         # leafsync::sync(lf.g6m, lf.g6sd)
         # save_leafsync(tags = leafsync::sync(lf.g6m, lf.g6sd), 
         #               file = paste0(getwd(), '/figures/leaflet/gamma_model_predictions_g6_mean_and_sd_', short_long, '_',
         #                             ifelse(use_projected, 'projected', 'unprojected'), '.html'))
      }
   }
   
   # combined hurdle model predictions
   {
      pdat <- pdat %>% 
         dplyr::mutate(z.pred.h6 = z.pred.b6 * z.pred.g6)
      pdat2 <- pdat2 %>% 
         dplyr::mutate(z.pred.h6 = z.pred.b6 * z.pred.g6)
      
      # plot model predictions
      (plot.h6.mean <- plot_covariates_gg(data = pdat %>%
                                             dplyr::mutate(lat = lat_unproj,
                                                           lon = lon_unproj),
                                          field = 'z.pred.h6',
                                          legend.title = paste0(mn, '\nPredicted\nNumber\nof Bycatch'),
                                          colormap = 'viridis',
                                          legend.color.range = plot.mean.range.h, 
                                          transformation = 'log1p', 
                                          legend.scale.breaks = plot.mean.legend.scale.breaks.h))
      ggsave(filename = file.path(base_folder, paste0('hurdle_model_predictions_h6.png')), plot = plot.h6.mean, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.h6.mean)
      
      # plot of spatial effect through time
      # This one does change every year, so I'll plot more years 
      plot.h6.space <- plot_covariates_gg(data = pdat2 %>%
                                             dplyr::mutate(lat = lat_unproj,
                                                           lon = lon_unproj) %>% 
                                             dplyr::filter(year_f %in% c(2002:2019)),
                                          field = 'z.pred.h6',
                                          legend.title = paste0(mn, '\nPredicted\nNumber\nof Bycatch'),
                                          colormap = 'viridis',
                                          legend.color.range = plot.mean.range.h, 
                                          transformation = 'log1p', 
                                          legend.scale.breaks = plot.mean.legend.scale.breaks.h,
                                          facet_field = 'year_f', 
                                          facet_rows = 3)
      ggsave(filename = file.path(base_folder, paste0('m6_spatial_predictions.png')), plot = plot.h6.space, width = 7.5, height = 9, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.h6.space)
   }
   
   # Is there any interaction between year and location? 
   {
      pdat2 %>% 
         dplyr::group_by(lat, lon) %>% 
         dplyr::summarise(range.b = diff(range(z.pred.b6)),
                          range.g = diff(range(z.pred.g6)),
                          .groups = 'drop') %>% 
         ggplot(., aes(x = range.g)) + geom_histogram(bins = 100)
      
      pdat2 %>% 
         dplyr::group_by(lat, lon) %>% 
         dplyr::summarise(sd.b = sd(z.pred.b6),
                          sd.g = sd(z.pred.g6),
                          .groups = 'drop') %>% 
         ggplot(., aes(x = sd.g)) + geom_histogram(bins = 100)
      # not too much in the binomial model; more in the gamma model.
      
      # can also map the SD to show areas that change the most from year to year
      ggsave(filename = file.path(base_folder, paste0('m6_spatial_SD_binomial_only.png')), 
             plot = plot_covariates_gg(data = pdat2 %>% 
                                          dplyr::mutate(lat = lat_unproj,
                                                        lon = lon_unproj) %>% 
                                          dplyr::group_by(lat, lon) %>% 
                                          dplyr::summarise(sd = sd(z.pred.b6), 
                                                           .groups = 'drop'),
                                       field = 'sd',
                                       legend.title = paste0(mn, '\nSD\n(across years)\nin Predicted\nProbability\nof Bycatch'),
                                       colormap = 'viridis'), 
             width = 4.5, height = 8, units = 'in')
      # gamma model
      ggsave(filename = file.path(base_folder, paste0('m6_spatial_SD_gamma_only.png')), 
             plot = plot_covariates_gg(data = pdat2 %>% 
                                          dplyr::mutate(lat = lat_unproj,
                                                        lon = lon_unproj) %>% 
                                          dplyr::group_by(lat, lon) %>% 
                                          dplyr::summarise(sd = sd(z.pred.g6), 
                                                           .groups = 'drop'),
                                       field = 'sd',
                                       legend.title = paste0(mn, '\nSD\n(across years)\nin Predicted\nBycatch'),
                                       colormap = 'viridis'), 
             width = 4.5, height = 8, units = 'in')
      # hurdle model
      ggsave(filename = file.path(base_folder, paste0('m6_spatial_SD_hurdle_model.png')), 
             plot = plot_covariates_gg(data = pdat2 %>% 
                                          dplyr::mutate(lat = lat_unproj,
                                                        lon = lon_unproj) %>% 
                                          dplyr::group_by(lat, lon) %>% 
                                          dplyr::summarise(sd = sd(z.pred.h6), 
                                                           .groups = 'drop'),
                                       field = 'sd',
                                       legend.title = paste0(mn, '\nSD\n(across years)\nin Predicted\nBycatch'),
                                       colormap = 'viridis'), 
             width = 4.5, height = 8, units = 'in')
   }
   
   # save pdat to memory to help clear memory
   # saveRDS(pdat, file = file.path(base_folder, paste0('temp_pdat_aftermodel6.rds')))
   # rm(pdat)
   
   # Datasets for marginal effects plots of fixed effects
   {
      # add a space in the list for each of them
      marginal_effect_list_b[[mn]] <- vector('list', length(c.6))
      names(marginal_effect_list_b[[mn]]) <- c.6
      marginal_effect_list_g[[mn]] <- vector('list', length(c.6))
      names(marginal_effect_list_g[[mn]]) <- c.6
      
      
      for(c in 1:length(c.6)){
         # I can also skip any covariates that end with "poly2" because polynomial predictors need to be considered together
         if( grepl(pattern = '.+_poly2$', x = c.6[c]) ) next
         
         # get the dataframe for the model predictions
         ndb <- ndg <- prediction_dfs[[c.6[c]]]
         
         # predict new values
         preds.b <- ranger:::predict.ranger(object = fit.b6,
                                            data = ndb,
                                            type = 'response')$predictions[, 2] # 2nd column for binomial predictions
            
         # gamma model
         preds.g <- ranger:::predict.ranger(object = fit.g6, 
                                            data = ndg, 
                                            type = 'response')$predictions
         # can estimate quantiles for regression trees
         preds.g.quantiles <- ranger:::predict.ranger(object = fit.g6,
                                                      data = ndg,
                                                      type = 'quantiles', 
                                                      quantiles = c(0.025, 0.975),
                                                      verbose = FALSE)

         # add the predictions into the nd object
         # ndb$z_link     <- preds.b$fit
         # ndb$z_link_lcl <- preds.b$fit - 2 * preds.b$se.fit
         # ndb$z_link_ucl <- preds.b$fit + 2 * preds.b$se.fit
         # 
         # ndg$z_link     <- preds.g$fit
         # ndg$z_link_lcl <- preds.g$fit - 2 * preds.g$se.fit
         # ndg$z_link_ucl <- preds.g$fit + 2 * preds.g$se.fit
         
         
         # convert to response scale
         ndb$bycatch     <- preds.b
         ndb$bycatch_lcl <- NA
         ndb$bycatch_ucl <- NA
         ndb$model <- mn
         
         # convert to response scale
         ndg$bycatch     <- preds.g
         ndg$bycatch_lcl <- preds.g.quantiles$predictions[,1]
         ndg$bycatch_ucl <- preds.g.quantiles$predictions[,2]
         ndg$model <- mn
         
         # get the name of the covariate (but not "poly"). It would've been easier to just use "sub()"...
         ndb$covariate <- 
            ndg$covariate <- 
            ifelse(grepl('.+_poly1', c.6[c] ), 
                   lsp_lut[ lsp_lut$final_name == c.6[c], 'original_name' ], 
                   c.6[c])
         
         # save the dataframe
         marginal_effect_list_b[[mn]][[c.6[c]]] <- ndb
         marginal_effect_list_g[[mn]][[c.6[c]]] <- ndg
      }
   }
}

# free up memory
{
   # R is running out of memory while estimating SD of model 7. Here are some ways to help prevent that.
   
   # option 1 
   # remove a few items to free memory
   rm(list = ls()[grepl(pattern = 'fit\\.+.', x = ls())])
   rm(list = ls()[grepl(pattern = 'predictions\\.+.', x = ls())])
   
   
   # option 2 
   # save output from model 6 and restart r before running model 7
   # save pdat, pdat2, marginal_effect_list_b & marginal_effect_list_g with just model 6 predictions
   saveRDS(object = list(pdat = pdat,
                         pdat2 = pdat2,
                         marginal_effect_list_b = marginal_effect_list_b,
                         marginal_effect_list_g = marginal_effect_list_g), 
           file.path(base_folder, paste0('temp_model_6_output.RDS')))
   
   # pdat2 <- readRDS(file.path(base_folder, paste0('temp_model_6_output.RDS')))$pdat2
}

#####
# Restart R & run all code before "1. Fit GLM 1"
#####

##### 7. Random Forest (with down-sampling of the over-represented class (no bycatch) in the Binomial model ONLY) #####
{
   # covariates for the model-fitting function
   covar.7 <- rf.covar
   # covariates needed to use the model fit to make predictions
   c.7 <- rf.covar
   
   # fit binomial model
   fit.b7 <- fit_m7_rf2(data = bdat,
                        covariates  = covar.7,
                        modelfamily = 'binomial',
                        rows.train  = 1:nrow(bdat),
                        rows.test   = 1,
                        use.package = 'ranger',
                        keep.inbag = TRUE, # necessary for estimating SE
                        save.model.fit = TRUE)$model
   
   # binomial model predictions
   {
      # make predictions using grid1 prediction dataset
      pdat$z.pred.b7 <- ranger:::predict.ranger(object = fit.b7, 
                                                data = pdat[,c.7], 
                                                type = 'response')$predictions[, 2] # take the 2nd column for the binomial predictions
      # I'm not sure what the best way to get estimates of SD is for binomial random forest models. 
      # the help documents for ranger:::predict.ranger and quantregForest::quantregForest both say that SE is only estimated for regression forests, but 
      # then there are tutorials like this: https://d3amtssd1tejdt.cloudfront.net/2018/26693/1/GeoMLA_README_thengl.pdf
      # and this: https://github.com/thengl/GeoMLA/blob/master/RF_vs_kriging/R/RF_uncertainty_maps.R
      # that use ranger:::predict.ranger to get the SE for categorical variables...
      # for more background on CI of random forests: 
      # https://github.com/imbs-hl/ranger/issues/136 
      # Option 1 = use package ranger. (slow and will probably run out of memory)
      # Option 2 = use package ranger, but split prediction dataset up into smaller pieces to hopefully avoid running out of memory (even sloooowwwwer)
      # Option 3 = re-run the model as a regression tree forest using quantreforest
      
      # take pdat2 out of memory to save a little more space
      # saveRDS(object = pdat2, file = file.path(base_folder, paste0('temp_pdat2.RDS'))); rm(pdat2)
      
      # option 1
      if(TRUE){
         pdat$z.pred.b7.sd <- ranger:::predict.ranger(object = fit.b7,
                                                      data = pdat[,c.7],
                                                      type = 'se')$se[,2]
      }
      
      # option 2
      if(FALSE){
         # create empty column for the data
         pdat$z.pred.b7.sd <- NA
         
         # number of rows to included in each loop
         rows_per_loop <- 100
         # number of loops
         nloops <- ceiling(nrow(pdat) / rows_per_loop)
         
         # set up a for loop timer
         counter <- 1
         pb <- pbapply::startpb(0, nloops)
         on.exit(pbapply::closepb(pb))
         
         for(l in 1:nloops){
            # get the rows to be included in this loop
            rows_this_loop <- (1:rows_per_loop + ((l-1)*rows_per_loop))
            # make sure all the rows are actually the dataset
            rows_this_loop <- rows_this_loop[rows_this_loop < nrow(pdat)]
            
            # calculate the SE
            out <- ranger:::predict.ranger(object = fit.b7, 
                                           data = pdat[ rows_this_loop ,c.7],
                                           type = 'se')
            # add SE back into the dataset
            pdat$z.pred.b7.sd[rows_this_loop] <- out$se[,2] # use the 2nd column for binomial model
            
            # increment the for loop timer
            pbapply::setpb(pb, counter)
            counter <- counter + 1
         }
      }
      
      # option 3 (quantregforest)
      if(FALSE){
         # estimate the SE using the quantreg package (and assuming response is continuous, i.e. fitting a regression tree)
         fit.qrf7 <- quantregForest::quantregForest(sampsize = rep(round(nmin/3),2), 
                                                    x = bdat[, covar.7], # training data predictors
                                                    y = as.numeric(as.character(bdat[, 'bycatch'])), # training data response
                                                    # "the response has to be continuous. Binary or count responses are not allowed"
                                                    # xtest = dat[, covar.g7], # test data predictors [same as training data in this case]
                                                    # ytest = as.numeric(as.character(dat[, 'bycatch'])), # test data response [same as training data in this case]
                                                    mtry = 3,           # Number of variables randomly sampled as candidates at each split. Note that the default values are different for classification (sqrt(p) where p is number of variables in x) and regression (p/3)
                                                    ntree = 1500,       # Number of trees to grow. This should not be set to too small a number, to ensure that every input row gets predicted at least a few times.
                                                    importance = FALSE, # Should importance of predictors be assessed?
                                                    do.trace = 250,     # print updates every 250 trees
                                                    keep.forest = TRUE)
         pdat$z.pred.b7.sd <- quantregForest:::predict.quantregForest(object = fit.qrf7,
                                                                      newdata = pdat[,c.7],
                                                                      what = sd)
      }
      
      # read back in pdat2
      # if( !('pdat2' %in% ls())) pdat2 <- readRDS(file.path(base_folder, paste0('temp_pdat2.RDS')))
      
      
      # make predictions using grid2 prediction dataset
      pdat2$z.pred.b7 <- ranger:::predict.ranger(object = fit.b7, 
                                                 data = pdat2[,c.7], 
                                                 type = 'response')$predictions[, 2] # take the 2nd column for the binomial predictions
      
      # the NOAA laptop can't handle methods 1 or 2. I didn't calculate SE for grid 2
      {
         # option 1
         if(FALSE){
            out <- ranger:::predict.ranger(object = fit.b7,
                                           data = pdat2[,c.7],
                                           type = 'se')
            pdat2$z.pred.b7.sd <- out$se[,2]
         }
         
         # option 2
         if(FALSE){
            # create empty column for the data
            pdat2$z.pred.b7.sd <- NA
            
            # number of rows to included in each loop
            rows_per_loop <- 100
            # number of loops
            nloops <- ceiling(nrow(pdat2) / rows_per_loop)
            
            # set up a for loop timer
            counter <- 1
            pb <- pbapply::startpb(0, nloops)
            on.exit(pbapply::closepb(pb))
            
            for(l in 1:nloops){
               # get the rows to be included in this loop
               rows_this_loop <- (1:rows_per_loop + ((l-1)*rows_per_loop))
               # make sure all the rows are actually the dataset
               rows_this_loop <- rows_this_loop[rows_this_loop < nrow(pdat2)]
               
               # calculate the SE
               out <- ranger:::predict.ranger(object = fit.b7, 
                                              data = pdat2[ rows_this_loop ,c.7],
                                              type = 'se')
               # add SE back into the dataset
               pdat2$z.pred.b7.sd[rows_this_loop] <- out$se[,2] # use the 2nd column for binomial model
               
               # increment the for loop timer
               pbapply::setpb(pb, counter)
               counter <- counter + 1
            }
         }
         
         # option 3 (quantregforest)
         if(FALSE){
            
            # estimate the SE using the quantreg package (and assuming response is continuous, i.e. fitting a regression tree)
            fit.qrf7 <- quantregForest::quantregForest(sampsize = rep(round(nmin/3),2), 
                                                       x = bdat[, covar.7], # training data predictors
                                                       y = as.numeric(as.character(bdat[, 'bycatch'])), # training data response
                                                       # "the response has to be continuous. Binary or count responses are not allowed"
                                                       # xtest = dat[, covar.g7], # test data predictors [same as training data in this case]
                                                       # ytest = as.numeric(as.character(dat[, 'bycatch'])), # test data response [same as training data in this case]
                                                       mtry = 3,           # Number of variables randomly sampled as candidates at each split. Note that the default values are different for classification (sqrt(p) where p is number of variables in x) and regression (p/3)
                                                       ntree = 1500,       # Number of trees to grow. This should not be set to too small a number, to ensure that every input row gets predicted at least a few times.
                                                       importance = FALSE, # Should importance of predictors be assessed?
                                                       do.trace = 250,     # print updates every 250 trees
                                                       keep.forest = TRUE)
            pdat2$z.pred.b7.sd <- quantregForest:::predict.quantregForest(object = fit.qrf7,
                                                                          newdata = pdat2[,c.7],
                                                                          what = sd)
         }
      }
      
      # model name for plot legends
      mn <- 'RF 2'
      
      # plot model predictions
      (plot.b7.mean <- plot_covariates_gg(data = pdat %>% 
                                             dplyr::mutate(lat = lat_unproj,
                                                           lon = lon_unproj), 
                                          field = 'z.pred.b7', 
                                          legend.title = paste0(mn, '\nPredicted\nProbability\nof Bycatch'),
                                          colormap = 'viridis',
                                          legend.color.range = plot.mean.range.b))
      ggsave(filename = file.path(base_folder, paste0('binomial_model_predictions_b7.png')), plot = plot.b7.mean, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.b7.mean)
      
      # plot SD of model predictions
      if( !is.null(pdat$z.pred.b7.sd) ){
         (plot.b7.sd <- plot_covariates_gg(data = pdat %>%
                                              dplyr::mutate(lat = lat_unproj,
                                                            lon = lon_unproj),
                                           field = 'z.pred.b7.sd',
                                           legend.title = paste0(mn, '\nSD of\nPredicted\nBycatch\nProbability'),
                                           legend.color.range = plot.sd.range.b,
                                           colormap = 'magma'))
         ggsave(filename = file.path(base_folder, paste0('binomial_model_predictions_b7_sd.png')), plot = plot.b7.sd, width = 4.5, height = 8, units = 'in')
         # remove plot from RAM to save memory
         rm(plot.b7.sd)
      }
      
      # interactive plot
      if(FALSE){
         # first convert the data.frame to a raster
         b7.mean.r <- raster::rasterFromXYZ(xyz = pdat  %>% 
                                               dplyr::mutate(lat = lat_unproj,
                                                             lon = lon_unproj) %>% 
                                               dplyr::select(lon, lat, z.pred.b7),    
                                            crs = sp::CRS('+init=EPSG:4326'))
         # b7.sd.r   <- raster::rasterFromXYZ(xyz = pdat  %>% 
         #                                       dplyr::mutate(lat = lat_unproj,
         #                                                     lon = lon_unproj) %>% 
         #                                       dplyr::select(lon, lat, z.pred.b7.sd),
         #                                    crs = sp::CRS('+init=EPSG:4326'))
         
         # leaflet uses epgs:3857, so I need to project to that first 
         # (leaflet will do it automatically, but it can screw up the color palate, so I'll do it first)
         # to get rid of errors see: https://github.com/rstudio/leaflet/issues/224
         b7.mean.rl <- leaflet::projectRasterForLeaflet(b7.mean.r, method = 'bilinear')
         # b7.sd.rl   <- leaflet::projectRasterForLeaflet(b7.sd.r, method = 'bilinear')
         # the warnings actually come from raster package: raster::projectRaster(from = haul_count_r, crs = sp::CRS('+init=EPSG:3857'))
         
         # first create a color palatte function
         pal.b7m <- leaflet::colorNumeric(palette  = 'viridis', 
                                          domain   = range(na.omit(raster::values(b7.mean.rl))), 
                                          na.color = "transparent")
         # pal.b7sd <- leaflet::colorNumeric(palette  = 'magma', 
         #                                   domain   = range(na.omit(raster::values(b7.sd.rl))), 
         #                                   na.color = "transparent")
         
         # then create the plot
         plot_height_pix <- 900
         lf.b7m <- leaflet::leaflet(height = plot_height_pix) %>% 
            leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>% 
            leaflet::addRasterImage(x = b7.mean.rl, 
                                    colors = pal.b7m,
                                    project = FALSE, 
                                    opacity = 0.9,
                                    group = "Bycatch",
                                    layerId = "Bycatch") %>%
            leafem::addImageQuery(x = b7.mean.rl, 
                                  digits = 4,
                                  position = 'topleft',
                                  project = TRUE,
                                  layerId = "Bycatch") %>%
            leafem::addMouseCoordinates() %>%
            leaflet::addLegend(position = 'topright', 
                               title = paste0(mn, '</br>Predicted</br>Bycatch</br>Probability'), # need HTML coding for line breaks </br> , not R \n
                               pal = pal.b7m, 
                               values = raster::values(b7.mean.rl))
         # lf.b7m
         ## save to standalone .html
         save_leaflet(plot = lf.b7m, 
                      file = file.path(base_folder, 'leaflet', 'binomial_model_predictions_b7_mean.html'))
         
         # lf.b7sd <- leaflet::leaflet(height = plot_height_pix) %>% 
         #    leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>% 
         #    leaflet::addRasterImage(x = b7.sd.rl, 
         #                            colors = pal.b7sd,
         #                            project = FALSE, 
         #                            opacity = 0.9,
         #                            group = "Bycatch_SD",
         #                            layerId = "Bycatch_SD") %>%
         #    leafem::addImageQuery(x = b7.sd.rl, 
         #                          digits = 6,
         #                          position = 'topleft',
         #                          project = TRUE,
         #                          layerId = "Bycatch_SD") %>%
         #    leafem::addMouseCoordinates() %>%
         #    leaflet::addLegend(position = 'topright', 
         #                       title = paste0(mn, '</br>SD of</br>Predicted</br>Bycatch</br>Probability'), # need HTML coding for line breaks </br> , not R \n
         #                       pal = pal.b7sd, 
         #                       values = raster::values(b7.sd.rl))
         # # lf.b7sd
         # save_leaflet(plot = lf.b7sd, 
         #              file = file.path(base_folder, 'leaflet', 'binomial_model_predictions_b7_sd.html'))
         
         # mean and SD together
         # leafsync::sync(lf.b7m, lf.b7sd)
         # save_leafsync(tags = leafsync::sync(lf.b7m, lf.b7sd), 
         #               file = paste0(getwd(), '/figures/leaflet/binomial_model_predictions_b7_mean_and_sd_', short_long, '_',
         #                             ifelse(use_projected, 'projected', 'unprojected'), '.html'))
      }
   }
   
   # spatial component of binomial model only
   {
      # plot of spatial effect through time
      # This one does change every year, so I'll plot more years 
      plot.h7.space <- plot_covariates_gg(data = pdat2 %>%
                                             dplyr::mutate(lat = lat_unproj,
                                                           lon = lon_unproj) %>% 
                                             dplyr::filter(year_f %in% c(2002:2019)),
                                          field = 'z.pred.b7',
                                          legend.title = paste0(mn, '\nPredicted\nProbability\nof Bycatch'),
                                          colormap = 'viridis',
                                          legend.color.range = plot.mean.range.b, 
                                          # transformation = 'log1p', 
                                          # legend.scale.breaks = plot.mean.legend.scale.breaks.,
                                          facet_field = 'year_f', 
                                          facet_rows = 3)
      ggsave(filename = file.path(base_folder, paste0('m7_spatial_predictions_binomial_only.png')), plot = plot.h7.space, width = 7.5, height = 9, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.h7.space)
   }
   
   # Is there any interaction between year and location? 
   {
      pdat2 %>% 
         dplyr::group_by(lat, lon) %>% 
         dplyr::summarise(range = diff(range(z.pred.b7))) %>% 
         ggplot(., aes(x = range)) + geom_histogram(bins = 100)
      
      pdat2 %>% 
         dplyr::group_by(lat, lon) %>% 
         dplyr::summarise(sd = sd(z.pred.b7)) %>% 
         ggplot(., aes(x = sd)) + geom_histogram(bins = 100)
      # not too much, but some.
      
      # can also map the SD to show areas that change the most from year to year
      ggsave(filename = file.path(base_folder, paste0('m7_spatial_SD_binomial_only.png')), 
             plot = plot_covariates_gg(data = pdat2 %>% 
                                          dplyr::mutate(lat = lat_unproj,
                                                        lon = lon_unproj) %>% 
                                          dplyr::group_by(lat, lon) %>% 
                                          dplyr::summarise(sd = sd(z.pred.b7), 
                                                           .groups = 'drop'),
                                       field = 'sd',
                                       legend.title = paste0(mn, '\nSD\n(across years)\nin Predicted\nProbability\nof Bycatch'),
                                       colormap = 'viridis'), 
             width = 4.5, height = 8, units = 'in')
   }
   
   # Datasets for marginal effects plots of fixed effects
   {
      # add a space in the list for each of them
      marginal_effect_list_b[[mn]] <- vector('list', length(c.7))
      names(marginal_effect_list_b[[mn]]) <- c.7
      
      for(c in 1:length(c.7)){
         # I can also skip any covariates that end with "poly2" because polynomial predictors need to be considered together
         if( grepl(pattern = '.+_poly2$', x = c.7[c]) ) next
         
         # get the dataframe for the model predictions
         ndb <- ndg <- prediction_dfs[[c.7[c]]]
         
         # predict new values
         preds.b <- ranger:::predict.ranger(object = fit.b7,
                                            data = ndb,
                                            type = 'response')$predictions[, 2] # 2nd column for binomial predictions
         
         # convert to response scale
         ndb$bycatch     <- preds.b
         ndb$bycatch_lcl <- NA
         ndb$bycatch_ucl <- NA
         ndb$model <- mn
         
         # get the name of the covariate (but not "poly"). It would've been easier to just use "sub()"...
         ndb$covariate <- 
            ifelse(grepl('.+_poly1', c.7[c] ), 
                   lsp_lut[ lsp_lut$final_name == c.7[c], 'original_name' ], 
                   c.7[c])
         
         # save the dataframe
         marginal_effect_list_b[[mn]][[c.7[c]]] <- ndb
      }
   }
}

# free up memory
{
   # R is running out of memory while estimating SD of model 7. Here are some ways to help prevent that.
   
   # option 1 
   # remove a few items to free memory
   rm(list = ls()[grepl(pattern = 'fit\\.+.', x = ls())])
   rm(list = ls()[grepl(pattern = 'predictions\\.+.', x = ls())])
   
   
   # option 2 
   # save output from model 6 and restart r before running model 7
   # save pdat, pdat2, marginal_effect_list_b & marginal_effect_list_g with just model 6 predictions
   saveRDS(object = list(pdat = pdat,
                         pdat2 = pdat2,
                         marginal_effect_list_b = marginal_effect_list_b,
                         marginal_effect_list_g = marginal_effect_list_g), 
           file.path(base_folder, paste0('temp_model_7_output.RDS')))
}

#####
# Restart R & run all code before "1. Fit GLM 1"
#####

##### 8. Random Forest (with Synthetic Minority (no bycatch) Over-sampling in the Binomial component ONLY) #####
{
   covar.8 <- rf.covar
   c.8 <- rf.covar
   
   # fit binomial model
   fit.b8 <- fit_m8_rf3(data = bdat,
                        covariates  = covar.8,
                        modelfamily = 'binomial',
                        rows.train  = 1:nrow(bdat),
                        rows.test   = 1,
                        use.package = 'ranger',
                        keep.inbag = TRUE, # necessary for estimating SE
                        save.model.fit = TRUE)$model
   
   # binomial model predictions
   {
      # make predictions using grid1 prediction dataset
      pdat$z.pred.b8 <- ranger:::predict.ranger(object = fit.b8, 
                                                data = pdat[,c.8], 
                                                type = 'response')$predictions[, 2] # take the 2nd column for the binomial predictions
      # I'm not sure what the best way to get estimates of SD is for binomial random forest models. 
      # the help documents for ranger:::predict.ranger and quantregForest::quantregForest both say that SE is only estimated for regression forests, but 
      # then there are tutorials like this: https://d3amtssd1tejdt.cloudfront.net/2018/26693/1/GeoMLA_README_thengl.pdf
      # and this: https://github.com/thengl/GeoMLA/blob/master/RF_vs_kriging/R/RF_uncertainty_maps.R
      # that use ranger:::predict.ranger to get the SE for categorical variables...
      # for more background on CI of random forests: 
      # https://github.com/imbs-hl/ranger/issues/136 
      # Option 1 = use package ranger. (slow and will probably run out of memory)
      # Option 2 = use package ranger, but split prediction dataset up into smaller pieces to hopefully avoid running out of memory (even sloooowwwwer)
      # Option 3 = re-run the model as a regression tree forest using quantreforest
      
      # take pdat2 out of memory to save a little more space
      # saveRDS(object = pdat2, file = file.path(base_folder, paste0('temp_pdat2.RDS'))); rm(pdat2)
      
      # option 1
      if(TRUE){
         pdat$z.pred.b8.sd <- ranger:::predict.ranger(object = fit.b8,
                                                      data = pdat[,c.8],
                                                      type = 'se')$se[,2]
      }
      
      # option 2
      if(FALSE){
         # create empty column for the data
         pdat$z.pred.b8.sd <- NA
         
         # number of rows to included in each loop
         rows_per_loop <- 100
         # number of loops
         nloops <- ceiling(nrow(pdat) / rows_per_loop)
         
         # set up a for loop timer
         counter <- 1
         pb <- pbapply::startpb(0, nloops)
         on.exit(pbapply::closepb(pb))
         
         for(l in 1:nloops){
            # get the rows to be included in this loop
            rows_this_loop <- (1:rows_per_loop + ((l-1)*rows_per_loop))
            # make sure all the rows are actually the dataset
            rows_this_loop <- rows_this_loop[rows_this_loop < nrow(pdat)]
            
            # calculate the SE
            out <- ranger:::predict.ranger(object = fit.b8, 
                                           data = pdat[ rows_this_loop ,c.8],
                                           type = 'se')
            # add SE back into the dataset
            pdat$z.pred.b8.sd[rows_this_loop] <- out$se[,2] # use the 2nd column for binomial model
            
            # increment the for loop timer
            pbapply::setpb(pb, counter)
            counter <- counter + 1
         }
      }
      
      # option 3 (quantregforest)
      if(FALSE){
         # estimate the SE using the quantreg package (and assuming response is continuous, i.e. fitting a regression tree)
         fit.qrf8 <- quantregForest::quantregForest(sampsize = rep(round(nmin/3),2), 
                                                    x = bdat[, covar.8], # training data predictors
                                                    y = as.numeric(as.character(bdat[, 'bycatch'])), # training data response
                                                    # "the response has to be continuous. Binary or count responses are not allowed"
                                                    # xtest = dat[, covar.g8], # test data predictors [same as training data in this case]
                                                    # ytest = as.numeric(as.character(dat[, 'bycatch'])), # test data response [same as training data in this case]
                                                    mtry = 3,           # Number of variables randomly sampled as candidates at each split. Note that the default values are different for classification (sqrt(p) where p is number of variables in x) and regression (p/3)
                                                    ntree = 1500,       # Number of trees to grow. This should not be set to too small a number, to ensure that every input row gets predicted at least a few times.
                                                    importance = FALSE, # Should importance of predictors be assessed?
                                                    do.trace = 250,     # print updates every 250 trees
                                                    keep.forest = TRUE)
         pdat$z.pred.b8.sd <- quantregForest:::predict.quantregForest(object = fit.qrf8,
                                                                      newdata = pdat[,c.8],
                                                                      what = sd)
      }
      
      # read back in pdat2
      # if( !('pdat2' %in% ls())) pdat2 <- readRDS(file.path(base_folder, paste0('temp_pdat2.RDS')))
      
      
      # make predictions using grid2 prediction dataset
      pdat2$z.pred.b8 <- ranger:::predict.ranger(object = fit.b8, 
                                                 data = pdat2[,c.8], 
                                                 type = 'response')$predictions[, 2] # take the 2nd column for the binomial predictions
      
      # the NOAA laptop can't handle methods 1 or 2. I didn't calculate SE for grid 2
      {
         # option 1
         if(FALSE){
            out <- ranger:::predict.ranger(object = fit.b8,
                                           data = pdat2[,c.8],
                                           type = 'se')
            pdat2$z.pred.b8.sd <- out$se[,2]
         }
         
         # option 2
         if(FALSE){
            # create empty column for the data
            pdat2$z.pred.b8.sd <- NA
            
            # number of rows to included in each loop
            rows_per_loop <- 100
            # number of loops
            nloops <- ceiling(nrow(pdat2) / rows_per_loop)
            
            # set up a for loop timer
            counter <- 1
            pb <- pbapply::startpb(0, nloops)
            on.exit(pbapply::closepb(pb))
            
            for(l in 1:nloops){
               # get the rows to be included in this loop
               rows_this_loop <- (1:rows_per_loop + ((l-1)*rows_per_loop))
               # make sure all the rows are actually the dataset
               rows_this_loop <- rows_this_loop[rows_this_loop < nrow(pdat2)]
               
               # calculate the SE
               out <- ranger:::predict.ranger(object = fit.b8, 
                                              data = pdat2[ rows_this_loop ,c.8],
                                              type = 'se')
               # add SE back into the dataset
               pdat2$z.pred.b8.sd[rows_this_loop] <- out$se[,2] # use the 2nd column for binomial model
               
               # increment the for loop timer
               pbapply::setpb(pb, counter)
               counter <- counter + 1
            }
         }
         
         # option 3 (quantregforest)
         if(FALSE){
            
            # estimate the SE using the quantreg package (and assuming response is continuous, i.e. fitting a regression tree)
            fit.qrf8 <- quantregForest::quantregForest(sampsize = rep(round(nmin/3),2), 
                                                       x = bdat[, covar.8], # training data predictors
                                                       y = as.numeric(as.character(bdat[, 'bycatch'])), # training data response
                                                       # "the response has to be continuous. Binary or count responses are not allowed"
                                                       # xtest = dat[, covar.g8], # test data predictors [same as training data in this case]
                                                       # ytest = as.numeric(as.character(dat[, 'bycatch'])), # test data response [same as training data in this case]
                                                       mtry = 3,           # Number of variables randomly sampled as candidates at each split. Note that the default values are different for classification (sqrt(p) where p is number of variables in x) and regression (p/3)
                                                       ntree = 1500,       # Number of trees to grow. This should not be set to too small a number, to ensure that every input row gets predicted at least a few times.
                                                       importance = FALSE, # Should importance of predictors be assessed?
                                                       do.trace = 250,     # print updates every 250 trees
                                                       keep.forest = TRUE)
            pdat2$z.pred.b8.sd <- quantregForest:::predict.quantregForest(object = fit.qrf8,
                                                                          newdata = pdat2[,c.8],
                                                                          what = sd)
         }
      }
      
      # model name for plot legends
      mn <- 'RF 3'
      
      # plot model predictions
      (plot.b8.mean <- plot_covariates_gg(data = pdat %>% 
                                             dplyr::mutate(lat = lat_unproj,
                                                           lon = lon_unproj), 
                                          field = 'z.pred.b8', 
                                          legend.title = paste0(mn, '\nPredicted\nProbability\nof Bycatch'),
                                          colormap = 'viridis',
                                          legend.color.range = plot.mean.range.b))
      ggsave(filename = file.path(base_folder, paste0('binomial_model_predictions_b8.png')), plot = plot.b8.mean, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.b8.mean)
      
      # plot SD of model predictions
      if( !is.null(pdat$z.pred.b8.sd) ){
         (plot.b8.sd <- plot_covariates_gg(data = pdat %>%
                                              dplyr::mutate(lat = lat_unproj,
                                                            lon = lon_unproj),
                                           field = 'z.pred.b8.sd',
                                           legend.title = paste0(mn, '\nSD of\nPredicted\nBycatch\nProbability'),
                                           legend.color.range = plot.sd.range.b,
                                           colormap = 'magma'))
         ggsave(filename = file.path(base_folder, paste0('binomial_model_predictions_b8_sd.png')), plot = plot.b8.sd, width = 4.5, height = 8, units = 'in')
         # remove plot from RAM to save memory
         rm(plot.b8.sd)
      }
      
      # interactive plot
      if(FALSE){
         # first convert the data.frame to a raster
         b8.mean.r <- raster::rasterFromXYZ(xyz = pdat  %>% 
                                               dplyr::mutate(lat = lat_unproj,
                                                             lon = lon_unproj) %>% 
                                               dplyr::select(lon, lat, z.pred.b8),    
                                            crs = sp::CRS('+init=EPSG:4326'))
         # b8.sd.r   <- raster::rasterFromXYZ(xyz = pdat  %>% 
         #                                       dplyr::mutate(lat = lat_unproj,
         #                                                     lon = lon_unproj) %>% 
         #                                       dplyr::select(lon, lat, z.pred.b8.sd),
         #                                    crs = sp::CRS('+init=EPSG:4326'))
         
         # leaflet uses epgs:3857, so I need to project to that first 
         # (leaflet will do it automatically, but it can screw up the color palate, so I'll do it first)
         # to get rid of errors see: https://github.com/rstudio/leaflet/issues/224
         b8.mean.rl <- leaflet::projectRasterForLeaflet(b8.mean.r, method = 'bilinear')
         # b8.sd.rl   <- leaflet::projectRasterForLeaflet(b8.sd.r, method = 'bilinear')
         # the warnings actually come from raster package: raster::projectRaster(from = haul_count_r, crs = sp::CRS('+init=EPSG:3857'))
         
         # first create a color palatte function
         pal.b8m <- leaflet::colorNumeric(palette  = 'viridis', 
                                          domain   = range(na.omit(raster::values(b8.mean.rl))), 
                                          na.color = "transparent")
         # pal.b8sd <- leaflet::colorNumeric(palette  = 'magma', 
         #                                   domain   = range(na.omit(raster::values(b8.sd.rl))), 
         #                                   na.color = "transparent")
         
         # then create the plot
         plot_height_pix <- 900
         lf.b8m <- leaflet::leaflet(height = plot_height_pix) %>% 
            leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>% 
            leaflet::addRasterImage(x = b8.mean.rl, 
                                    colors = pal.b8m,
                                    project = FALSE, 
                                    opacity = 0.9,
                                    group = "Bycatch",
                                    layerId = "Bycatch") %>%
            leafem::addImageQuery(x = b8.mean.rl, 
                                  digits = 4,
                                  position = 'topleft',
                                  project = TRUE,
                                  layerId = "Bycatch") %>%
            leafem::addMouseCoordinates() %>%
            leaflet::addLegend(position = 'topright', 
                               title = paste0(mn, '</br>Predicted</br>Bycatch</br>Probability'), # need HTML coding for line breaks </br> , not R \n
                               pal = pal.b8m, 
                               values = raster::values(b8.mean.rl))
         # lf.b8m
         ## save to standalone .html
         save_leaflet(plot = lf.b8m, 
                      file = file.path(base_folder, 'leaflet', 'binomial_model_predictions_b8_mean.html'))
         
         # lf.b8sd <- leaflet::leaflet(height = plot_height_pix) %>% 
         #    leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>% 
         #    leaflet::addRasterImage(x = b8.sd.rl, 
         #                            colors = pal.b8sd,
         #                            project = FALSE, 
         #                            opacity = 0.9,
         #                            group = "Bycatch_SD",
         #                            layerId = "Bycatch_SD") %>%
         #    leafem::addImageQuery(x = b8.sd.rl, 
         #                          digits = 6,
         #                          position = 'topleft',
         #                          project = TRUE,
         #                          layerId = "Bycatch_SD") %>%
         #    leafem::addMouseCoordinates() %>%
         #    leaflet::addLegend(position = 'topright', 
         #                       title = paste0(mn, '</br>SD of</br>Predicted</br>Bycatch</br>Probability'), # need HTML coding for line breaks </br> , not R \n
         #                       pal = pal.b8sd, 
         #                       values = raster::values(b8.sd.rl))
         # # lf.b8sd
         # save_leaflet(plot = lf.b8sd, 
         #              file = file.path(base_folder, 'leaflet', 'binomial_model_predictions_b8_sd.html'))
         
         # mean and SD together
         # leafsync::sync(lf.b8m, lf.b8sd)
         # save_leafsync(tags = leafsync::sync(lf.b8m, lf.b8sd), 
         #               file = paste0(getwd(), '/figures/leaflet/binomial_model_predictions_b8_mean_and_sd_', short_long, '_',
         #                             ifelse(use_projected, 'projected', 'unprojected'), '.html'))
      }
   }
   
   # spatial component of binomial model only
   {
      # plot of spatial effect through time
      # This one does change every year, so I'll plot more years 
      plot.h8.space <- plot_covariates_gg(data = pdat2 %>%
                                             dplyr::mutate(lat = lat_unproj,
                                                           lon = lon_unproj) %>% 
                                             dplyr::filter(year_f %in% c(2002:2019)),
                                          field = 'z.pred.b8',
                                          legend.title = paste0(mn, '\nPredicted\nProbability\nof Bycatch'),
                                          colormap = 'viridis',
                                          legend.color.range = plot.mean.range.b, 
                                          # transformation = 'log1p', 
                                          # legend.scale.breaks = plot.mean.legend.scale.breaks.,
                                          facet_field = 'year_f', 
                                          facet_rows = 3)
      ggsave(filename = file.path(base_folder, paste0('m8_spatial_predictions_binomial_only.png')), plot = plot.h8.space, width = 7.5, height = 9, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.h8.space)
   }
   
   # Is there any interaction between year and location? 
   {
      pdat2 %>% 
         dplyr::group_by(lat, lon) %>% 
         dplyr::summarise(range = diff(range(z.pred.b8))) %>% 
         ggplot(., aes(x = range)) + geom_histogram(bins = 100)
      
      pdat2 %>% 
         dplyr::group_by(lat, lon) %>% 
         dplyr::summarise(sd = sd(z.pred.b8)) %>% 
         ggplot(., aes(x = sd)) + geom_histogram(bins = 100)
      # not too much, but some.
      
      # can also map the SD to show areas that change the most from year to year
      ggsave(filename = file.path(base_folder, paste0('m8_spatial_SD_binomial_only.png')), 
             plot = plot_covariates_gg(data = pdat2 %>% 
                                          dplyr::mutate(lat = lat_unproj,
                                                        lon = lon_unproj) %>% 
                                          dplyr::group_by(lat, lon) %>% 
                                          dplyr::summarise(sd = sd(z.pred.b8), 
                                                           .groups = 'drop'),
                                       field = 'sd',
                                       legend.title = paste0(mn, '\nSD\n(across years)\nin Predicted\nProbability\nof Bycatch'),
                                       colormap = 'viridis'), 
             width = 4.5, height = 8, units = 'in')
   }
   
   # Datasets for marginal effects plots of fixed effects
   {
      # add a space in the list for each of them
      marginal_effect_list_b[[mn]] <- vector('list', length(c.8))
      names(marginal_effect_list_b[[mn]]) <- c.8
      
      for(c in 1:length(c.8)){
         # I can also skip any covariates that end with "poly2" because polynomial predictors need to be considered together
         if( grepl(pattern = '.+_poly2$', x = c.8[c]) ) next
         
         # get the dataframe for the model predictions
         ndb <- ndg <- prediction_dfs[[c.8[c]]]
         
         # predict new values
         preds.b <- ranger:::predict.ranger(object = fit.b8,
                                            data = ndb,
                                            type = 'response')$predictions[, 2] # 2nd column for binomial predictions
         
         # convert to response scale
         ndb$bycatch     <- preds.b
         ndb$bycatch_lcl <- NA
         ndb$bycatch_ucl <- NA
         ndb$model <- mn
         
         # get the name of the covariate (but not "poly"). It would've been easier to just use "sub()"...
         ndb$covariate <- 
            ifelse(grepl('.+_poly1', c.8[c] ), 
                   lsp_lut[ lsp_lut$final_name == c.8[c], 'original_name' ], 
                   c.8[c])
         
         # save the dataframe
         marginal_effect_list_b[[mn]][[c.8[c]]] <- ndb
      }
   }
}

# free up memory
{
   # R is running out of memory while estimating SD of model 8. Here are some ways to help prevent that.
   
   # option 1 
   # remove a few items to free memory
   rm(list = ls()[grepl(pattern = 'fit\\.+.', x = ls())])
   rm(list = ls()[grepl(pattern = 'predictions\\.+.', x = ls())])
   
   
   # option 2 
   # save output from model 8 and restart r before running model 9
   # save pdat, pdat2, marginal_effect_list_b & marginal_effect_list_g with just model 8 predictions
   saveRDS(object = list(pdat = pdat,
                         pdat2 = pdat2,
                         marginal_effect_list_b = marginal_effect_list_b,
                         marginal_effect_list_g = marginal_effect_list_g), 
           file.path(base_folder, paste0('temp_model_8_output.RDS')))
} # then restart R before running model 9

#####
# Restart R & run all code before "1. Fit GLM 1"
#####

##### Model 9: GBT 1 ####
{
   covar.9 <- rf.covar
   c.9 <- rf.covar
   
   # fit binomial model
   fit.b9 <- fit_m9_gbt1(data = bdat,
                         covariates  = covar.9,
                         modelfamily = 'binomial',
                         rows.train  = 1:nrow(bdat),
                         rows.test   = 1,
                         use.package = 'xgboost',
                         save.model.fit = TRUE)$model
   # fit gamma model
   fit.g9 <- fit_m9_gbt1(data = gdat,
                         covariates  = covar.9,
                         modelfamily = 'gamma',
                         rows.train  = 1:nrow(gdat),
                         rows.test   = 1,
                         use.package = 'xgboost',
                         save.model.fit = TRUE)$model
   
   # binomial model predictions
   {
      # first need to get "treatment plan"
      {
         # Create the treatment plan from the training data
         treatplan.b <- vtreat::designTreatmentsZ(bdat[,], covar.9, verbose = FALSE)
         
         # Get the "clean" variable names from the scoreFrame
         new_vars <- treatplan.b %>%
            magrittr::use_series(scoreFrame) %>%
            dplyr::filter(code %in% c("clean", "lev")) %>%
            magrittr::use_series(varName)
         # don't need to double this for the gamma model b/c binomial & gamma models were fit using the same covariates
         
         # Prepare the prediction data: grid 1
         features_pred.b <- vtreat::prepare(treatplan.b, 
                                            pdat[,covar.9]%>% dplyr::mutate(doy = as.integer(doy)), 
                                            varRestriction = new_vars) %>% as.matrix()
         # Prepare the prediction data: grid 2
         features_pred.b2 <- vtreat::prepare(treatplan.b, 
                                            pdat2[,covar.9]%>% dplyr::mutate(doy = as.integer(doy)), 
                                            varRestriction = new_vars) %>% as.matrix()
      }
      
      # get the predicted values (on the response scale) for the whole pred.dat dataset
      pdat$z.pred.b9 <- xgboost:::predict.xgb.Booster(object = fit.b9,
                                                      newdata = features_pred.b)
      
      # predictions for grid 2
      pdat2$z.pred.b9 <- xgboost:::predict.xgb.Booster(object = fit.b9,
                                                      newdata = features_pred.b2)
      
      # no built-in method for producing estimates of uncertainty. 
      # see: 
      #  - https://github.com/dmlc/xgboost/issues/1433
      #  - https://stats.stackexchange.com/questions/255783/confidence-interval-for-xgb-forecast
      #  - https://stackoverflow.com/questions/37418938/how-to-obtain-a-confidence-interval-or-a-measure-of-prediction-dispersion-when-u
      # see: https://towardsdatascience.com/regression-prediction-intervals-with-xgboost-428e0a018b
      # https://datascience.stackexchange.com/questions/25443/prediction-intervals-using-xgboost
      # 
      pdat$z.pred.b9.sd <- NA
      pdat2$z.pred.b9.sd <- NA
      
      # model name for plot legends
      mn <- 'GBT 1'
      
      # the model was fit the projected coordinates, but I can still make a pretty plot by making model predictions as coordinates
      # that line up nicely in un-projected space. 
      (plot.b9.mean <- plot_covariates_gg(data = pdat %>% 
                                             dplyr::mutate(lat = lat_unproj,
                                                           lon = lon_unproj), 
                                          field = 'z.pred.b9', 
                                          legend.title = paste0(mn, '\nPredicted\nProbability\nof Bycatch'),
                                          colormap = 'viridis',
                                          legend.color.range = plot.mean.range.b))
      ggsave(filename = file.path(base_folder, paste0('binomial_model_predictions_b9.png')), plot = plot.b9.mean, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.b9.mean)
      
      # interactive plot
      if(FALSE){
         # first convert the data.frame to a raster
         b9.mean.r <- raster::rasterFromXYZ(xyz = pdat  %>% 
                                               dplyr::mutate(lat = lat_unproj,
                                                             lon = lon_unproj) %>% 
                                               dplyr::select(lon, lat, z.pred.b9),    
                                            crs = sp::CRS('+init=EPSG:4326'))
         
         # leaflet uses epgs:3857, so I need to project to that first 
         # (leaflet will do it automatically, but it can screw up the color palate, so I'll do it first)
         # to get rid of errors see: https://github.com/rstudio/leaflet/issues/224
         b9.mean.rl <- leaflet::projectRasterForLeaflet(b9.mean.r, method = 'bilinear')
         # the warnings actually come from raster package: raster::projectRaster(from = haul_count_r, crs = sp::CRS('+init=EPSG:3857'))
         
         # first create a color palatte function
         pal.b9m <- leaflet::colorNumeric(palette  = 'viridis', 
                                          domain   = range(na.omit(raster::values(b9.mean.rl))), 
                                          na.color = "transparent")
         
         # then create the plot
         plot_height_pix <- 900
         lf.b9m <- leaflet::leaflet(height = plot_height_pix) %>% 
            leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>% 
            leaflet::addRasterImage(x = b9.mean.rl, 
                                    colors = pal.b9m,
                                    project = FALSE, 
                                    opacity = 0.9,
                                    group = "Bycatch",
                                    layerId = "Bycatch") %>%
            leafem::addImageQuery(x = b9.mean.rl, 
                                  digits = 4,
                                  position = 'topleft',
                                  project = TRUE,
                                  layerId = "Bycatch") %>%
            leafem::addMouseCoordinates() %>%
            leaflet::addLegend(position = 'topright', 
                               title = paste0(mn, '</br>Predicted</br>Bycatch</br>Probability'), # need HTML coding for line breaks </br> , not R \n
                               pal = pal.b9m, 
                               values = raster::values(b9.mean.rl))
         # lf.b9m
         ## save to standalone .html
         save_leaflet(plot = lf.b9m, 
                      file = file.path(base_folder, 'leaflet', 'binomial_model_predictions_b9_mean.html'))
      }
   }
   
   # gamma model predictions
   {
      # first need to get "treatment plan"
      {
         # Create the treatment plan from the training data
         treatplan.g <- vtreat::designTreatmentsZ(gdat[,], covar.9, verbose = FALSE)
         
         # use same "newvers" from binomial model
         # don't need to double this for the gamma model b/c binomial & gamma models were fit using the same covariates
         
         features_pred.g <- vtreat::prepare(treatplan.g, 
                                            pdat[,covar.9]%>% dplyr::mutate(doy = as.integer(doy)), 
                                            varRestriction = new_vars) %>% as.matrix()
         # grid 2
         features_pred.g2 <- vtreat::prepare(treatplan.g, 
                                            pdat2[,covar.9]%>% dplyr::mutate(doy = as.integer(doy)), 
                                            varRestriction = new_vars) %>% as.matrix()
      }
      
      # gamma model
      pdat$z.pred.g9 <- xgboost:::predict.xgb.Booster(object = fit.g9,
                                                      newdata = features_pred.g)
      pdat2$z.pred.g9 <- xgboost:::predict.xgb.Booster(object = fit.g9,
                                                      newdata = features_pred.g2)
      
      
      # no built-in method for producing estimates of uncertainty. 
      # see: 
      #  - https://github.com/dmlc/xgboost/issues/1433
      #  - https://stats.stackexchange.com/questions/255783/confidence-interval-for-xgb-forecast
      #  - https://stackoverflow.com/questions/37418938/how-to-obtain-a-confidence-interval-or-a-measure-of-prediction-dispersion-when-u
      # see: https://towardsdatascience.com/regression-prediction-intervals-with-xgboost-428e0a018b
      # https://datascience.stackexchange.com/questions/25443/prediction-intervals-using-xgboost
      # 
      pdat$z.pred.g9.sd <- NA
      pdat2$z.pred.g9.sd <- NA
      
      # model name for plot legends
      mn <- 'GBT 1'
      
      # the model was fit the projected coordinates, but I can still make a pretty plot by making model predictions as coordinates
      # that line up nicely in un-projected space. 
      (plot.g9.mean <- plot_covariates_gg(data = pdat %>% 
                                             dplyr::mutate(lat = lat_unproj,
                                                           lon = lon_unproj), 
                                          field = 'z.pred.g9', 
                                          legend.title = paste0(mn, '\nPredicted\nNumber\nof Bycatch'),
                                          colormap = 'viridis',
                                          legend.color.range = plot.mean.range.g, 
                                          transformation = 'log1p', 
                                          legend.scale.breaks = plot.mean.legend.scale.breaks.g))
      ggsave(filename = file.path(base_folder, paste0('gamma_model_predictions_g9.png')), plot = plot.g9.mean, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.g9.mean)
      
      # interactive plot
      if(FALSE){
         # first convert the data.frame to a raster
         g9.mean.r <- raster::rasterFromXYZ(xyz = pdat  %>% 
                                               dplyr::mutate(lat = lat_unproj,
                                                             lon = lon_unproj) %>% 
                                               dplyr::select(lon, lat, z.pred.g9),    
                                            crs = sp::CRS('+init=EPSG:4326'))
         
         # leaflet uses epgs:3857, so I need to project to that first 
         # (leaflet will do it automatically, but it can screw up the color palate, so I'll do it first)
         # to get rid of errors see: https://github.com/rstudio/leaflet/issues/224
         g9.mean.rl <- leaflet::projectRasterForLeaflet(g9.mean.r, method = 'bilinear')
         # the warnings actually come from raster package: raster::projectRaster(from = haul_count_r, crs = sp::CRS('+init=EPSG:3857'))
         
         # first create a color palatte function
         pal.g9m <- leaflet::colorNumeric(palette  = 'viridis', 
                                          domain   = range(na.omit(raster::values(g9.mean.rl))), 
                                          na.color = "transparent")
         
         # then create the plot
         plot_height_pix <- 900
         lf.g9m <- leaflet::leaflet(height = plot_height_pix) %>% 
            leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>% 
            leaflet::addRasterImage(x = g9.mean.rl, 
                                    colors = pal.g9m,
                                    project = FALSE, 
                                    opacity = 0.9,
                                    group = "Bycatch",
                                    layerId = "Bycatch") %>%
            leafem::addImageQuery(x = g9.mean.rl, 
                                  digits = 4,
                                  position = 'topleft',
                                  project = TRUE,
                                  layerId = "Bycatch") %>%
            leafem::addMouseCoordinates() %>%
            leaflet::addLegend(position = 'topright', 
                               title = paste0(mn, '</br>Predicted</br>Bycatch</br>Probability'), # need HTML coding for line breaks </br> , not R \n
                               pal = pal.g9m, 
                               values = raster::values(g9.mean.rl))
         # lf.g9m
         ## save to standalone .html
         save_leaflet(plot = lf.g9m, 
                      file = file.path(base_folder, 'leaflet', 'binomial_model_predictions_g9_mean.html'))
      }
   }
   
   # combined hurdle model predictions
   {
      pdat <- pdat %>% 
         dplyr::mutate(z.pred.h9 = z.pred.b9 * z.pred.g9)
      pdat2 <- pdat2 %>% 
         dplyr::mutate(z.pred.h9 = z.pred.b9 * z.pred.g9)
      
      # plot model predictions
      (plot.h9.mean <- plot_covariates_gg(data = pdat %>%
                                             dplyr::mutate(lat = lat_unproj,
                                                           lon = lon_unproj),
                                          field = 'z.pred.h9',
                                          legend.title = paste0(mn, '\nPredicted\nNumber\nof Bycatch'),
                                          colormap = 'viridis',
                                          legend.color.range = plot.mean.range.h, 
                                          transformation = 'log1p', 
                                          legend.scale.breaks = plot.mean.legend.scale.breaks.h))
      ggsave(filename = file.path(base_folder, paste0('hurdle_model_predictions_h9.png')), plot = plot.h9.mean, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.h9.mean)
      
      # plot of spatial effect through time
      # This one does change every year, so I'll plot more years 
      plot.h9.space <- plot_covariates_gg(data = pdat2 %>%
                                             dplyr::mutate(lat = lat_unproj,
                                                           lon = lon_unproj) %>% 
                                             dplyr::filter(year_f %in% c(2002:2019)),
                                          field = 'z.pred.h9',
                                          legend.title = paste0(mn, '\nPredicted\nNumber\nof Bycatch'),
                                          colormap = 'viridis',
                                          legend.color.range = plot.mean.range.h, 
                                          transformation = 'log1p', 
                                          legend.scale.breaks = plot.mean.legend.scale.breaks.h,
                                          facet_field = 'year_f', 
                                          facet_rows = 3)
      ggsave(filename = file.path(base_folder, paste0('m9_spatial_predictions.png')), plot = plot.h9.space, width = 7.5, height = 9, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.h9.space)
   }
   
   # Is there any interaction between year and location? 
   {
      pdat2 %>% 
         dplyr::group_by(lat, lon) %>% 
         dplyr::summarise(range.b = diff(range(z.pred.b9)),
                          range.g = diff(range(z.pred.g9)),
                          .groups = 'drop') %>% 
         ggplot(., aes(x = range.g)) + geom_histogram(bins = 100)
      
      pdat2 %>% 
         dplyr::group_by(lat, lon) %>% 
         dplyr::summarise(sd.b = sd(z.pred.b9),
                          sd.g = sd(z.pred.g9),
                          .groups = 'drop') %>% 
         ggplot(., aes(x = sd.g)) + geom_histogram(bins = 100)
      # not too much in the binomial model; more in the gamma model.
      
      # can also map the SD to show areas that change the most from year to year
      ggsave(filename = file.path(base_folder, paste0('m9_spatial_SD_binomial_only.png')), 
             plot = plot_covariates_gg(data = pdat2 %>% 
                                          dplyr::mutate(lat = lat_unproj,
                                                        lon = lon_unproj) %>% 
                                          dplyr::group_by(lat, lon) %>% 
                                          dplyr::summarise(sd = sd(z.pred.b9), 
                                                           .groups = 'drop'),
                                       field = 'sd',
                                       legend.title = paste0(mn, '\nSD\n(across years)\nin Predicted\nProbability\nof Bycatch'),
                                       colormap = 'viridis'), 
             width = 4.5, height = 8, units = 'in')
      # gamma model
      ggsave(filename = file.path(base_folder, paste0('m9_spatial_SD_gamma_only.png')), 
             plot = plot_covariates_gg(data = pdat2 %>% 
                                          dplyr::mutate(lat = lat_unproj,
                                                        lon = lon_unproj) %>% 
                                          dplyr::group_by(lat, lon) %>% 
                                          dplyr::summarise(sd = sd(z.pred.g9), 
                                                           .groups = 'drop'),
                                       field = 'sd',
                                       legend.title = paste0(mn, '\nSD\n(across years)\nin Predicted\nBycatch'),
                                       colormap = 'viridis'), 
             width = 4.5, height = 8, units = 'in')
      # hurdle model
      ggsave(filename = file.path(base_folder, paste0('m9_spatial_SD_hurdle_model.png')), 
             plot = plot_covariates_gg(data = pdat2 %>% 
                                          dplyr::mutate(lat = lat_unproj,
                                                        lon = lon_unproj) %>% 
                                          dplyr::group_by(lat, lon) %>% 
                                          dplyr::summarise(sd = sd(z.pred.h9), 
                                                           .groups = 'drop'),
                                       field = 'sd',
                                       legend.title = paste0(mn, '\nSD\n(across years)\nin Predicted\nBycatch'),
                                       colormap = 'viridis'), 
             width = 4.5, height = 8, units = 'in')
   }
   
   # save pdat to memory to help clear memory
   # saveRDS(pdat, file = file.path(base_folder, paste0('temp_pdat_aftermodel9.rds')))
   # rm(pdat)
   
   # Datasets for marginal effects plots of fixed effects
   {
      # add a space in the list for each of them
      marginal_effect_list_b[[mn]] <- vector('list', length(c.9))
      names(marginal_effect_list_b[[mn]]) <- c.9
      marginal_effect_list_g[[mn]] <- vector('list', length(c.9))
      names(marginal_effect_list_g[[mn]]) <- c.9
      
      
      for(c in 1:length(c.9)){
         # I can also skip any covariates that end with "poly2" because polynomial predictors need to be considered together
         if( grepl(pattern = '.+_poly2$', x = c.9[c]) ) next
         
         # get the dataframe for the model predictions
         ndb <- ndg <- prediction_dfs[[c.9[c]]]
         
         # use the treatment plan for the original data
         # Prepare the test data
         features_test.b <- vtreat::prepare(treatplan.b, 
                                          ndb[,covar.9] %>% 
                                             dplyr::mutate(doy = as.integer(doy),
                                                           sector = factor(sector, levels = levels(dat$sector)),
                                                           year_f = factor(year_f, levels = levels(dat$year_f))), 
                                          varRestriction = new_vars) %>% 
            as.matrix()
         features_test.g <- vtreat::prepare(treatplan.g, 
                                            ndg[,covar.9] %>% 
                                               dplyr::mutate(doy = as.integer(doy),
                                                             sector = factor(sector, levels = levels(dat$sector)),
                                                             year_f = factor(year_f, levels = levels(dat$year_f))), 
                                            varRestriction = new_vars) %>% 
            as.matrix()
         
         # get the predicted values (on the response scale)
         ndb$bycatch <- xgboost:::predict.xgb.Booster(object = fit.b9,
                                                     newdata = features_test.b)
         # get the predicted values (on the response scale)
         ndg$bycatch <- xgboost:::predict.xgb.Booster(object = fit.g9,
                                                      newdata = features_test.g)
         
         
         # no estimates of uncertainty
         ndb$bycatch_lcl <- NA
         ndb$bycatch_ucl <- NA
         ndb$model <- mn
         ndg$bycatch_lcl <- NA
         ndg$bycatch_ucl <- NA
         ndg$model <- mn
         
         # get the name of the covariate (but not "poly"). It would've been easier to just use "sub()"...
         ndb$covariate <- 
            ndg$covariate <- 
            ifelse(grepl('.+_poly1', c.9[c] ), 
                   lsp_lut[ lsp_lut$final_name == c.9[c], 'original_name' ], 
                   c.9[c])
         
         # save the dataframe
         marginal_effect_list_b[[mn]][[c.9[c]]] <- ndb
         marginal_effect_list_g[[mn]][[c.9[c]]] <- ndg
      }
   }
}

##### Model 10: GBT 2 ####
{
   covar.10 <- rf.covar
   c.10 <- rf.covar
   
   # fit binomial model
   fit.b10 <- fit_m10_gbt2(data = bdat,
                           covariates  = covar.10,
                           modelfamily = 'binomial',
                           rows.train  = 1:nrow(bdat),
                           rows.test   = 1,
                           save.model.fit = TRUE)$model
   # fit gamma model
   fit.g10 <- fit_m10_gbt2(data = gdat,
                           covariates  = covar.10,
                           modelfamily = 'gamma',
                           rows.train  = 1:nrow(gdat),
                           rows.test   = 1,
                           save.model.fit = TRUE)$model
   
   # binomial model predictions
   {
      # first need to get "treatment plan"
      {
         # Create the treatment plan from the training data
         treatplan.b <- vtreat::designTreatmentsZ(bdat[,], covar.10, verbose = FALSE)
         
         # Get the "clean" variable names from the scoreFrame
         new_vars <- treatplan.b %>%
            magrittr::use_series(scoreFrame) %>%
            dplyr::filter(code %in% c("clean", "lev")) %>%
            magrittr::use_series(varName)
         # don't need to double this for the gamma model b/c binomial & gamma models were fit using the same covariates
         
         # Prepare the prediction data: grid 1
         features_pred.b <- vtreat::prepare(treatplan.b, 
                                            pdat[,covar.10]%>% dplyr::mutate(doy = as.integer(doy)), 
                                            varRestriction = new_vars) %>% as.matrix()
         # Prepare the prediction data: grid 2
         features_pred.b2 <- vtreat::prepare(treatplan.b, 
                                             pdat2[,covar.10]%>% dplyr::mutate(doy = as.integer(doy)), 
                                             varRestriction = new_vars) %>% as.matrix()
      }
      
      # get the predicted values (on the response scale) for the whole pred.dat dataset
      pdat$z.pred.b10 <- xgboost:::predict.xgb.Booster(object = fit.b10,
                                                      newdata = features_pred.b)
      
      # predictions for grid 2
      pdat2$z.pred.b10 <- xgboost:::predict.xgb.Booster(object = fit.b10,
                                                       newdata = features_pred.b2)
      
      # no built-in method for producing estimates of uncertainty. 
      # see: 
      #  - https://github.com/dmlc/xgboost/issues/1433
      #  - https://stats.stackexchange.com/questions/255783/confidence-interval-for-xgb-forecast
      #  - https://stackoverflow.com/questions/37418938/how-to-obtain-a-confidence-interval-or-a-measure-of-prediction-dispersion-when-u
      # see: https://towardsdatascience.com/regression-prediction-intervals-with-xgboost-428e0a018b
      # https://datascience.stackexchange.com/questions/25443/prediction-intervals-using-xgboost
      # 
      pdat$z.pred.b10.sd <- NA
      pdat2$z.pred.b10.sd <- NA
      
      # model name for plot legends
      mn <- 'GBT 2'
      
      # the model was fit the projected coordinates, but I can still make a pretty plot by making model predictions as coordinates
      # that line up nicely in un-projected space. 
      (plot.b10.mean <- plot_covariates_gg(data = pdat %>% 
                                             dplyr::mutate(lat = lat_unproj,
                                                           lon = lon_unproj), 
                                          field = 'z.pred.b10', 
                                          legend.title = paste0(mn, '\nPredicted\nProbability\nof Bycatch'),
                                          colormap = 'viridis',
                                          legend.color.range = plot.mean.range.b))
      ggsave(filename = file.path(base_folder, paste0('binomial_model_predictions_b10.png')), plot = plot.b10.mean, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.b10.mean)
      
      # interactive plot
      if(FALSE){
         # first convert the data.frame to a raster
         b10.mean.r <- raster::rasterFromXYZ(xyz = pdat  %>% 
                                               dplyr::mutate(lat = lat_unproj,
                                                             lon = lon_unproj) %>% 
                                               dplyr::select(lon, lat, z.pred.b10),    
                                            crs = sp::CRS('+init=EPSG:4326'))
         
         # leaflet uses epgs:3857, so I need to project to that first 
         # (leaflet will do it automatically, but it can screw up the color palate, so I'll do it first)
         # to get rid of errors see: https://github.com/rstudio/leaflet/issues/224
         b10.mean.rl <- leaflet::projectRasterForLeaflet(b10.mean.r, method = 'bilinear')
         # the warnings actually come from raster package: raster::projectRaster(from = haul_count_r, crs = sp::CRS('+init=EPSG:3857'))
         
         # first create a color palatte function
         pal.b10m <- leaflet::colorNumeric(palette  = 'viridis', 
                                          domain   = range(na.omit(raster::values(b10.mean.rl))), 
                                          na.color = "transparent")
         
         # then create the plot
         plot_height_pix <- 900
         lf.b10m <- leaflet::leaflet(height = plot_height_pix) %>% 
            leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>% 
            leaflet::addRasterImage(x = b10.mean.rl, 
                                    colors = pal.b10m,
                                    project = FALSE, 
                                    opacity = 0.9,
                                    group = "Bycatch",
                                    layerId = "Bycatch") %>%
            leafem::addImageQuery(x = b10.mean.rl, 
                                  digits = 4,
                                  position = 'topleft',
                                  project = TRUE,
                                  layerId = "Bycatch") %>%
            leafem::addMouseCoordinates() %>%
            leaflet::addLegend(position = 'topright', 
                               title = paste0(mn, '</br>Predicted</br>Bycatch</br>Probability'), # need HTML coding for line breaks </br> , not R \n
                               pal = pal.b10m, 
                               values = raster::values(b10.mean.rl))
         # lf.b10m
         ## save to standalone .html
         save_leaflet(plot = lf.b10m, 
                      file = file.path(base_folder, 'leaflet', 'binomial_model_predictions_b10_mean.html'))
      }
   }
   
   # gamma model predictions
   {
      # first need to get "treatment plan"
      {
         # Create the treatment plan from the training data
         treatplan.g <- vtreat::designTreatmentsZ(gdat[,], covar.10, verbose = FALSE)
         
         # use same "newvers" from binomial model
         # don't need to double this for the gamma model b/c binomial & gamma models were fit using the same covariates
         
         features_pred.g <- vtreat::prepare(treatplan.g, 
                                            pdat[,covar.10]%>% dplyr::mutate(doy = as.integer(doy)), 
                                            varRestriction = new_vars) %>% as.matrix()
         # grid 2
         features_pred.g2 <- vtreat::prepare(treatplan.g, 
                                             pdat2[,covar.10]%>% dplyr::mutate(doy = as.integer(doy)), 
                                             varRestriction = new_vars) %>% as.matrix()
      }
      
      # gamma model
      pdat$z.pred.g10 <- xgboost:::predict.xgb.Booster(object = fit.g10,
                                                      newdata = features_pred.g)
      pdat2$z.pred.g10 <- xgboost:::predict.xgb.Booster(object = fit.g10,
                                                       newdata = features_pred.g2)
      
      
      # no built-in method for producing estimates of uncertainty. 
      # see: 
      #  - https://github.com/dmlc/xgboost/issues/1433
      #  - https://stats.stackexchange.com/questions/255783/confidence-interval-for-xgb-forecast
      #  - https://stackoverflow.com/questions/37418938/how-to-obtain-a-confidence-interval-or-a-measure-of-prediction-dispersion-when-u
      # see: https://towardsdatascience.com/regression-prediction-intervals-with-xgboost-428e0a018b
      # https://datascience.stackexchange.com/questions/25443/prediction-intervals-using-xgboost
      # 
      pdat$z.pred.g10.sd <- NA
      pdat2$z.pred.g10.sd <- NA
      
      # model name for plot legends
      mn <- 'GBT 2'
      
      # the model was fit the projected coordinates, but I can still make a pretty plot by making model predictions as coordinates
      # that line up nicely in un-projected space. 
      (plot.g10.mean <- plot_covariates_gg(data = pdat %>% 
                                             dplyr::mutate(lat = lat_unproj,
                                                           lon = lon_unproj), 
                                          field = 'z.pred.g10', 
                                          legend.title = paste0(mn, '\nPredicted\nNumber\nof Bycatch'),
                                          colormap = 'viridis',
                                          legend.color.range = plot.mean.range.g, 
                                          transformation = 'log1p', 
                                          legend.scale.breaks = plot.mean.legend.scale.breaks.g))
      ggsave(filename = file.path(base_folder, paste0('gamma_model_predictions_g10.png')), plot = plot.g10.mean, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.g10.mean)
      
      # interactive plot
      if(FALSE){
         # first convert the data.frame to a raster
         g10.mean.r <- raster::rasterFromXYZ(xyz = pdat  %>% 
                                               dplyr::mutate(lat = lat_unproj,
                                                             lon = lon_unproj) %>% 
                                               dplyr::select(lon, lat, z.pred.g10),    
                                            crs = sp::CRS('+init=EPSG:4326'))
         
         # leaflet uses epgs:3857, so I need to project to that first 
         # (leaflet will do it automatically, but it can screw up the color palate, so I'll do it first)
         # to get rid of errors see: https://github.com/rstudio/leaflet/issues/224
         g10.mean.rl <- leaflet::projectRasterForLeaflet(g10.mean.r, method = 'bilinear')
         # the warnings actually come from raster package: raster::projectRaster(from = haul_count_r, crs = sp::CRS('+init=EPSG:3857'))
         
         # first create a color palatte function
         pal.g10m <- leaflet::colorNumeric(palette  = 'viridis', 
                                          domain   = range(na.omit(raster::values(g10.mean.rl))), 
                                          na.color = "transparent")
         
         # then create the plot
         plot_height_pix <- 900
         lf.g10m <- leaflet::leaflet(height = plot_height_pix) %>% 
            leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>% 
            leaflet::addRasterImage(x = g10.mean.rl, 
                                    colors = pal.g10m,
                                    project = FALSE, 
                                    opacity = 0.9,
                                    group = "Bycatch",
                                    layerId = "Bycatch") %>%
            leafem::addImageQuery(x = g10.mean.rl, 
                                  digits = 4,
                                  position = 'topleft',
                                  project = TRUE,
                                  layerId = "Bycatch") %>%
            leafem::addMouseCoordinates() %>%
            leaflet::addLegend(position = 'topright', 
                               title = paste0(mn, '</br>Predicted</br>Bycatch</br>Probability'), # need HTML coding for line breaks </br> , not R \n
                               pal = pal.g10m, 
                               values = raster::values(g10.mean.rl))
         # lf.g10m
         ## save to standalone .html
         save_leaflet(plot = lf.g10m, 
                      file = file.path(base_folder, 'leaflet', 'binomial_model_predictions_g10_mean.html'))
      }
   }
   
   # combined hurdle model predictions
   {
      pdat <- pdat %>% 
         dplyr::mutate(z.pred.h10 = z.pred.b10 * z.pred.g10)
      pdat2 <- pdat2 %>% 
         dplyr::mutate(z.pred.h10 = z.pred.b10 * z.pred.g10)
      
      # plot model predictions
      (plot.h10.mean <- plot_covariates_gg(data = pdat %>%
                                             dplyr::mutate(lat = lat_unproj,
                                                           lon = lon_unproj),
                                          field = 'z.pred.h10',
                                          legend.title = paste0(mn, '\nPredicted\nNumber\nof Bycatch'),
                                          colormap = 'viridis',
                                          legend.color.range = plot.mean.range.h, 
                                          transformation = 'log1p', 
                                          legend.scale.breaks = plot.mean.legend.scale.breaks.h))
      ggsave(filename = file.path(base_folder, paste0('hurdle_model_predictions_h10.png')), plot = plot.h10.mean, width = 4.5, height = 8, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.h10.mean)
      
      # plot of spatial effect through time
      # This one does change every year, so I'll plot more years 
      plot.h10.space <- plot_covariates_gg(data = pdat2 %>%
                                             dplyr::mutate(lat = lat_unproj,
                                                           lon = lon_unproj) %>% 
                                             dplyr::filter(year_f %in% c(2002:2019)),
                                          field = 'z.pred.h10',
                                          legend.title = paste0(mn, '\nPredicted\nNumber\nof Bycatch'),
                                          colormap = 'viridis',
                                          legend.color.range = plot.mean.range.h, 
                                          transformation = 'log1p', 
                                          legend.scale.breaks = plot.mean.legend.scale.breaks.h,
                                          facet_field = 'year_f', 
                                          facet_rows = 3)
      ggsave(filename = file.path(base_folder, paste0('m10_spatial_predictions.png')), plot = plot.h10.space, width = 7.5, height = 9, units = 'in')
      # remove plot from RAM to save memory
      rm(plot.h10.space)
   }
   
   # Is there any interaction between year and location? 
   {
      pdat2 %>% 
         dplyr::group_by(lat, lon) %>% 
         dplyr::summarise(range.b = diff(range(z.pred.b10)),
                          range.g = diff(range(z.pred.g10)),
                          .groups = 'drop') %>% 
         ggplot(., aes(x = range.g)) + geom_histogram(bins = 100)
      
      pdat2 %>% 
         dplyr::group_by(lat, lon) %>% 
         dplyr::summarise(sd.b = sd(z.pred.b10),
                          sd.g = sd(z.pred.g10),
                          .groups = 'drop') %>% 
         ggplot(., aes(x = sd.g)) + geom_histogram(bins = 100)
      # not too much in the binomial model; more in the gamma model.
      
      # can also map the SD to show areas that change the most from year to year
      ggsave(filename = file.path(base_folder, paste0('m10_spatial_SD_binomial_only.png')), 
             plot = plot_covariates_gg(data = pdat2 %>% 
                                          dplyr::mutate(lat = lat_unproj,
                                                        lon = lon_unproj) %>% 
                                          dplyr::group_by(lat, lon) %>% 
                                          dplyr::summarise(sd = sd(z.pred.b10), 
                                                           .groups = 'drop'),
                                       field = 'sd',
                                       legend.title = paste0(mn, '\nSD\n(across years)\nin Predicted\nProbability\nof Bycatch'),
                                       colormap = 'viridis'), 
             width = 4.5, height = 8, units = 'in')
      # gamma model
      ggsave(filename = file.path(base_folder, paste0('m10_spatial_SD_gamma_only.png')), 
             plot = plot_covariates_gg(data = pdat2 %>% 
                                          dplyr::mutate(lat = lat_unproj,
                                                        lon = lon_unproj) %>% 
                                          dplyr::group_by(lat, lon) %>% 
                                          dplyr::summarise(sd = sd(z.pred.g10), 
                                                           .groups = 'drop'),
                                       field = 'sd',
                                       legend.title = paste0(mn, '\nSD\n(across years)\nin Predicted\nBycatch'),
                                       colormap = 'viridis'), 
             width = 4.5, height = 8, units = 'in')
      # hurdle model
      ggsave(filename = file.path(base_folder, paste0('m10_spatial_SD_hurdle_model.png')), 
             plot = plot_covariates_gg(data = pdat2 %>% 
                                          dplyr::mutate(lat = lat_unproj,
                                                        lon = lon_unproj) %>% 
                                          dplyr::group_by(lat, lon) %>% 
                                          dplyr::summarise(sd = sd(z.pred.h10), 
                                                           .groups = 'drop'),
                                       field = 'sd',
                                       legend.title = paste0(mn, '\nSD\n(across years)\nin Predicted\nBycatch'),
                                       colormap = 'viridis'), 
             width = 4.5, height = 8, units = 'in')
   }
   
   # save pdat to memory to help clear memory
   # saveRDS(pdat, file = file.path(base_folder, paste0('temp_pdat_aftermodel10.rds')))
   # rm(pdat)
   
   # Datasets for marginal effects plots of fixed effects
   {
      # add a space in the list for each of them
      marginal_effect_list_b[[mn]] <- vector('list', length(c.10))
      names(marginal_effect_list_b[[mn]]) <- c.10
      marginal_effect_list_g[[mn]] <- vector('list', length(c.10))
      names(marginal_effect_list_g[[mn]]) <- c.10
      
      
      for(c in 1:length(c.10)){
         # I can also skip any covariates that end with "poly2" because polynomial predictors need to be considered together
         if( grepl(pattern = '.+_poly2$', x = c.10[c]) ) next
         
         # get the dataframe for the model predictions
         ndb <- ndg <- prediction_dfs[[c.10[c]]]
         
         # use the treatment plan for the original data
         # Prepare the test data
         features_test.b <- vtreat::prepare(treatplan.b, 
                                            ndb[,covar.10] %>% 
                                               dplyr::mutate(doy = as.integer(doy),
                                                             sector = factor(sector, levels = levels(dat$sector)),
                                                             year_f = factor(year_f, levels = levels(dat$year_f))), 
                                            varRestriction = new_vars) %>% 
            as.matrix()
         features_test.g <- vtreat::prepare(treatplan.g, 
                                            ndg[,covar.10] %>% 
                                               dplyr::mutate(doy = as.integer(doy),
                                                             sector = factor(sector, levels = levels(dat$sector)),
                                                             year_f = factor(year_f, levels = levels(dat$year_f))), 
                                            varRestriction = new_vars) %>% 
            as.matrix()
         
         # get the predicted values (on the response scale)
         ndb$bycatch <- xgboost:::predict.xgb.Booster(object = fit.b10,
                                                      newdata = features_test.b)
         # get the predicted values (on the response scale)
         ndg$bycatch <- xgboost:::predict.xgb.Booster(object = fit.g10,
                                                      newdata = features_test.g)
         
         
         # no estimates of uncertainty
         ndb$bycatch_lcl <- NA
         ndb$bycatch_ucl <- NA
         ndb$model <- mn
         ndg$bycatch_lcl <- NA
         ndg$bycatch_ucl <- NA
         ndg$model <- mn
         
         # get the name of the covariate (but not "poly"). It would've been easier to just use "sub()"...
         ndb$covariate <- 
            ndg$covariate <- 
            ifelse(grepl('.+_poly1', c.10[c] ), 
                   lsp_lut[ lsp_lut$final_name == c.10[c], 'original_name' ], 
                   c.10[c])
         
         # save the dataframe
         marginal_effect_list_b[[mn]][[c.10[c]]] <- ndb
         marginal_effect_list_g[[mn]][[c.10[c]]] <- ndg
      }
   }
}

# free up memory
{
   # check how we're doing on memory
   data.frame('object' = ls()) %>% 
      dplyr::mutate(size_unit = object %>%sapply(. %>% get() %>% object.size %>% format(., unit = 'auto')),
                    size = as.numeric(sapply(strsplit(size_unit, split = ' '), FUN = function(x) x[1])),
                    unit = factor(sapply(strsplit(size_unit, split = ' '), FUN = function(x) x[2]), levels = c('Gb', 'Mb', 'Kb', 'bytes'))) %>% 
      dplyr::arrange(unit, dplyr::desc(size)) %>% 
      dplyr::select(-size_unit)
   
   # remove a few items to free memory
   rm(list = ls()[grepl(pattern = 'fit\\.+.', x = ls())])
   rm(list = ls()[grepl(pattern = 'predictions\\.+.', x = ls())])
   rm(list = ls()[grepl(pattern = 'features_pred\\.+.', x = ls())])
   
   # save output from models 9-10
   # save pdat, pdat2, marginal_effect_list_b & marginal_effect_list_g
   saveRDS(object = list(pdat = pdat,
                         pdat2 = pdat2,
                         marginal_effect_list_b = marginal_effect_list_b,
                         marginal_effect_list_g = marginal_effect_list_g), 
           file.path(base_folder, paste0('temp_model_9-10_output.RDS')))
}

# prediction maps of model averages (pred/grid 1)
{
   # aggregate pdat from various files
   {
      # models 1-5
      pdat_1.5 <- readRDS(file.path(base_folder, paste0('temp_model_1-5_output.RDS')))$pdat
      # model 6
      pdat_6 <- readRDS(file.path(base_folder, paste0('temp_model_6_output.RDS')))$pdat
      # model 7
      pdat_7 <- readRDS(file.path(base_folder, paste0('temp_model_7_output.RDS')))$pdat
      # model 8
      pdat_8 <- readRDS(file.path(base_folder, paste0('temp_model_8_output.RDS')))$pdat
      # models 9-10
      pdat_9.10 <- readRDS(file.path(base_folder, paste0('temp_model_9-10_output.RDS')))$pdat
         
      # join them all together
      # they should all still be in the same order, but I'll use a "join" instead of "rbind" just to be sure
      temp <- dplyr::left_join(x = pdat_1.5, 
                               y = pdat_6 %>% dplyr::select(c('lat', 'lon', names(pdat_6)[ !(names(pdat_6) %in% names(pdat_1.5))])), 
                               by = c('lat', 'lon'))
      temp <- temp %>% 
         dplyr::left_join(x = ., 
                          y = pdat_7 %>% dplyr::select(c('lat', 'lon', names(pdat_7)[ !(names(pdat_7) %in% names(temp))])), 
                          by = c('lat', 'lon'))
      temp <- temp %>% 
         dplyr::left_join(x = .,
                          y = pdat_8 %>% dplyr::select(c('lat', 'lon', names(pdat_8)[ !(names(pdat_8) %in% names(temp))])), 
                          by = c('lat', 'lon'))
      pdat <- temp %>% 
         dplyr::left_join(x = .,
                          y = pdat_9.10 %>% dplyr::select(c('lat', 'lon', names(pdat_9.10)[ !(names(pdat_9.10) %in% names(temp))])), 
                          by = c('lat', 'lon'))
      names(pdat)
   }
   
   # add model averages
   {
      pdat <- pdat %>% 
         dplyr::rowwise() %>% 
         dplyr::mutate(
            z.pred.b11 = mean(c(z.pred.b1, 
                                z.pred.b2, 
                                z.pred.b3, 
                                z.pred.b4, 
                                z.pred.b5, 
                                z.pred.b6, 
                                z.pred.b7, 
                                z.pred.b8, 
                                z.pred.b9,
                                z.pred.b10)),
            z.pred.b12 = mean(c(z.pred.b1, 
                                z.pred.b2, 
                                z.pred.b3, 
                                z.pred.b4,            
                                
                                z.pred.b6, 
                                z.pred.b7, 
                                z.pred.b8, 
                                z.pred.b9,
                                z.pred.b10)),
            z.pred.g11 = mean(c(z.pred.g1, 
                                z.pred.g2, 
                                z.pred.g3, 
                                z.pred.g4, 
                                z.pred.g5, 
                                z.pred.g6, 
                                
                                
                                z.pred.g9,
                                z.pred.g10)),
            z.pred.g12 = mean(c(z.pred.g1, 
                                z.pred.g2, 
                                z.pred.g3, 
                                z.pred.g4,            
                                
                                z.pred.g6, 
                                
                                
                                z.pred.g9,
                                z.pred.g10)),
            z.pred.h11 = mean(c(z.pred.h1, 
                                z.pred.h2, 
                                z.pred.h3, 
                                z.pred.h4, 
                                z.pred.h5, 
                                z.pred.h6, 
                                
                                
                                z.pred.h9,
                                z.pred.h10)),
            z.pred.h12 = mean(c(z.pred.h1, 
                                z.pred.h2, 
                                z.pred.h3, 
                                z.pred.h4,            
                                
                                z.pred.h6, 
                                
                                
                                z.pred.h9,
                                z.pred.h10))) %>% 
         dplyr::ungroup()
      
      # save results
      saveRDS(object = pdat,file.path(base_folder, paste0('temp_pdat_with_mod_avg.RDS')))
   }
   
   # plot model averages
   {
      # AVG 1
      {
         mn <- 'AVG 1'
         
         # binomial model average 1
         (plot.b11.mean <- plot_covariates_gg(data = pdat %>% 
                                                 dplyr::mutate(lat = lat_unproj,
                                                               lon = lon_unproj), 
                                              field = 'z.pred.b11', 
                                              legend.title = paste0(mn, '\nPredicted\nProbability\nof Bycatch'),
                                              colormap = 'viridis',
                                              legend.color.range = plot.mean.range.b))
         ggsave(filename = file.path(base_folder, paste0('binomial_model_predictions_b11.png')), plot = plot.b11.mean, width = 4.5, height = 8, units = 'in')
         # remove plot from RAM to save memory
         rm(plot.b11.mean)
         
         # interactive plot
         if(FALSE){
            # first convert the data.frame to a raster
            b11.mean.r <- raster::rasterFromXYZ(xyz = pdat  %>% 
                                                   dplyr::mutate(lat = lat_unproj,
                                                                 lon = lon_unproj) %>% 
                                                   dplyr::select(lon, lat, z.pred.b11),    
                                                crs = sp::CRS('+init=EPSG:4326'))
            # leaflet uses epgs:3857, so I need to project to that first 
            # (leaflet will do it automatically, but it can screw up the color palate, so I'll do it first)
            # to get rid of errors see: https://github.com/rstudio/leaflet/issues/224
            b11.mean.rl <- leaflet::projectRasterForLeaflet(b11.mean.r, method = 'bilinear')
            
            # first create a color palatte function
            pal.b11m <- leaflet::colorNumeric(palette  = 'viridis', 
                                              domain   = range(na.omit(raster::values(b11.mean.rl))), 
                                              na.color = "transparent")
            
            # then create the plot
            plot_height_pix <- 900
            lf.b11m <- leaflet::leaflet(height = plot_height_pix) %>% 
               leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>% 
               leaflet::addRasterImage(x = b11.mean.rl, 
                                       colors = pal.b11m,
                                       project = FALSE, 
                                       opacity = 0.9,
                                       group = "Bycatch",
                                       layerId = "Bycatch") %>%
               leafem::addImageQuery(x = b11.mean.rl, 
                                     digits = 4,
                                     position = 'topleft',
                                     project = TRUE,
                                     layerId = "Bycatch") %>%
               leafem::addMouseCoordinates() %>%
               leaflet::addLegend(position = 'topright', 
                                  title = paste0(mn, '</br>Predicted</br>Bycatch</br>Probability'), # need HTML coding for line breaks </br> , not R \n
                                  pal = pal.b11m, 
                                  values = raster::values(b11.mean.rl))
            # lf.b11m
            ## save to standalone .html
            save_leaflet(plot = lf.b11m, 
                         file = file.path(base_folder, 'leaflet', 'binomial_model_predictions_b11_mean.html'))
         }
         
         # gamma model average 1
         (plot.g11.mean <- plot_covariates_gg(data = pdat %>% 
                                                 dplyr::mutate(lat = lat_unproj,
                                                               lon = lon_unproj), 
                                              field = 'z.pred.g11', 
                                              legend.title = paste0(mn, '\nPredicted\nNumber\nof Bycatch'),
                                              colormap = 'viridis',
                                              legend.color.range = plot.mean.range.g, 
                                              transformation = 'log1p', 
                                              legend.scale.breaks = plot.mean.legend.scale.breaks.g))
         ggsave(filename = file.path(base_folder, paste0('gamma_model_predictions_g11.png')), plot = plot.g11.mean, width = 4.5, height = 8, units = 'in')
         # remove plot from RAM to save memory
         rm(plot.g11.mean)
         
         # interactive plot
         if(FALSE){
            # first convert the data.frame to a raster
            g11.mean.r <- raster::rasterFromXYZ(xyz = pdat  %>% 
                                                   dplyr::mutate(lat = lat_unproj,
                                                                 lon = lon_unproj) %>% 
                                                   dplyr::select(lon, lat, z.pred.g11),    
                                                crs = sp::CRS('+init=EPSG:4326'))
            
            # leaflet uses epgs:3857, so I need to project to that first 
            # (leaflet will do it automatically, but it can screw up the color palate, so I'll do it first)
            # to get rid of errors see: https://github.com/rstudio/leaflet/issues/224
            g11.mean.rl <- leaflet::projectRasterForLeaflet(g11.mean.r, method = 'bilinear')
            # the warnings actually come from raster package: raster::projectRaster(from = haul_count_r, crs = sp::CRS('+init=EPSG:3857'))
            
            # first create a color palatte function
            pal.g11m <- leaflet::colorNumeric(palette  = 'viridis', 
                                              domain   = range(na.omit(raster::values(g11.mean.rl))), 
                                              na.color = "transparent")
            
            # then create the plot
            plot_height_pix <- 900
            lf.g11m <- leaflet::leaflet(height = plot_height_pix) %>% 
               leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>% 
               leaflet::addRasterImage(x = g11.mean.rl, 
                                       colors = pal.g11m,
                                       project = FALSE, 
                                       opacity = 0.9,
                                       group = "Bycatch",
                                       layerId = "Bycatch") %>%
               leafem::addImageQuery(x = g11.mean.rl, 
                                     digits = 4,
                                     position = 'topleft',
                                     project = TRUE,
                                     layerId = "Bycatch") %>%
               leafem::addMouseCoordinates() %>%
               leaflet::addLegend(position = 'topright', 
                                  title = paste0(mn, '</br>Predicted</br>Bycatch</br>Probability'), # need HTML coding for line breaks </br> , not R \n
                                  pal = pal.g11m, 
                                  values = raster::values(g11.mean.rl))
            # lf.g11m
            ## save to standalone .html
            save_leaflet(plot = lf.g11m, 
                         file = file.path(base_folder, 'leaflet', 'binomial_model_predictions_g11_mean.html'))
         }
         
         # hurdle model average 1
         (plot.h11.mean <- plot_covariates_gg(data = pdat %>%
                                                 dplyr::mutate(lat = lat_unproj,
                                                               lon = lon_unproj),
                                              field = 'z.pred.h11',
                                              legend.title = paste0(mn, '\nPredicted\nNumber\nof Bycatch'),
                                              colormap = 'viridis',
                                              legend.color.range = plot.mean.range.h, 
                                              transformation = 'log1p', 
                                              legend.scale.breaks = plot.mean.legend.scale.breaks.h))
         ggsave(filename = file.path(base_folder, paste0('hurdle_model_predictions_h11.png')), plot = plot.h11.mean, width = 4.5, height = 8, units = 'in')
         # remove plot from RAM to save memory
         rm(plot.h11.mean)
      }
      
      # AVG 2
      {
         mn <- 'AVG 2'
         # 2nd model average
         (plot.b12.mean <- plot_covariates_gg(data = pdat %>% 
                                                 dplyr::mutate(lat = lat_unproj,
                                                               lon = lon_unproj), 
                                              field = 'z.pred.b12', 
                                              legend.title = paste0(mn, '\nPredicted\nProbability\nof Bycatch'),
                                              colormap = 'viridis',
                                              legend.color.range = plot.mean.range.b))
         ggsave(filename = file.path(base_folder, paste0('binomial_model_predictions_b12.png')), plot = plot.b12.mean, width = 4.5, height = 8, units = 'in')
         
         # interactive plot
         if(FALSE){
            # first convert the data.frame to a raster
            b12.mean.r <- raster::rasterFromXYZ(xyz = pdat  %>% 
                                                   dplyr::mutate(lat = lat_unproj,
                                                                 lon = lon_unproj) %>% 
                                                   dplyr::select(lon, lat, z.pred.b12),    
                                                crs = sp::CRS('+init=EPSG:4326'))
            # leaflet uses epgs:3857, so I need to project to that first 
            # (leaflet will do it automatically, but it can screw up the color palate, so I'll do it first)
            # to get rid of errors see: https://github.com/rstudio/leaflet/issues/224
            b12.mean.rl <- leaflet::projectRasterForLeaflet(b12.mean.r, method = 'bilinear')
            
            # first create a color palatte function
            pal.b12m <- leaflet::colorNumeric(palette  = 'viridis', 
                                              domain   = range(na.omit(raster::values(b12.mean.rl))), 
                                              na.color = "transparent")
            
            # then create the plot
            plot_height_pix <- 900
            lf.b12m <- leaflet::leaflet(height = plot_height_pix) %>% 
               leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>% 
               leaflet::addRasterImage(x = b12.mean.rl, 
                                       colors = pal.b12m,
                                       project = FALSE, 
                                       opacity = 0.9,
                                       group = "Bycatch",
                                       layerId = "Bycatch") %>%
               leafem::addImageQuery(x = b12.mean.rl, 
                                     digits = 4,
                                     position = 'topleft',
                                     project = TRUE,
                                     layerId = "Bycatch") %>%
               leafem::addMouseCoordinates() %>%
               leaflet::addLegend(position = 'topright', 
                                  title = paste0(mn, '</br>Predicted</br>Bycatch</br>Probability'), # need HTML coding for line breaks </br> , not R \n
                                  pal = pal.b12m, 
                                  values = raster::values(b12.mean.rl))
            # lf.b12m
            ## save to standalone .html
            save_leaflet(plot = lf.b12m, 
                         file = file.path(base_folder, 'leaflet', 'binomial_model_predictions_b12_mean.html'))
         }
         
         # gamma model average 2
         (plot.g12.mean <- plot_covariates_gg(data = pdat %>% 
                                                 dplyr::mutate(lat = lat_unproj,
                                                               lon = lon_unproj), 
                                              field = 'z.pred.g12', 
                                              legend.title = paste0(mn, '\nPredicted\nNumber\nof Bycatch'),
                                              colormap = 'viridis',
                                              legend.color.range = plot.mean.range.g, 
                                              transformation = 'log1p', 
                                              legend.scale.breaks = plot.mean.legend.scale.breaks.g))
         ggsave(filename = file.path(base_folder, paste0('gamma_model_predictions_g12.png')), plot = plot.g12.mean, width = 4.5, height = 8, units = 'in')
         # remove plot from RAM to save memory
         rm(plot.g12.mean)
         
         # interactive plot
         if(FALSE){
            # first convert the data.frame to a raster
            g12.mean.r <- raster::rasterFromXYZ(xyz = pdat  %>% 
                                                   dplyr::mutate(lat = lat_unproj,
                                                                 lon = lon_unproj) %>% 
                                                   dplyr::select(lon, lat, z.pred.g12),    
                                                crs = sp::CRS('+init=EPSG:4326'))
            
            # leaflet uses epgs:3857, so I need to project to that first 
            # (leaflet will do it automatically, but it can screw up the color palate, so I'll do it first)
            # to get rid of errors see: https://github.com/rstudio/leaflet/issues/224
            g12.mean.rl <- leaflet::projectRasterForLeaflet(g12.mean.r, method = 'bilinear')
            # the warnings actually come from raster package: raster::projectRaster(from = haul_count_r, crs = sp::CRS('+init=EPSG:3857'))
            
            # first create a color palatte function
            pal.g12m <- leaflet::colorNumeric(palette  = 'viridis', 
                                              domain   = range(na.omit(raster::values(g12.mean.rl))), 
                                              na.color = "transparent")
            
            # then create the plot
            plot_height_pix <- 900
            lf.g12m <- leaflet::leaflet(height = plot_height_pix) %>% 
               leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>% 
               leaflet::addRasterImage(x = g12.mean.rl, 
                                       colors = pal.g12m,
                                       project = FALSE, 
                                       opacity = 0.9,
                                       group = "Bycatch",
                                       layerId = "Bycatch") %>%
               leafem::addImageQuery(x = g12.mean.rl, 
                                     digits = 4,
                                     position = 'topleft',
                                     project = TRUE,
                                     layerId = "Bycatch") %>%
               leafem::addMouseCoordinates() %>%
               leaflet::addLegend(position = 'topright', 
                                  title = paste0(mn, '</br>Predicted</br>Bycatch</br>Probability'), # need HTML coding for line breaks </br> , not R \n
                                  pal = pal.g12m, 
                                  values = raster::values(g12.mean.rl))
            # lf.g12m
            ## save to standalone .html
            save_leaflet(plot = lf.g12m, 
                         file = file.path(base_folder, 'leaflet', 'binomial_model_predictions_g12_mean.html'))
         }
         
         # hurdle model average 2
         (plot.h12.mean <- plot_covariates_gg(data = pdat %>%
                                                 dplyr::mutate(lat = lat_unproj,
                                                               lon = lon_unproj),
                                              field = 'z.pred.h12',
                                              legend.title = paste0(mn, '\nPredicted\nNumber\nof Bycatch'),
                                              colormap = 'viridis',
                                              legend.color.range = plot.mean.range.h, 
                                              transformation = 'log1p', 
                                              legend.scale.breaks = plot.mean.legend.scale.breaks.h))
         ggsave(filename = file.path(base_folder, paste0('hurdle_model_predictions_h12.png')), plot = plot.h12.mean, width = 4.5, height = 8, units = 'in')
         # remove plot from RAM to save memory
         rm(plot.h12.mean)
      }
   }
   
   # 1 massive plot/map of all BINOMIAL model predictions
   {
      # pivot grid 1 (pdat) data into long format so that I can use facet_wrap to plot them all at once.
      plot.data.b <- pdat %>% 
         # use the unprojected coordinates
         dplyr::mutate(lat = lat_unproj,
                       lon = lon_unproj) %>% 
         dplyr::select(lon, lat, 
                       dplyr::starts_with('z.pred'), # only want the predicted values
                       # -dplyr::contains('.b'), # but not the binomial model predictions
                       -dplyr::contains('.g'), # and not the gamma model predictions
                       -dplyr::contains('.h'), # and not the hurdle model predictions
                       # -dplyr::contains('.sd'), # and not the SD of predictions
                       -dplyr::contains('.link'), # nothing on the link scale
                       -dplyr::contains('.lcl'), # no confidence limits
                       -dplyr::contains('.ucl'), 
                       -dplyr::contains('.cl.range')) %>% 
         tidyr::pivot_longer(data = ., 
                             cols = starts_with('z.pred'), 
                             names_to = 'old_name', 
                             values_to = 'Probability') %>% 
         dplyr::mutate(metric = if_else(condition = grepl(pattern = '\\.sd', x = old_name), 
                                        true = 'SD', 
                                        false = 'Mean'),
                       model  = factor(if_else(grepl(pattern = '\\.b1$|\\.b1.sd$', x = old_name), # need to get both b1 and b1.sd but NOT b11 or b12
                                               true = 'GLM 1', false = 
                                                  if_else(grepl(pattern = '\\.b2', x = old_name), 
                                                          true = 'GLM 2', false = 
                                                             if_else(grepl(pattern = '\\.b3', x = old_name), 
                                                                     true = 'GAM 1', false = 
                                                                        if_else(grepl(pattern = '\\.b4', x = old_name), 
                                                                                true = 'GAM 2', false = 
                                                                                   if_else(grepl(pattern = '\\.b5', x = old_name), 
                                                                                           true = 'GAM 3', false = 
                                                                                              if_else(grepl(pattern = '\\.b6', x = old_name), 
                                                                                                      true = 'RF 1', false = 
                                                                                                         if_else(grepl(pattern = '\\.b7', x = old_name), 
                                                                                                                 true = 'RF 2', false = 
                                                                                                                    if_else(condition = grepl(pattern = '\\.b8', x = old_name), 
                                                                                                                            true = 'RF 3', false = 
                                                                                                                               if_else(condition = grepl(pattern = '\\.b9', x = old_name), 
                                                                                                                                       true = 'GBT 1', false = 
                                                                                                                                          if_else(condition = grepl(pattern = '\\.b10', x = old_name), 
                                                                                                                                                  true = 'GBT 2', false = 
                                                                                                                                                     if_else(grepl(pattern = '\\.b11', x = old_name), 
                                                                                                                                                             true = 'AVG 1', false = 
                                                                                                                                                                if_else(condition = grepl(pattern = '\\.b12', x = old_name), 
                                                                                                                                                                        true = 'AVG 2', false = '_problem_')))))))))))), 
                                       levels = c('GLM 1', 'GLM 2', 'GAM 1',
                                                  'GAM 2', 'GAM 3', 'RF 1',
                                                  'RF 2', 'RF 3', 'GBT 1', 'GBT 2',
                                                  'AVG 1', 'AVG 2')))
      
      # Plot mean values
      (plot.all.mean <- plot_covariates_gg(data = plot.data.b %>% 
                                              dplyr::filter(metric == 'Mean'), 
                                           field = 'Probability', 
                                           legend.title = 'Predicted\nProbability\nof Bycatch',
                                           colormap = 'viridis',
                                           legend.color.range = plot.mean.range.b, 
                                           facet_field = 'model', 
                                           facet_rows = 3))
      ggsave(filename = file.path(base_folder, paste0('binomial_model_predictions.png')), 
             plot = plot.all.mean, width = 5.9, height = 10, units = 'in')
      rm(plot.all.mean)
      
      # Plot SD values
      (plot.all.sd <- plot_covariates_gg(data = plot.data.b %>% 
                                            dplyr::filter(metric == 'SD'), 
                                         field = 'Probability', 
                                         legend.title = 'SD of\nPredicted\nProbability\nof Bycatch',
                                         colormap = 'magma',
                                         legend.color.range = plot.sd.range.b, 
                                         facet_field = 'model', 
                                         facet_rows = 2))
      ggsave(filename = file.path(base_folder, paste0('binomial_model_predictions_sd.png')), 
             plot = plot.all.sd, width = 5.9, height = 7, units = 'in')
      rm(plot.all.sd)
   }
   
   # 1 massive plot/map of all GAMMA model predictions
   {
      # pivot grid 1 (pdat) data into long format so that I can use facet_wrap to plot them all at once.
      plot.data.g <- pdat %>% 
         # use the unprojected coordinates
         dplyr::mutate(lat = lat_unproj,
                       lon = lon_unproj) %>% 
         dplyr::select(lon, lat, 
                       dplyr::starts_with('z.pred'), # only want the predicted values
                       -dplyr::contains('.b'), # but not the binomial model predictions
                       # -dplyr::contains('.g'), # and not the gamma model predictions
                       -dplyr::contains('.h'), # and not the hurdle model predictions
                       # -dplyr::contains('.sd'), # and not the SD of predictions
                       -dplyr::contains('.link'), # nothing on the link scale
                       -dplyr::contains('.lcl'), # no confidence limits
                       -dplyr::contains('.ucl'), 
                       -dplyr::contains('.cl.range')) %>% 
         tidyr::pivot_longer(data = ., 
                             cols = starts_with('z.pred'), 
                             names_to = 'old_name', 
                             values_to = 'Probability') %>% 
         dplyr::mutate(metric = if_else(condition = grepl(pattern = '\\.sd', x = old_name), 
                                        true = 'SD', 
                                        false = 'Mean'),
                       model  = factor(if_else(grepl(pattern = '\\.g1$|\\.g1.sd$', x = old_name), # need to get both b1 and b1.sd but NOT b11 or b12
                                               true = 'GLM 1', false = 
                                                  if_else(grepl(pattern = '\\.g2', x = old_name), 
                                                          true = 'GLM 2', false = 
                                                             if_else(grepl(pattern = '\\.g3', x = old_name), 
                                                                     true = 'GAM 1', false = 
                                                                        if_else(grepl(pattern = '\\.g4', x = old_name), 
                                                                                true = 'GAM 2', false = 
                                                                                   if_else(grepl(pattern = '\\.g5', x = old_name), 
                                                                                           true = 'GAM 3', false = 
                                                                                              if_else(grepl(pattern = '\\.g6', x = old_name), 
                                                                                                      true = 'RF 1', false = 
                                                                                                         if_else(grepl(pattern = '\\.g7', x = old_name), 
                                                                                                                 true = 'RF 2', false = 
                                                                                                                    if_else(condition = grepl(pattern = '\\.g8', x = old_name), 
                                                                                                                            true = 'RF 3', false = 
                                                                                                                               if_else(condition = grepl(pattern = '\\.g9', x = old_name), 
                                                                                                                                       true = 'GBT 1', false = 
                                                                                                                                          if_else(condition = grepl(pattern = '\\.g10', x = old_name), 
                                                                                                                                                  true = 'GBT 2', false = 
                                                                                                                                                     if_else(grepl(pattern = '\\.g11', x = old_name), 
                                                                                                                                                             true = 'AVG 1', false = 
                                                                                                                                                                if_else(condition = grepl(pattern = '\\.g12', x = old_name), 
                                                                                                                                                                        true = 'AVG 2', false = '_problem_')))))))))))), 
                                       levels = c('GLM 1', 'GLM 2', 'GAM 1',
                                                  'GAM 2', 'GAM 3', 'RF 1',
                                                  'RF 2', 'RF 3', 'GBT 1', 'GBT 2',
                                                  'AVG 1', 'AVG 2')))
      
      # Plot mean values
      (plot.all.mean <- plot_covariates_gg(data = plot.data.g %>% 
                                              dplyr::filter(metric == 'Mean'), 
                                           field = 'Probability', 
                                           legend.title = 'Predicted\nBycatch',
                                           colormap = 'viridis',
                                           legend.color.range = plot.mean.range.g, 
                                           transformation = 'log1p', 
                                           legend.scale.breaks = plot.mean.legend.scale.breaks.g,
                                           facet_field = 'model', 
                                           facet_rows = 3))
      ggsave(filename = file.path(base_folder, paste0('gamma_model_predictions.png')), 
             plot = plot.all.mean, width = 5.9, height = 10, units = 'in')
      rm(plot.all.mean)
      
      # Plot SD values
      (plot.all.sd <- plot_covariates_gg(data = plot.data.g %>% 
                                            dplyr::filter(metric == 'SD'), 
                                         field = 'Probability', 
                                         legend.title = 'SD of\nPredicted\nBycatch',
                                         colormap = 'magma',
                                         legend.color.range = plot.sd.range.g,
                                         transformation = 'log1p', 
                                         legend.scale.breaks = plot.sd.legend.scale.breaks.g,
                                         facet_field = 'model', 
                                         facet_rows = 2))
      ggsave(filename = file.path(base_folder, paste0('gamma_model_predictions_sd.png')), 
             plot = plot.all.sd, width = 4.8, height = 7, units = 'in')
      rm(plot.all.sd)
   }
   
   # 1 massive plot/map of all HURDLE model predictions
   {
      # pivot grid 1 (pdat) data into long format so that I can use facet_wrap to plot them all at once.
      plot.data.h <- pdat %>% 
         # use the unprojected coordinates
         dplyr::mutate(lat = lat_unproj,
                       lon = lon_unproj) %>% 
         dplyr::select(lon, lat, 
                       dplyr::starts_with('z.pred'), # only want the predicted values
                       -dplyr::contains('.b'), # but not the binomial model predictions
                       -dplyr::contains('.g'), # and not the gamma model predictions
                       # -dplyr::contains('.h'), # and not the hurdle model predictions
                       # -dplyr::contains('.sd'), # and not the SD of predictions
                       -dplyr::contains('.link'), # nothing on the link scale
                       -dplyr::contains('.lcl'), # no confidence limits
                       -dplyr::contains('.ucl'), 
                       -dplyr::contains('.cl.range')) %>% 
         tidyr::pivot_longer(data = ., 
                             cols = starts_with('z.pred'), 
                             names_to = 'old_name', 
                             values_to = 'Probability') %>% 
         dplyr::mutate(metric = if_else(condition = grepl(pattern = '\\.sd', x = old_name), 
                                        true = 'SD', 
                                        false = 'Mean'),
                       model  = factor(if_else(grepl(pattern = '\\.h1$|\\.h1.sd$', x = old_name), # need to get both b1 and b1.sd but NOT b11 or b12
                                               true = 'GLM 1', false = 
                                                  if_else(grepl(pattern = '\\.h2', x = old_name), 
                                                          true = 'GLM 2', false = 
                                                             if_else(grepl(pattern = '\\.h3', x = old_name), 
                                                                     true = 'GAM 1', false = 
                                                                        if_else(grepl(pattern = '\\.h4', x = old_name), 
                                                                                true = 'GAM 2', false = 
                                                                                   if_else(grepl(pattern = '\\.h5', x = old_name), 
                                                                                           true = 'GAM 3', false = 
                                                                                              if_else(grepl(pattern = '\\.h6', x = old_name), 
                                                                                                      true = 'RF 1', false = 
                                                                                                         if_else(grepl(pattern = '\\.h7', x = old_name), 
                                                                                                                 true = 'RF 2', false = 
                                                                                                                    if_else(condition = grepl(pattern = '\\.h8', x = old_name), 
                                                                                                                            true = 'RF 3', false = 
                                                                                                                               if_else(condition = grepl(pattern = '\\.h9', x = old_name), 
                                                                                                                                       true = 'GBT 1', false = 
                                                                                                                                          if_else(condition = grepl(pattern = '\\.h10', x = old_name), 
                                                                                                                                                  true = 'GBT 2', false = 
                                                                                                                                                     if_else(grepl(pattern = '\\.h11', x = old_name), 
                                                                                                                                                             true = 'AVG 1', false = 
                                                                                                                                                                if_else(condition = grepl(pattern = '\\.h12', x = old_name), 
                                                                                                                                                                        true = 'AVG 2', false = '_problem_')))))))))))), 
                                       levels = c('GLM 1', 'GLM 2', 'GAM 1',
                                                  'GAM 2', 'GAM 3', 'RF 1',
                                                  'RF 2', 'RF 3', 'GBT 1', 'GBT 2',
                                                  'AVG 1', 'AVG 2')))
      
      # Plot mean values
      (plot.all.mean <- plot_covariates_gg(data = plot.data.h %>% 
                                              dplyr::filter(metric == 'Mean'), 
                                           field = 'Probability', 
                                           legend.title = 'Predicted\nBycatch',
                                           colormap = 'viridis',
                                           legend.color.range = plot.mean.range.h, 
                                           transformation = 'log1p', 
                                           legend.scale.breaks = plot.mean.legend.scale.breaks.h,
                                           facet_field = 'model', 
                                           facet_rows = 3))
      ggsave(filename = file.path(base_folder, paste0('hurdle_model_predictions.png')), 
             plot = plot.all.mean, width = 5.9, height = 10, units = 'in')
      rm(plot.all.mean)
   }
}

# spatial prediction maps of model averages (pred/grid 2)
# NOTE: currently I calculate the SD across years including models that do NOT allow
#       spatial-temporal interactions. That supresses the variation across years. 
{
   # aggregate pdat2 from various files
   {
      # models 1-5
      pdat2_1.5 <- readRDS(file.path(base_folder, paste0('temp_model_1-5_output.RDS')))$pdat2
      # model 6
      pdat2_6 <- readRDS(file.path(base_folder, paste0('temp_model_6_output.RDS')))$pdat2
      # model 7
      pdat2_7 <- readRDS(file.path(base_folder, paste0('temp_model_7_output.RDS')))$pdat2
      # model 8
      pdat2_8 <- readRDS(file.path(base_folder, paste0('temp_model_8_output.RDS')))$pdat2
      # models 9-10
      pdat2_9.10 <- readRDS(file.path(base_folder, paste0('temp_model_9-10_output.RDS')))$pdat2
      
      # join them all together
      # they should all still be in the same order, but I'll use a "join" instead of "rbind" just to be sure
      temp <- dplyr::left_join(x = pdat2_1.5, 
                               y = pdat2_6 %>% dplyr::select(c('lat', 'lon', 'year_f', names(pdat2_6)[ !(names(pdat2_6) %in% names(pdat2_1.5))])), 
                               by = c('lat', 'lon', 'year_f'))
      rm(pdat2_1.5); rm(pdat2_6)
      
      temp <- temp %>% 
         dplyr::left_join(x = ., 
                          y = pdat2_7 %>% dplyr::select(c('lat', 'lon', 'year_f', names(pdat2_7)[ !(names(pdat2_7) %in% names(temp))])), 
                          by = c('lat', 'lon', 'year_f'))
      rm(pdat2_7)
      
      temp <- temp %>% 
         dplyr::left_join(x = .,
                          y = pdat2_8 %>% dplyr::select(c('lat', 'lon', 'year_f', names(pdat2_8)[ !(names(pdat2_8) %in% names(temp))])), 
                          by = c('lat', 'lon', 'year_f'))
      rm(pdat2_8)
      
      pdat2 <- temp %>% 
         dplyr::left_join(x = .,
                          y = pdat2_9.10 %>% dplyr::select(c('lat', 'lon', 'year_f', names(pdat2_9.10)[ !(names(pdat2_9.10) %in% names(temp))])), 
                          by = c('lat', 'lon', 'year_f'))
      rm(temp); rm(pdat2_9.10)
      names(pdat2)
   }
   
   # add model averages into the map data
   pdat2 <- pdat2 %>% 
      dplyr::rowwise() %>% 
      dplyr::mutate(
         z.pred.b11 = mean(c(z.pred.b1, 
                             z.pred.b2, 
                             z.pred.b3, 
                             z.pred.b4, 
                             z.pred.b5, 
                             z.pred.b6, 
                             z.pred.b7, 
                             z.pred.b8, 
                             z.pred.b9,
                             z.pred.b10)),
         z.pred.b12 = mean(c(z.pred.b1, 
                             z.pred.b2, 
                             z.pred.b3, 
                             z.pred.b4,            
                             
                             z.pred.b6, 
                             z.pred.b7, 
                             z.pred.b8, 
                             z.pred.b9,
                             z.pred.b10)),
         z.pred.g11 = mean(c(z.pred.g1, 
                             z.pred.g2, 
                             z.pred.g3, 
                             z.pred.g4, 
                             z.pred.g5, 
                             z.pred.g6, 
                             
                             
                             z.pred.g9,
                             z.pred.g10)),
         z.pred.g12 = mean(c(z.pred.g1, 
                             z.pred.g2, 
                             z.pred.g3, 
                             z.pred.g4,            
                             
                             z.pred.g6, 
                             
                             
                             z.pred.g9,
                             z.pred.g10)),
         z.pred.h11 = mean(c(z.pred.h1, 
                             z.pred.h2, 
                             z.pred.h3, 
                             z.pred.h4, 
                             z.pred.h5, 
                             z.pred.h6, 
                             
                             
                             z.pred.h9,
                             z.pred.h10)),
         z.pred.h12 = mean(c(z.pred.h1, 
                             z.pred.h2, 
                             z.pred.h3, 
                             z.pred.h4,            
                             
                             z.pred.h6, 
                             
                             
                             z.pred.h9,
                             z.pred.h10))) %>% 
      dplyr::ungroup()
   
   # save results
   saveRDS(object = pdat2, file.path(base_folder, paste0('temp_pdat2_with_mod_avg.RDS')))
   
   
   mn <- 'AVG 1'
   # plot of spatial effect through time
   # This one does change every year, so I'll plot more years 
   plot.h11.space <- plot_covariates_gg(data = pdat2 %>%
                                           dplyr::mutate(lat = lat_unproj,
                                                         lon = lon_unproj) %>% 
                                           dplyr::filter(year_f %in% c(2002:2019)),
                                        field = 'z.pred.h11',
                                        legend.title = paste0(mn, '\nPredicted\nNumber\nof Bycatch'),
                                        colormap = 'viridis',
                                        legend.color.range = plot.mean.range.h, 
                                        transformation = 'log1p', 
                                        legend.scale.breaks = plot.mean.legend.scale.breaks.h,
                                        facet_field = 'year_f', 
                                        facet_rows = 3)
   ggsave(filename = file.path(base_folder, paste0('m11_spatial_predictions.png')), plot = plot.h11.space, width = 7.5, height = 9, units = 'in')
   # remove plot from RAM to save memory
   rm(plot.h11.space)
   
   # variation from year-to-year
   # can also map the SD to show areas that change the most from year to year
   ggsave(filename = file.path(base_folder, paste0('m11_spatial_SD_binomial_only.png')), 
          plot = plot_covariates_gg(data = pdat2 %>% 
                                       dplyr::mutate(lat = lat_unproj,
                                                     lon = lon_unproj) %>% 
                                       dplyr::group_by(lat, lon) %>% 
                                       dplyr::summarise(sd = sd(z.pred.b11), 
                                                        .groups = 'drop'),
                                    field = 'sd',
                                    legend.title = paste0(mn, '\nSD\n(across years)\nin Predicted\nProbability\nof Bycatch'),
                                    colormap = 'viridis'), 
          width = 4.5, height = 8, units = 'in')
   # gamma model
   ggsave(filename = file.path(base_folder, paste0('m11_spatial_SD_gamma_only.png')), 
          plot = plot_covariates_gg(data = pdat2 %>% 
                                       dplyr::mutate(lat = lat_unproj,
                                                     lon = lon_unproj) %>% 
                                       dplyr::group_by(lat, lon) %>% 
                                       dplyr::summarise(sd = sd(z.pred.g11), 
                                                        .groups = 'drop'),
                                    field = 'sd',
                                    legend.title = paste0(mn, '\nSD\n(across years)\nin Predicted\nBycatch'),
                                    legend.color.range = plot.mean.range.h, 
                                    transformation = 'log1p', 
                                    legend.scale.breaks = plot.mean.legend.scale.breaks.h, 
                                    colormap = 'viridis'), 
          width = 4.5, height = 8, units = 'in')
   # hurdle model
   ggsave(filename = file.path(base_folder, paste0('m11_spatial_SD_hurdle_model.png')), 
          plot = plot_covariates_gg(data = pdat2 %>% 
                                       dplyr::mutate(lat = lat_unproj,
                                                     lon = lon_unproj) %>% 
                                       dplyr::group_by(lat, lon) %>% 
                                       dplyr::summarise(sd = sd(z.pred.h11), 
                                                        .groups = 'drop'),
                                    field = 'sd',
                                    legend.title = paste0(mn, '\nSD\n(across years)\nin Predicted\nBycatch'),
                                    legend.color.range = plot.mean.range.h, 
                                    transformation = 'log1p', 
                                    legend.scale.breaks = plot.mean.legend.scale.breaks.h,
                                    colormap = 'viridis'), 
          width = 4.5, height = 8, units = 'in')
   
   
   
   
   
   
   
   
   
   mn <- 'AVG 2'
   # 2nd model average
   # plot of spatial effect through time
   # This one does change every year, so I'll plot more years 
   plot.h12.space <- plot_covariates_gg(data = pdat2 %>%
                                           dplyr::mutate(lat = lat_unproj,
                                                         lon = lon_unproj) %>% 
                                           dplyr::filter(year_f %in% c(2002:2019)),
                                        field = 'z.pred.h12',
                                        legend.title = paste0(mn, '\nPredicted\nNumber\nof Bycatch'),
                                        colormap = 'viridis',
                                        legend.color.range = plot.mean.range.h, 
                                        transformation = 'log1p', 
                                        legend.scale.breaks = plot.mean.legend.scale.breaks.h,
                                        facet_field = 'year_f', 
                                        facet_rows = 3)
   ggsave(filename = file.path(base_folder, paste0('m12_spatial_predictions.png')), plot = plot.h12.space, width = 7.5, height = 9, units = 'in')
   # remove plot from RAM to save memory
   rm(plot.h12.space)
   
   # variation from year-to-year
   # can also map the SD to show areas that change the most from year to year
   ggsave(filename = file.path(base_folder, paste0('m12_spatial_SD_binomial_only.png')), 
          plot = plot_covariates_gg(data = pdat2 %>% 
                                       dplyr::mutate(lat = lat_unproj,
                                                     lon = lon_unproj) %>% 
                                       dplyr::group_by(lat, lon) %>% 
                                       dplyr::summarise(sd = sd(z.pred.b12), 
                                                        .groups = 'drop'),
                                    field = 'sd',
                                    legend.title = paste0(mn, '\nSD\n(across years)\nin Predicted\nProbability\nof Bycatch'),
                                    colormap = 'viridis'), 
          width = 4.5, height = 8, units = 'in')
   # gamma model
   ggsave(filename = file.path(base_folder, paste0('m12_spatial_SD_gamma_only.png')), 
          plot = plot_covariates_gg(data = pdat2 %>% 
                                       dplyr::mutate(lat = lat_unproj,
                                                     lon = lon_unproj) %>% 
                                       dplyr::group_by(lat, lon) %>% 
                                       dplyr::summarise(sd = sd(z.pred.g12), 
                                                        .groups = 'drop'),
                                    field = 'sd',
                                    legend.title = paste0(mn, '\nSD\n(across years)\nin Predicted\nBycatch'),
                                    legend.color.range = plot.mean.range.h, 
                                    transformation = 'log1p', 
                                    legend.scale.breaks = plot.mean.legend.scale.breaks.h, 
                                    colormap = 'viridis'), 
          width = 4.5, height = 8, units = 'in')
   # hurdle model
   ggsave(filename = file.path(base_folder, paste0('m12_spatial_SD_hurdle_model.png')), 
          plot = plot_covariates_gg(data = pdat2 %>% 
                                       dplyr::mutate(lat = lat_unproj,
                                                     lon = lon_unproj) %>% 
                                       dplyr::group_by(lat, lon) %>% 
                                       dplyr::summarise(sd = sd(z.pred.h12), 
                                                        .groups = 'drop'),
                                    field = 'sd',
                                    legend.title = paste0(mn, '\nSD\n(across years)\nin Predicted\nBycatch'),
                                    legend.color.range = c(0,8), # plot.mean.range.h, 
                                    transformation = 'log1p', 
                                    legend.scale.breaks = c(0,1,2,4,8), # plot.mean.legend.scale.breaks.h,
                                    colormap = 'viridis'), 
          width = 4.5, height = 8, units = 'in')
}

# backup just to make sure I don't lose anything
# save.image(file = file.path(base_folder, paste0('workspace_image_after_model_averages.RDS')))
# 

# "Marginal effect" plots
{
   # list to hold results
   # sublist for each covariate
   # sublist for each component of the hurdle model
   outl <- vector('list', length(rf.covar))
   names(outl) <- rf.covar
   for(c in 1:length(rf.covar)){
      outl[[c]] <- vector('list', 3)
      names(outl[[c]]) <- c('Binomial', 'Gamma', 'Hurdle')
   }
   
   # Binomial models
   {
      # aggregate marginal_effect_list_b from various files
      {
         # models 1-5
         melb_1.5 <- readRDS(file.path(base_folder, paste0('temp_model_1-5_output.RDS')))$marginal_effect_list_b
         # model 6
         melb_6 <- readRDS(file.path(base_folder, paste0('temp_model_6_output.RDS')))$marginal_effect_list_b
         # model 7
         melb_7 <- readRDS(file.path(base_folder, paste0('temp_model_7_output.RDS')))$marginal_effect_list_b
         # model 8
         melb_8 <- readRDS(file.path(base_folder, paste0('temp_model_8_output.RDS')))$marginal_effect_list_b
         # models 9-10
         melb_9.10 <- readRDS(file.path(base_folder, paste0('temp_model_9-10_output.RDS')))$marginal_effect_list_b
         
         # join them all together
         
         # make another list to hold all the results
         melb <- vector('list', length(model_names))
         names(melb) <- model_names
         
         # model 1
         melb[['glm 1']] <- melb_1.5$`GLM 1`
         # model 2
         melb[['glm 2']] <- melb_1.5$`GLM 2`
         # make sure the names are correct on model 2: 
         names(melb[['glm 2']]) <- rf.covar
         # model 3
         melb[['gam 1']] <- melb_1.5$`GAM 1`
         # model 4
         melb[['gam 2']] <- melb_1.5$`GAM 2`
         # model 5
         melb[['gam 3']] <- melb_1.5$`GAM 3`
         # model 6
         melb[['rf 1']] <- melb_6$`RF 1`
         # model 7
         melb[['rf 2']] <- melb_7$`RF 2`
         # model 8
         melb[['rf 3']] <- melb_8$`RF 3`
         # model 9
         melb[['gbt 1']] <- melb_9.10$`GBT 1`
         # model 10
         melb[['gbt 2']] <- melb_9.10$`GBT 2`
         
         # str(melb, max.level = 2)
      }
      
      # combine binomial model estimates for each covariate
      {
         # loop over covariates
         for(v in 1:length(rf.covar)){
            
            cv <- rf.covar[v]
            
            # get the estimates for all the models
            cl <- lapply(model_names[ !(model_names %in% c('avg 1', 'avg 2'))], function(x){
               # get the binomial df
               bdf0 <- melb[[x]][[cv]]
               
               # convert to common format and return
               data.frame(
                  covariate = bdf0$covariate,
                  value     = bdf0[,cv],
                  bycatch   = bdf0$bycatch,
                  model     = bdf0$model
               )
            })
            
            bdf <- do.call(rbind, cl)
            
            # calculate model average 1
            ma.1 <- bdf %>% 
               dplyr::group_by(covariate, value) %>%
               dplyr::summarize(
                  bycatch = mean(bycatch),
                  model = 'AVG 1',
                  .groups = 'drop'
               )
            
            # calculate model average 2
            ma.2 <- bdf %>% 
               dplyr::filter(model != 'GAM 3') %>% 
               dplyr::group_by(covariate, value) %>% 
               dplyr::summarize(
                  bycatch = mean(bycatch),
                  model = 'AVG 2',
                  .groups = 'drop'
               )
            
            # add the model averages back to the list
            melb[['avg 1']][[cv]] <- ma.1
            melb[['avg 2']][[cv]] <- ma.2
            
            # combine the model average dataframes with the original dataframe
            # add save to the outl list
            outl[[cv]][['Binomial']] <- rbind(
               bdf,
               ma.1,
               ma.2
            ) %>% 
               dplyr::mutate(model = factor(toupper(model), levels = toupper(model_names)))
         }
         # Don't combine the results into a single DF b/c some covariates are factors. Joining will force the "value" column to be character instead of numeric.
      }
      
      # plot each covariate
      {
         # empty lists to hold the marginal plot
         mppl_b <- vector('list', length(rf.covar))
         names(mppl_b) <- rf.covar
         
         # loop over covariates
         for(c in 1:length(rf.covar)){
            
            cv <- rf.covar[c]
            
            # get the dataset for the covariate
            dfp <- outl[[cv]][['Binomial']]
            
            
            # got the deciles of the original data 
            if( !(rf.covar[c] %in% c('sector', 'year_f')) ){
               # get the original data
               d <- bdat[, cv]
               
               # calculate the deciles
               deciles <- data.frame(value = quantile(d, 0:10/10),
                                     size = c(2, rep(1, 9), 2))
               
               # could convert the deciles to match the original, unprojected coordinates, 
               # but, again, because I vary the coordinates independently for these plots, 
               # the unprojected coordinates won't line up line you expect
               
               # if(rfc == 'lon'){
               #    c1 <- deciles %>%
               #       dplyr::mutate(lon = value,
               #                     lat_proj = mean(dat$lat_proj),
               #                     lon_proj = lon * 1000 + mean(dat$lon_proj)) %>%
               #       # convert to a spatial "sf" object
               #       sf::st_as_sf(.,
               #                    coords = c('lon_proj', 'lat_proj'),
               #                    crs = '+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs',
               #                    remove = F) %>%
               #       # transform to standard unprojected coordinates
               #       sf::st_transform(crs = 4326) %>% 
               #       sf::st_coordinates()
               #    
               #    deciles$value <- c1[,'X']
               #    } else if(rfc == 'lat'){
               #       c1 <- deciles %>%
               #          dplyr::mutate(lat = value,
               #                        lat_proj = lat * 1000 + mean(dat$lat_proj),
               #                        lon_proj = mean(dat$lon_proj)) %>%
               #          # convert to a spatial "sf" object
               #          sf::st_as_sf(.,
               #                       coords = c('lon_proj', 'lat_proj'),
               #                       crs = '+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs',
               #                       remove = F) %>%
               #          # transform to standard unprojected coordinates
               #          sf::st_transform(crs = 4326) %>% 
               #          sf::st_coordinates()
               #       
               #       deciles$value <- c1[,'Y']
               # }
            }
            
            # plots the covariate
            (pl <- ggplot(dfp, 
                          aes(x = value, 
                              y = bycatch, 
                              color = model)) + 
                  {if(cv %in% c('sector', 'year_f')) geom_point(size = 3) else geom_line(size = 1) } +
                  {if( !(cv %in% c('sector', 'year_f')) ) geom_rug(aes(x = value, size = size), data = deciles, inherit.aes = F)} + 
                  {if( !(cv %in% c('sector', 'year_f')) ) scale_size_continuous(range = c(0.8, 1.4), guide = 'none') } + 
                  coord_cartesian(ylim = c(0,1)) + 
                  theme_bw() + 
                  ylab(label = 'Probability of Bycatch') + 
                  xlab(label = parse(text = lsp_lut[ match(rf.covar[c], lsp_lut$original_name), 'plot_label'])))
            
            # add the plot to the list of plots
            mppl_b[[c]] <- pl
         }
         # view all the plots
         mppl_b
      }
   }
   
   # Gamma models
   {
      # aggregate marginal_effect_list_g from various files
      {
         # models 1-5
         melg_1.5 <- readRDS(file.path(base_folder, paste0('temp_model_1-5_output.RDS')))$marginal_effect_list_g
         # model 6
         melg_6 <- readRDS(file.path(base_folder, paste0('temp_model_6_output.RDS')))$marginal_effect_list_g
         # model 7
         melg_7 <- readRDS(file.path(base_folder, paste0('temp_model_7_output.RDS')))$marginal_effect_list_g
         # model 8
         melg_8 <- readRDS(file.path(base_folder, paste0('temp_model_8_output.RDS')))$marginal_effect_list_g
         # models 9-10
         melg_9.10 <- readRDS(file.path(base_folder, paste0('temp_model_9-10_output.RDS')))$marginal_effect_list_g
         
         # join them all together
         
         # make another list to hold all the results
         melg <- vector('list', length(model_names))
         names(melg) <- model_names
         
         # model 1
         melg[['glm 1']] <- melg_1.5$`GLM 1`
         # model 2
         melg[['glm 2']] <- melg_1.5$`GLM 2`
         # make sure the names are correct on model 2: 
         names(melg[['glm 2']]) <- rf.covar
         # model 3
         melg[['gam 1']] <- melg_1.5$`GAM 1`
         # model 4
         melg[['gam 2']] <- melg_1.5$`GAM 2`
         # model 5
         melg[['gam 3']] <- melg_1.5$`GAM 3`
         # model 6
         melg[['rf 1']] <- melg_6$`RF 1`
         # model 7
         melg[['rf 2']] <- melg_7$`RF 2`
         # model 8
         melg[['rf 3']] <- melg_8$`RF 3`
         # model 9
         melg[['gbt 1']] <- melg_9.10$`GBT 1`
         # model 10
         melg[['gbt 2']] <- melg_9.10$`GBT 2`
         
         # str(melg, max.level = 1)
      }
      
      # combine binomial model estimates for each covariate
      {
         # loop over covariates
         for(v in 1:length(rf.covar)){
            
            cv <- rf.covar[v]
            
            # get the estimates for all the models
            cl <- lapply(model_names[ !(model_names %in% c('avg 1', 'avg 2'))], function(x){
               # get the binomial df
               bdf0 <- melg[[x]][[cv]]
               
               # convert to common format and return
               data.frame(
                  covariate = bdf0$covariate,
                  value     = bdf0[,cv],
                  bycatch   = bdf0$bycatch,
                  model     = bdf0$model
               )
            })
            
            bdf <- do.call(rbind, cl)
            
            # calculate model average 1
            ma.1 <- bdf %>% 
               dplyr::group_by(covariate, value) %>%
               dplyr::summarize(
                  bycatch = mean(bycatch),
                  model = 'AVG 1',
                  .groups = 'drop'
               )
            
            # calculate model average 2
            ma.2 <- bdf %>% 
               dplyr::filter(model != 'GAM 3') %>% 
               dplyr::group_by(covariate, value) %>% 
               dplyr::summarize(
                  bycatch = mean(bycatch),
                  model = 'AVG 2',
                  .groups = 'drop'
               )
            
            # add the model averages back to the list
            melg[['avg 1']][[cv]] <- ma.1
            melg[['avg 2']][[cv]] <- ma.2
            
            # combine the model average dataframes with the original dataframe
            # add save to the outl list
            outl[[cv]][['Gamma']] <- rbind(
               bdf,
               ma.1,
               ma.2
            ) %>% 
               dplyr::mutate(model = factor(toupper(model), levels = toupper(model_names)))
         }
         # Don't combine the results into a single DF b/c some covariates are factors. Joining will force the "value" column to be character instead of numeric.
      }
      
      # plot each covariate
      {
         # empty lists to hold the marginal plot
         mppl_g <- vector('list', length(rf.covar))
         names(mppl_g) <- rf.covar
         
         # loop over covariates
         for(c in 1:length(rf.covar)){
            
            cv <- rf.covar[c]
            
            # get the dataset for the covariate
            dfp <- outl[[cv]][['Gamma']]
            
            
            # got the deciles of the original data 
            if( !(rf.covar[c] %in% c('sector', 'year_f')) ){
               # get the original data
               d <- bdat[, cv]
               
               # calculate the deciles
               deciles <- data.frame(value = quantile(d, 0:10/10),
                                     size = c(2, rep(1, 9), 2))
               
               # could convert the deciles to match the original, unprojected coordinates, 
               # but, again, because I vary the coordinates independently for these plots, 
               # the unprojected coordinates won't line up line you expect
               
               # if(rfc == 'lon'){
               #    c1 <- deciles %>%
               #       dplyr::mutate(lon = value,
               #                     lat_proj = mean(dat$lat_proj),
               #                     lon_proj = lon * 1000 + mean(dat$lon_proj)) %>%
               #       # convert to a spatial "sf" object
               #       sf::st_as_sf(.,
               #                    coords = c('lon_proj', 'lat_proj'),
               #                    crs = '+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs',
               #                    remove = F) %>%
               #       # transform to standard unprojected coordinates
               #       sf::st_transform(crs = 4326) %>% 
               #       sf::st_coordinates()
               #    
               #    deciles$value <- c1[,'X']
               #    } else if(rfc == 'lat'){
               #       c1 <- deciles %>%
               #          dplyr::mutate(lat = value,
               #                        lat_proj = lat * 1000 + mean(dat$lat_proj),
               #                        lon_proj = mean(dat$lon_proj)) %>%
               #          # convert to a spatial "sf" object
               #          sf::st_as_sf(.,
               #                       coords = c('lon_proj', 'lat_proj'),
               #                       crs = '+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs',
               #                       remove = F) %>%
               #          # transform to standard unprojected coordinates
               #          sf::st_transform(crs = 4326) %>% 
               #          sf::st_coordinates()
               #       
               #       deciles$value <- c1[,'Y']
               # }
            }
            
            # plots the covariate
            (pl <- ggplot(dfp, 
                          aes(x = value, 
                              y = bycatch, 
                              color = model)) + 
                  {if(cv %in% c('sector', 'year_f')) geom_point(size = 3) else geom_line(size = 1) } +
                  {if( !(cv %in% c('sector', 'year_f')) ) geom_rug(aes(x = value, size = size), data = deciles, inherit.aes = F)} + 
                  {if( !(cv %in% c('sector', 'year_f')) ) scale_size_continuous(range = c(0.8, 1.4), guide = 'none') } + 
                  theme_bw() + 
                  ylab(label = 'Probability of Bycatch') + 
                  xlab(label = parse(text = lsp_lut[ match(rf.covar[c], lsp_lut$original_name), 'plot_label'])))
            
            # add the plot to the list of plots
            mppl_g[[c]] <- pl
         }
         # view all the plots
         mppl_g
      }
   }
   
   # Hurdle models
   {
      # combine binomial & gamma model estimates
      {
         # loop over all covariates
         for(v in 1:length(rf.covar)){
            cv <- rf.covar[v]
            
            # get the binomial df
            bdf <- outl[[cv]][['Binomial']] %>% 
               dplyr::mutate(bycatch_binomial = bycatch) %>% 
               dplyr::select(-bycatch)
            
            # get the gamma df
            gdf <- outl[[cv]][['Gamma']] %>% 
               dplyr::mutate(bycatch_gamma = bycatch) %>% 
               dplyr::select(-bycatch)
            
            # join the binomial & gamma predictions together. 
            # store the results to a dataframe
            hdf <- full_join(x = bdf, y = gdf, by = c('covariate', 'value', 'model')) %>% 
               mutate(bycatch = bycatch_binomial * bycatch_gamma)
            
            # NOTE: this creates a dataframe where the hurdle model averages DO include the effects of RF 2 & RF 3
            #       b/c it just multiples the model averages for binomial (including RF 2 & RF 3) by the model averages
            #       for gamma (NOT including RF 2 & RF 3)
            
            # Optionally re-calculate model averages NOT including RF 2 & RF 3
            if(TRUE){
               ma.1 <- hdf %>% 
                  # exclude the model averages
                  dplyr::filter( !(model %in% c('AVG 1', 'AVG 2')) ) %>% 
                  # group by covariate values
                  dplyr::group_by(covariate, value) %>%
                  # re-calculate model average
                  dplyr::summarize(
                     bycatch_binomial = mean(bycatch_binomial, na.rm = T),
                     bycatch_gamma = mean(bycatch_gamma, na.rm = T),
                     model = 'AVG 1',
                     .groups = 'drop'
                  ) %>% 
                  dplyr::mutate(bycatch = bycatch_binomial * bycatch_gamma)
               
               # calculate model average 2
               ma.2 <- hdf %>% 
                  # exclude the model averages & GAM 3
                  dplyr::filter( !(model %in% c('GAM 3', 'AVG 1', 'AVG 2')) ) %>% 
                  # group by covariate values
                  dplyr::group_by(covariate, value) %>%
                  # re-calculate model average
                  dplyr::summarize(
                     bycatch_binomial = mean(bycatch_binomial, na.rm = T),
                     bycatch_gamma = mean(bycatch_gamma, na.rm = T),
                     model = 'AVG 2',
                     .groups = 'drop'
                  ) %>% 
                  dplyr::mutate(bycatch = bycatch_binomial * bycatch_gamma)
               
               # combine the model average dataframes with the original dataframe
               # add save to the list
               hdf <- rbind(
                  hdf %>% dplyr::filter( !(model %in% c('AVG 1', 'AVG 2')) ),
                  ma.1 %>% dplyr::select(names(hdf)),
                  ma.2 %>% dplyr::select(names(hdf))
               )
            }
            
            # save results
            outl[[cv]][['Hurdle']] <- hdf
         }
      }

      # plot each covariate
      {
         # empty lists to hold the marginal plot
         mppl_h <- vector('list', length(rf.covar))
         names(mppl_h) <- rf.covar
         
         # loop over covariates
         for(c in 1:length(rf.covar)){
            
            cv <- rf.covar[c]
            
            # get the dataset for the covariate
            dfp <- outl[[cv]][['Hurdle']]
            
            
            # got the deciles of the original data 
            if( !(rf.covar[c] %in% c('sector', 'year_f')) ){
               # get the original data
               d <- bdat[, cv]
               
               # calculate the deciles
               deciles <- data.frame(value = quantile(d, 0:10/10),
                                     size = c(2, rep(1, 9), 2))
               
               # could convert the deciles to match the original, unprojected coordinates, 
               # but, again, because I vary the coordinates independently for these plots, 
               # the unprojected coordinates won't line up line you expect
               
               # if(rfc == 'lon'){
               #    c1 <- deciles %>%
               #       dplyr::mutate(lon = value,
               #                     lat_proj = mean(dat$lat_proj),
               #                     lon_proj = lon * 1000 + mean(dat$lon_proj)) %>%
               #       # convert to a spatial "sf" object
               #       sf::st_as_sf(.,
               #                    coords = c('lon_proj', 'lat_proj'),
               #                    crs = '+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs',
               #                    remove = F) %>%
               #       # transform to standard unprojected coordinates
               #       sf::st_transform(crs = 4326) %>% 
               #       sf::st_coordinates()
               #    
               #    deciles$value <- c1[,'X']
               #    } else if(rfc == 'lat'){
               #       c1 <- deciles %>%
               #          dplyr::mutate(lat = value,
               #                        lat_proj = lat * 1000 + mean(dat$lat_proj),
               #                        lon_proj = mean(dat$lon_proj)) %>%
               #          # convert to a spatial "sf" object
               #          sf::st_as_sf(.,
               #                       coords = c('lon_proj', 'lat_proj'),
               #                       crs = '+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs',
               #                       remove = F) %>%
               #          # transform to standard unprojected coordinates
               #          sf::st_transform(crs = 4326) %>% 
               #          sf::st_coordinates()
               #       
               #       deciles$value <- c1[,'Y']
               # }
            }
            
            # plots the covariate
            (pl <- ggplot(dfp, 
                          aes(x = value, 
                              y = bycatch, 
                              color = model)) + 
                  {if(cv %in% c('sector', 'year_f')) geom_point(size = 3) else geom_line(size = 1) } +
                  {if( !(cv %in% c('sector', 'year_f')) ) geom_rug(aes(x = value, size = size), data = deciles, inherit.aes = F)} + 
                  {if( !(cv %in% c('sector', 'year_f')) ) scale_size_continuous(range = c(0.8, 1.4), guide = 'none') } + 
                  theme_bw() + 
                  ylab(label = 'Probability of Bycatch') + 
                  xlab(label = parse(text = lsp_lut[ match(rf.covar[c], lsp_lut$original_name), 'plot_label'])))
            
            # add the plot to the list of plots
            mppl_h[[c]] <- pl
         }
         # view all the plots
         mppl_h
      }
   }
   
   # combination plots
   {
      cpl <- vector('list', length = length(rf.covar))
      names(cpl) <- rf.covar
      
      for( c in 1:length(rf.covar) ){
         cv <- rf.covar[c]
         
         # get the 3 datasets and combine them
         dfp <- rbind(
            # get the binomial df again so that its model averages will include RF 2 & RF 3
            outl[[cv]][['Binomial']] %>% 
               dplyr::mutate(hurdle = 'Binomial'), 
            # get the gamma df
            outl[[cv]][['Gamma']] %>% 
               dplyr::mutate(#bycatch = log(1 + bycatch),
                             hurdle = 'Gamma'),
            # get the hurdle df
            outl[[cv]][['Hurdle']] %>% 
               dplyr::mutate(#bycatch = log(1 + bycatch),
                             hurdle = 'Hurdle') %>% 
               dplyr::select(covariate, value, bycatch, model, hurdle)
         ) %>% 
            dplyr::mutate(hurdle = factor(hurdle))
         
         if( !(cv %in% c('sector', 'year_f')) ){
            # got the deciles of the original data 
            # get the original data
            d <- bdat[, cv]
            
            # calculate the deciles
            deciles <- data.frame(value = quantile(d, 0:10/10),
                                  size = c(2, rep(1, 9), 2))
            
            # plots the covariate
            (pl <- ggplot(dfp %>% na.omit(), 
                          aes(x = value, 
                              y = bycatch, 
                              color = model)) + 
                  geom_line(size = 1) +
                  geom_rug(aes(x = value, size = size), data = deciles, inherit.aes = F) + 
                  scale_size_continuous(range = c(0.8, 1.4), guide = 'none') + 
                  theme_bw() + 
                  ylab(label = 'Bycatch') + 
                  xlab(label = parse(text = lsp_lut[ match(cv, lsp_lut$original_name), 'plot_label'])) + 
                  facet_wrap(facets = dplyr::vars(hurdle), scales = "free"))
            
            colors <- scales::hue_pal()(length(levels(dfp$model)))
            
            # plot each independently and them combine them: 
            (pl1 <- ggplot(dfp %>% na.omit() %>% dplyr::filter(hurdle == 'Binomial'), 
                           aes(x = value, 
                               y = bycatch, 
                               color = model)) + 
                  geom_line(size = 1) +
                  geom_rug(aes(x = value, size = size), data = deciles, inherit.aes = F) + 
                  scale_size_continuous(range = c(0.8, 1.4), guide = 'none') + 
                  theme_bw() + 
                  ylab(label = '') + 
                  scale_y_continuous(labels = scales::percent_format(), limits = c(0,1)) +
                  scale_color_manual(values = colors, 
                                     breaks = levels(dfp$model), 
                                     name = 'Model') + 
                  # xlab(label = parse(text = lsp_lut[ match(cv, lsp_lut$original_name), 'plot_label'])) + 
                  xlab(label = '') + 
                  theme(axis.text.x = element_text(angle = 90, 
                                                   vjust = 0.5, 
                                                   hjust = 1),
                        plot.margin = unit(x = c(0.1, 0.1,0.1,-0.75), units = 'lines'),
                        axis.text.y = element_text(angle = 90, vjust = 0, hjust = 0.5)) +
                  facet_wrap(facets = dplyr::vars(hurdle), scales = "free"))
            
            # put Gamma & Hurdle on the same scale
            yrange <- dfp %>% na.omit() %>% dplyr::filter(hurdle != 'Binomial') %>% pull(bycatch) %>% range()
            
            (pl2 <- ggplot(dfp %>% na.omit() %>% dplyr::filter(hurdle == 'Gamma'), 
                           aes(x = value, 
                               y = bycatch, 
                               color = model)) + 
                  geom_line(size = 1) +
                  geom_rug(aes(x = value, size = size), data = deciles, inherit.aes = F) + 
                  scale_size_continuous(range = c(0.8, 1.4), guide = 'none') + 
                  theme_bw() + 
                  ylab(label = '') + 
                  scale_y_continuous(trans = 'log1p', 
                                     limits = yrange)+
                  scale_color_manual(values = colors, 
                                     breaks = levels(dfp$model), 
                                     guide = 'none') + 
                  xlab(label = parse(text = lsp_lut[ match(cv, lsp_lut$original_name), 'plot_label'])) + 
                  theme(axis.text.x = element_text(angle = 90, 
                                                   vjust = 0.5, 
                                                   hjust = 1),
                        plot.margin = unit(x = c(0.1, 0.1,0.1,-0.75), units = 'lines'),
                        axis.text.y = element_text(angle = 90, vjust = 0, hjust = 0.5)) +
                  facet_wrap(facets = dplyr::vars(hurdle), scales = "free"))
            (pl3 <- ggplot(dfp %>% na.omit() %>% dplyr::filter(hurdle == 'Hurdle'), 
                           aes(x = value, 
                               y = bycatch, 
                               color = model)) + 
                  geom_line(size = 1) +
                  geom_rug(aes(x = value, size = size), data = deciles, inherit.aes = F) + 
                  scale_size_continuous(range = c(0.8, 1.4), guide = 'none') + 
                  theme_bw() + 
                  ylab(label = 'Bycatch') + 
                  scale_y_continuous(trans = 'log1p', 
                                     limits = yrange)+
                  scale_color_manual(values = colors, 
                                     breaks = levels(dfp$model), 
                                     guide = 'none') + 
                  # xlab(label = parse(text = lsp_lut[ match(cv, lsp_lut$original_name), 'plot_label'])) + 
                  xlab(label = '') + 
                  theme(axis.text.x = element_text(angle = 90, 
                                                   vjust = 0.5, 
                                                   hjust = 1),
                        plot.margin = unit(x = c(0.1, 0.1,0.1,0), units = 'lines'),
                        axis.text.y = element_text(angle = 90, vjust = 0, hjust = 0.5)) +
                  facet_wrap(facets = dplyr::vars(hurdle), scales = "free"))
            
            ggsave(filename = file.path(base_folder, paste0('marginal_effect_plot_', cv ,'.png')),
                   plot = cowplot::plot_grid(pl3, pl2, pl1, nrow = 1, rel_widths = c(.5, .2, .3)), 
                   width = 6.5 * 1.5, height = 4 * 1.5, units = 'in')
         } else {
            # plots the covariate
            (pl <- ggplot(dfp %>% na.omit(), 
                          aes(x = value, 
                              y = bycatch, 
                              color = model)) + 
                geom_point(size = 3) +
                theme_bw() + 
                ylab(label = 'Bycatch') + 
                xlab(label = parse(text = lsp_lut[ match(cv, lsp_lut$original_name), 'plot_label'])) + 
                theme(axis.text.x = element_text(angle = 90, 
                                                 vjust = 0.5, 
                                                 hjust = 1)) +
                facet_wrap(facets = dplyr::vars(hurdle), scales = "free"))
            
            colors <- scales::hue_pal()(length(levels(dfp$model)))
            
            # plot each independently and them combine them: 
            (pl1 <- ggplot(dfp %>% na.omit() %>% dplyr::filter(hurdle == 'Binomial'), 
                           aes(x = value, 
                               y = bycatch, 
                               color = model)) + 
                  geom_point(size = 3) +
                  theme_bw() + 
                  ylab(label = '') + 
                  scale_y_continuous(labels = scales::percent_format(), limits = c(0,1)) +
                  scale_color_manual(values = colors, 
                                     breaks = levels(dfp$model), 
                                     name = 'Model') + 
                  # xlab(label = parse(text = lsp_lut[ match(cv, lsp_lut$original_name), 'plot_label'])) + 
                  xlab(label = '') + 
                  theme(axis.text.x = element_text(angle = 90, 
                                                   vjust = 0.5, 
                                                   hjust = 1),
                        plot.margin = unit(x = c(0.1, 0.1,0.1,-0.75), units = 'lines'),
                        axis.text.y = element_text(angle = 90, vjust = 0, hjust = 0.5)) +
                  facet_wrap(facets = dplyr::vars(hurdle), scales = "free"))
            
            # put Gamma & Hurdle on the same scale
            yrange <- dfp %>% na.omit() %>% dplyr::filter(hurdle != 'Binomial') %>% pull(bycatch) %>% range()
            
            (pl2 <- ggplot(dfp %>% na.omit() %>% dplyr::filter(hurdle == 'Gamma'), 
                           aes(x = value, 
                               y = bycatch, 
                               color = model)) + 
                  geom_point(size = 3) +
                  theme_bw() + 
                  ylab(label = '') + 
                  scale_y_continuous(trans = 'log1p', 
                                     limits = yrange)+
                  scale_color_manual(values = colors, 
                                     breaks = levels(dfp$model), 
                                     guide = 'none') + 
                  xlab(label = parse(text = lsp_lut[ match(cv, lsp_lut$original_name), 'plot_label'])) + 
                  theme(axis.text.x = element_text(angle = 90, 
                                                   vjust = 0.5, 
                                                   hjust = 1),
                        plot.margin = unit(x = c(0.1, 0.1,0.1,-0.75), units = 'lines'),
                        axis.text.y = element_text(angle = 90, vjust = 0, hjust = 0.5)) +
                  facet_wrap(facets = dplyr::vars(hurdle), scales = "free"))
            (pl3 <- ggplot(dfp %>% na.omit() %>% dplyr::filter(hurdle == 'Hurdle'), 
                           aes(x = value, 
                               y = bycatch, 
                               color = model)) + 
                  geom_point(size = 3) +
                  theme_bw() + 
                  ylab(label = 'Bycatch') + 
                  scale_y_continuous(trans = 'log1p', 
                                     limits = yrange)+
                  scale_color_manual(values = colors, 
                                     breaks = levels(dfp$model), 
                                     guide = 'none') + 
                  # xlab(label = parse(text = lsp_lut[ match(cv, lsp_lut$original_name), 'plot_label'])) + 
                  xlab(label = '') + 
                  theme(axis.text.x = element_text(angle = 90, 
                                                   vjust = 0.5, 
                                                   hjust = 1),
                        plot.margin = unit(x = c(0.1, 0.1,0.1,0), units = 'lines'),
                        axis.text.y = element_text(angle = 90, vjust = 0, hjust = 0.5)) +
               facet_wrap(facets = dplyr::vars(hurdle), scales = "free"))
         
            ggsave(filename = file.path(base_folder, paste0('marginal_effect_plot_', cv ,'.png')),
                   plot = cowplot::plot_grid(pl3, pl2, pl1, nrow = 1, rel_widths = c(.5, .2, .3)), 
                   width = 6.5 * 1.5, height = 4 * 1.5, units = 'in')
         }
         
         # save the subplots
         cpl[[cv]][['Binomial']] <- pl1
         cpl[[cv]][['Gamma']]    <- pl2
         cpl[[cv]][['Hurdle']]   <- pl3
         cpl[[cv]][['Combined']] <- cowplot::plot_grid(pl3, pl2, pl1, nrow = 1, rel_widths = c(.5, .2, .3))
      }
   }
}
