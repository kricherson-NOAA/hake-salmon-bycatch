# Define functions for fitting models & calculating summary information

# Functions: 
# 1) fit_m1_glm  = GLM with either linear or polynomial terms, depending on "covariates" argument
#                   used to fit model "GLM 1"
#                   used to fit model "GLM 2"
# 2) fit_m2_gam1 = GAM with linear or polynomial terms, depending on "covariates" argument, and spatial smooth [s(lon, lat, k=100)]
#                   used to fit model "GAM 1"
# 3) fit_m3_gam2 = GAM with low-basis-dimension smooths [s(covariate, bs="cr", k=4, fx = TRUE)] and spatial smooth [s(lon, lat, k=100)]
#                   used to fit model "GAM 2"
# 4) fit_m4_gam3 = GAM with low-basis-dimension smooths [s(covariate, bs="cr", k=4, fx = FALSE)] and spatio-temporal smooth: [either s(lon, lat, k = 25, by = year_f) for factor time or te(lon, lat, time, bs = c('tp', 'tp'), k = c(40,8), d=c(2,1)) for continuous time]
#                   used to fit model "GAM 3"
# 5) fit_m5_INLA = INLA with single GMRF [NOTE: I gave up on this a while ago, and haven't been updating it to match other models. I'm just leaving it in so that someone can resurect it if they so desire.]
# 6) fit_m6_rf1  = Random Forest 1: [probability or regression tree; 1500 trees, mtry = 3, sample without replacement]
#                   used to fit model "RF 1"
# 7) fit_m7_rf2  = Random Forest with downsampling of over-represented class (aka Balanced Random Forest) [downsampling + same settings as RF1]
#                   used to fit model "RF 2"
# 8) fit_m8_rf3  = Random Forest with Synthetic Minority (no bycatch) Over-sampling (SMOTE) [downsampling + oversampling + same settings as RF1]
#                   used to fit model "RF 3"
# 9) fit_m9_gbt1 = Gradient Boosting Trees 
#                   used to fit model "GBT 1"
#10) fit_m10_gbt2 = Gradient Boosting Trees with DART
#                   used to fit model "GBT 2"

# calc_AUC  = function to calculate Area Under the Receiver-Operating Curve
# calc_RMSE = function to calculate Root-Mean Squared Error (optionally standardized)
# process_covariates = function to calculate covariate transformations. I made this into a function 
#                      because I transform covariates based only on the training datasets
#                      before fitting models rather than doing a single transformation on 
#                      the full dataset.
# lsp_lut = a dataframe specifying which transformations should be applied to each
#           covariate. It's also used for making marginal effect plots. 


# GLM with either linear or polynomial terms, depending on "covariates" argument
fit_m1_glm <- function(data,        # dataset with response = 'bycatch' and all covariates
                       covariates,  # character vector of covariate names (matching column names in "data")
                       modelfamily, # "binomial" or "gamma"
                       rows.train,  # rows of "data" to use in training/fitting
                       rows.test,
                       prediction.data = NULL, # optional 2ndary dataset to use the model to make more predictions without having to save the model
                       save.model.fit = FALSE, # whether or not to return the model fit object with other results
                       log_model = FALSE, # whether or not to save the model fit to file using "log_model_filename" argument
                       log_model_filename){  # rows of "data" to use in testing/validating
   # record the start time
   time1 <- Sys.time()
   
   # create the formula for the model
   formula.glm <- as.formula(paste0("bycatch ~ ", paste(covariates, collapse=" + ")))
   
   # fit the model inside of a tryCatch call so that an error won't stop the whole script
   out <- tryCatch(expr = {
      # fit the model
      fit <- mgcv::gam(formula = formula.glm, 
                       family = ifelse(modelfamily=='binomial', 
                                       'binomial', 
                                       'Gamma(link="log")'),
                       data = data[rows.train,])
      
      # optionally save the model fit object to file
      if(log_model){
         saveRDS(object = fit, file = log_model_filename)
      }
      
      # whether or not the model converged, according to mgcv package
      model_converged <- fit$converged
      
      # calculated expected values for the testing dataset (if data is provided)
      observed <- NA
      expected <- NA
      if( length(rows.test) > 0 ){
         if(any(!is.null(rows.test)) & any(!is.na(rows.test))) { # prevents crashing when there's no testing data provided
            observed <- data[rows.test, 'bycatch'] # observations at test locations
            expected <- as.vector(mgcv::predict.gam(fit,
                                                    newdata = data[rows.test,], 
                                                    type = 'response')) # return prediction on the response scale (either probability of success or number of bycatch)
      }}
      
      # make predictions using extra prediction dataset
      if(!is.null(prediction.data)){
         # make sure all the necessary covariates are in the prediction dataset
         if( !all( covariates %in% names(prediction.data) ) ){
            print('Not using "prediction.data" to make predictions because prediction.data is missing necessary covariate data.')  
         } else {
            predictions <- tryCatch(expr = {
               as.vector(mgcv::predict.gam(fit,
                                           newdata = prediction.data[,covariates], 
                                           type = 'response'))},
               error=function(cond) {
                  message(cond)
                  return = rep(NA, nrow(prediction.data))
               })
         }
      }
      
      # calculate fit metric
      if(modelfamily=='binomial'){
         # calculate AUC
         (gof  <- calc_AUC(expected, observed))
      } 
      if(modelfamily=='gamma'){
         # calculate root mean squared error
         (gof <- calc_RMSE(expected, as.numeric(as.character(observed))))
      }
      
      # finish time
      time2 <- Sys.time()
      # total runtime
      runtime <- as.numeric(round(difftime(time2, time1, units = 'min'), 1))
      
      # return results
      results <- list('gof' = gof, 
                      'observed' = observed, 
                      'expected' = expected, 
                      'runtime'  = runtime,
                      'error' = NA,
                      'model' = NA,
                      'model_converged' = model_converged)
      # optionally return model fit & prediction dataset
      if(save.model.fit) results[['model']] <- fit
      if(!is.null(prediction.data)) results[['predictions']] <- list('haul_id' = prediction.data$haul_id, 'predictions' = predictions)
      names(results)[1] <- if(modelfamily=='binomial') 'AUC' else 'RMSE'
      results
   }, 
   error=function(cond) {
      message(cond)
      
      results <- list('gof' = NA_real_, 
                      'observed' = data[rows.test,'bycatch'],
                      'expected' = rep(NA_real_, length(rows.test)), 
                      'runtime'  = as.numeric(round(difftime( Sys.time(), time1, units = 'min'), 1)),
                      'error' = cond,
                      'model' = NA,
                      'predictions' = NULL,
                      model_converged = FALSE)
      names(results)[1] <- if(modelfamily=='binomial') 'AUC' else 'RMSE'
      
      return(results)
   })
   
   return(out)
}


# function to fit model 2: GAM with spatial smooth (linear or polynomial terms, depending on "covariates" argument)
fit_m2_gam1 <- function(data,        # dataset with response = 'bycatch' and all covariates
                        covariates,  # character vector of covariate names (matching column names in "data")
                        modelfamily, # "binomial" or "gamma"
                        rows.train,  # rows of "data" to use in training/fitting
                        rows.test,
                        prediction.data = NULL, # optional 2ndary dataset to use the model to make more predictions without having to save the model
                        save.model.fit = FALSE,
                        log_model = FALSE,
                        log_model_filename){  # rows of "data" to use in testing/validating
   # record the start time
   time1 <- Sys.time()
   
   # model formula
   formula.gam1 <- as.formula(paste0( "bycatch ~ s(lon, lat, k=100) + ", paste(covariates, collapse=" + ")))
   
   out <- tryCatch(expr = {
      # fit the model
      fit <- mgcv::gam(formula = formula.gam1, 
                       family = ifelse(modelfamily=='binomial', 
                                       'binomial', 
                                       'Gamma(link="log")'),
                       data = data[rows.train,])
      
      if(log_model){
         saveRDS(object = fit, file = log_model_filename)
      }
      
      model_converged <- fit$converged
      
      # calculated expected values for the testing dataset (if data is provided)
      observed <- NA
      expected <- NA
      if( length(rows.test) > 0 ){
         if(any(!is.null(rows.test)) & any(!is.na(rows.test))) { # prevents crashing when there's no testing data provided } else NA
            observed <- data[rows.test, 'bycatch'] # observations at test locations
            expected <- as.vector(mgcv::predict.gam(fit, 
                                                    newdata = data[rows.test,], 
                                                    type = 'response')) # return prediction on the response scale (either probability of success or number of bycatch)
      }}
      
      # make predictions using extra prediction dataset
      if(!is.null(prediction.data)){
         # make sure all the necessary covariates are in the prediction dataset
         if( !all( covariates %in% names(prediction.data) ) ){
            print('Not using "prediction.data" to make predictions because prediction.data is missing necessary covariate data.')  
         } else {
            predictions <- tryCatch(expr = {
               as.vector(mgcv::predict.gam(fit,
                                           newdata = prediction.data[,c(covariates, 'lat', 'lon')], 
                                           type = 'response'))},
               error=function(cond) {
                  message(cond)
                  return = rep(NA, nrow(prediction.data))
               })
         }
      }
      
      # finish time
      time2 <- Sys.time()
      # total runtime
      runtime <- as.numeric(round(difftime(time2, time1, units = 'min'), 1))
      
      if(modelfamily=='binomial') gof  <- calc_AUC(expected, observed)
      if(modelfamily=='gamma')    gof <- calc_RMSE(expected, observed)
      
      # return results
      results <- list('gof' = gof, 
                      'observed' = observed, 
                      'expected' = expected, 
                      'runtime'  = runtime,
                      'error' = NA,
                      'model' = NA,
                      model_converged = model_converged)
      if(save.model.fit) results[['model']] <- fit
      if(!is.null(prediction.data)) results[['predictions']] <- list('haul_id' = prediction.data$haul_id, 'predictions' = predictions)
      names(results)[1] <- if(modelfamily=='binomial') 'AUC' else 'RMSE' 
      results
   }, 
   error=function(cond) {
      message(cond)
      
      results <- list('gof' = NA_real_, 
                      'observed' = data[rows.test,'bycatch'],
                      'expected' = rep(NA_real_, length(rows.test)), 
                      'runtime'  = as.numeric(round(difftime( Sys.time(), time1, units = 'min'), 1)),
                      'error' = cond,
                      'model' = NA,
                      'predictions' = NULL,
                      model_converged = FALSE)
      names(results)[1] <- if(modelfamily=='binomial') 'AUC' else 'RMSE' 
      
      return(results)
   })
   
   return(out)
}

# function to fit model 3: GAM with low-basis-dimension smooths (k=4, fx = TRUE) and spatial smooth
fit_m3_gam2 <- function(data,        # dataset with response = 'bycatch' and all covariates
                        covariates,  # character vector of covariate names (matching column names in "data")
                        modelfamily, # "binomial" or "gamma"
                        rows.train,  # rows of "data" to use in training/fitting
                        rows.test,
                        prediction.data = NULL, # optional 2ndary dataset to use the model to make more predictions without having to save the model
                        save.model.fit = FALSE,
                        log_model = FALSE,
                        log_model_filename){  # rows of "data" to use in testing/validating
   # record the start time
   time1 <- Sys.time()
   
   # rename a variable to make sure it's the same everywhere
   if( ('bottom_slope' %in% covariates) &  !('bottom_slope' %in% names(data)) ) data$bottom_slope <- data$bottom_slope_GEBCO
   
   # don't want a smoothing term for categorical predictors
   cov.cat <- as.vector(unlist(sapply(covariates, function(c){ if(is.factor(data[,c])) c })))
   cov.con <- covariates[ !(covariates %in% cov.cat) ]
   
   # model formula
   form.character <- paste( "bycatch ~ s(lon, lat, k=100)", 
                            paste(cov.cat, collapse = ' + '), 
                            paste("s(", cov.con, ', bs="cr", k=4, fx = TRUE)', collapse = ' + '), 
                            sep = ' + ')
   # fx = TRUE sets the df of the smooth to k. The alternative (and default) is a penalized regression spline. 
   
   formula.gam2 <- as.formula(form.character)
   
   
   
   # model formula
   # s() forces isotrophy, which kind of makes sense for lat-lon, but maybe not if they're not projected.
   # tensor products allow anisotrophy, which is especially useful if fitting a multidimensional smooth that has
   #   very different units in different dimentions (e.g. space in one axis and time in another)
   # But note that fitting a 3-dimensional smooth (lon, lat, time) still runs into the problem of autocorrelation.
   #   If the model doesn't explicitly account for autocorrelation (via specifying the correlation structure of residuals)
   #   then the smooth may be fitting noise in the data rather than an underlying trend.
   #   If the model does include autocorrelation, there may be fitting issues because it can be difficult to
   #   distinguish between smooths and autocorrelation.
   
   # there are a lot of options for smooth terms here. See Woods (2017) section 7.7.1 for a simple starting place.
   # te(lon, lat, time, k = c(100,10), d=c(2,1)) = I think this is 2 smooths, the first has 2 dimensions and the 2nd 1 dimension.
   # s(lon, lat, k = 100) + s(time, k = 5) + ti(lon, lat, time, k = c(100,10), d=c(2,1)) = this is an attempt to partition to the previous smooth into its components and the interaction in order to figure out whether or not the effect of space is really changing through time.
   # s(lon, lat, by = time, k = 50) = "geographic regression model". Basically, the effect of time would vary through space. Would need another smooth for space itself.
   # another option is to use the "soap film" smoother to enforce more realistic boundaries with land.
   # te(lon, lat, time, bs=c("sw","cr"), k = c(100,10), d = c(2,1), xt = list(list(bnd = land_polygon),NULL))
   
   # data$time <- as.numeric(dat.ik$date) - as.numeric(as.Date('2010-01-01'))
   # formula.gam2 <- as.formula(paste0( "bycatch ~ te(lon, lat, time, bs = c('tp', 'tp'), k = c(100,10), d=c(2,1)) + ", paste(covariates, collapse=" + ")))
   #               as.formula(paste0( "bycatch ~ s(lon, lat, k = 100) + s(time, k = 10) + ti(lon, lat, time, k = c(100,10), d=c(2,1)) + ", paste(covariates, collapse=" + ")))
   # a tensor product of:
   #   1. a thin plate regression spline of lon & lat
   #   2. a thin plate regression spline (or any other spline) of time
   #   This assumes isotropy for spatial dependence, but not for the space-time interaction.
   
   # k sets the dimension(s) of the bases used to represent the smooth term. Here 2 values: first is spatial term (very wiggly), second is temporal term (not so wiggly)
   # d = "array of marginal basis dimensions. For example if you want a smooth for 3 covariates made up of a tensor product of a 2 dimensional t.p.r.s. basis and a 1-dimensional basis, then set d=c(2,1)"
   
   # If EDF end up close to the basis dimension (k), it means the model isn't doing a good job because:
   # 1. basis dimension is too low. Solution = increase k and refit model
   # 2. un-modelled residual auto-correlation. Solution = use gamm() and specify autocorrelation
   # 3. incorrectly specified mean variance relationship. Solution = fit a different model family??
   
   out <- tryCatch(expr = {
      # fit the model
      fit <- mgcv::gam(formula = formula.gam2,
                       family = ifelse(modelfamily=='binomial',
                                       'binomial',
                                       'Gamma(link="log")'),
                       data = data[rows.train,])
      
      if(log_model){
         saveRDS(object = fit, file = log_model_filename)
      }
      
      model_converged <- fit$converged
      
      # calculated expected values for the testing dataset (if data is provided)
      observed <- NA
      expected <- NA
      if( length(rows.test) > 0 ){
         if(any(!is.null(rows.test)) & any(!is.na(rows.test))) { # prevents crashing when there's no testing data provided } else NA
            observed <- data[rows.test, 'bycatch'] # observations at test locations
            expected <- as.vector(mgcv::predict.gam(fit, # predict.gam still seems to work, but I'll try to be consistant.
                                                    newdata = data[rows.test,],
                                                    type = 'response')) # return prediction on the response scale (either probability of success or number of bycatch)
      }}
      
      # make predictions using extra prediction dataset
      if(!is.null(prediction.data)){
         # make sure all the necessary covariates are in the prediction dataset
         if( !all( covariates %in% names(prediction.data) ) ){
            print('Not using "prediction.data" to make predictions because prediction.data is missing necessary covariate data.')  
         } else {
            predictions <- tryCatch(expr = {
               as.vector(mgcv::predict.gam(fit,
                                           newdata = prediction.data[,c(covariates, 'lat', 'lon')], 
                                           type = 'response'))},
               error=function(cond) {
                  message(cond)
                  return = rep(NA, nrow(prediction.data))
               })
         }
      }
      
      # finish time
      time2 <- Sys.time()
      # total runtime
      runtime <- as.numeric(round(difftime(time2, time1, units = 'min'), 1))
      
      if(modelfamily=='binomial') gof <- calc_AUC(expected, observed)
      if(modelfamily=='gamma')    gof <- calc_RMSE(expected, observed)
      
      # return results
      results <- list('gof' = gof,
                      'observed' = observed,
                      'expected' = expected,
                      'runtime'  = runtime,
                      'error' = NA,
                      'model' = NA,
                      model_converged = model_converged)
      if(save.model.fit) results[['model']] <- fit
      if(!is.null(prediction.data)) results[['predictions']] <- list('haul_id' = prediction.data$haul_id, 'predictions' = predictions)
      names(results)[1] <- if(modelfamily=='binomial') 'AUC' else 'RMSE'
      results
   },
   error=function(cond) {
      message(cond)
      
      results <- list('gof' = NA_real_,
                      'observed' = data[rows.test,'bycatch'],
                      'expected' = rep(NA_real_, length(rows.test)),
                      'runtime'  = as.numeric(round(difftime( Sys.time(), time1, units = 'min'), 1)),
                      'error' = cond,
                      'model' = NA,
                      'predictions' = NULL,
                      model_converged = FALSE)
      names(results)[1] <- if(modelfamily=='binomial') 'AUC' else 'RMSE'
      
      return(results)
   })
   
   return(out)
}

# function to fit model 3: GAM with lots of smoothing terms
fit_m4_gam3 <- function(data,        # dataset with response = 'bycatch' and all covariates
                        covariates,  # character vector of covariate names (matching column names in "data")
                        modelfamily, # "binomial" or "gamma"
                        rows.train,  # rows of "data" to use in training/fitting
                        rows.test,
                        prediction.data = NULL, # optional 2ndary dataset to use the model to make more predictions without having to save the model
                        year_type = 'factor', # "factor" or "continuous"
                        save.model.fit = FALSE,
                        log_model = FALSE,
                        log_model_filename){  # rows of "data" to use in testing/validating
   # record the start time
   time1 <- Sys.time()
   
   # rename a variable to make sure it's the same everywhere
   if( ('bottom_slope' %in% covariates) &  !('bottom_slope' %in% names(data)) ) data$bottom_slope <- data$bottom_slope_GEBCO
   
   # don't want a smoothing term for categorical predictors
   cov.cat <- as.vector(unlist(sapply(covariates, function(c){ if(is.factor(data[,c])) c })))
   cov.con <- covariates[ !(covariates %in% cov.cat) ]
   # remove year from the list of continuous predictors b/c it's already included in the spatial smooth
   cov.con <- cov.con[ cov.con != 'year' ]
   
   # function to create the text specification for a smooth based on the number of unique values for the covariate
   smooth_text <- function(covar_name){
      # mgcv documentation says "k must be chosen: the defaults are essentially arbitrary"
      # I'll set it to the minimum of 10 or 2/3 * number of unique values. I'm purposely keeping k pretty low to speed up model fitting. It may also help to avoid over-fitting.
      nu <- length(unique(data[,covar_name]))
      k <- min( ceiling(nu*2/3), 10)
      stext <- paste0('s(', covar_name, ', bs="cr", k=', k, ')')
      # bs="cr" gives cubic regression splines, which are a little faster than the default (better) thin plate regression splines
   }
   
   cov.con.smooth.text <- paste(sapply(cov.con, smooth_text), collapse = ' + ')
   
   # model formula
   form.character <- paste( 
      if(year_type == 'factor'){
         "bycatch ~ s(lon, lat, k = 25, by = year_f)" # there are up to 12 years of training data. 25*12 = 300
      } else if(year_type == 'continuous'){
         "bycatch ~ te(lon, lat, time, bs = c('tp', 'tp'), k = c(40,8), d=c(2,1))" # 40*8 = 320
      },
      paste(cov.cat, collapse = ' + '),
      # cov.con.smooth.text,
      # instead of the more complex smooths, I'll use the low-complexity smooths from gam2
      paste("s(", cov.con, ', bs="cr", k=4)', collapse = ' + '),# removed , fx = TRUE b/c it may decrease the stability of model fitting procedures (https://r.789695.n4.nabble.com/Two-repeated-warnings-when-runing-gam-mgcv-to-analyze-my-dataset-td842891.html)
      sep = ' + ')
   
   formula.gam3 <- as.formula(form.character)
   
   out <- tryCatch(expr = {
      # fit the model
      fit <- mgcv::gam(formula = formula.gam3, 
                       family = ifelse(modelfamily=='binomial', 
                                       'binomial', 
                                       'Gamma(link="log")'),
                       data = data[rows.train,])
      
      if(log_model){
         saveRDS(object = fit, file = log_model_filename)
      }
      
      model_converged <- fit$converged
      
      # calculated expected values for the testing dataset (if data is provided)
      observed <- NA
      expected <- NA
      if( length(rows.test) > 0 ){
         if(any(!is.null(rows.test)) & any(!is.na(rows.test))) { # prevents crashing when there's no testing data provided } else NA
            observed <- data[rows.test, 'bycatch'] # observations at test locations
            expected <- as.vector(mgcv::predict.gam(fit, # predict.gam still seems to work, but I'll try to be consistant. 
                                                    newdata = data[rows.test,], 
                                                    type = 'response')) # return prediction on the response scale (either probability of success or number of bycatch)
      }}
      
      # make predictions using extra prediction dataset
      if(!is.null(prediction.data)){
         # make sure all the necessary covariates are in the prediction dataset
         if( !all( covariates %in% names(prediction.data) ) ){
            print('Not using "prediction.data" to make predictions because prediction.data is missing necessary covariate data.')  
         } else {
            predictions <- tryCatch(expr = {
               as.vector(mgcv::predict.gam(fit,
                                           #newdata = prediction.data[,c(covariates, 'lat', 'lon')], 
                                           newdata = prediction.data[,c(covariates, 'lat', 'lon', 'time')],
                                           type = 'response'))},
               error=function(cond) {
                  message(cond)
                  return = rep(NA, nrow(prediction.data))
               })
         }
      }
      
      # finish time
      time2 <- Sys.time()
      # total runtime
      runtime <- as.numeric(round(difftime(time2, time1, units = 'min'), 1))
      
      if(modelfamily=='binomial') gof  <- calc_AUC(expected, observed)
      if(modelfamily=='gamma')    gof <- calc_RMSE(expected, observed)
      
      # return results
      results <- list('gof' = gof, 
                      'observed' = observed, 
                      'expected' = expected, 
                      'runtime'  = runtime,
                      'error' = NA,
                      'model' = NA,
                      model_converged = model_converged)
      if(save.model.fit) results[['model']] <- fit
      if(!is.null(prediction.data)) results[['predictions']] <- list('haul_id' = prediction.data$haul_id, 'predictions' = predictions)
      names(results)[1] <- if(modelfamily=='binomial') 'AUC' else 'RMSE' 
      results
   }, 
   error=function(cond) {
      message(cond)
      
      results <- list('gof' = NA_real_, 
                      'observed' = data[rows.test,'bycatch'],
                      'expected' = rep(NA_real_, length(rows.test)), 
                      'runtime'  = as.numeric(round(difftime( Sys.time(), time1, units = 'min'), 1)),
                      'error' = cond,
                      'model' = NA,
                      'predictions' = NULL,
                      model_converged = FALSE)
      names(results)[1] <- if(modelfamily=='binomial') 'AUC' else 'RMSE' 
      
      return(results)
   })
   
   return(out)
}

# function to fit model 5: INLA with single GMRF
# SLOW! & not completely up-to-date. If you want to run INLA models, you'll probably 
#   have to edit this code first. I'm including it as a starting point.
fit_m5_INLA <- function(data,        # dataset with response = 'bycatch' and all covariates
                        covariates,  # character vector of covariate names (matching column names in "data")
                        modelfamily, # "binomial" or "gamma"
                        rows.train,  # rows of "data" to use in training/fitting
                        rows.test,
                        prediction.data = NULL, # optional 2ndary dataset to use the model to make more predictions without having to save the model
                        save.model.fit = FALSE,
                        log_model = FALSE,
                        log_model_filename){
   # could add another argument to the function: GMRF = 'constant'
   #    GMRF = constant fits model with a single GMRF to describe spatial correlation in response 
   #    GMRF = exchangeable fits a model with a different GMRF for each year 
   
   # I probably won't use this model b/c it runs very slowly, but I'll add it in here as a possibility just in case
   if(modelfamily == 'binomial') data$bycatch <- as.numeric(as.character(data$bycatch))
   
   # INLA conceptual overview: 
   {
      # Summary largely based on Zuur et al. 2017 page 201
      # 1. We have N sampling locations s_1 to s_N.
      # 2. We have a random effect u(s_i) at each sampling location.
      # 3. We assume that the Gaussian Field (i.e. u(s_1), ... , u(s_N) are normal distributed with mean 0 and covariance Sigma_GF.
      # 4. Problem: How do we get Sigma_GF?
      # 5. Shortcut 1: Assume a Markovian ( = local) behavior. This gives a Gaussian Markov Random Field (GMRF) with a sparse (lots of zeros) Sigma_GMRF.
      # 6. Shortcut 2: To quantify the covariance matrix Sigma_GMRF, we use the Matern correlation function. It has a couple of parameters that we need 
      #    to estimate. And we also need the inverse of Sigma_GMRF.
      # 7. Shortcut 3: The parameters in the Stochastic Partial Differential Equation (SPDE) are related to the parameters that we need.
      # 8. Shortcut 4: Using the finite element approach (defined on an irregularly spaced mesh) in combination with a modified SPDE gives 
      #    us all the parameters that we need [i.e. the u(s_i) and all covariance terms].
      # 9. Bonus: Step 8 provides a spatial component at each vertex of the mesh. Using linear interpolation and graphical software we can visualise it.
   }
   
   # CONSTANT model (single spatial random field) includes offset terms for each year (if year is included as a predictor)
   n.covar <- length(covariates)
   
   # Get the inverse link function for the model
   #  binomial = inverse logit
   #  gamma    = exp
   inverse_link_function <- ifelse(modelfamily == 'binomial', gtools::inv.logit, exp)
   # for more on the gamma distribution in INLA: https://inla.r-inla-download.org/r-inla.org/doc/likelihood/gamma.pdf
   #    - the default link is log
   #    - "The hyperparameter is the precision parameter phi, which is represented as phi = exp(theta) and the prior is defined on theta."
   
   # Steps to fit a GMRF model in INLA (from Zuur et al. (2017) page 209)
   # 1. Make a mesh.
   #    R function: inla.mesh.2d
   # 2. Define the weighting factors a_ik (also called the projector matrix).
   #    R function: inla.spde.make.A
   # 3. Define the SPDE.
   #    R function: inla.spde2.matern
   # 4. Define the spatial field.
   #    R function: inla.spde.make.index
   # 5. Make a stack. In this process we tell R-INLA at which points on
   #    the mesh we sampled the response variable and the covariates. We
   #    also need to inform R-INLA (via the stack) at which points of the
   #    mesh we have any other terms, e.g. the random effects.
   #        Intermediate steps: Make a list that contains the multipliers for each coefficient value.
   #                            - For the random effects (i.e. GMRF), the multipliers is the projection matrix
   #                            - For the fixed effects, the multiplier is 1
   #                            Make a list of all the covariate values
   #                            - For the random effects, the covariate value is the index of nodes within the mesh (step 2)
   #                            - For the fixed effects, manually add an intercept and then include the covariate values from the model matrix
   #    R function: -inla.stack
   # 6. Specify the model formula in terms of the response variable,
   #    covariates, and the spatial correlated term.
   # 7. Run the spatial model in R-INLA.
   
   out <- tryCatch(expr = {
      # 1. Create a mesh over the study area
      {
         # get the coordinates for all the points & subset them into training and testing
         coords.all   <- as.matrix(data[,c('lon', 'lat')])
         coords.train <- coords.all[rows.train,]
         coords.test  <- coords.all[rows.test,]
         
         # set up a nonconvex boundary around ALL the points (so that I can use the same model for the TEST predictions)
         bnd <- INLA::inla.nonconvex.hull(coords.all, 
                                          resolution = 250, # if(use_projected) c(45,45) else c(30,60)
                                          # convex  = -0.03, # add some space inside the boundary
                                          concave = -0.03) # control curvature
         
         # define the mesh using the training data and the boundary
         mesh1 <- INLA::inla.mesh.2d(loc = coords.train,
                                     boundary = bnd,
                                     offset   = c(-0.01,-0.04), # add some extra space to inner & outer domains
                                     # cutoff   = 0.08, # min edge length (0.1 degrees = 11.1 km at equator; 7.9 km at 45 degrees N) so 0.08 is about 8.9 km latitude & 6.3 km longitude at 45 deg N. NEED TO CHANGE THIS IF USING PROJECTED COORDINATES!! For example: if(use_projected) 8000 else 0.08
                                     cutoff   = 0.12, 
                                     min.angle = c(25,20), # really small angles are problematic. 
                                     # max.edge = c(.24,0.5) # max edge lengths NEED TO CHANGE THIS IF USING PROJECTED COORDINATES!!! For example: if(use_projected) c(25000, 50000) else c(.24,0.5)
                                     max.edge = c(.4,.6)) 
         # mesh1$n
         # Preview mesh
         # plot(mesh1, asp = c(1,1))
         # can set up mesh interactively using mesh builder: INLA::meshbuilder()
      }
      
      # 2. Define the weighting factors a_ik (also called the projector matrix).
      {
         # create a projection matrix for the training data
         A.train <- INLA::inla.spde.make.A(mesh = mesh1, 
                                           loc = coords.train)
         # and a 2nd for the testing data
         A.test  <- INLA::inla.spde.make.A(mesh = mesh1, 
                                           loc = coords.test)
      }
      
      # 3. Define the SPDE.
      {
         # use SPDE to define the Matern correlation function parameters
         spde = INLA::inla.spde2.matern(mesh1, 
                                        alpha = 2) # alpha = Matern parameter (that is set, not estimated). alpha = 2 is for 2-dimensional data. 
      }
      
      # 4. Define the spatial field.
      {
         # Make index (GMRF random effect) for spatial field
         wset <- INLA::inla.spde.make.index(name = "w", # name the points so that they're easy to refer to later
                                            n.spde = mesh1$n) # number of points in the mesh
         # note: Zuur calls the values of the GMRF at the nodes of the mesh "w". Gomez-Rubio refers to them as "i".
      }
      
      # 5. Make a data stack.
      {
         # I still don't fully understand how factor variables are handled in INLA, so I'm going to specify the model matrix manually
         # (The inlabru package actually requires this)
         # Generally following the steps here: https://rdrr.io/github/inbo/INLA/f/vignettes/stack-with-factors.Rmd
         
         # model formula for just the fixed effects
         feform <- as.formula(paste ('~ 0 +', paste(covariates, collapse = '+') ))
         
         # Prep model matrix (REMOVE THE INTERCEPT)
         mm.train <- model.matrix(feform, data = data[rows.train, covariates])
         # ensure that there are no spaces or "-" in the colnames
         colnames(mm.train) <- sub(pattern = ' |-', replacement = '_', x = colnames(mm.train))
         
         # model matrix for the testing data
         mm.test <- model.matrix(feform, data = data[rows.test, covariates])
         colnames(mm.test) <- colnames(mm.train)
         
         # Make a list that contains the multipliers for each coefficient value.
         # - For the random effects (i.e. GMRF), the multipliers is the projection matrix
         # - For the fixed effects, the multiplier is 1
         A.list.train = c( list(A.train), 
                           as.list(rep(1, 1 + ncol(mm.train))) ) # add 1 for the explicit intercept
         A.list.test = c( list(A.test), 
                          as.list(rep(1, 1 + ncol(mm.test))) ) # add 1 for the explicit intercept
         
         # Make a list of all the covariate values
         # - For the random effects, the covariate value is the index of nodes within the mesh (step 2)
         # - For the fixed effects, manually add an intercept and then include the covariate values from the model matrix
         effect.list.train = c( list(w = wset), # GMRF (1 random point for each node in the mesh)
                                as.list(data.frame(Intercept = 1, # manually add an intercept
                                                   mm.train)) ) # the model matrix with all the fixed effect covariates
         effect.list.test = c( list(w = wset), # GMRF (1 random point for each node in the mesh)
                               as.list(data.frame(Intercept = 1, # manually add an intercept
                                                  mm.test)) ) # the model matrix with all the fixed effect covariates
         
         # Make the data stack
         stack.train <- INLA::inla.stack(tag      = 'train',
                                         data     = list(bycatch = data[rows.train, 'bycatch']), # response data [bycatch rate]
                                         A        = A.list.train,      # projection matrices
                                         effects  = effect.list.train) # fixed & random effects
         stack.test <- INLA::inla.stack(tag      = 'test',
                                        data     = list(bycatch = rep(NA, length(rows.test))), # set bycatch = NA to tell R-INLA to predict for these locations
                                        A        = A.list.test,      # projection matrices
                                        effects  = effect.list.test) # fixed & random effects
         
         # combine the training & testing stacks into a single stack (INLA fits the model to both at the same time)
         stack.all <- INLA::inla.stack(stack.train, 
                                       stack.test)
      }
      
      # 6. Specify the model formula in terms of the response variable,
      {
         formula.inla <- as.formula(paste0("bycatch ~ -1 + Intercept + ", 
                                           paste( colnames(mm.train) , collapse="+"),
                                           "+ f(w, model = spde)")) # "spde" here refers to the object created in step 3 using inla.spde2.matern
         
         # I don't fully understand the need to remove the intercept yet. Many examples I see online manually 
         #  add an intercept into the effect list, so that it includes 1) projection matrix index, 2) Intercept, 3) fixed effects
         #  but I don't add in an extra intercept there. If I try to run the model WITH the automated intercept, I get the following warning: 
         #  "The A-matrix in the predictor (see ?control.predictor) is specified
         #   but an intercept is in the formula. This will likely result
         #   in the intercept being applied multiple times in the model, and is likely
         #   not what you want. See ?inla.stack for more information.
         #   You can remove the intercept adding ``-1'' to the formula.
         # Only explanation I've found so far is: "In the formula, we remove the intercept (adding 0) and add it as a 
         #          covariate term (adding b0), so all the covariate terms can be captured in the projection matrix."
         #          https://www.paulamoraga.com/book-geospatial/sec-geostatisticaldataexamplespatial.html
         # This is also kind of helpful: https://rdrr.io/github/inbo/INLA/f/vignettes/stack-with-factors.Rmd
      }
      
      # 7. Run the spatial model in R-INLA.
      {
         # quick run to find posterior mode, using gaussian approximation and 
         #   empirical Bayes integration strategy over the hyperparameters
         start.inla <- INLA::inla(formula = formula.inla, 
                                  family  = modelfamily, # use the previously-defined family
                                  data = INLA::inla.stack.data(stack.all), # get the data from the stack
                                  control.predictor = list(link = 1, # Link function (ONLY FOR UNOBSERVED OBSERVATIONS (i.e. predictions) & "The link-argument only influence the fitted.values in the result-object". 1 = default link (natural log for Gamma; logit for binomial; ?? for guassian field)
                                                           compute = FALSE, # don't bother computing the marginal distribution for each point in the GRF
                                                           A = INLA::inla.stack.A(stack.all)), # get the projector matrix from the stack
                                  # verbose = TRUE, 
                                  # debug   = TRUE, 
                                  keep    = FALSE, # do NOT save any working files to disk 
                                  control.inla = list(strategy = "gaussian", # The strategy to use for the approximations; one of 'gaussian', 'simplified.laplace' (default), 'laplace' or 'adaptive'. "Guassian" is the simplest and fastest approximation method, but can give poor results, so it's often used to fit a primer model to get starting values for the more computationally intense Laplace fit. 
                                                      int.strategy = "eb"),  # The integration strategy to use; one of 'auto' (default), 'ccd', 'grid', 'eb' (empirical bayes), 'user' or 'user.std'. Using Empirical Bayes means "no integration with respect to theta"
                                  control.compute = list(dic = TRUE, # calculate DIC for model comparison
                                                         cpo = TRUE), # calculate CPO for model comparison
                                  control.fixed = list(# expand.factor.strategy = 'model.matrix',# 'inla', # to handle factor predictor variables. "NA's in a factor x  is not allowed unless NA is a level in itself, or  'control.fixed = list(expand.factor.strategy = "inla")'  is set." "The strategy used to expand factors into fixed effects based on their levels. The default strategy is us use the model.matrix-function for which NA's are not allowed (expand.factor.strategy="model.matrix") and levels are possible removed. The alternative option (expand.factor.strategy="inla") use an inla-spesific expansion which expand a factor into one fixed effects for each level, do allow for NA's and all levels are present in the model. In this case, factors MUST BE factors in the data.frame/list and NOT added as ...+factor(x1)+... in the formula only."
                                     correlation.matrix = TRUE), # "Compute the posterior correlation matrix for all fixed effects? (default FALSE) OOPS: This option will set up appropriate linear combinations and the results are shown as the posterior correlation matrix of the linear combinations. This option will imply control.inla=list(lincomb.derived.correlation.matrix=TRUE)"
                                  control.results=list(return.marginals.random = FALSE, # don't bother returning marginal distributions (save RAM)
                                                       return.marginals.predictor = FALSE))
         
         
         # longer run using more accurate approximation, uses posterior mode found in previous step
         system.time(
            fit <- INLA::inla(formula   = formula.inla, 
                              family = modelfamily, # use control.family to change link function. https://www.r-inla.org/faq#h.rzr46apslza4
                              data   = INLA::inla.stack.data(stack.all), 
                              control.predictor = list(link = 1, 
                                                       compute = TRUE, # DO compute the marginal distribution for each point in the GRF mesh
                                                       A = INLA::inla.stack.A(stack.all)), 
                              # verbose = TRUE, 
                              # debug   = TRUE,
                              keep    = FALSE, 
                              control.compute = list(dic    = TRUE,
                                                     # waic   = TRUE,
                                                     config = TRUE, # need this option to be able to sample from the posterior distribution, which is the best way to make predictions for a large number of data points. Otherwise, don't include it.
                                                     cpo    = TRUE), 
                              control.fixed   = list( # expand.factor.strategy = 'inla',
                                 correlation.matrix     = TRUE), 
                              # using the estimates from the "start" model can speed up the computations. "control.mode" = "Options on the initial values (and modes) of the hyperparameters for model fitting."
                              control.mode    = list(theta   = start.inla$mode$theta, # initial values for theta
                                                     restart = FALSE), # "should we restart the optimisation from this configuration or fix the mode at this configuration? (Default FALSE.)"
                              # many online sources suggest having restart = TRUE. What's the difference???
                              #    'theta' tells INLA where to start looking for the mode
                              #    restart = TRUE tells it to keep improving the approximation [how is this any different from "fixed"???]
                              #    I think: "restart = FALSE" sets the MODE of the distribution (rest of dist. still estimated). "fixed = TRUE" sets the value to a constant (i.e. NO distribution)
                              # I'm guessing that Stock had trouble (inconsistent results?) with the model when "restart=TRUE", so he set it to "restart=FALSE" to simplify things.
                              #     Note that model consistency can be assessed using: r = inla(...); r = inla.rerun(r)
                              control.results = list(return.marginals.random    = TRUE,     # save this so that I can plot the Matern correlation function
                                                     return.marginals.predictor = FALSE)) # only save the summary stats, not the full distributions
         )
         
         if(log_model){
            saveRDS(object = fit, file = log_model_filename)
         }
         
         # assess model "convergence"
         # see https://groups.google.com/g/r-inla-discussion-group/c/LsCpuCsr-Qo
         # INLA uses *many* optimizations, not just one as for glm. If everything is fine, then r$mode$mode.status is 0. increasing value means that more is 'not fine', but likely, the results are still useful.
         model_converged <- fit$mode$mode.status
      }
      
      # calculate and return performance
      observed <- data[rows.test, 'bycatch'] # observations at test locations
      # to get the predicted values, first get the index of the test data to extract them from the model fit
      # index of the predicted values (in order to extract the predicted values from the INLA model fit)
      ind.test <- INLA::inla.stack.index(stack = stack.all,
                                         tag = 'test')$data
      # predicted values at the test locations
      expected <- fit$summary.fitted.values[ind.test,"mean"]
      
      # finish time
      time2 <- Sys.time()
      # total runtime
      runtime <- as.numeric(round(difftime(time2, time1, units = 'min'), 1))
      
      if(modelfamily=='binomial') gof  <- calc_AUC(expected, observed)
      if(modelfamily=='gamma')    gof <- calc_RMSE(expected, observed)
      
      # return results
      results <- list('gof' = gof, 
                      'observed' = observed, 
                      'expected' = expected, 
                      'runtime'  = runtime,
                      'error' = NA,
                      'model' = NA,
                      model_converged = model_converged)
      if(save.model.fit) results[['model']] <- fit
      names(results)[1] <- if(modelfamily=='binomial') 'AUC' else 'RMSE' 
      results
   }, 
   error=function(cond) {
      message(cond)
      
      results <- list('gof' = NA_real_, 
                      'observed' = data[rows.test,'bycatch'],
                      'expected' = rep(NA_real_, length(rows.test)), 
                      'runtime'  = as.numeric(round(difftime( Sys.time(), time1, units = 'min'), 1)),
                      'error' = cond,
                      'model' = NA,
                      model_converged = FALSE)
      names(results)[1] <- if(modelfamily=='binomial') 'AUC' else 'RMSE' 
      
      return(results)
   })
   return(out)
}

# function to fit model 6: Random Forest
fit_m6_rf1 <- function(data,        # dataset with response = 'bycatch' and all covariates
                       covariates,  # character vector of covariate names (matching column names in "data")
                       modelfamily, # "binomial" or "gamma"
                       rows.train,  # rows of "data" to use in training/fitting
                       rows.test,
                       prediction.data = NULL, # optional 2ndary dataset to use the model to make more predictions without having to save the model
                       use.package = 'ranger', # use r package "party", "ranger", or "randomForest" to fit models
                       keep.inbag = FALSE,      # necessary for estimating SE
                       quantile.forest = FALSE, # necessary for estimating quantiles
                       save.model.fit = FALSE,
                       log_model = FALSE,
                       log_model_filename){  # rows of "data" to use in testing/validating
   # record the start time
   time1 <- Sys.time()
   
   # rename a variable to make sure it's the same everywhere
   if( ('bottom_slope' %in% covariates) &  !('bottom_slope' %in% names(data)) ) data$bottom_slope <- data$bottom_slope_GEBCO
   
   out <- tryCatch(expr = {
      # fit the model
      if(use.package %in% c('randomForest', 'randomforest')){
         fit <- randomForest::randomForest(x = data[rows.train, covariates], # training data predictors
                                           y = data[rows.train, 'bycatch'],  # training data response
                                           # xtest = data[rows.test, covariates], # test data predictors [same as training data in this case]
                                           # ytest = data[rows.test, 'bycatch'],  # test data response [same as training data in this case]
                                           mtry = 3,          # Number of variables randomly sampled as candidates at each split. Note that the default values are different for classification (sqrt(p) where p is number of variables in x) and regression (p/3)
                                           ntree = 1500,      # Number of trees to grow. This should not be set to too small a number, to ensure that every input row gets predicted at least a few times.
                                           importance = FALSE, # Should importance of predictors be assessed?
                                           # do.trace = 200,    # print updates every 200 trees
                                           keep.forest = TRUE,
                                           replace = FALSE) # keep forest for prediction at test locations
         
         if(log_model){
            saveRDS(object = fit, file = log_model_filename)
         }
         
         # predictions for the test data
         expected <- NA
         if( length(rows.test) > 0 ){
            if(any(!is.null(rows.test)) & any(!is.na(rows.test))) { # prevents crashing when there's no testing data provided } else NA
               expected <- if(modelfamily == 'binomial'){
                  unname(randomForest:::predict.randomForest(fit, 
                                                             newdata = data[rows.test, covariates], 
                                                             type = 'prob')[,2])
                  } else {
                     unname(randomForest:::predict.randomForest(fit, 
                                                                newdata = data[rows.test, covariates]))
         }}}
         
         # make predictions using extra prediction dataset
         if(!is.null(prediction.data)){
            # make sure all the necessary covariates are in the prediction dataset
            if( !all( covariates %in% names(prediction.data) ) ){
               print('Not using "prediction.data" to make predictions because prediction.data is missing necessary covariate data.')  
            } else {
               predictions <- tryCatch(expr = {
                  if(modelfamily == 'binomial'){
                     unname(randomForest:::predict.randomForest(fit, 
                                                                newdata = prediction.data[,covariates], 
                                                                type = 'prob')[,2])
                  } else {
                     unname(randomForest:::predict.randomForest(fit, 
                                                                newdata = prediction.data[,covariates]))
                  }},
                  error=function(cond) {
                     message(cond)
                     return = rep(NA, nrow(prediction.data))
                  })
            }
         }
         
         # type = 'prob' returns a matrix with the probability of belonging to each class. I just want the probability of belonging to class "bycatch==1"
      }
      
      if(use.package == 'ranger'){
         fit <- ranger::ranger(
            formula         = bycatch ~ ., 
            data = data[rows.train, c('bycatch', covariates)],
            num.trees       = 1500, 
            importance = 'none', 
            probability = if(modelfamily == 'binomial') TRUE else FALSE, # "soft" vote counting
            mtry = 3, 
            # min.node.size = 5, # default = Default 1 for classification, 5 for regression, 3 for survival, and 10 for probability.
            #                      simulations on full dataset suggest this doesn't make much difference.
            # max.depth = 10,  value of NULL or 0 (the default) corresponds to unlim-ited depth, 1 to tree stumps (1 split per tree).
            #                      simulations did best with 0, but that's for interpolation, not extrapolation
            verbose = FALSE,
            keep.inbag = keep.inbag,     # needed for estimating SE
            quantreg  = quantile.forest, # needed for quantiles
            replace = FALSE # sample withOUT replacement (apparently this can reduce bias a little, and these sample sizes are pretty big)
         )
         
         if(log_model){
            saveRDS(object = fit, file = log_model_filename)
         }
         
         # predictions for the test data
         expected <- NA
         if( length(rows.test) > 0 ){ 
            if(any(!is.null(rows.test)) & any(!is.na(rows.test))) { # prevents crashing when there's no testing data provided } else NA
               expected <- if(modelfamily == 'binomial'){
                  ranger:::predict.ranger(object = fit, 
                                          data = data[rows.test,  covariates], 
                                          type = 'response')$predictions[,2]
               } else {
                  ranger:::predict.ranger(object = fit, 
                                          data = data[rows.test,  covariates], 
                                          type = 'response')$predictions
               }
            }}
         # type ='response' (the default) returns the predicted classes for classification, predicted numeric values for regression, and predicted probabilities for probability estimation
         
         # make predictions using extra prediction dataset
         if(!is.null(prediction.data)){
            # make sure all the necessary covariates are in the prediction dataset
            if( !all( covariates %in% names(prediction.data) ) ){
               print('Not using "prediction.data" to make predictions because prediction.data is missing necessary covariate data.')  
            } else {
               predictions <- tryCatch(expr = {
                  if(modelfamily == 'binomial'){
                     ranger:::predict.ranger(object = fit, 
                                             data = prediction.data[,covariates], 
                                             type = 'response')$predictions[,2]
                  } else {
                     ranger:::predict.ranger(object = fit, 
                                             data = prediction.data[,covariates], 
                                             type = 'response')$predictions
                  }},
                  error=function(cond) {
                     message(cond)
                     return = rep(NA, nrow(prediction.data))
                  })
            }
         }
      }
      
      # I added in "party" implementation b/c of Sage et al. 2020
      if(use.package == 'party'){
         
         # set tuning parameters
         
         # Function "cforest_unbiased" returns the settings suggested for the construction of unbiased randomforests 
         #    (teststat = "quad",testtype = "Univ",replace = FALSE) by Strobl et al. (2007) and is the default since 
         #    version 0.9-90. 
         # Hyper parameter settings mimicing the behaviour of randomForest are available in cforest_classical which 
         #     have been used as default up to version 0.9-14.
         pars <- party::cforest_unbiased(
            mtry  = 3, 
            ntree = 400, 
            # The party and partykit packages provide two parameters associated with nodesize. A node is only considered for splitting if the
            #   number of observations it contains is greater than or equal to a parameter referred to as minsplit. Furthermore, a split
            #   is only performed if each resulting node would have at least minbucket observations.
            minsplit = 15, # default = 20
            minbucket = 5, # default = minsplit/3
            trace = TRUE
         )
         
         fit <- party::cforest(
            formula         = bycatch ~ ., 
            data = data[rows.train, c('bycatch', covariates)], 
            controls = pars
         )
         
         if(log_model){
            saveRDS(object = fit, file = log_model_filename)
         }
         
         # The optimal choices for complexity parameters vary considerably between applications so tuning needs to be
         #   done for each dataset on which predictions are to be made.
         
         # predictions for the test data
         expected <- NA
         if( length(rows.test) > 0 ){
            if(any(!is.null(rows.test)) & any(!is.na(rows.test))) { # prevents crashing when there's no testing data provided } else NA
               expected <- if(modelfamily == 'binomial'){
                  preds <- party:::predict.RandomForest(object = fit, 
                                                        newdata = data[rows.test, covariates], 
                                                        type = 'prob')
                  unname(sapply(preds, function(i) i[,2]))
                  } else {
                     as.vector(
                        party:::predict.RandomForest(object = fit, 
                                                     newdata = data[rows.test, covariates], 
                                                     type = 'response')
                     )
                  }
            }
         }
         
         
         # make predictions using extra prediction dataset
         if(!is.null(prediction.data)){
            # make sure all the necessary covariates are in the prediction dataset
            if( !all( covariates %in% names(prediction.data) ) ){
               print('Not using "prediction.data" to make predictions because prediction.data is missing necessary covariate data.')  
            } else {
               predictions <- tryCatch(expr = {
                  if(modelfamily == 'binomial'){
                     preds <- party:::predict.RandomForest(object = fit, 
                                                           newdata = prediction.data[,covariates], 
                                                           type = 'prob')
                     unname(sapply(preds, function(i) i[,2]))
                  } else {
                     as.vector(
                        party:::predict.RandomForest(object = fit, 
                                                     newdata = prediction.data[,covariates], 
                                                     type = 'response')
                     )
                  }},
                  error=function(cond) {
                     message(cond)
                     return = rep(NA, nrow(prediction.data))
                  })
            }
         }
      }
      
      # observed test response
      observed <- NA
      if( length(rows.test) > 0 ){
         if(any(!is.null(rows.test)) & any(!is.na(rows.test))) { # prevents crashing when there's no testing data provided } else NA
            observed <- data[rows.test, 'bycatch'] # observations at test locations
      }}
      
      # finish time
      time2 <- Sys.time()
      # total runtime
      runtime <- as.numeric(round(difftime(time2, time1, units = 'min'), 1))
      
      if(modelfamily=='binomial') gof  <- calc_AUC(expected, observed)
      if(modelfamily=='gamma')    gof <- calc_RMSE(expected, observed)
      
      # return results
      results <- list('gof' = gof, 
                      'observed' = observed, 
                      'expected' = expected, 
                      'runtime'  = runtime,
                      'error' = NA,
                      'model' = NA,
                      model_converged = NA)
      if(save.model.fit) results[['model']] <- fit
      if(!is.null(prediction.data)) results[['predictions']] <- list('haul_id' = prediction.data$haul_id, 'predictions' = predictions)
      names(results)[1] <- if(modelfamily=='binomial') 'AUC' else 'RMSE' 
      results
   }, 
   error=function(cond) {
      message(cond)
      
      results <- list('gof' = NA_real_, 
                      'observed' = data[rows.test,'bycatch'],
                      'expected' = rep(NA_real_, length(rows.test)), 
                      'runtime'  = as.numeric(round(difftime( Sys.time(), time1, units = 'min'), 1)), # as.numeric(round(difftime(time2, time1, units = 'min'), 1))
                      'error' = cond,
                      'model' = NA,
                      'predictions' = NULL,
                      model_converged = NA)
      names(results)[1] <- if(modelfamily=='binomial') 'AUC' else 'RMSE' 
      
      return(results)
   })
   
   return(out)
}

# function to fit model 7: Random Forest with downsampling of over-represented class (aka Balanced Random Forest)
fit_m7_rf2 <- function(data,        # dataset with response = 'bycatch' and all covariates
                       covariates,  # character vector of covariate names (matching column names in "data")
                       modelfamily, # "binomial" or "gamma"
                       rows.train,  # rows of "data" to use in training/fitting
                       rows.test,
                       prediction.data = NULL, # optional 2ndary dataset to use the model to make more predictions without having to save the model
                       use.package = 'ranger', # use r package "ranger", or "randomForest" to fit models
                       keep.inbag = FALSE,      # necessary for estimating SE
                       quantile.forest = FALSE, # necessary for estimating quantiles
                       save.model.fit = FALSE,
                       log_model = FALSE,
                       log_model_filename){  # rows of "data" to use in testing/validating
   # This function downsamples the majority class (binomial component only)
   # downsample = if classes are imbalanced, train RF using equal #s of 0s and 1s
   
   if(!is.factor(data$bycatch)) stop('Downsampling only works with categorical variables.')
   
   # record the start time
   time1 <- Sys.time()
   
   # rename a variable to make sure it's the same everywhere
   if( ('bottom_slope' %in% covariates) &  !('bottom_slope' %in% names(data)) ) data$bottom_slope <- data$bottom_slope_GEBCO
   
   out <- tryCatch(expr = {
      # number of samples with bycatch
      nmin <- as.vector(table(data[rows.train, 'bycatch'])[2])
      # proportion of samples with bycatch
      prop1 <- nmin/length(rows.train)
      # proportion of samples withOUT bycatch
      prop0 <- 1 - prop1
      # if 0s are minority, use # of 0s for minority class instead of 1s
      if(prop0 < prop1) nmin <- length(rows.train) - nmin
      nmin.prop <- nmin / length(rows.train)
      
      # fit the model
      if(use.package %in% c('randomForest', 'randomforest')){
         # fit the model
         fit <- randomForest::randomForest(sampsize = rep(round(nmin/3),2), 
                                           # sampsize = size(s) of sample to draw. 
                                           #  For classification, if sampsize is a vector of the length of the 
                                           #  number of strata, then sampling is stratified by strata, and the 
                                           #  elements of sampsize indicate the numbers to be drawn from each strata.
                                           x = data[rows.train, covariates], # training data predictors
                                           y = data[rows.train, 'bycatch'],  # training data response
                                           # xtest = data[rows.test, covariates], # test data predictors [same as training data in this case]
                                           # ytest = data[rows.test, 'bycatch'],  # test data response [same as training data in this case]
                                           mtry  = 3,          # Number of variables randomly sampled as candidates at each split. Note that the default values are different for classification (sqrt(p) where p is number of variables in x) and regression (p/3)
                                           ntree = 1500,      # Number of trees to grow. This should not be set to too small a number, to ensure that every input row gets predicted at least a few times.
                                           importance = FALSE, # Should importance of predictors be assessed?
                                           # do.trace = 200,    # print updates every 200 trees
                                           keep.forest = TRUE,
                                           replace = FALSE) # keep forest for prediction at test locations
         
         if(log_model){
            saveRDS(object = fit, file = log_model_filename)
         }
         
         # calculate and return performance
         expected <- NA
         # if testing data is provided, replace expected = NA with expected values for testing data
         if( length(rows.test) > 0 ){
            if(any(!is.null(rows.test)) & any(!is.na(rows.test))) { # prevents crashing when there's no testing data provided } else NA
               expected <- unname(randomForest:::predict.randomForest(fit, 
                                                                      newdata = data[rows.test, covariates], 
                                                                      type = 'prob')[,2])
         }}
         
         # type = 'prob' returns a matrix with the probability of belonging to each class. I just want the probability of belonging to class "bycatch==1"
         # make predictions using extra prediction dataset
         if(!is.null(prediction.data)){
            # make sure all the necessary covariates are in the prediction dataset
            if( !all( covariates %in% names(prediction.data) ) ){
               print('Not using "prediction.data" to make predictions because prediction.data is missing necessary covariate data.')  
            } else {
               predictions <- tryCatch(expr = {
                     unname(randomForest:::predict.randomForest(fit, 
                                                                newdata = prediction.data[,covariates], 
                                                                type = 'prob')[,2])
                  },
                  error=function(cond) {
                     message(cond)
                     return = rep(NA, nrow(prediction.data))
                  })
            }
         }
      }
      
      if(use.package == 'ranger'){
         
         # for implementation in Ranger, see: https://stats.stackexchange.com/questions/171380/implementing-balanced-random-forest-brf-in-r-using-randomforests
         fit <- ranger::ranger(
            formula         = bycatch ~ ., 
            data = data[rows.train, c('bycatch', covariates)], 
            sample.fraction = rep(nmin.prop/3,2), 
            # sample.fraction can be a single value or a vector of class-specific values. These are proportions of the total number of samples
            num.trees       = 1500, 
            importance = 'none', 
            probability = TRUE, # "soft" vote counting
            mtry = 3, 
            verbose = FALSE,
            keep.inbag = keep.inbag,     # needed for estimating SE
            quantreg  = quantile.forest, # needed for quantiles
            replace = FALSE # sample withOUT replacement (apparently this can reduce bias a little, and these sample sizes are pretty big)
         )
         
         if(log_model){
            saveRDS(object = fit, file = log_model_filename)
         }
         
         # calculate expected values of testing data (if testing data is provided)
         expected <- NA
         if( length(rows.test) > 0 ){
            if(any(!is.null(rows.test)) & any(!is.na(rows.test))) { # prevents crashing when there's no testing data provided } else NA
               expected <- ranger:::predict.ranger(object = fit, 
                                                   data = data[rows.test, covariates], 
                                                   type = 'response')$predictions[,2]
              # type ='response' (the default) returns the predicted classes for classification, predicted numeric values for regression, and predicted probabilities for probability estimation
         }}
         
         # make predictions using extra prediction dataset
         if(!is.null(prediction.data)){
            # make sure all the necessary covariates are in the prediction dataset
            if( !all( covariates %in% names(prediction.data) ) ){
               print('Not using "prediction.data" to make predictions because prediction.data is missing necessary covariate data.')  
            } else {
               predictions <- tryCatch(expr = {
                     ranger:::predict.ranger(object = fit, 
                                             data = prediction.data[,covariates], 
                                             type = 'response')$predictions[,2]
                     },
                  error=function(cond) {
                     message(cond)
                     return = rep(NA, nrow(prediction.data))
                  })
            }
         }
      }
      
      
      # calculate and return performance
      observed <- NA
      if( length(rows.test) > 0 ){
         if(any(!is.null(rows.test)) & any(!is.na(rows.test))) { # prevents crashing when there's no testing data provided } else NA
            observed <- data[rows.test, 'bycatch'] # observations at test locations
      }}
      
      # finish time
      time2 <- Sys.time()
      # total runtime
      runtime <- as.numeric(round(difftime(time2, time1, units = 'min'), 1))
      
      # calculate AUC
      (AUC  <- calc_AUC(expected, observed))
      # return results
      results <- list('AUC' = AUC, 
                      'observed' = observed, 
                      'expected' = expected, 
                      'runtime'  = runtime,
                      'error' = NA,
                      'model' = NA,
                      model_converged = NA)
      if(save.model.fit) results[['model']] <- fit
      if(!is.null(prediction.data)) results[['predictions']] <- list('haul_id' = prediction.data$haul_id, 'predictions' = predictions)
      results
      # other things to consider returning: rows.train, rows.test, fit
   }, 
   error=function(cond) {
      message(cond)
      return(list('AUC' = NA_real_, 
                  'observed' = data[rows.test,'bycatch'],
                  'expected' = rep(NA_real_, length(rows.test)), 
                  'runtime'  = as.numeric(round(difftime( Sys.time(), time1, units = 'min'), 1)),
                  'error' = cond,
                  'model' = NA,
                  'predictions' = NULL,
                  model_converged = NA))
   })
   
   return(out)
}

# function to fit model 8: Random Forest with Synthetic Minority (no bycatch) Over-sampling (SMOTE)
fit_m8_rf3 <- function(data,        # dataset with response = 'bycatch' and all covariates
                       covariates,  # character vector of covariate names (matching column names in "data")
                       modelfamily, # "binomial" or "gamma"
                       rows.train,  # rows of "data" to use in training/fitting
                       rows.test,
                       prediction.data = NULL, # optional 2ndary dataset to use the model to make more predictions without having to save the model
                       use.package = 'ranger', # use r package "ranger", or "randomForest" to fit models
                       keep.inbag = FALSE,      # necessary for estimating SE
                       quantile.forest = FALSE, # necessary for estimating quantiles
                       save.model.fit = FALSE,
                       log_model = FALSE,
                       log_model_filename){  # rows of "data" to use in testing/validating
   # SMOTE = Synthetic Minority Over-sampling Technique
   #   combines downsampling of majority class with oversampling of minority class
   #   creates synthetic minority class instances by generating random linear combinations
   
   if(!is.factor(data$bycatch)) stop('Down- and over-sampling only works with categorical variables.')
   
   # record the start time
   time1 <- Sys.time()
   
   # rename a variable to make sure it's the same everywhere
   if( ('bottom_slope' %in% covariates) &  !('bottom_slope' %in% names(data)) ) data$bottom_slope <- data$bottom_slope_GEBCO
   
   out <- tryCatch(expr = {
      # get percent minority class (1s)
      prop   <- as.vector(table(data[rows.train, 'bycatch'])[2]) / length(rows.train)
      p.over <- round(50/prop) # percent to oversample to get to 50%
      p.under <- round(100/(1-prop)) # percent to undersample the majority class (no bycatch) to get to 50%
      # The parameters perc.over and perc.under control the amount of over-sampling of the minority class and under-sampling 
      #   of the majority classes, respectively. 
      # perc.over will tipically be a number above 100. With this type of values, for each case in the orginal data set 
      #   belonging to the minority class, perc.over/100 new examples of that class will be created. If perc.over is a 
      #   value below 100 than a single case will be generated for a randomly selected proportion (given by perc.over/100) 
      #   of the cases belonging to the minority class on the original data set. 
      # perc.under controls the proportion of cases of the majority class that will be randomly selected for the final 
      #   "balanced" data set. This proportion is calculated with respect to the number of newly generated minority class 
      #   cases. For instance, if 200 new examples were generated for the minority class, a value of perc.under of 100 will 
      #   randomly select exactly 200 cases belonging to the majority classes from the original data set to belong to the 
      #   final data set. Values above 100 will select more examples from the majority classes.
      
      # subset out the relevant data
      X <- cbind(data[rows.train, covariates],
                 data[rows.train, 'bycatch'])
      names(X) <- c(covariates, "bycatch")
      
      # define the SMOTE model formula
      formula.rf <- formula(paste0("bycatch ~ ",
                                   paste0(covariates, collapse=" + ")))
      
      # create the synthetic (SMOTE) dataset
      X.SMOTE <- DMwR::SMOTE(form = formula.rf,
                             data = X,
                             k = 3, # number of nearest neighbors used to generate new examples of the minority class
                             perc.over  = p.over,   # amount of over-sampling for the minority class
                             perc.under = p.under) # p.under) # amount of undersampling for the majority class
      # table(X.SMOTE$bycatch) # check now we roughly have class balance
      
      # fit the model
      if(use.package %in% c('randomForest', 'randomforest')){
         # fit the model
         fit <- randomForest::randomForest(x = X.SMOTE[,covariates],
                                           y = X.SMOTE[,"bycatch"],
                                           mtry  = 3,          # Number of variables randomly sampled as candidates at each split. Note that the default values are different for classification (sqrt(p) where p is number of variables in x) and regression (p/3)
                                           ntree = 1500,      # Number of trees to grow. This should not be set to too small a number, to ensure that every input row gets predicted at least a few times.
                                           importance = FALSE, # Should importance of predictors be assessed?
                                           # do.trace = 200,    # print updates every 200 trees
                                           keep.forest = TRUE,
                                           replace = FALSE) # keep forest for prediction at test locations
         
         if(log_model){
            saveRDS(object = fit, file = log_model_filename)
         }
         
         # calculate and return performance
         expected <- NA
         if( length(rows.test) > 0 ){
            if(any(!is.null(rows.test)) & any(!is.na(rows.test))) { # prevents crashing when there's no testing data provided } else NA
               expected <- unname(randomForest:::predict.randomForest(fit, 
                                                                      newdata = data[rows.test, covariates], 
                                                                      type = 'prob')[,2])
         }}
         # type = 'prob' returns a matrix with the probability of belonging to each class. I just want the probability of belonging to class "bycatch==1"
         # make predictions using extra prediction dataset
         if(!is.null(prediction.data)){
            # make sure all the necessary covariates are in the prediction dataset
            if( !all( covariates %in% names(prediction.data) ) ){
               print('Not using "prediction.data" to make predictions because prediction.data is missing necessary covariate data.')  
            } else {
               predictions <- tryCatch(expr = {
                  unname(randomForest:::predict.randomForest(fit, 
                                                             newdata = prediction.data[,covariates], 
                                                             type = 'prob')[,2])
               },
               error=function(cond) {
                  message(cond)
                  return = rep(NA, nrow(prediction.data))
               })
            }
         }
      }
      
      if(use.package == 'ranger'){
         
         # for implementation in Ranger, see: https://stats.stackexchange.com/questions/171380/implementing-balanced-random-forest-brf-in-r-using-randomforests
         fit <- ranger::ranger(
            formula = bycatch ~ ., 
            data = X.SMOTE, 
            num.trees       = 1500, 
            importance = 'none', 
            probability = TRUE, # "soft" vote counting
            mtry = 3, 
            verbose = FALSE,
            keep.inbag = keep.inbag,     # needed for estimating SE
            quantreg  = quantile.forest, # needed for quantiles
            replace = FALSE # sample withOUT replacement (apparently this can reduce bias a little, and these sample sizes are pretty big)
         )
         
         if(log_model){
            saveRDS(object = fit, file = log_model_filename)
         }
         
         # calculated expected values for the testing dataset (if data is provided)
         expected <- NA
         if( length(rows.test) > 0 ){
            if(any(!is.null(rows.test)) & any(!is.na(rows.test))) { # prevents crashing when there's no testing data provided } else NA
               expected <- ranger:::predict.ranger(object = fit, 
                                                   data = data[rows.test, covariates], 
                                                   type = 'response')$predictions[,2]
         }}
         # type ='response' (the default) returns the predicted classes for classification, predicted numeric values for regression, and predicted probabilities for probability estimation
         # make predictions using extra prediction dataset
         if(!is.null(prediction.data)){
            # make sure all the necessary covariates are in the prediction dataset
            if( !all( covariates %in% names(prediction.data) ) ){
               print('Not using "prediction.data" to make predictions because prediction.data is missing necessary covariate data.')  
            } else {
               predictions <- tryCatch(expr = {
                     ranger:::predict.ranger(object = fit, 
                                             data = prediction.data[,covariates], 
                                             type = 'response')$predictions[,2]
                  },
                  error=function(cond) {
                     message(cond)
                     return = rep(NA, nrow(prediction.data))
                  })
            }
         }
      }
      
      # calculate and return performance
      observed <- NA
      if( length(rows.test) > 0 ){
         if(any(!is.null(rows.test)) & any(!is.na(rows.test))) { # prevents crashing when there's no testing data provided } else NA
            observed <- data[rows.test, 'bycatch'] # observations at test locations
      }}
      
      # finish time
      time2 <- Sys.time()
      # total runtime
      runtime <- as.numeric(round(difftime(time2, time1, units = 'min'), 1))
      
      # calculate AUC
      (AUC  <- calc_AUC(expected, observed))
      # return results
      results <- list('AUC' = AUC, 
                      'observed' = observed, 
                      'expected' = expected, 
                      'runtime'  = runtime,
                      'error' = NA,
                      'model' = NA,
                      model_converged = NA)
      if(save.model.fit) results[['model']] <- fit
      if(!is.null(prediction.data)) results[['predictions']] <- list('haul_id' = prediction.data$haul_id, 'predictions' = predictions)
      results
      # other things to consider returning: rows.train, rows.test, fit
   }, 
   error=function(cond) {
      message(cond)
      return(list('AUC' = NA_real_, 
                  'observed' = data[rows.test,'bycatch'],
                  'expected' = rep(NA_real_, length(rows.test)), 
                  'runtime'  = as.numeric(round(difftime( Sys.time(), time1, units = 'min'), 1)),
                  'error' = cond,
                  'model' = NA,
                  'predictions' = NULL,
                  model_converged = NA))
   })
   
   return(out)
}

# function to fit model 9: Gradient Boosting Machine
fit_m9_gbt1 <- function(data,        # dataset with response = 'bycatch' and all covariates
                        covariates,  # character vector of covariate names (matching column names in "data")
                        modelfamily, # "binomial" or "gamma"
                        rows.train,  # rows of "data" to use in training/fitting
                        rows.test,
                        prediction.data = NULL, # optional 2ndary dataset to use the model to make more predictions without having to save the model
                        use.package = 'xgboost', # "xgboost" or "gbm"
                        save.model.fit = FALSE,
                        log_model = FALSE,
                        log_model_filename){  # rows of "data" to use in testing/validating
   # record the start time
   time1 <- Sys.time()
   
   # rename a variable to make sure it's the same everywhere
   if( ('bottom_slope' %in% covariates) &  !('bottom_slope' %in% names(data)) ) data$bottom_slope <- data$bottom_slope_GEBCO
   
   
   # code to fit model using xgboost package
   if(use.package == 'xgboost'){
      # XGBoost only works with matrices that contain all numeric variables
      # so it's necessary to one hot encode our data
      # use vtreat package to prep data
      
      # Create the treatment plan from the training data
      treatplan <- vtreat::designTreatmentsZ(data[rows.train,], covariates, verbose = FALSE)
      
      # Get the "clean" variable names from the scoreFrame
      new_vars <- treatplan %>%
         magrittr::use_series(scoreFrame) %>%
         dplyr::filter(code %in% c("clean", "lev")) %>%
         magrittr::use_series(varName)
      
      # Prepare the training data
      features_train <- vtreat::prepare(treatplan, data[rows.train,], varRestriction = new_vars) %>% as.matrix()
      response_train <- data[rows.train, 'bycatch']
      
      # Prepare the test data
      if( length(rows.test) > 0 ){
         if(any(!is.null(rows.test)) & any(!is.na(rows.test))) { # prevents crashing when there's no testing data provided } else NA
            features_test <- vtreat::prepare(treatplan, data[rows.test,], varRestriction = new_vars) %>% as.matrix()
            response_test <- data[rows.test, 'bycatch']
      }}
      
      if(!is.null(prediction.data)){
         features_prediction <- vtreat::prepare(treatplan, prediction.data[,covariates], varRestriction = new_vars) %>% as.matrix()
      }
      
      # force haul duration to have an monotone increasing relationship with bycatch
      # monotone <- rep(0, length(covariates)) # 0 means no constraint
      # monotone[ covariates == 'duration' ] <- 1 # 1 means monotone increasing. 
      
      out <- tryCatch(expr = {
         # fit the model
         # for more info on parameter options: https://xgboost.readthedocs.io/en/latest/parameter.html 
         fit <- xgboost::xgboost(
            data  = features_train,
            label = as.numeric(as.character(response_train)),
            nrounds   = 1000, # previously I set this to 4000,
            max.depth = 3,    # previous (and default) = 6
            objective = ifelse(modelfamily == 'binomial', 'binary:logistic', 'reg:gamma'),
            booster = 'gbtree', # 'gblinear',
            params = list(eta = .1, # eta = learning rate. default = 0.3. Low values require more nrounds. High values risk over-fitting.
                          # monotone_constraints = monotone,
                          eval_metric = ifelse(modelfamily == 'binomial', 'auc', 'rmse'), 
                          # eval_metric is just that: an evaluation metric. It doesn't change the model fit. Default for binary classification = 'logloss'; default for objective = 'reg:gamma' is 'gamma-nloglik'.
                          subsample = 0.7), # force subsampling to hopefully decrease over-fitting.
            # print_every_n = 250,
            verbose = 0 # 0 = no updates. 1 = some updates. 2 = lots of info.
         )
         
         if(log_model){
            saveRDS(object = fit, file = log_model_filename)
         }
         
         # calculate and return performance
         # set observed & expected to NA, and then replace those values iff there's testing data
         observed <- NA
         expected <- NA
         if( length(rows.test) > 0 ){ 
            if(any(!is.null(rows.test)) & any(!is.na(rows.test))) { # prevents crashing when there's no testing data provided } else NA
               observed <- data[rows.test, 'bycatch'] # observations at test locations
               expected <- xgboost:::predict.xgb.Booster(object = fit,
                                                         newdata = features_test)
         # this will give predicted probabilities for logistic and amounts for gamma regression
         }}

         # make predictions using extra prediction dataset
         if(!is.null(prediction.data)){
            # make sure all the necessary covariates are in the prediction dataset
            if( !all( covariates %in% names(prediction.data) ) ){
               print('Not using "prediction.data" to make predictions because prediction.data is missing necessary covariate data.')  
            } else {
               predictions <- tryCatch(expr = {
                  xgboost:::predict.xgb.Booster(object = fit,
                                                newdata = features_prediction)},
                  error=function(cond) {
                     message(cond)
                     return = rep(NA, nrow(prediction.data))
                  })
            }
         }
         
         # finish time
         time2 <- Sys.time()
         # total runtime
         runtime <- as.numeric(round(difftime(time2, time1, units = 'min'), 1))
         
         # calculate summary stats (these functions are ok with NA)
         if(modelfamily=='binomial') gof <- calc_AUC(expected, observed)
         if(modelfamily=='gamma')    gof <- calc_RMSE(expected, observed)
         
         # return results
         results <- list('gof' = gof,
                         'observed' = observed,
                         'expected' = expected,
                         'runtime'  = runtime,
                         'error' = NA,
                         'model' = NA,
                         model_converged = NA)
         if(save.model.fit) results[['model']] <- fit
         if(!is.null(prediction.data)) results[['predictions']] <- list('haul_id' = prediction.data$haul_id, 'predictions' = predictions)
         names(results)[1] <- if(modelfamily=='binomial') 'AUC' else 'RMSE'
         results
      },
      error=function(cond) {
         message(cond)
         
         results <- list('gof' = NA_real_,
                         'observed' = data[rows.test,'bycatch'],
                         'expected' = rep(NA_real_, length(rows.test)),
                         'runtime'  = as.numeric(round(difftime( Sys.time(), time1, units = 'min'), 1)),
                         'error' = cond,
                         'model' = NA,
                         'predictions' = NULL,
                         model_converged = NA)
         names(results)[1] <- if(modelfamily=='binomial') 'AUC' else 'RMSE'
         
         return(results)
      })
   }
   
   # use gbm package instead
   if(use.package == 'gbm'){
      #workaround for lack of gamma distribution is to use lognormal instead. https://stats.stackexchange.com/questions/105218/r-which-distribution-to-use-with-gbm-for-gamma-distributed-data
      
      # force haul duration to have an monotone increasing relationship with bycatch
      # monotone <- rep(0, length(covariates))
      # monotone[ covariates == 'duration' ] <- 1
      
      out <- tryCatch(expr = {
         
         # GBM PACKAGE DOESN'T COME WITH GAMMA DISTRIBUTION!! Using lognormal instead.
         # fit the model
         fit <- gbm::gbm.fit(x = data[rows.train, covariates], 
                             y = if(modelfamily == 'gamma') log(data[rows.train, 'bycatch']) else as.character(data[rows.train, 'bycatch']), 
                             distribution = ifelse(modelfamily=='binomial', 'bernoulli', 'gaussian'), 
                             # var.monotone = monotone, # in xgboost, "monotone_constraints"
                             n.trees = 1000, # 4000,  
                             bag.fraction = 0.7, # default = 0.5
                             interaction.depth = 3, # in xgboost, interactions are controlled with "interaction_constraints", and must be manually specified, rather than a single number applying to all covariates
                             shrinkage = 0.1, # learning rate, which is "eta" parameter in xgboost
                             verbose = FALSE)
         
         if(log_model){
            saveRDS(object = fit, file = log_model_filename)
         }
         
         # calculate and return performance
         observed <- NA
         expected <- NA
         if( length(rows.test) > 0 ){ 
            if(any(!is.null(rows.test)) & any(!is.na(rows.test))) { # prevents crashing when there's no testing data provided } else NA
               observed <- data[rows.test, 'bycatch'] # observations at test locations
               expected <- as.numeric(as.character(gbm:::predict.gbm(object = fit, 
                                                                     newdata = data[rows.test, covariates], 
                                                                     n.trees = 1000, 
                                                                     type = 'response')))
               if(modelfamily == 'gamma') expected <- exp(expected)
         }}
         
         # make predictions using extra prediction dataset
         if(!is.null(prediction.data)){
            # make sure all the necessary covariates are in the prediction dataset
            if( !all( covariates %in% names(prediction.data) ) ){
               print('Not using "prediction.data" to make predictions because prediction.data is missing necessary covariate data.')  
            } else {
               predictions <- tryCatch(expr = {
                  predictions <- as.numeric(as.character(gbm:::predict.gbm(object = fit, 
                                                                           newdata = prediction.data[,covariates],
                                                                           n.trees = 1000, 
                                                                           type = 'response')))
                  if(modelfamily == 'gamma') predictions <- exp(predictions)
                  predictions},
                  error=function(cond) {
                     message(cond)
                     return = rep(NA, nrow(prediction.data))
                  })
            }
         }
         
         # finish time
         time2 <- Sys.time()
         # total runtime
         runtime <- as.numeric(round(difftime(time2, time1, units = 'min'), 1))
         
         # calculate summary statistics
         if(modelfamily=='binomial') gof <- calc_AUC(expected, observed)
         if(modelfamily=='gamma')    gof <- calc_RMSE(expected, observed)
         
         # return results
         results <- list('gof' = gof, 
                         'observed' = observed, 
                         'expected' = expected, 
                         'runtime'  = runtime,
                         'error' = NA,
                         'model' = NA,
                         model_converged = NA)
         if(save.model.fit) results[['model']] <- fit
         if(!is.null(prediction.data)) results[['predictions']] <- list('haul_id' = prediction.data$haul_id, 'predictions' = predictions)
         names(results)[1] <- if(modelfamily=='binomial') 'AUC' else 'RMSE' 
         results
      }, 
      error=function(cond) {
         message(cond)
         
         results <- list('gof' = NA_real_, 
                         'observed' = data[rows.test,'bycatch'],
                         'expected' = rep(NA_real_, length(rows.test)), 
                         'runtime'  = as.numeric(round(difftime( Sys.time(), time1, units = 'min'), 1)),
                         'error' = cond,
                         'model' = NA,
                         'predictions' = NULL,
                         model_converged = NA)
         names(results)[1] <- if(modelfamily=='binomial') 'AUC' else 'RMSE' 
         
         return(results)
      })
   }
   
   return(out)
}

# function to fit model 9: Gradient Boosting Machine using DART (supposed to help avoid over-fitting)
fit_m10_gbt2 <- function(data,        # dataset with response = 'bycatch' and all covariates
                         covariates,  # character vector of covariate names (matching column names in "data")
                         modelfamily, # "binomial" or "gamma"
                         rows.train,  # rows of "data" to use in training/fitting
                         rows.test,
                         prediction.data = NULL, # optional 2ndary dataset to use the model to make more predictions without having to save the model
                         save.model.fit = FALSE,
                         log_model = FALSE,
                         log_model_filename){  # rows of "data" to use in testing/validating
   # more on DART: https://xgboost.readthedocs.io/en/latest/tutorials/dart.html
   
   # record the start time
   time1 <- Sys.time()
   
   # rename a variable to make sure it's the same everywhere
   if( ('bottom_slope' %in% covariates) &  !('bottom_slope' %in% names(data)) ) data$bottom_slope <- data$bottom_slope_GEBCO
   
   
   # code to fit model using xgboost package
   # XGBoost only works with matrices that contain all numeric variables
   # so it's necessary to one hot encode our data
   # use vtreat package to prep data
   
   # Create the treatment plan from the training data
   treatplan <- vtreat::designTreatmentsZ(data[rows.train,], covariates, verbose = FALSE)
   
   # Get the "clean" variable names from the scoreFrame
   new_vars <- treatplan %>%
      magrittr::use_series(scoreFrame) %>%
      dplyr::filter(code %in% c("clean", "lev")) %>%
      magrittr::use_series(varName)
   
   # Prepare the training data
   features_train <- vtreat::prepare(treatplan, data[rows.train,], varRestriction = new_vars) %>% as.matrix()
   response_train <- data[rows.train, 'bycatch']
   
   # Prepare the test data
   if( length(rows.test) > 0 ){
      if(any(!is.null(rows.test)) & any(!is.na(rows.test))) { # prevents crashing when there's no testing data provided } else NA
         features_test <- vtreat::prepare(treatplan, data[rows.test,], varRestriction = new_vars) %>% as.matrix()
         response_test <- data[rows.test, 'bycatch']
   }}
   
   # prepare the prediction dataset
   if(!is.null(prediction.data)){
      features_prediction <- vtreat::prepare(treatplan, prediction.data[,covariates], varRestriction = new_vars) %>% as.matrix()
   }
   
   # force haul duration to have an monotone increasing relationship with bycatch
   # monotone <- rep(0, length(covariates)) # 0 means no constraint
   # monotone[ covariates == 'duration' ] <- 1 # 1 means monotone increasing. 
   
   out <- tryCatch(expr = {
      # fit the model
      # for more info on parameter options: https://xgboost.readthedocs.io/en/latest/parameter.html 
      fit <- xgboost::xgboost(
         data  = features_train,
         label = as.numeric(as.character(response_train)),
         nrounds   = 1000, # previously I set this to 4000,
         max.depth = 3,    # previous (and default) = 6
         objective = ifelse(modelfamily == 'binomial', 'binary:logistic', 'reg:gamma'),
         booster = 'dart',
         params = list(eta = .15, # eta = learning rate. default = 0.3. Low values require more nrounds. High values risk over-fitting.
                       # monotone_constraints = monotone,
                       eval_metric = ifelse(modelfamily == 'binomial', 'auc', 'rmse'), 
                       # eval_metric is just that: an evaluation metric. It doesn't change the model fit. Default for binary classification = 'logloss'; default for objective = 'reg:gamma' is 'gamma-nloglik'. 
                       subsample = 0.7, # force subsampling to hopefully decrease over-fitting.
                       # DART-specific parameter settings
                       sample_type = 'uniform', # uniform (default) = dropped trees are selected uniformly. weighted: dropped trees are selected in proportion to weight.
                       normalize_type = 'tree', # tree (default) = New trees have the same weight of each of dropped trees. forest: New trees have the same weight of sum of dropped trees (forest).
                       skip_drop = 0.25, # If skip_drop = 1, the dropout procedure is skipped and dart is the same as gbtree. 
                       rate_drop = .5 # if rate_drop of 0 = gbtree. rate_drop of 1 = random forest.
                       ), 
         # print_every_n = 250,
         verbose = 0 # 0 = no updates. 1 = some updates. 2 = lots of info.
      )
      
      if(log_model){
         saveRDS(object = fit, file = log_model_filename)
      }
      
      # calculate and return performance (but only if there is test data)
      observed <- NA
      expected <- NA
      if( length(rows.test) > 0 ){ 
         if(any(!is.null(rows.test)) & any(!is.na(rows.test))) { # prevents crashing when there's no testing data provided } else NA
            observed <- data[rows.test, 'bycatch'] # observations at test locations
            expected <- xgboost:::predict.xgb.Booster(object = fit,
                                                      newdata = features_test, 
                                                      ntreelimit = fit$niter) 
      # The help documentation says that setting ntree_limit > 0 is necessary: https://xgboost.readthedocs.io/en/latest/parameter.html#additional-parameters-for-dart-booster-booster-dart
      # this will give predicted probabilities for logistic and amounts for gamma regression
      }}
      
      # make predictions using extra prediction dataset
      if(!is.null(prediction.data)){
         # make sure all the necessary covariates are in the prediction dataset
         if( !all( covariates %in% names(prediction.data) ) ){
            print('Not using "prediction.data" to make predictions because prediction.data is missing necessary covariate data.')  
         } else {
            predictions <- tryCatch(expr = {
               xgboost:::predict.xgb.Booster(object = fit,
                                             newdata = features_prediction,
                                             ntreelimit = fit$niter)},
               error=function(cond) {
                  message(cond)
                  return = rep(NA, nrow(prediction.data))
               })
         }
      }
      
      # finish time
      time2 <- Sys.time()
      # total runtime
      runtime <- as.numeric(round(difftime(time2, time1, units = 'min'), 1))
      
      if(modelfamily=='binomial') gof <- calc_AUC(expected, observed)
      if(modelfamily=='gamma')    gof <- calc_RMSE(expected, observed)
      
      # return results
      results <- list('gof' = gof,
                      'observed' = observed,
                      'expected' = expected,
                      'runtime'  = runtime,
                      'error' = NA,
                      'model' = NA,
                      model_converged = NA)
      if(save.model.fit) results[['model']] <- fit
      if(!is.null(prediction.data)) results[['predictions']] <- list('haul_id' = prediction.data$haul_id, 'predictions' = predictions)
      names(results)[1] <- if(modelfamily=='binomial') 'AUC' else 'RMSE'
      results
   },
   error=function(cond) {
      message(cond)
      
      results <- list('gof' = NA_real_,
                      'observed' = data[rows.test,'bycatch'],
                      'expected' = rep(NA_real_, length(rows.test)),
                      'runtime'  = as.numeric(round(difftime( Sys.time(), time1, units = 'min'), 1)),
                      'error' = cond,
                      'model' = NA,
                      'predictions' = NULL,
                      model_converged = NA)
      names(results)[1] <- if(modelfamily=='binomial') 'AUC' else 'RMSE'
      
      return(results)
   })
   
   return(out)
}


# Functions to evaluate model performance
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

# function & dataframe for organizing and transforming covariates
# function to process the covariates (log, scale, and/or polynomialize them)
# rather than transforming covariates in the full dataset before starting analyses,
#    I will transform the training data for each set of models. And to speed that up,
#    I condensed it into a function. 
process_covariates <- function(covariate,
                               log = FALSE,
                               scale = FALSE,
                               polynomialize = FALSE,
                               # polynomial_degree = 2,
                               index.train){
   # covariate = vector of actual covariate values
   # log = (logical) whether or not to log the covariate
   # scale = (logical) whether or not to scale the covariate using the mean and SD
   # polynomialize = (logical) whether or not to calculate the orthogonal polynomial 
   # index.train = (numeric) vector index of covariate values to include in the scale and polynomial functions.
   #                testing values are also predicted using the values calculated from the training data. 
   
   if(!class(covariate) %in% c('integer', 'numeric', 'POSIXct', 'POSIXt', 'Date')) stop('Covariate must be numeric or similar (e.g. integer, Date, etc.)')
   
   # rename covariate to make it a little easier
   x <- covariate
   
   # log covariate
   if(log) x <- log(x)
   
   # scale covariate (based only on training data)
   if(scale){
      xscale <- attributes(scale(x[index.train]))
      x <- scale(x, 
                 center = xscale$`scaled:center`, 
                 scale  = xscale$`scaled:scale`)
   }
   
   # polynomialize covariate (based only on training data)
   if(polynomialize){
      xpoly <- poly(x[index.train], 
                    degree = 2) # polynomial_degree)
      x <- stats:::predict.poly(xpoly, as.vector(x))
   }
   
   # return the transformed covariate as a dataframe (possibly multiple columns if polynomialize == TRUE)
   return(as.data.frame(x))
}

# data.frame to specify which covariates should be logged, scaled, and/or polynomialized
lsp_lut <- rbind(
   # Year
   data.frame('original_name' = 'year',
              'logged'        = FALSE,
              'scaled'        = TRUE,
              'polyd'         = TRUE,
              'final_name'    = 'year_poly1',
              'pair_name'     = 'year_poly2',
              'pretty_name'   = 'Year',
              'plot_label'    = "'Year'"), # use " ' ' " so that I can later use "parse()" on this text. (it only *needs* multiple quotes if there's a space)
   # Day of Year
   data.frame('original_name' = 'doy',
              'logged'        = FALSE,
              'scaled'        = TRUE,
              'polyd'         = TRUE,
              'final_name'    = 'doy_poly1',
              'pair_name'     = 'doy_poly2',
              'pretty_name'   = 'Day of Year',
              'plot_label'    = "'Day of Year'"),
   # Time of Day
   data.frame('original_name' = 'ToD',
              'logged'        = FALSE,
              'scaled'        = TRUE,
              'polyd'         = TRUE,
              'final_name'    = 'ToD_poly1',
              'pair_name'     = 'ToD_poly2',
              'pretty_name'   = 'Time of Day',
              'plot_label'    = "'Hour of Day'"),
   # Fishing haul duration
   data.frame('original_name' = 'duration',
              'logged'        = FALSE,
              'scaled'        = TRUE,
              'polyd'         = TRUE,
              'final_name'    = 'duration_poly1',
              'pair_name'     = 'duration_poly2',
              'pretty_name'   = 'Duration',
              'plot_label'    = "'Duration (min)'"),
   # Fishing Depth
   data.frame('original_name' = 'depth_fishing',
              'logged'        = TRUE,
              'scaled'        = TRUE,
              'polyd'         = TRUE,
              'final_name'    = 'depth_fishing_poly1',
              'pair_name'     = 'depth_fishing_poly2',
              'pretty_name'   = 'Fishing Depth',
              'plot_label'    = "'Fishing Depth (m)'"),
   # Bottom Depth
   data.frame('original_name' = 'depth_bottom',
              'logged'        = TRUE,
              'scaled'        = TRUE,
              'polyd'         = TRUE,
              'final_name'    = 'depth_bottom_poly1',
              'pair_name'     = 'depth_bottom_poly2',
              'pretty_name'   = 'Bottom Depth',
              'plot_label'    = "'Bottom Depth (m)'"),
   # Bottom Slope
   data.frame('original_name' = 'slope_bottom',
              'logged'        = FALSE,
              'scaled'        = TRUE,
              'polyd'         = TRUE,
              'final_name'    = 'slope_bottom_poly1',
              'pair_name'     = 'slope_bottom_poly2',
              'pretty_name'   = 'Bottom Slope',
              'plot_label'    = "'Bottom Slope'"),
   # Sea Surface Temperature
   data.frame('original_name' = 'sst',
              'logged'        = FALSE,
              'scaled'        = TRUE,
              'polyd'         = TRUE,
              'final_name'    = 'sst_poly1',
              'pair_name'     = 'sst_poly2',
              'pretty_name'   = 'SST',
              'plot_label'    = "'SST (' * degree * 'C)'"),
   # Sea Surface Temperature lag 7
   data.frame('original_name' = 'sst_lag_7',
              'logged'        = FALSE,
              'scaled'        = TRUE,
              'polyd'         = TRUE,
              'final_name'    = 'sst_lag_7_poly1',
              'pair_name'     = 'sst_lag_7_poly2',
              'pretty_name'   = 'Lagged SST',
              'plot_label'    = "'SST (' * degree * 'C; 7 day lag)'"),
   # Sea Surface Temperature lag 365
   data.frame('original_name' = 'sst_lag_365',
              'logged'        = FALSE,
              'scaled'        = TRUE,
              'polyd'         = TRUE,
              'final_name'    = 'sst_lag_365_poly1',
              'pair_name'     = 'sst_lag_365_poly2',
              'pretty_name'   = 'Lagged SST',
              'plot_label'    = "'SST (' * degree * 'C; 1 year lag)'"),
   # Sea Surface Temperature Anomaly
   data.frame('original_name' = 'sst_anomaly',
              'logged'        = FALSE,
              'scaled'        = TRUE,
              'polyd'         = TRUE,
              'final_name'    = 'sst_anomaly_poly1',
              'pair_name'     = 'sst_anomaly_poly2',
              'pretty_name'   = 'SST Anomaly',
              'plot_label'    = "'SST Anomaly (' * degree * 'C)'"),
   # Sea Surface Temperature Anomaly (lag 7)
   data.frame('original_name' = 'sst_anom_lag_7',
              'logged'        = FALSE,
              'scaled'        = TRUE,
              'polyd'         = TRUE,
              'final_name'    = 'sst_anomaly_lag_7_poly1',
              'pair_name'     = 'sst_anomaly_lag_7_poly2',
              'pretty_name'   = 'Lagged SST Anomaly',
              'plot_label'    = "'SST Anomaly (' * degree * 'C; 7 day lag)'"),
   # Sea Surface Temperature Anomaly (lag 365)
   data.frame('original_name' = 'sst_anom_lag_365',
              'logged'        = FALSE,
              'scaled'        = TRUE,
              'polyd'         = TRUE,
              'final_name'    = 'sst_anomaly_lag_365_poly1',
              'pair_name'     = 'sst_anomaly_lag_365_poly2',
              'pretty_name'   = 'Lagged SST Anomaly',
              'plot_label'    = "'SST Anomaly (' * degree * 'C; 1 year lag)'"),
   # Coastal Upwelling Transport Index
   data.frame('original_name' = 'cuti',
              'logged'        = FALSE,
              'scaled'        = TRUE,
              'polyd'         = TRUE,
              'final_name'    = 'cuti_poly1',
              'pair_name'     = 'cuti_poly2',
              'pretty_name'   = 'Coastal Upwelling Transport Index',
              'plot_label'    = "'Coastal Upwelling Transport Index'"), # \n(volume of vertical transport per second per meter of coastline)
   # Coastal Upwelling Transport Index (lag 7)
   data.frame('original_name' = 'cuti_lag_7',
              'logged'        = FALSE,
              'scaled'        = TRUE,
              'polyd'         = TRUE,
              'final_name'    = 'cuti_lag_7_poly1',
              'pair_name'     = 'cuti_lag_7_poly2',
              'pretty_name'   = 'Lagged Coastal Upwelling Transport Index',
              'plot_label'    = "'Coastal Upwelling Transport Index (7 day lag)'"), # \n(volume of vertical transport per second per meter of coastline)
   # Coastal Upwelling Transport Index
   data.frame('original_name' = 'cuti_lag_365',
              'logged'        = FALSE,
              'scaled'        = TRUE,
              'polyd'         = TRUE,
              'final_name'    = 'cuti_lag_365_poly1',
              'pair_name'     = 'cuti_lag_365_poly2',
              'pretty_name'   = 'Lagged Coastal Upwelling Transport Index',
              'plot_label'    = "'Coastal Upwelling Transport Index (1 year lag)'"), # \n(volume of vertical transport per second per meter of coastline)
   # Latitude
   data.frame('original_name' = 'lat',
              'logged'        = FALSE,
              'scaled'        = FALSE,
              'polyd'         = TRUE,
              'final_name'    = 'lat_poly1',
              'pair_name'     = 'lat_poly2',
              'pretty_name'   = 'Latitude',
              'plot_label'    = "Latitude"),
   # Longitude
   data.frame('original_name' = 'lon',
              'logged'        = FALSE,
              'scaled'        = FALSE,
              'polyd'         = TRUE,
              'final_name'    = 'lon_poly1',
              'pair_name'     = 'lon_poly2',
              'pretty_name'   = 'Longitude',
              'plot_label'    = "Longitude"),
   # add in categorical covariates just for the plot label
   # Sector
   data.frame('original_name' = 'sector',
              'logged'        = FALSE,
              'scaled'        = FALSE,
              'polyd'         = FALSE,
              'final_name'    = 'sector',
              'pair_name'     = NA,
              'pretty_name'   = 'Sector',
              'plot_label'    = "'Sector'"),
   # Year_f
   data.frame('original_name' = 'year_f',
              'logged'        = FALSE,
              'scaled'        = FALSE,
              'polyd'         = FALSE,
              'final_name'    = 'year_f',
              'pair_name'     = NA,
              'pretty_name'   = 'Year',
              'plot_label'    = "'Year'"))