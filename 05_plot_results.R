# Script to produce plots for the manuscript
{
   # Inputs: 
   #  - 'results/aggregated_results.RDS'
   #    From 04_aggregate_results.R
   
   # Outputs: (all saved to base_folder: results/cv-yearly)
   #  - A bunch of images (.png files) that are all saved to 'figures' folder.
}

library(tidyverse)

# get data
{
   # read in the results
   results <- readRDS(file = file.path('results', 'aggregated_results.RDS'))
   
   # Make model names a little prettier
   model_names <- c('GLM 1', 'GLM 2', 
                    'GAM 1', 'GAM 2', 'GAM 3', 
                    'RF 1', 'RF 2', 'RF 3', 
                    'GBT 1', 'GBT 2',
                    'AVG 1', 'AVG 2')
   
   fdat      <- results[['predicted_observed_by_haul']]
   # binomial model AUC
   auc_total <- results[['AUCs']][['auc_total']]
   auc_year  <- results[['AUCs']][['auc_year']]
   auc_month <- results[['AUCs']][['auc_month']]
   aucs      <- results[['AUCs']][['auc_year_month']]
   # gamma model RMSE
   rmse_total <- results[['RMSE_gamma']][['rmse_total']]
   rmse_year  <- results[['RMSE_gamma']][['rmse_year']]
   rmse_month <- results[['RMSE_gamma']][['rmse_month']]
   rmses      <- results[['RMSE_gamma']][['rmse_year_month']]
   # hurdle model RMSE
   rmse_total_hurdle <- results[['RMSE_hurdle']][['rmse_total']]
   rmse_year_hurdle  <- results[['RMSE_hurdle']][['rmse_year']]
   rmse_month_hurdle <- results[['RMSE_hurdle']][['rmse_month']]
   rmses_hurdle      <- results[['RMSE_hurdle']][['rmse_year_month']]

   
   btts <- results$bycatch_target_ratios_hurdle
   btts_binomial <- results$bycatch_target_ratios_binomial
   btts_w1 <- results$bycatch_target_ratios_hurdle_w1
   
   # some colors for plots (make sure they all have the same colors even if some models aren't there)
   #colors <- scales::hue_pal()(length(model_names))
   #switch to viridis
   colors <- viridis::viridis(length(model_names))
   names(colors) <- model_names
   mycolorscale <- scale_color_manual(values = colors, 
                                      breaks = model_names)
   myfillscale  <- scale_fill_manual(values = colors,
                                     breaks = model_names)
   
   # make sure that the "figures" folder exists
   dir.create('figures', showWarnings = FALSE)
   
   # some functions for adding numbers to plots
   give.n <- function(x, y=NA){
      yval <- if(is.numeric(x)) y else median(x)
      return(c(y = y, label = length(x))) # median(x) + 0.1
   }
   give.mean <- function(x, y=NA){
      yval <- if(is.numeric(y)) y else mean(x)
      return(c(y = yval, label = round(mean(x), 2)))
   }
}

# AUC (binomial model) plots
{
   # AUC model comparison
   {
      (aucplot <- ggplot(auc_total %>% 
                            dplyr::filter(shoreside == 'included') %>% 
                            dplyr::mutate(yt = factor(n_years_train, levels = c('4', '12', '18'))), 
                         aes(x = model,
                             y = AUC, 
                             shape = yt,
                             color = cross_val_type,
                             group = interaction(model, yt, cross_val_type))) +
          geom_point(size = 4, 
                     position = position_dodge(width = .5), alpha = 0.75) + 
          theme_bw() + 
          # stat_summary(fun.data = give.mean, 
          #              fun.args = list(y = 0.55),
          #              geom = "text", 
          #              # angle = 90,
          #              size = 3,
          #              mapping = aes(group = cross_val_type),
          #              position = ggstance::position_dodgev(height = -0.1)) + # position_dodge(width = 0.6)) + 
          guides(shape = guide_legend(title = "Years of\nTraining\nData"),
                 color = guide_legend(title = 'Cross\nValidation\nType')) + 
          xlab(label = 'Model')+
          coord_cartesian(ylim = c(0.6, .9)) + 
          theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1))+
          viridis::scale_color_viridis(discrete=TRUE))
      ggsave(filename = 'figures/plot_01a_AUC.png', plot = aucplot, width = 6, height = 4, units = 'in')
   }
   
   # AUC by year
   {
      # select individual models and only include those results
      k_fold_model <- 'rf 1'
      weekly_model <- 'avg 1'
      yearly_model <- 'avg 1'
      
      
      (auc_by_year_plot <- ggplot(auc_year %>% 
                                     dplyr::filter(shoreside == 'included' & 
                                                      ((cross_val_type == 'k-fold' & model == k_fold_model) |
                                                          (cross_val_type == 'weekly' & model == weekly_model) |
                                                          (cross_val_type == 'yearly' & model == yearly_model))) %>% 
                                     dplyr::mutate(yt = factor(n_years_train, 
                                                               levels = c('4', '12', '18'))) %>%
                                     dplyr::filter(yt %in% c('4', '12', '18')), 
                                  aes(x = year,
                                      y = AUC, 
                                      shape = yt,
                                      color = cross_val_type,
                                      group = interaction(year, yt, cross_val_type))) +
            geom_point(position = position_dodge(width = .2),
                       size = 4,
                       alpha = 0.75) + 
            theme_bw() + 
            geom_text(mapping = aes(x = year, 
                                    y = 0.7, 
                                    label = n, 
                                    color = NULL),
                      size = 2.5) + 
            # stat_summary(fun.data = give.mean, 
            #              fun.args = list(y = 0.6),
            #              geom = "text", 
            #              # angle = 90,
            #              size = 3,
            #              mapping = aes(group = interaction(model_name, cross_val_type)),
            #              position = ggstance::position_dodgev(height = -0.08)) + # position_dodge(width = 0.7)) +
            # theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1))+
            # coord_cartesian(ylim = c(0.5, 1)) +
            guides(shape = guide_legend(title = "Years of\nTraining\nData"),
                   color = guide_legend(title = 'Cross\nValidation\nType')) + 
            xlab(label = 'yearly')+
            viridis::scale_color_viridis(discrete=TRUE))
      ggsave(filename = 'figures/plot_01b_AUC_by_year.png', plot = auc_by_year_plot, width = 5, height = 4, units = 'in')
   }
   
   # AUC by Month
   {
      (auc_by_month_plot <- ggplot(auc_month %>%  
                                      dplyr::filter(shoreside == 'included' & 
                                                       ((cross_val_type == 'k-fold' & model == k_fold_model) |
                                                           (cross_val_type == 'weekly' & model == weekly_model) |
                                                           (cross_val_type == 'yearly' & model == yearly_model))) %>% 
                                      dplyr::mutate(yt = factor(n_years_train, 
                                                                levels = c('4', '12', '18'))) %>%
                                      dplyr::filter(yt %in% c('4', '12', '18')) %>% 
                                      dplyr::mutate(month = factor(month.abb[as.numeric(month)], levels = month.abb)), 
                                   aes(x = month,
                                       y = AUC, 
                                       shape = yt,
                                       color = cross_val_type,
                                       group = interaction(month, yt, cross_val_type))) +
          geom_point(position = position_dodge(width = 0.2),
                     size = 4, 
                     alpha = 0.75) + 
          theme_bw() + 
          geom_text(mapping = aes(x = month, 
                                  y = 0.65, 
                                  label = n, 
                                  color = NULL),
                    size = 2.5) + 
          # stat_summary(fun.data = give.mean, 
          #              fun.args = list(y = 0.6),
          #              geom = "text", 
          #              # angle = 90,
          #              size = 3,
          #              mapping = aes(group = interaction(model_name, cross_val_type)),
          #              position = ggstance::position_dodgev(height = -0.08)) + # position_dodge(width = 0.7)) +
          # theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1))+
          # coord_cartesian(ylim = c(0.5, 1)) +
          guides(shape = guide_legend(title = "Years of\nTraining\nData"),
                 color = guide_legend(title = 'Cross\nValidation\nType')) + 
          xlab(label = 'Month')+
          viridis::scale_color_viridis(discrete=TRUE))
      ggsave(filename = 'figures/plot_01c_AUC_by_month.png', plot = auc_by_month_plot, width = 5, height = 4, units = 'in')   
   }
}

# RMSE (Gamma model) comparison plots
{
   # RMSE Model comparison
   {
      # New facet label names for shoreside
      yit.labs <- c(paste0("Years in training = 4"),
                    paste0("Years in training = 12"))
      names(yit.labs) <- c('4', '12')
      
      
      (rmse_plot <- ggplot(rmse_total %>% 
                              dplyr::filter(shoreside == 'included') %>% 
                              dplyr::mutate(yt = factor(n_years_train, 
                                                        levels = c('4', '12', '18'))) %>%
                              dplyr::filter(yt %in% c('4', '12', '18')), 
                           aes(x = model,
                               y = RMSE, 
                               shape = yt,
                               color = cross_val_type,
                               group = interaction(model, yt,cross_val_type))) +
            geom_point(size = 4, 
                       position = position_dodge(width = .5),
                       alpha = 0.75) + 
            theme_bw() + 
            # stat_summary(fun.data = give.mean, 
            #              fun.args = list(y = 13.5),
            #              geom = "text", 
            #              # angle = 90,
            #              size = 3,
            #              mapping = aes(group = cross_val_type),
            #              position = ggstance::position_dodgev(height = 1.8)) + # position_dodge(width = 0.6)) + 
            guides(shape = guide_legend(title = "Years of\nTraining\nData"),
                   color = guide_legend(title = 'Cross\nValidation\nType')) + 
            xlab(label = 'Model')+
            coord_cartesian(ylim = c(16, 23)) + 
            theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1))+
            viridis::scale_color_viridis(discrete=TRUE))
      ggsave(filename = 'figures/plot_02a_RMSE.png', plot = rmse_plot, width = 6.5, height = 4, units = 'in')
   }
   
   # RMSE by year
   {
      k_fold_model <- 'rf 1'
      weekly_model <- 'avg 2'
      yearly_model <- 'avg 2'
      
      (RMSE_by_year_plot <- ggplot(rmse_year %>% 
                                      dplyr::filter(shoreside == 'included' & 
                                                       ((cross_val_type == 'k-fold' & model == k_fold_model) |
                                                           (cross_val_type == 'weekly' & model == weekly_model) |
                                                           (cross_val_type == 'yearly' & model == yearly_model))) %>% 
                                      dplyr::mutate(yt = factor(n_years_train, 
                                                                levels = c('4', '12', '18'))) %>%
                                      dplyr::filter(yt %in% c('4', '12', '18')), 
                                   aes(x = year,
                                       y = nRMSE_sd, 
                                       shape = yt,
                                       color = cross_val_type,
                                       group = interaction(year, yt, cross_val_type))) +
            geom_point(position = position_dodge(width = 0.2),
                       size = 4,
                       alpha = 0.75) + 
            theme_bw() + 
            geom_text(mapping = aes(x = year, 
                                    y = 0.78, 
                                    label = n, 
                                    color = NULL),
                      size = 2.5) + 
            # theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1))+
            coord_cartesian(ylim = c(0.78, 1.06)) +
            guides(shape = guide_legend(title = "Years of\nTraining\nData"),
                   color = guide_legend(title = 'Cross\nValidation\nType')) + 
            xlab(label = 'yearly') + 
            ylab(label = 'Normalized (SD) RMSE')+
            viridis::scale_color_viridis(discrete=TRUE))
      ggsave(filename = 'figures/plot_02b_RMSE_by_year.png', plot = RMSE_by_year_plot, width = 6.5, height = 4, units = 'in')
   }
   
   # rmse by Month
   {
      (rmse_by_month_plot <- ggplot(rmse_month %>% 
                                       dplyr::filter(shoreside == 'included' & 
                                                        ((cross_val_type == 'k-fold' & model == k_fold_model) |
                                                            (cross_val_type == 'weekly' & model == weekly_model) |
                                                            (cross_val_type == 'yearly' & model == yearly_model))) %>% 
                                       dplyr::mutate(yt = factor(n_years_train, 
                                                                 levels = c('4', '12', '18'))) %>%
                                       dplyr::filter(yt %in% c('4', '12', '18')) %>% 
                                       dplyr::mutate(month = factor(month.abb[as.numeric(month)], levels = month.abb)), 
                                    aes(x = month,
                                        y = nRMSE_sd, 
                                        shape = yt,
                                        color = cross_val_type,
                                        group = interaction(month, yt, cross_val_type))) +
          geom_point(position = position_dodge(width = 0.2),
                     size = 4,
                     alpha = 0.75) + 
          theme_bw() + 
          geom_text(mapping = aes(x = month, 
                                  y = 0.8, 
                                  label = n, 
                                  color = NULL),
                    size = 2.5) + 
          # theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1))+
          # coord_cartesian(ylim = c(0.5, 1)) +
          guides(shape = guide_legend(title = "Years of\nTraining\nData"),
                 color = guide_legend(title = 'Cross\nValidation\nType')) + 
          xlab(label = 'Month') + 
          ylab(label = 'Normalized (SD) RMSE')+
          viridis::scale_color_viridis(discrete=TRUE))
      ggsave(filename = 'figures/plot_02c_RMSE_by_month.png', plot = rmse_by_month_plot, width = 6.5, height = 4, units = 'in')
   }
}

# RMSE (Hurdle model)
{
   # RMSE comparison plot
   {
      # New facet label names for shoreside
      yit.labs <- c(paste0("Years in training = 4"),
                    paste0("Years in training = 12"))
      names(yit.labs) <- c('4', '12')
      
      
      (rmse_plot_h <- ggplot(rmse_total_hurdle %>% 
                              dplyr::filter(shoreside == 'included') %>% 
                              dplyr::mutate(yt = factor(n_years_train, 
                                                        levels = c('4', '12', '18'))) %>%
                              dplyr::filter(yt %in% c('4', '12', '18')), 
                           aes(x = model,
                               y = RMSE, 
                               shape = yt,
                               color = cross_val_type,
                               group = interaction(model, yt,cross_val_type))) +
            geom_point(size = 4, 
                       position = position_dodge(width = .5),
                       alpha = 0.75) + 
            theme_bw() + 
            # stat_summary(fun.data = give.mean, 
            #              fun.args = list(y = 13.5),
            #              geom = "text", 
            #              # angle = 90,
            #              size = 3,
            #              mapping = aes(group = cross_val_type),
            #              position = ggstance::position_dodgev(height = 1.8)) + # position_dodge(width = 0.6)) + 
            guides(shape = guide_legend(title = "Years of\nTraining\nData"),
                   color = guide_legend(title = 'Cross\nValidation\nType')) + 
            xlab(label = 'Model')+
            coord_cartesian(ylim = c(7, 10)) + 
            theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1))+
            viridis::scale_color_viridis(discrete=TRUE))
      ggsave(filename = 'figures/plot_03a_RMSE_hurdle.png', plot = rmse_plot_h, width = 6.5, height = 4, units = 'in')
   }
   
   # RMSE by year
   {
      k_fold_model <- 'rf 1'
      weekly_model <- 'avg 2'
      yearly_model <- 'avg 2'
      
      (RMSE_by_year_plot_h <- ggplot(rmse_year_hurdle %>% 
                                      dplyr::filter(shoreside == 'included' & 
                                                       ((cross_val_type == 'k-fold' & model == k_fold_model) |
                                                           (cross_val_type == 'weekly' & model == weekly_model) |
                                                           (cross_val_type == 'yearly' & model == yearly_model))) %>% 
                                      dplyr::mutate(yt = factor(n_years_train, 
                                                                levels = c('4', '12', '18'))) %>%
                                      dplyr::filter(yt %in% c('4', '12', '18')), 
                                   aes(x = year,
                                       y = nRMSE_sd, 
                                       shape = yt,
                                       color = cross_val_type,
                                       group = interaction(year, yt, cross_val_type))) +
            geom_point(position = position_dodge(width = 0.2),
                       size = 4, 
                       alpha = 0.75) + 
            theme_bw() + 
            geom_text(mapping = aes(x = year, 
                                    y = 0.78, 
                                    label = n, 
                                    color = NULL),
                      size = 2.5) + 
            # theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1))+
            coord_cartesian(ylim = c(0.78, 1.08)) +
            guides(shape = guide_legend(title = "Years of\nTraining\nData"),
                   color = guide_legend(title = 'Cross\nValidation\nType')) + 
            xlab(label = 'yearly') + 
            ylab(label = 'Normalized (SD) RMSE')+
            viridis::scale_color_viridis(discrete=TRUE))
      ggsave(filename = 'figures/plot_03b_RMSE_by_year_hurdle.png', plot = RMSE_by_year_plot_h, width = 6.5, height = 4, units = 'in')
   }
   
   # rmse by Month
   {
      (rmse_by_month_plot_h <- ggplot(rmse_month_hurdle %>% 
                                       dplyr::filter(shoreside == 'included' & 
                                                        ((cross_val_type == 'k-fold' & model == k_fold_model) |
                                                            (cross_val_type == 'weekly' & model == weekly_model) |
                                                            (cross_val_type == 'yearly' & model == yearly_model)) & 
                                                        nRMSE_sd < 2) %>% 
                                       dplyr::mutate(yt = factor(n_years_train, 
                                                                 levels = c('4', '12', '18'))) %>%
                                       dplyr::filter(yt %in% c('4', '12', '18')) %>% 
                                       dplyr::mutate(month = factor(month.abb[as.numeric(month)], levels = month.abb)), 
                                    aes(x = month,
                                        y = nRMSE_sd, 
                                        shape = yt,
                                        color = cross_val_type,
                                        group = interaction(month, yt, cross_val_type))) +
          geom_point(position = position_dodge(width = 0.2),
                     size = 4,
                     alpha = 0.75) + 
          theme_bw() + 
          geom_text(mapping = aes(x = month, 
                                  y = 0.78, 
                                  label = n, 
                                  color = NULL),
                    size = 2.5) + 
          # theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1))+
          coord_cartesian(ylim = c(0.75, 1.25)) +
          guides(shape = guide_legend(title = "Years of\nTraining\nData"),
                 color = guide_legend(title = 'Cross\nValidation\nType')) + 
          xlab(label = 'Month') + 
          ylab(label = 'Normalized (SD) RMSE')+
          viridis::scale_color_viridis(discrete=TRUE))
      ggsave(filename = 'figures/plot_03c_RMSE_by_month_hurdle.png', plot = rmse_by_month_plot_h, width = 6.5, height = 4, units = 'in')
   }
}





# Bycatch-to-Target ratio
{
   # model comparison
   {
      (btt_plot_model_comp <- btts %>% 
          dplyr::filter(shoreside == 'included') %>% 
          dplyr::mutate(Model = factor(toupper(model), levels = toupper(levels(model))),
                        yt = factor(n_years_train, levels = c('4', '12', '18'))) %>% 
          dplyr::filter(yt %in% c('4', '18')) %>% 
          ggplot(., mapping = aes(x = prop_removed,
                                  y = btt_relative,
                                  color = Model)) + 
          theme_bw() + 
          geom_line(size = 1) + 
          xlab(label = 'Hauls Removed') + 
          ylab(label = 'Chinook (n) per mt Hake (relative to all hauls)') + 
          guides(color = guide_legend(title = 'Model'),
                 linetype = guide_legend(title = 'Years of\nTraining\nData')) +
          facet_grid(cols = vars(cross_val_type)) +
          scale_y_continuous(breaks = seq(0.3,1,0.1), 
                             labels = scales::percent_format())+
          scale_x_continuous(breaks = seq(0,.1,0.025), 
                             labels = scales::percent_format())+
          viridis::scale_color_viridis(discrete=TRUE))
      
      ggsave(filename = 'figures/plot_04a_Bycatch-to-Target_hurdle_model_comparison.png', 
             plot = btt_plot_model_comp, 
             width = 6.5 * 1.5, 
             height = 3 * 1.5, units = 'in')
   }
   
   # model comparison for 1st week only 
   {
      (btt_plot_model_comp_w1 <- btts_w1 %>% 
          dplyr::filter(shoreside == 'included') %>% 
          dplyr::mutate(Model = factor(toupper(model), levels = toupper(levels(model))),
                        yt = factor(n_years_train, levels = c('4', '12', '18'))) %>% 
          dplyr::filter(yt %in% c('4', '18')) %>% 
          ggplot(., mapping = aes(x = prop_removed,
                                  y = btt_relative,
                                  color = Model)) + 
          theme_bw() + 
          geom_line(size = 1) + 
          xlab(label = 'Hauls Removed') + 
          ylab(label = 'Chinook (n) per mt Hake (relative to all hauls)') + 
          guides(color = guide_legend(title = 'Model'),
                 linetype = guide_legend(title = 'Years of\nTraining\nData')) +
          facet_grid(cols = vars(cross_val_type)) +
          scale_y_continuous(breaks = seq(0.3,1,0.1), 
                             labels = scales::percent_format())+
          scale_x_continuous(breaks = seq(0,.1,0.025), 
                             labels = scales::percent_format())+
          viridis::scale_color_viridis(discrete=TRUE))
      
      ggsave(filename = 'figures/plot_04a_Bycatch-to-Target_hurdle_model_comparison_week_1_only.png', 
             plot = btt_plot_model_comp_w1, 
             width = 4.5 * 1.5, 
             height = 3 * 1.5, units = 'in')
   }
   
   
   # Hurdle model bycatch-to-target reduction
   {
      k_fold_model <- c('rf 1', 'gbt 1')
      weekly_model <- c('avg 1', 'gbt 1')
      yearly_model <- c('avg 1', 'gbt 1')
      k_fold_model <- 'rf 1'
      weekly_model <- 'avg 2'
      yearly_model <- 'avg 2'
      k_fold_model <- weekly_model <- yearly_model <- 'gbt 1'
      
      # New facet label names for shoreside
      yit.labs <- c(paste0("Years in training = 4"),
                    paste0("Years in training = 12"))
      names(yit.labs) <- c('4', '12')
      
      (btt_plot <- btts %>% 
            dplyr::filter(shoreside == 'included') %>% 
            dplyr::mutate(yt = factor(n_years_train, levels = c('4', '12', '18'))) %>% 
            dplyr::filter( ((cross_val_type == 'yearly' & model %in% yearly_model) | 
                               (cross_val_type == 'weekly' & model %in% weekly_model) | 
                               (cross_val_type == 'k-fold' & model %in% k_fold_model)) & 
                              yt %in% c('4', '12', '18')) %>% 
            ggplot(., mapping = aes(x = prop_removed,
                                    y = btt_relative,
                                    color = cross_val_type, 
                                    linetype = yt)) + 
            theme_bw() + 
            geom_line(size = 1) + 
            xlab(label = 'Hauls Removed') + 
            ylab(label = 'Chinook (n) per mt Hake (relative to all hauls)') + 
            guides(color = guide_legend(title = 'Cross\nValidation\nType'),
                   linetype = guide_legend(title = 'Years of\nTraining\nData')) +
            # facet_grid(cols = vars(cross_val_type)) +
            scale_linetype_manual(values = c('11', '41', 'f1'),
                                  breaks = c('4', '12', '18')) +
            scale_y_continuous(breaks = seq(0.3,1,0.1), 
                               labels = scales::percent_format())+
            scale_x_continuous(breaks = seq(0,.1,0.025), 
                               labels = scales::percent_format())+
            viridis::scale_color_viridis(discrete=TRUE))
      
      ggsave(filename = 'figures/plot_04b_Bycatch-to-Target_hurdle.png', 
             plot = btt_plot, 
             width = 5, 
             height = 4, units = 'in')
      
      
      
      lm1 <- btts %>% 
         dplyr::filter(shoreside == 'included') %>% 
         dplyr::mutate(yt = factor(n_years_train, 
                                   levels = c('4', '12', '18'))) %>% 
         dplyr::filter( ((cross_val_type == 'yearly' & model == yearly_model) | 
                            (cross_val_type == 'weekly' & model == weekly_model) | 
                            (cross_val_type == 'k-fold' & model == k_fold_model)) & 
                           yt %in% c('4', '12', '18')) %>%
         lm(formula = hake_relative ~ prop_removed, data = .)
      summary(lm1)
   }
}