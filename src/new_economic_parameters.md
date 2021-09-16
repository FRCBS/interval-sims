## R Markdown

Try the economic effect calculations with the new parameters values.

    new_fixed_parameters <- list(Pm=2.287, Pd=20.342, F=1, Fn=0.1066, rloss=0.167)
    #new_fixed_parameters <- list(Pm=2.287, Pd=19.96, F=1, Fn=0.1066, rloss=0.167)
    #new_fixed_parameters <- list(Pm=2.287, Pd=18.08, F=1, Fn=0.1066, rloss=0)

    new_parameters <- c(data_parameters, new_fixed_parameters)
    if (compute) {
      all_results <- process_all_data(all_ids, parameters=new_parameters)
      write_tsv(all_results, file=all_results_filename)
    } else {
      all_results <- read_tsv(file=all_results_filename)  
    }

    ## Rows: 492 Columns: 4

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: "\t"
    ## chr (3): Id, variable, type
    ## dbl (1): value

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

The cost surface below is for the random forest on eProgesa data.

    cost_function <- do.call(cost_func_factory, new_parameters[c("Pm", "Pd", "F", "Fn", "rloss", "d")])
    g <- draw_surface(results=all_results %>% filter(Id == "progesa-both-rf"), cost_function=cost_function)

    ## # A tibble: 2 × 11
    ##   Id      month E_value   E_low E_high a_value a_low a_high q_value q_low q_high
    ##   <chr>   <int>   <dbl>   <dbl>  <dbl>   <dbl> <dbl>  <dbl>   <dbl> <dbl>  <dbl>
    ## 1 proges…     6 -0.146  -0.159  -0.131    1.10  1.10   1.10   0.511 0.492  0.530
    ## 2 proges…    12  0.0966  0.0831  0.110    1.15  1.15   1.15   0.307 0.289  0.324

    if (save_figs) {
      filename <- sprintf("%s/cme_cost_surface_werr.pdf", fig_path)
      ggsave(filename=filename,  width = 180,  height = 120, units="mm", dpi=600, scale=1.0, device=cairo_pdf)
    }
    g

![](new_economic_parameters_files/figure-markdown_strict/Cost%20surface-1.png)

    # finngen-male-dlmm  E12 a12 and f1 values are for some reason NA
    #df <- all_results %>% 
    #  replace_na(list(value=0))  

    res <- create_performance_forest_plot(all_results)
    g <- res$g
    df <- res$df
    if (save_figs) {
      filename <- sprintf("%s/performance-forest-plot.pdf", fig_path)
      ggsave(filename=filename, #  title="Performance forest plot", 
             plot=g, dpi=600, units="mm", width=180, device=cairo_pdf)
    }
    g

    ## Warning: Removed 1 rows containing missing values (geom_pointrangeh).

![](new_economic_parameters_files/figure-markdown_strict/Performance%20plot-1.png)
