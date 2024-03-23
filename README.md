**Project:** Distance Between Schools and Gun Retailers and Odds of School Gun Incidents in the United States

**Authors:** Falco Bargagli Stoffi, Michelle Qin

**Contributors:** Michelle Audirac, Nishtha Sardana

**Created:** June 2022

**Overview:** Causal inference to estimate causal and associative effects of average firearm-dealer-to-school proximity on occurrence of a school gun incident within a school-containing census tract in the United States.

### How to Run the Code in This Repository

1. Request `data_input_private.zip` from the authors. Unzip in the [data/input/private/](./data/input/private/) folder.
2. Generate the dataset used for analysis and Table 1 by running:
   ```
   cd code
   Rscript --vanilla make_datasets.R
   Rscript --vanilla make_table1.R
   ```
3. Install the [CausalGPS](https://github.com/NSAPH-Software/CausalGPS) R package (use version 0.4.0 or higher).
4. Decide whether to run the GPS matching model that uses Generalized Estimating Equations (GEE) for the outcome model.
   - Set `run_gee_model = F` or `run_gee_model = T` in `causal_analyses.R` accordingly.
   - GEE models may take up to 96 GB to run (and up to 250 GB for the "trim 1/99" sensitivity analysis).
5. Perform all main analyses by running:

   ```
   bash associational_analyses.sh
   bash causal_analyses.sh
   ```

6. Consolidate and visualize the results by running:
   ```
   Rscript --vanilla read_and_plot_covariate_balance.R
   Rscript --vanilla read_all_model_results.R
   Rscript --vanilla plot_main_results_as_odds_ratio.R
   ```
7. Perform additional sensitivity analyses by running (i) commented lines in `associational_analyses.sh` and `causal_analyses.sh` and (ii) code in the `supplementary/` folder.
   - When selecting which code to run in `supplementary/`, you will again need to decide whether to run GEE models (for GPS matching models and for the state-level spatial confounding robustness check, denoted `gee_associational_model`).
   - If you choose to run the batch scripts in this repo, first create a `logs/` folder within `supplementary/` to save the output.

### Contact Us

Michelle Qin (mqin8 [at] jh.edu), Falco Bargagli Stoffi (fbargaglistoffi [at] hsph.harvard.edu)
