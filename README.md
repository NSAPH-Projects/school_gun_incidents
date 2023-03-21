**Project**: Firearm-dealer-to-school proximity and school gun incidents

**Authors**: Falco Bargagli Stoffi, Michelle Qin

**Contributors**: Michelle Audirac, Nishtha Sardana

**Created**: June 2022

**Overview**: Causal inference to estimate causal and associative effects of average firearm-dealer-to-school proximity on occurrence of a school gun incident within a school-containing census tract in the United States.

**Code**:
1. [code/make_datasets.R](./code/make_datasets.R): combines our various data sources, excludes certain outlier rows, and creates one csv for our analyses: `data/intermediate/all_tracts_2020_subset_vars_revised.csv`.
2. [lib/functions_to_load_data.R](./lib/functions_to_load_data.R): contains multiple functions, such as a function to read in the columns we want from `all_tracts_2020_subset_vars_revised.csv`, which are used by most of the following files.
3. [notebooks/distributions_of_variables.Rmd](./notebooks/_knit/distributions_of_variables.md): explores summary statistics of our variables (Table 1 in our manuscript).
4.  [lib/functions_to_get_associational_models.R](./lib/functions_to_get_associational_models.R): contains 1 function (glm), to run our associational logistic and binomial regressions; called by [code/associational_analyses.R](./code/associational_analyses.R)
5. [code/associational_analyses.R](./code/associational_analyses.R): performs our associational analyses, i.e., logistic and binomial regression.
6. [code/associational_analyses.sh](./code/associational_analyses.sh): bash script used to run [code/associational_analyses.R].
7. [lib/functions_using_gps.R](./lib/functions_using_gps.R): requires the [CausalGPS package](https://github.com/cran/CausalGPS) to be installed and contains functions used by [code/continuous_treatment_causal_analyses.R](./code/continuous_treatment_causal_analyses.R) and [code/sensitivity_analyses/matching_without_covariates_for_evalue.R](./code/sensitivity_analyses/matching_without_covariates_for_evalue.R).
8. [lib/functions_to_measure_covariate_balance.R](./lib/functions_to_measure_covariate_balance.R): some functions to measure correlation between covariates and exposure (covariate balance); used by [code/continuous_treatment_causal_analyses.R](./code/continuous_treatment_causal_analyses.R) and [code/sensitivity_analyses/matching_without_covariates_for_evalue.R](./code/sensitivity_analyses/matching_without_covariates_for_evalue.R).
9. [code/continuous_treatment_causal_analyses.R](./code/continuous_treatment_causal_analyses.R): performs our main causal analysis.
10. [code/continuous_treatment_causal_analyses.sh](./code/continuous_treatment_causal_analyses.sh): bash script used to run [code/continuous_treatment_causal_analyses.R](./code/continuous_treatment_causal_analyses.R).
11. [code/sensitivity_analyses/matching_without_covariates_for_evalue.R](./code/matching_without_covariates_for_evalue.R): repeats our main analysis while excluding classes of covariates, so that the results may be inputted to [the E-value calculator](https://www.evalue-calculator.com/evalue/).
12. [code/sensitivity_analyses/matching_without_covariates_for_evalue.sh](./code/sensitivity_analyses/matching_without_covariates_for_evalue.sh): bash script used to run [code/sensitivity_analyses/matching_without_covariates_for_evalue.R](./code/sensitivity_analyses/matching_without_covariates_for_evalue.R).
13. [code/sensitivity_analyses/gee_model.R](./code/gee_model.R): logistic regression using states as GEE clusters; used as a sensitivity analysis to check for spatial confounding.

**Contact Us**: michelleqin@college.harvard.edu, fbargaglistoffi@hsph.harvard.edu
