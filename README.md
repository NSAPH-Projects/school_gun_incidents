**Project**: Firearm-dealer-to-school proximity and school gun incidents

**Authors**: Falco Bargagli Stoffi, Michelle Qin

**Contributors**: Michelle Audirauc, Nishta Sinha

**Created**: June 2022

**Overview**: Causal inference to estimate causal and associative effects of average firearm-dealer-to-school proximity on occurrence of a school gun incident within a school-containing census tract in the United States.

**Code**:
1. [code/make_datasets.R](./code/make_datasets.R): combines our various data sources, excludes certain outlier rows, and creates one csv for our analyses: `data/intermediate/all_tracts_2020_subset_vars_revised.csv`.
2. [lib/helper_functions.R](./lib/helper_functions.R): contains many functions, such as a function to read in the columns we want from `all_tracts_2020_subset_vars_revised.csv`, which are used by most of the following files.
3. [code/distributions_of_variables.R](./code/distributions_of_variables.R): creates Table 1 in our manuscript, i.e., summary statistics of our variables.
4. [code/associational_analyses.R](./code/associational_analyses.R): performs our associational analyses, i.e., logistic and binomial regression.
5. [code/associational_analyses.sh](./code/associational_analyses.sh): bash script used to run [code/associational_analyses.R].
6. [lib/functions_using_gps.R](./lib/functions_using_gps.R): requires the [CausalGPS package](https://github.com/cran/CausalGPS) to be installed and contains functions used by [code/continuous_treatment_causal_analyses.R](./code/continuous_treatment_causal_analyses.R) and [code/matching_without_covariates_for_evalue.R](./code/matching_without_covariates_for_evalue.R).
7. [code/continuous_treatment_causal_analyses.R](./code/continuous_treatment_causal_analyses.R): performs our main causal analysis.
8. [code/continuous_treatment_causal_analyses.sh](./code/continuous_treatment_causal_analyses.sh): bash script used to run [code/continuous_treatment_causal_analyses.R](./code/continuous_treatment_causal_analyses.R)
9. [code/matching_without_covariates_for_evalue.R](./code/matching_without_covariates_for_evalue.R): repeats our main analysis while excluding classes of covariates, so that the results may be inputted to [the E-value calculator](https://www.evalue-calculator.com/evalue/).
10: [code/gee_model](./code/gee_model): logistic regression using states as GEE clusters; used as a sensitivity analysis to check for spatial confounding.

**Contact Us**: michelleqin@college.harvard.edu, fbargaglistoffi@hsph.harvard.edu
