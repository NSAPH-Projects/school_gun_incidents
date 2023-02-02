**Project**: Firearm-dealer-to-school proximity and school gun incidents

**Authors**: Falco Bargagli Stoffi, Michelle Qin

**Created**: June 2022

**Overview**: Causal inference to estimate causal and associative effects of average firearm-dealer-to-school proximity on occurrence of a school gun incident within a school-containing census tract in the United States.

**Code**:
1. [make_datasets.R](https://github.com/NSAPH/firearm_store_proximity_school_shootings/blob/main/code/make_datasets.R) combines our various data sources, excludes certain outlier rows, and creates one csv for our analyses: `all_tracts_2020_subset_vars_revised.csv`.
2. [helper_functions.R](https://github.com/NSAPH/firearm_store_proximity_school_shootings/blob/main/lib/helper_functions.R) contains many functions, such as a function to read in the columns we want from `all_tracts_2020_subset_vars_revised.csv`, which are used by most of the following files.
3. [distributions_of_variables.R](https://github.com/NSAPH/firearm_store_proximity_school_shootings/blob/main/code/distributions_of_variables.R) creates Table 1 in our manuscript, i.e., summary statistics of our variables.
4. [naive_logistic_regression.R](https://github.com/NSAPH/firearm_store_proximity_school_shootings/blob/main/code/naive_logistic_regression.R) performs our associational analyses, i.e., logistic and binomial regression.
5. [functions_using_gps.R](https://github.com/NSAPH/firearm_store_proximity_school_shootings/blob/main/lib/functions_using_gps.R) requires the [CausalGPS package](https://github.com/cran/CausalGPS) to be installed and contains functions used by [continuous_treatment_causal_analysis.R](https://github.com/NSAPH/firearm_store_proximity_school_shootings/blob/main/code/continuous_treatment_causal_analysis.R) and [matching_without_covariates_for_evalue.R](https://github.com/NSAPH/firearm_store_proximity_school_shootings/blob/main/code/matching_without_covariates_for_evalue.R).
6. [continuous_treatment_causal_analysis.R](https://github.com/NSAPH/firearm_store_proximity_school_shootings/blob/main/code/continuous_treatment_causal_analysis.R) performs our main causal analysis.
7. [functions_for_factual_vs_counterfactual.R](https://github.com/NSAPH/firearm_store_proximity_school_shootings/blob/main/lib/functions_for_factual_vs_counterfactual.R) contains several functions used by [factual_vs_counterfactual_causal_continuous_exposure.R](https://github.com/NSAPH/firearm_store_proximity_school_shootings/blob/main/code/factual_vs_counterfactual_causal_continuous_exposure.R).
8. [factual_vs_counterfactual_causal_continuous_exposure.R](https://github.com/NSAPH/firearm_store_proximity_school_shootings/blob/main/code/factual_vs_counterfactual_causal_continuous_exposure.R) performs our events avoided and people affected calculations.
9. [matching_without_covariates_for_evalue.R](https://github.com/NSAPH/firearm_store_proximity_school_shootings/blob/main/code/matching_without_covariates_for_evalue.R) repeats our main analysis while excluding classes of covariates, so that the results may be inputted to [the E-value calculator](https://www.evalue-calculator.com/evalue/).

**Contact Us**: michelleqin@college.harvard.edu, fbargaglistoffi@hsph.harvard.edu
