# Process-aware Bayesian Networks for Sequential Inference and Querying on Event Logs

Implementation of Process-aware Bayesian Networks for Sequential Inference and Querying on Event Logs

## Framework

We provide a framework to generate Process-aware Bayesian Network structures from event log data. A given dataset with case identifiers, ordered activities and potentially additional case or event attributes is transformed to fit the model assumptions of Bayesian Networks for the underlying model structures to be free of cycles.

For that, the activity information in the trace data is annotated with the respective place information. For the generated structure, we preprocess the event log data accordingly to ensure that parameter learning for the designed approach is possible. Eventually, unknown trace information can be predicted via Bayesian Inference by querying the Process-aware Bayesian Network with a given set of evidence about known prefixes, suffixes or evidence about desired outcomes of traces.

## Usage

-   Load a desired event log data set. We provide the analyzed datasets in the `data/` folder (Helpdesk[[1]](#1), BPI2012[[2]](#2) (with variations: Full, Sub and WC), BPI2020 - Travel Permits[[3]](#3) and Road Traffic Fines[[4]](#4)).
-   Adjust the predefined model setup and constants if necessary.
-   Generate the Process-aware network structure with the provided set of functions.
-   Preprocess the data with the provided set of functions.
-   Query the Process-aware Bayesian Network with the framework provided by the `bnlearn` package we rely on in our implementations. We provide exemplary queries that can be used and adapted by practitioners.

## Scripts

We provide multiple scripts for execution. The `2x_model_training_x.R` files are for model training for each application (Next Activity/Remaining Trace Prediction, Overall Case Duration Class Prediction or the Process Query System). Files (`3x_evaluation_x.R`) are for evaluating the three applications. `10_preliminaries.R` loads the required extensions as well as the `custom_BN_functions.R`.

## Dataset References

<a id="1">[1]</a> Verenich, Ilya (2016). Helpdesk. Mendeley (Link: <https://doi.org/10.17632/39BP3VV62T.1>).

<a id="2">[2]</a> van Dongen, Boudewijn (2012). BPI Challenge 2012. Eindhoven University of Technology (Link: <https://data.4tu.nl/articles/_/12689204/1>)

<a id="3">[3]</a> van Dongen, Boudewijn (2020). BPI Challenge 2020: Travel Permit Data. 4TU.Centre for Research Data (Link: <https://data.4tu.nl/articles/dataset/BPI_Challenge_2020_Travel_Permit_Data/12718178/1>).

<a id="4">[4]</a> de Leoni, M. and Mannhardt, Felix (2015). Road Traffic Fine Management Process. Eindhoven University of Technology (Link: <https://data.4tu.nl/articles/_/12683249/1>).
