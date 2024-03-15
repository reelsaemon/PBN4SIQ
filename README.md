# PBN4SIQ
Implementation of Process-aware Bayesian Networks for Sequential Inference and Querying on Event Logs

## Framework
We provide a framework to generate Process-aware Bayesian Network structures from event log data. A given dataset with case identifiers, ordered activities and potentially additional case or event attributes is transformed to fit the model assumptions of Bayesian Networks for the underlying model structures to be free of cycles.

For that, the activity information in the trace data is annotated with the respective place information. For the generated structure, we preprocess the event log data accordingly to ensure that parameter learning for the designed approach is possible. Eventually, unknown trace information can be predicted via Bayesian Inference by querying the Process-aware Bayesian Network with a given set of evidence about known prefixes, suffixes or evidence about desired outcomes of traces.

## Usage
- Load a desired event log data set. We provide an exemplary synthetic event log (`data/sim_event_log.csv`).
- Adjust the predefined model setup and constants if necessary.
- Generate the Process-aware network structure with the provided set of functions.
- Preprocess the data with the provided set of functions.
- Query the Process-aware Bayesian Network with the framework provided by the `bnlearn` package we rely on in our implementations. We provide exemplary queries that can be used and adapted by practitioners.

## Scripts
We provide two scripts for execution. One for Next Activity and Remaining Trace Prediction (`evaluation_suffix.R`) and one for using the Process-aware Bayesian Network as a Process Query System (`evaluation_query.R`).
