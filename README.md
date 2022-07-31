# South East Queensland Koala Offsets Assessment Model

## Models

This contains the R code for a spatial simulation model to assess offset policies for koalas in South East Queensland, and includes:

- A land-use change model, accuracy assessment, and forward simulations (koala_ca_select_fit_data.r, koala_ca_model_fitting.r, koala_ca_accuracy_assesment.r)
- A koala habitat clearing model (clearing_model.r)
- Forward simulations of the land-use change model with alterntive development scenarios and regulation and offset policy setting rules (koala_ca_forward.r)  
- functions used in the models (functions.r)
- code to create figuresa and tables (figures_code.r)

## Data

- Input data can be obtained from Jonathan Rhodes (j.rhodes@uq.edu.au). Will eventually be archived in an open data repository.  

## Instructions

- parameterise the land-use change model by running in this order: koala_ca_select_fit_data.r (data preparation), koala_ca_model_fitting.r (model fitting), koala_ca_accuracy_assesment.r (accrancy assessment)
- parameterise the habitat clearing model running: clearing_model.r
- assess offset policies through forward simulations by running: koala_ca_forward.r
- generate figurea and table by running: figures_code.r
