# South East Queensland Koala Offsets Assessment Model

## Models

This contains the R code for a spatial simulation model to assess offset policies for koalas in South East Queensland, and includes:

- A land-use change model, accuracy assessment, and forward simulations (koala_ca_select_fit_data.r, koala_ca_model_fitting.r, koala_ca_accuracy_assesment.r)
- A koala habitat clearing model (clearing_model.r)
- Forward simulations of the land-use change model with alternative development scenarios and regulation and offset policy setting rules (koala_ca_forward.r)  
- functions used in the models (functions.r)
- code to create figures and tables (figures_code.r)

## Data

- Input data are available from: https://doi.org/10.48610/1c1164e. 

## Instructions

- copy code to working directory
- create the following directories in the working directory: /input, /sim_results, /sim_results/accuracy_assessment, /figures
- copy input data to the /input directory
- parameterise the land-use change model by running in this order: koala_ca_select_fit_data.r (data preparation), koala_ca_model_fitting.r (model fitting), koala_ca_accuracy_assesment.r (accrancy assessment)
- parameterise the habitat clearing model by running: clearing_model.r
- assess offset policies through forward simulations by running: koala_ca_forward.r
- generate figures and tables by running: figures_code.r
