# Ecological Impacts Model for the SEQ Koala Offsets Project

This contains the code for the ecological impact models that does the following:

- Estimates the proportion woody vegetation in 1999 that has at least one clearing event between 1999 and 2016 (measured at a 25m resolution) within 1 ha land-use grids as a function of observed land-use transitions between 1999 and 2016.

## Data

Data should be located in /data. Currently available for project members within UQ at: R:\KOALA2018-A0206\06 Ecological Data\data_for_ecological_model. otherwise available on request from j.rhodes@uq.edu.au.

## Methods

The aim of this analysis is to estimate the proportion of woody vegetation cleared in the 1 ha grid cells as a
function of the starting land-use in 1999 and the land-use transitions between 1999 to 2016. To construct the data
sets required for the estimation we conducted the following analysis:

1) Using SLATS foliage projective cover (FPC) data from 1999 (http://qldspatial.information.qld.gov.au/catalogue/),
we identified the woody vegetation (i.e., FPC > 0) at resolution of 25 m.

2) Using SLATS clearing data between 1999 and 2016 (http://qldspatial.information.qld.gov.au/catalogue/), we
identified those cells that had been cleared at least once between 1999 and 2016.

3) Within each 1 ha grid cell used for the land-use change model (see https://github.com/KoalaOffsets/LandUseModel)
we identified the number of 25 m cells that were woody vegetation in 1999 and how many of these were cleared at least
once between 1999 and 2016.

4) We then fitted a logistic regression to these data (at the 1 ha resolution) as a function of the starting land-use
in 1999 and land-use transitions between 1999 and 2016 to estimate the probability of clearing woody vegetation. Note
that the clearing data are zero-inflated but zero-inflated or zero-adjusted binomial models failed to converge due to
parameter identifiability problems. However, since the data are a census (rather than a sample), we do not conduct
any statistical tests and therefore the impacts of failing to meet distributional assumptions on parameter standard
error estimates are not important and expected clearing probability estimates should be affected much
less.                        
