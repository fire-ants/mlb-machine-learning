### mlb-machine-learning
Building a classifier using logistic regression to produce optimized pitching recommendations

## Core script description

```
def hv_model(features):

"""
Function fits a model to per-pitch features of interest and hitterval label using logistic regression. The data source is hard coded into the function. Currently, implemented as a read from a csv file, but to be implemented reading from a SQL database instead shortly.
    
    Arguments:
        features: table containing features of interest and Hitter-Val (HVAL) labels per event
    
    Returns:    
        dict of recommendations per hitter and subset by pitcher handedness, i.e., recommendations for right handed and left handed pitchers. Model information (# of data points, accuracy) is also included, and results are returned in the form of an odds ratio compared to "chance" baseline of predicting success
"""
```
## hv_model script flow

1. Place (currently hard-coded) list of batters of interest in list

2. Initialize handedness list and results list for RHP and LHP

3. For loop, main script: Per batter in (1) and events subset by pitcher handedness, complete the following:

  - Organize "features" table into actual feature space, "X" and label space (e.g., HitterVal per event), "Y"

  - Produce "count type" feature and drop "count" feature. Note that this is only done if count is included in variables of interest, currently not.

  - One-hot encode feature space of interest. Currently, combined feature: pitch type & zone, aka 'ptz'. One hot encoding stretches d features with c_i classes per feature d_i into $\sum_{i=1}^{n}c_i$ features, each encoded via a 1, "on" or 0, "off". Only one feature will be on per event. 
  
  - Initialize logistic regression model from sklearn package and implement with one-hot encoded features in X and HVAL labels Y
  
  - Place results in new dataframe "top5", sort by abs value of regression coefficient (i.e., relative importance in model) and place top 5 results in new table
  
  - Convert logit reg coefficients from top5 into odds ratios, e.g., odds of success using this feature instance (pitch/zone combo)
  
  - Format codified results into legible "recommmendation statements" known as **_Fire Recommendations_** (TM pending)
  
  - Insert results into Right-handed and Left-handed results dicts, and push these dicts to S3 bucket to be read into Pitcher's Friend application

## Open issues

1. Update function arguments to explicitly include feature table X, label vector Y, and batters list. This will replace hard coding of this info as currently implemented, and features of interest can be established via input argument X rather than explicitly defined.

2. Set up config file to call SQL database (once running as service in PCF)
