
# coding: utf-8

# In[3]:

from sklearn import datasets
from sklearn import preprocessing
from sklearn.datasets import make_classification
from sklearn.ensemble import (ExtraTreesClassifier, RandomTreesEmbedding, RandomForestClassifier)
from sklearn.preprocessing import OneHotEncoder
import numpy as np
import matplotlib.pyplot as plt
import pandas


# In[4]:

from sklearn.model_selection import train_test_split
from sklearn.pipeline import make_pipeline
from sklearn import linear_model, decomposition, datasets


# In[5]:

def hv_model(data, features):
    
    #import data
    raw = pandas.read_csv(data, encoding = "utf-8-sig")
    
    #select features to incorporate into model
    var_interest = raw[features]
    
    #remove missing values
    var_interest = var_interest.dropna(axis = 0)
    
    #zone is categorical, prep for encoding
    if 'zone' in var_interest:
        var_interest['zone'] = var_interest['zone'].astype(object)
        
    #predictors of interest
    X = var_interest.drop(['hv_binary'], axis = 1)
    
    #estimator vector: binary representation of hitter_val: if hv <0, 1; else, 0. 
    Y = var_interest[['hv_binary']]
    
    #Create count_type var
    X['Balls'] = X['count'].apply(lambda x: x[:1])
    X['Strikes'] = X['count'].apply(lambda x: x[-1:])
    
    conditions = [(X['Balls'] > X['Strikes']), (X['Balls'] < X['Strikes'])]
    choices = ['Behind', 'Ahead']
    X['Count_type'] = np.select(conditions, choices, default='Even')
    
    zone = X['zone'].to_frame()
    
    #Drop unneeded columns
    X = X.drop(['count','Balls','Strikes'], axis = 1)
    
    #Encode categorical predictor data
    label_encode = preprocessing.LabelEncoder()
    X_encoded = X.apply(label_encode.fit_transform)
    X_encoded = X_encoded.drop(['zone'],axis=1)
    #re-insert zone feature w/o encoding, to maintain original values
    X_encoded = pandas.merge(X_encoded, zone, left_index = True, right_index = True)
    
    #Create one_hot encoded predictor array for logistic regression
    X_hot = pandas.get_dummies(X, columns = ['pitch_type','inning_side.x','p_throws','stand','zone','Count_type'])
    
    #Split up train and test data, 80/20 split
    X_hot_train, X_hot_test, y_train, y_test = train_test_split(X_hot, Y, test_size = 0.2)
    
    #initiate ExtraTreesClassifier for feature importance estimation. 

    #From sklearn doc: This class implements a meta estimator that fits a number of randomized decision trees (a.k.a. extra-trees) 
    #on various sub-samples of the dataset and use averaging to improve the predictive accuracy and control over-fitting.

    forest = ExtraTreesClassifier(n_estimators = 33, random_state = None)
    #fit ExtraTreesClassifier to encoded categorical indicators, pitch type to binary hitter_val Y
    forest.fit(X_hot_train, y_train.values.ravel())
    
    #Logistic regression model
    logit_reg = linear_model.LogisticRegression()

    #linear regression on one_hot data, using separate training set
    model = logit_reg.fit(X_hot_train, y_train.values.ravel())
    
    print("Mean accuracy on given test data and labels:", model.score(X_hot_test, y_test))
    print("")
    print("Mean hitter_val, e.g, accuracy if all chosen as positive outcome:", y_test.mean())
    print("")
    
    #Net feature importance- NOTE that this does not discern between good or bad outcome, just importance to model

    #code borrowed and modified for visualization of ExtraTrees classifier, from sci-kit learn website: http://scikit-learn.org/stable/auto_examples/ensemble/plot_forest_importances.html#sphx-glr-auto-examples-ensemble-plot-forest-importances-py

    importances = forest.feature_importances_
    std = np.std([tree.feature_importances_ for tree in forest.estimators_],
                 axis=0)
    indices = np.argsort(importances)[::-1]

    # Print the feature ranking
    print("Feature ranking:")

    for f in range(X_hot_test.shape[1]):
        print("%d. feature %d (%f)" % (f + 1, indices[f], importances[indices[f]]))

    # Plot the feature importances of the forest
    plt.figure(figsize=(20,5))
    plt.title("Feature importances")
    plt.bar(range(X_hot_test.shape[1]), importances[indices],
           color="g", yerr=std[indices], align="center")
    plt.xticks(range(X_hot_test.shape[1]), indices)
    plt.xlim([-1, X_hot_test.shape[1]])
    plt.show()

    print("")
    
    print("Decode features from ExtraTreesClassifier:")
    for c in X_hot.columns:
        print (X_hot.columns.get_loc(c), c)
        
    print("")
    print("Results of logistic regression model")
    print("")
    print("Variable of interest, log odds, odds:")
    print("interpretation is that if any one of these variables is 'on', there is an increase (+) or decrease (-) in likelihood of a good outcome. Remember that a 'good outcome' is encoded into a modified hitter_val where '1' signifies good, aka, a negative hitter-val")

    print("")
    print pandas.DataFrame(zip(X_hot.columns, np.transpose(model.coef_), np.transpose(np.exp(model.coef_)-1)))
    
    print("Column 2 represents the % increase or decrease in likelihood of a positive outcome for *any one of these feature's presence*, excluding interactions")
    print("")
    print("Some examples:")
    print("-pitch type AB shows a 21% decrease in a positive outcome")
    print("-pitch type FO increases the odds of a positive outcome by 77%")
    print("-a 'behind' count, e.g., 0-1, 0-2, demonstrates an 18% increase in positive outcome vs almost no net effect from an 'ahead' count, 2.4%")
  


# In[6]:

hv_model("data_export_hv_binary_distinct_300_days.csv", ['pitch_type','zone','inning_side.x','p_throws','stand','count','hv_binary'])


# In[ ]:

# show plots in the notebook
# inspiration from http://nbviewer.jupyter.org/gist/justmarkham/6d5c061ca5aee67c4316471f8c2ae976
get_ipython().magic(u'matplotlib inline')


# In[ ]:

X_encoded.pitch_type.hist(bins=11)
plt.title('Pitch Type Histogram')
plt.xlabel('Pitch Type')
plt.ylabel('Frequency')

#interesting that some pitch types are very rare compared to others


# In[ ]:

X_encoded.zone.hist(bins=14)
plt.title('Zone Histogram')
plt.xlabel('Zone')
plt.ylabel('Frequency')

#Note the mysteriously missing zone 10

