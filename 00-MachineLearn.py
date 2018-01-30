
# coding: utf-8

# In[1]:


from sklearn import datasets
from sklearn import preprocessing
from sklearn.datasets import make_classification
from sklearn.ensemble import (ExtraTreesClassifier, RandomTreesEmbedding, RandomForestClassifier)
from sklearn.preprocessing import OneHotEncoder
import numpy as np
import matplotlib.pyplot as plt
import pandas

from sklearn.model_selection import train_test_split
from sklearn.pipeline import make_pipeline
from sklearn import linear_model, decomposition, datasets

import sympy
from sympy.solvers import solve
from sympy import Symbol

import requests # importing the requests library
import json
import os


# In[83]:


os.chdir("/db")


# In[2]:


#os.getcwd()


# In[ ]:


#[514888,453568,457759,519317,458015,547180,641355,592450,545361,457705,502671,518626,502517,518934,471865,592178,519346]


# In[20]:


def hv_model(features):
    
    #"rawdata_ML.csv"
    
    #import data, remove rows with NA values
    raw = pandas.read_csv("rawdata_ML.csv", encoding = "utf-8-sig").dropna(axis=0)

    #identify unique batter ids in dataset
    Batters = raw.batter.unique()
    
    #Note: opportunity to pass in argument of batters in the future
    Batters_list = [514888,453568,457759,519317,458015,547180,641355,592450,545361,457705,502671,518626,502517,518934,471865,592178,519346]
    
    #identify pitcher handedness. Like Jason has yet to see my ambidextrosity, we have yet to see anything more than "L" or "R", but we prefer this method to hard coding :P
    P_throws = raw.p_throws.unique()
    
    #for each batter ID, produce two model results- against left handed pitchers and right handed pitchers
    RHPfindingslist = list()
    LHPfindingslist = list()
        
    #generate results for each batter in list
    for batter_id in Batters_list:
        
        #Separating right handed pitcher results from LHP results
        for hand in P_throws: 
            records = raw[(raw.batter == batter_id) & (raw.p_throws == hand)]
            num_events = len(records.index)
    
            #select features to incorporate into model based on input argument
            var_interest = records[features]

            #zone is categorical, prep for encoding
            if 'zone' in var_interest:
                var_interest.loc[:,'zone'] = var_interest[:,'zone'].astype(object)   
            else:
                pass
            
             #ptz variable (pitch_zone combo) is categorical, prep for encoding
            if 'ptz' in var_interest:
                var_interest.ptz = var_interest.ptz.astype(object)   
            else:
                pass
            
            #label space Y: binary representation of hitter_val: if hv <0, 1; else, 0. 
            Y = var_interest[['hv_binary']]
            
            #feature space X: drop labels in 'hv_binary'
            X = var_interest.drop(['hv_binary'], axis = 1)

            #Create count_type var (ahead, behind, even) if including in model
            if 'count' in var_interest:
                X['Balls'] = X['count'].apply(lambda x: x[:1])
                X['Strikes'] = X['count'].apply(lambda x: x[-1:])

                conditions = [(X['Balls'] > X['Strikes']), (X['Balls'] < X['Strikes'])]
                choices = ['Behind', 'Ahead']
                X['Count_type'] = np.select(conditions, choices, default='Even')

                zone = X['zone'].to_frame()

                #Drop unneeded columns
                X = X.drop(['count','Balls','Strikes'], axis = 1)

            else:
                pass


            #Create one_hot encoded predictor array for logistic regression
            X_hot = pandas.get_dummies(X)
            
            #note that we do not need to create train/test splits for this case, as we are interested in the coefficientso of the model, not predicting non-classified results
            #the code to do so follows anyway
            
            #Split up train and test data, 80/20 split
            #X_hot_train, X_hot_test, y_train, y_test = train_test_split(X_hot, Y, test_size = 0.3)

            #initiate ExtraTreesClassifier for feature importance estimation. 

            #From sklearn doc: This class implements a meta estimator that fits a number of randomized decision trees (a.k.a. extra-trees) 
            #on various sub-samples of the dataset and use averaging to improve the predictive accuracy and control over-fitting.
            
            #forest = ExtraTreesClassifier(n_estimators = len(X_hot.columns), random_state = None)
            
            #fit ExtraTreesClassifier to encoded categorical indicators, pitch type to binary hitter_val Y
            #forest.fit(X_hot, Y.values.ravel())
            
            #Net feature importance- NOTE that this does not discern between good or bad outcome, just importance to model
            #code borrowed and modified for visualization of ExtraTrees classifier, from sci-kit learn website: http://scikit-learn.org/stable/auto_examples/ensemble/plot_forest_importances.html#sphx-glr-auto-examples-ensemble-plot-forest-importances-py
            
            #importances = forest.feature_importances_
            #std = np.std([tree.feature_importances_ for tree in forest.estimators_], axis=0)
            #indices = np.argsort(importances)[::-1]

            # Print the feature importance ranking, from ExtraTreesClassifier
            #print("Feature ranking:")

            #for f in range(X_hot_test.shape[1]):
            #    print("%d. feature %d (%f)" % (f + 1, indices[f], importances[indices[f]]))

            # Plot the feature importances of the forest
            #plt.figure(figsize=(10,5))
            #plt.title("Feature importances")
            #plt.bar(range(X_hot_test.shape[1]), importances[indices], color="g", yerr=std[indices], align="center")
            #plt.xticks(range(X_hot_test.shape[1]), indices)
            #plt.xlim([-1, X_hot_test.shape[1]])
            #plt.show()
            
            #print("Decode features from ExtraTreesClassifier:")
            #for c in X_hot.columns:
             #   print (X_hot.columns.get_loc(c), c)

                
                
            #Logistic regression model, initialize
            logit_reg = linear_model.LogisticRegression()

            #linear regression on one-hot encoded data X and Y HVAL labels
            model = logit_reg.fit(X_hot, Y.values.ravel())


            #Average success, from pitcher's perspective
            avg_success=Y.mean().values[0]
            #print avg_success
            #print Y.mean()[0], Y.mean()[1]
        
            #Baseline pitcher success rate
        
            #print "Random %s-handed pitcher's baseline success ratio against hitter:" % (hand, , Y.mean()
            if hand == 'R':
                    RHPfindingslist.append(("Based on the last 90 days' worth of pitches against this batter, %s-handed pitchers have a %s success rate." % (hand,"{0:.0f}%".format(avg_success* 100))))
            elif hand == 'L':
                    LHPfindingslist.append(("Based on the last 90 days' worth of pitches against this batter, %s-handed pitchers have a %s success rate." % (hand,"{0:.0f}%".format(avg_success* 100))))
            else:
                pass
         
        
        
            #logistic regression results
            Results = pandas.DataFrame(list(zip(X_hot.columns, np.transpose(model.coef_), np.transpose(np.exp(model.coef_)), abs(np.transpose(np.exp(model.coef_)-1)))))
            
            Results.columns = ['Suggestion', 'LR_coeff/Log_Odds', 'Odds_Ratio', 'abs_pi']
    
            #sorted results
            Results = Results.sort_values(by='abs_pi', ascending = False)
            Top_5 = Results[['Suggestion','Odds_Ratio']][:5]
            Top_5.Odds_Ratio = Top_5.Odds_Ratio.astype(float)
            
            
            
            #below loop is to translate logistic regression coefficients into odds ratio, e.g., improved odds of success of choosing this feature
            x = Symbol('x')
            
            for index,row in Top_5.iterrows():
              
                Top_5.loc[index,'New_Odds'] = solve(Top_5.loc[index,'Odds_Ratio']-((x/(1-x))/(Y.mean()/(1-Y.mean()))), x)
            
            #string formatting
            Top_5['New_Odds']= Top_5['New_Odds'].str.get(0)
            #print Top_5['New_Odds'], Top_5['Odds_Ratio']
                
            
            
            #Creating new series pt:pitch_type and zc:zone_catcher, e.g., zone from catcher's perspective
            #Credit to Benita!
            s= Top_5['Suggestion'].apply(lambda x: x.split('_'))
            Top_5['pt'] = s.apply(lambda x: x[1])
            Top_5['zc'] = s.apply(lambda x: x[2])
            
            
            
            #Translation of codes to descriptive terms, pitch type and zone
            
            def applyFunc(s):
                if s == 'FF':
                    return 'four-seam fastball'
                elif s == 'SI':
                    return 'sinker'
                elif s == 'SL':
                    return 'slider'
                elif s == 'KN':
                    return 'knuckleball'
                elif s == 'CH':
                    return 'change-up'
                elif s == 'CU':
                    return 'curve-ball'
                elif s == 'FT':
                    return 'two-seam fastball'
                return ''

            #create series of descriptive pitch type labels
            Top_5['pd'] = Top_5['pt'].apply(applyFunc)

            def applyFunc2(s):
                if s == '1':
                    return 'to the top right'
                elif s == '2':
                    return 'top center'
                elif s == '3':
                    return 'to the top left'
                elif s == '4':
                    return 'center right'
                elif s == '5':
                    return 'down the middle'
                elif s == '6':
                    return 'center left'
                elif s == '7':
                    return 'to the bottom right'
                elif s == '8':
                    return 'bottom center'
                elif s == '9':
                    return 'to the bottom left'
                elif s == '10':
                    return 'into outer space'
                elif s == '11':
                    return 'upper right of strike zone'
                elif s == '12':
                    return 'upper left of strike zone'
                elif s == '13':
                    return 'bottom right of strike zone'
                elif s == '14':
                    return 'bottom left of strike zone'
                return ''

            #create series to describe zones, mirroring to be seen from pitcher's perspective
            Top_5['zd'] = Top_5['zc'].apply(applyFunc2)    
                 
            
            
            #Print the results!
            
            print("Batter ID: %s" % (batter_id))
            print("")
            print("Based on the last 90 days' worth of pitches against this batter, %s-handed pitchers have a %s success rate." % (hand,"{0:.0f}%".format(avg_success* 100)))
            print ("")
            
            for index,row in Top_5.iterrows():
                print("Throw a %s %s for a success rate of %s." % (Top_5.loc[index,'pd'], Top_5.loc[index, 'zd'], "{0:.0f}%".format(Top_5.loc[index,'New_Odds'] * 100)))
                
                if hand == 'R':
                    RHPfindingslist.append("Throw a %s %s for a success rate of %s." % (Top_5.loc[index,'pd'], Top_5.loc[index, 'zd'], "{0:.0f}%".format(Top_5.loc[index,'New_Odds'] * 100)))
                    
                elif hand == 'L':
                    LHPfindingslist.append("Throw a %s %s for a success rate of %s." % (Top_5.loc[index,'pd'], Top_5.loc[index, 'zd'], "{0:.0f}%".format(Top_5.loc[index,'New_Odds'] * 100)))
                else:
                    pass
            
            print("")
            print("Note: Model Accuracy, based on %s pitches:" % num_events, model.score(X_hot, Y))
            print ("")
            print ("")
           
            #print model.decision_function(X_hot)
        
            #payload = {'findings': ['Throw a curve ball to the top right', 'Throw a curve ball down the middle']}
            #findingsList = ('Throw a curve ball to the top right', 'Throw a curve ball down the middle')
        
   
        
        
        #load data to object store
        
             #Note- this is dictionary containing findings results per pitcher. 
        findingsDict = {'leftyFindings': LHPfindingslist, 'rightyFindings': RHPfindingslist}
        #print(findingsDict.items())
        
        #payload = json.dumps(findingsDict)
        #print(payload)
        
        # api-endpoint
        URL = 'http://mlb-player-api.cfapps.io/player/%d/insight' % (batter_id)
        try:
            r = requests.post(url = URL, data = findingsDict)
            print(r.status_code)
            r.raise_for_status()
        except requests.exceptions.HTTPError as err:
            print(err)

        print ("")


# In[21]:


hv_model(['ptz','hv_binary'])

