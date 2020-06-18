import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import sklearn.model_selection
import statsmodels.api as sm
import numpy as np

data = pd.read_csv('insurance.csv')
# Plot the correlations between each pairs
# Convert our categorical features to numerical ones.
data['sex'] = data['sex'].astype('category').cat.codes
data['smoker'] = data['smoker'].astype('category').cat.codes
region_new = {'region': {'southwest': 1, 'southeast': 2, 'northwest': 3,'northeast': 4}}
data.replace(region_new, inplace=True)
# Now we can get the correlation between the 'charges' and all the other variables.
plt.figure(figsize=(12, 10))
cor = data.corr()
sns.heatmap(cor, annot=True, cmap=plt.cm.Reds)
plt.title('Correlations')
labels = data.columns
xi = list(range(7))
plt.xticks(xi, labels)
plt.yticks(xi, labels)
plt.show()
# ----------------------------------------------------------------------------------------------------------------------
# selecting the features Age, BMi, Smoker.
# split the data set into training and testing:
#  80% of data into training data and the rest should be the test data:
training_dataset, test_dataset = sklearn.model_selection.train_test_split(data, test_size=0.2)
X_train = training_dataset[['age','bmi','smoker']]
Y_train = training_dataset['charges']
X_test = test_dataset[['age','bmi','smoker']]
Y_test = test_dataset['charges']
Linear_Model = sm.OLS(Y_train, X_train).fit()
print(Linear_Model.summary())
# the predictive performance (in terms of the root mean squared error) for the training and for the test data.
Y_pred = Linear_Model.predict(X_train)
MSE = np.mean((Y_train-Y_pred)**2)
RMSE = np.sqrt(MSE)
print("Root mean squared error for the training data: {}".format(RMSE))
Y_pred = Linear_Model.predict(X_test)
MSE = np.mean((Y_test-Y_pred)**2)
RMSE = np.sqrt(MSE)
print("Root mean squared error for the test data: {}".format(RMSE))

# ----------------------------------------------------------------------------------------------------------------------

# residual versus fitted value plot
Y_pred = Linear_Model.predict(X_test)
Y_pred = Y_pred.to_numpy()
sns.residplot(Y_pred.reshape(-1),Y_test, data=data,lowess=True,
              line_kws={'color': 'red', 'lw': 1, 'alpha': 1})
plt.xlabel("Fitted values")
plt.ylabel("Residuals")
plt.title('Residual plot')
plt.show()

# ----------------------------------------------------------------------------------------------------------------------

# Plot Cook’s distance of your model and identify influential observations:
# c is the distance and p is p-value
influence = Linear_Model.get_influence()
(c, p) = influence.cooks_distance
plt.stem(np.arange(len(c)), c, markerfmt=",",use_line_collection=True)
plt.title('Cook’s distance')
plt.xlabel("Observations")
plt.ylabel("Influence")
plt.show()













