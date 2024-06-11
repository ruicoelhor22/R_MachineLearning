# R_MachineLearning
This project was developed in the Master's Degree Course TIAPOSE, during the 4th year of MEGSI. It applies machine learning and optimization algorithms to a dataset related to Walmart sales and employee numbers, using R and Shiny as an interface.

![image](https://github.com/ruicoelhor22/R_MachineLearning/assets/58275291/698be65a-2354-441d-9aa9-4cd516dfe66e)


## Running the Project
Download the file or clone the repo, set the Working Directory to the project folder, and run the app.R file.

# Models (Machine Learning)
For our models, we chose those that fitted the criteria of our data and prediction objective (Time Series Forecasting) from two major packages: **Rminer** and **Forecast**.

# Packages
Both packages were recommended by Paulo Cortez, our teacher and supervisor for the duration of this project.

## Rminer
**Description:** Facilitates the use of data mining algorithms in classification and regression (including time series forecasting) tasks by presenting a short and coherent set of functions.
**Author:** Paulo Cortez

### Models

#### Linear Regression (lm)
Linear regression attempts to model the relationship between two variables by fitting a linear equation to observed data. One variable is considered to be an explanatory variable, and the other is considered to be a dependent variable. For example, a modeler might want to relate the weights of individuals to their heights using a linear regression model.

#### Multilayer Perceptron Ensemble (mlpe)
The Multilayer Perceptron Ensemble is an advanced modeling technique that harnesses the collective intelligence of multiple neural networks. By amalgamating these networks into a unified ensemble, it effectively synthesizes diverse insights and nuanced patterns from the data. Each constituent neural network undergoes individual training, contributing its unique perspective to the ensemble's predictive prowess. This collaborative approach fosters a comprehensive understanding of complex data relationships, resulting in a highly adaptable and robust forecasting model.

#### Naive (naive)
The "Naive" method selects the most common class (in classification tasks) or the mean output value (in regression tasks) as its prediction. In essence, this method takes a straightforward approach, based on the assumption that the simplest prediction is the average or most common value from the observed data. Although it is a basic technique, the "Naive" method can provide a useful baseline for comparison with more sophisticated models.

#### Random Forest (randomForest)
Random forest is a type of supervised learning algorithm that uses ensemble methods (bagging) to solve both regression and classification problems. The algorithm operates by constructing a multitude of decision trees at training time and outputting the mean/mode of prediction of the individual trees. The fundamental concept behind random forest is the wisdom of crowds, wherein a large number of uncorrelated models operating as a committee will outperform any of the individual constituent models.

## Forecast
**Description:** The R package forecast provides methods and tools for displaying and analyzing univariate time series forecasts including exponential smoothing via state space models and automatic ARIMA modeling. This package is now retired in favor of the fable package.
**Authors:** Rob Hyndman, George Athanasopoulos, Christoph Bergmeir, Gabriel Caceres, Leanne Chhay, Mitchell O'Hara-Wild, Fotios Petropoulos, Slava Razbash, Earo Wang, Farah Yasmeen, R Core Team, Ross Ihaka, Daniel Reid, David Shaub, Yuan Tang, Zhenyu Zhou

### Models

#### Holt Winters (HW)
Holt-Winters Exponential Smoothing, named after its contributors Charles Holt and Peter Winters, is a venerable time series analysis technique renowned for its adeptness in forecasting while accommodating both trend and seasonality. This method comprises three fundamental aspects, collectively referred to as triple exponential smoothing:

- **Exponential Smoothing:** This foundational technique, known as simple exponential smoothing, is employed when the dataset exhibits neither trend nor seasonality, providing a baseline forecast.
- **Holt’s Smoothing Method:** Holt’s smoothing, also recognized as linear exponential smoothing, is tailored for datasets with discernible trends. It effectively captures and extrapolates trend dynamics to generate forecasts.
- **Winter’s Smoothing Method:** Winter’s smoothing method extends Holt’s approach to incorporate seasonality into the forecasting process. By integrating both trend and seasonal components, it enhances the accuracy of time series predictions.

In essence, the Holt-Winters method synthesizes these components, namely level (ℓt), trend (bt), and smoothing coefficients (α and β), to formulate forecasts. This amalgamation enables the model to produce robust predictions that account for both trend evolution and seasonal fluctuations, making it a versatile tool in time series forecasting.

#### Autoregressive Integrated Moving Average (ARIMA)
ARIMA is the abbreviation for AutoRegressive Integrated Moving Average. AutoRegressive (AR) terms refer to the lags of the differenced series, Moving Average (MA) terms refer to the lags of errors, and I is the number of differences used to make the time series stationary. It's a forecasting algorithm based on the idea that the information in the past values of the time series alone can be used to predict future values.

#### Neural Network Time Series Forecasts (nnetar)
Neural Network Time Series Forecasts, abbreviated as nnetar, is a powerful modeling technique that leverages artificial neural networks (ANNs) to perform time series forecasting. Unlike traditional statistical methods, neural networks are capable of capturing complex nonlinear relationships within the data, making them well-suited for forecasting tasks with intricate patterns and dynamics. The nnetar model employs a feedforward neural network architecture, where information flows in one direction from input to output layer, allowing it to learn from historical data and generate forecasts with high accuracy.

#### Exponential Smoothing (ETS)
Exponential Smoothing (ETS) is a widely used time series forecasting method that is based on the principle of exponentially weighted averages. It is particularly effective for capturing and forecasting data with trend, seasonality, and/or noise. The ETS model encompasses various smoothing techniques, including simple exponential smoothing (SES), Holt's linear trend method (Holt), and Holt-Winters' seasonal method (HW). By adapting the smoothing parameters to the specific characteristics of the data, ETS is capable of producing accurate forecasts for a wide range of time series patterns.

# Optimization Algorithms

#### Hill Climbing
In numerical analysis, hill climbing is a mathematical optimization technique that belongs to the family of local search. It is an iterative algorithm that starts with an arbitrary solution to a problem, then attempts to find a better solution by making an incremental change to the solution. If the change produces a better solution, another incremental change is made to the new solution, and so on until no further improvements can be found.

#### Monte Carlo
Monte Carlo methods, or Monte Carlo experiments, are a broad class of computational algorithms that rely on repeated random sampling to obtain numerical results. The underlying concept is to use randomness to solve problems that might be deterministic in principle. They are often used in physical and mathematical problems and are most useful when it is difficult or impossible to use other approaches. Monte Carlo methods are mainly used in three problem classes: optimization, numerical integration, and generating draws from a probability distribution.

#### Tabu Search (Tabu)
Tabu search is a metaheuristic local search method used for mathematical optimization. Local search methods have the tendency to be stuck in suboptimal regions. Tabu search enhances the performance of these techniques by prohibiting already visited solutions or others through user-provided rules.

#### Simulated Annealing (Sann)
Simulated annealing (SA) is a probabilistic technique for approximating the global optimum of a given function. Specifically, it is a metaheuristic to approximate global optimization in a large search space for an optimization problem. It is often used when the search space is discrete (for example, the traveling salesman problem, the boolean satisfiability problem, protein structure prediction, and job-shop scheduling).

# Best Models
After analyzing all the results of the selected models, those that showed the best values for the different medians were chosen as the best option in the final solution. To achieve this goal, the various forecasting models were evaluated based on the median NMAE, MAE, RMSE, and R^2.

# Evaluation of Univariate Forecasting Models by Department
Here we can see the best univariate forecasting methods for each department and the best global method.

## Results by Department

### WSdep1

- Best Method: **ARIMAt**
- Median NMAE: 4.76
- Median MAE: 8271.15
- Median RMSE: 8536.96
- Median R^2: 0.01

### WSdep2

- Best Method: **mlpe**
- Median NMAE: 9.22
- Median MAE: 5256.67
- Median RMSE: 6263.29
- Median R^2: 0.80

### WSdep3

- Best Method: **Random Forest**
- Median NMAE: 1.53
- Median MAE: 611.19
- Median RMSE: 761.51
- Median R^2: 0.80

### WSdep4

- Best Method: **Random Forest**
- Median NMAE: 3.31
- Median MAE: 1832.95
- Median RMSE: 2075.87
- Median R^2: 0.83

## Global Best Univariate Method
The best global method for univariate models is **Random Forest**.

# Evaluation of Multivariate Forecasting Models by Department
Here we can see the best multivariate forecasting methods for each department, as well as the best global method.

## Results by Department

### WSdep1

- Best Method: **HoltWinters**
- Median NMAE: 57.23
- Median MAE: 10987.66
- Median RMSE: 12754.44
- Median R^2: 0.47

### WSdep2

- Best Method: **VAR**
- Median NMAE: 21.61
- Median MAE: 5901.11
- Median RMSE: 6680.86
- Median R^2: 0.74

### WSdep3

- Best Method: **HoltWinters**
- Median NMAE: 59.35
- Median MAE: 1961.41
- Median RMSE: 2277.70
- Median R^2: 0.16

### WSdep4

- Best Method: **Arimax**
- Median NMAE: 19.19
- Median MAE: 1848.86
- Median RMSE: 2009.30
- Median R^2: 0.73

## Global Best Multivariate Method
The best global method for multivariate models is **HoltWinters**.

## Conclusion
Based on the presented results, we can conclude that the **Random Forest** method is consistently the best forecasting model for all analyzed departments. This method has been identified as the most effective, considering the median NMAE, MAE, RMSE, and R^2.
