# Best Models

After analyzing all the results of the selected models, those that showed the best values for the different medians were chosen as the best option in the final solution.
To achieve this goal, the various forecasting models were evaluated based on the median NMAE, MAE, RMSE, and R^2.

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

- Best Method: HoltWinters
- Median NMAE: 57.23
- Median MAE: 10987.66
- Median RMSE: 12754.44
- Median R^2: 0.47

### WSdep2

- Best Method: VAR
- Median NMAE: 21.61
- Median MAE: 5901.11
- Median RMSE: 6680.86
- Median R^2: 0.74

### WSdep3

- Best Method: HoltWinters
- Median NMAE: 59.35
- Median MAE: 1961.41
- Median RMSE: 2277.70
- Median R^2: 0.16

### WSdep4

- Best Method: Arimax
- Median NMAE: 19.19
- Median MAE: 1848.86
- Median RMSE: 2009.30
- Median R^2: 0.73

## Global Best Multivariate Method

The best global method for multivariate models is **HoltWinters**.


## Conclusion

Based on the presented results, we can conclude that the **Random Forest** method is consistently the best forecasting model for all analyzed departments. This method has been identified as the most effective, considering the median NMAE, MAE, RMSE, and R^2.
