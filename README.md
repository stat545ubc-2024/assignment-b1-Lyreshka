# Model Calibration and Plotting

This repository contains a function called `calibrate_and_plot` that calibrates multiple model forecasts and provides visualization options. It is designed to streamline the process of model evaluation and visualization in predictive modeling workflows. While testing multiple models or generating predictions, the process of visualizing and calibrating each model can become time-consuming, especially with larger datasets. This function helps streamline these tasks by efficiently evaluating and comparing multiple models with minimal code. 

## Usage

To use the `calibrate_and_plot`, you need to provide fitted model objects and specify whether to use testing or training data.

```r
calibrate_and_plot(model_fit1, model_fit2, type = "testing")
calibrate_and_plot(model_fit1, model_fit2, type = "training")
```
