# INR-USD 
# Time Series Analysis of Indian National Rupee against US Dollar

Motivation for this project is the fact that INR has lost its value against USD over time. For instance, 1 USD was equivalent to 31.4 INR in January 1994 and it is about 74 rupees in November 2020. 

This repo has all the files needed to execute this project. I have used data from Jan 1994-Dec 2016 for model building and data from Jan 2017-Aug 2018 is used for forecasting. I conducted thorough data analysis and detected strong autocorrelation along with seasonality during government elections in India.
Considered ARIMA, SARIMA, Naive, & Decomposition methods to build models and provided forecasts for future rates. Input files are maindatacsv.csv and forecastdata.csv (test data). moneydata.R is the R script for analysis. 
