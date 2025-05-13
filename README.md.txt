# Nowcasting and Forecasting German House Prices

This repository contains code for forecasting German housing prices using mixed frequency time series models such as MIDAS. The project is based on a master's thesis.

---

## Overview

This project implements Mixed-Data Sampling (MIDAS) and Mixed-Frequency VAR (MF-VAR) models for nowcasting and forecasting German house prices. The models use high-frequency economic indicators (monthly and daily) to produce timely updates of the quarterly German House Price Index (HPI).

Forecasts are generated using both single-indicator and combinated models across two sample periods—before and after the COVID-19 economic crisis. Results show that MF-VAR provides the most accurate nowcasts, while AR-MIDAS and U-MIDAS perform better over longer forecast horizons.

This repository includes the core code used to data preprocessing, modelling and model evaluation.

---

## Tools

- **Language**: R
- **Models**:
  - MIDAS (Mixed-Data Sampling)
  - AR-MIDAS (AutoRegressive MIDAS)
  - U-MIDAS (Unrestricted MIDAS)
  - MF-VAR (Mixed-Frequency VAR)

---

## Required Libraries

- **`midasr` (v0.8)** – For estimation and forecasting of MIDAS models
- **`mfbvar` (v0.5.6)** – For Bayesian estimation and forecasting of mixed-frequency Bayesian VAR models
- **`RJDemetra` (v0.2.6)** – Interface to *JDemetra+* for seasonal adjustment (used by the European Central Bank)

## Data


The following predictors were used for nowcasting and forecasting the German House Price Index (HPI):

| Predictor                     | Abb.   | Frequency  | Publication Lag (Months) |
|------------------------------|--------|------------|---------------------------|
| House price index            | `hpi`  | Quarterly  | 3                         |
| DAX close price              | `dax`  | Daily      | 0                         |
| Unemployment rate            | `unrate` | Monthly  | 2                         |
| Residential building permits | `rbp`  | Monthly    | 2                         |
| Consumer price index         | `cpi`  | Monthly    | 1                         |
| Housing loan interest rate   | `hl`   | Monthly    | 2                         |
| National disposable income * | `din`  | Quarterly  | 3                         |

> *Note: National disposable income (`din`) is interpolated from quarterly to monthly using the `zoo` package.

