# Milan House Price Prediction 🏠📈

Regression models to predict apartment prices in Milan using advanced preprocessing.

## Project Overview
- **Dataset**: Milan housing (8k train, 4.8k test) - **private university dataset**
- **Target**: Selling price per sqm (€, log scale)  
- **Language**: R (mice, glmnet, mgcv/GAM)

## Pipeline Highlights
1. **Feature Engineering** :
   - Text parsing "other features" → balcony, pool, jacuzzi, fiber, etc.
   - MICE + linear regression imputation (sqm, bathrooms, condo fees)
   - Coherence checks: conditions/year/energy class
   - Parking categorization (no/1spot/2+spot/shared)

2. **Models** (5-fold CV, MAE €/sqm):
   | Model          | MAE (€/sqm) | 
   |----------------|-------------|
   | **GAM** (best) | **80,095**  |
   | Elastic Net    | 83,983      |
   | Lasso          | 83,917      |
   | Forward Reg.   | 83,835      | 
   | OLS Full       | 84,316      | 

## Repository Contents
- `Codice.R` → Full preprocessing + modeling pipeline
- `Report.pdf` → Complete analysis with methodology

