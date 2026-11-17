# Used Car Price Prediction

What drives the selling price of a used car in India, and how well can we predict it?

An MSc Data Science and Statistics project (University of Limerick) by **Kavvin Kathiresan** and **Peter Lee**. We use linear and multiple linear regression in R to study how distance driven, car age, engine power, transmission and fuel type relate to used-car selling prices, and we deliver the results in an interactive Shiny dashboard.

**Live dashboard:** https://usedcarprediction.shinyapps.io/used_car_price_prediction/

---

## Research questions

1. Does distance driven matter for selling price?
2. Does car age add explanatory power?
3. Do power, transmission and fuel type improve prediction?
4. Which model predicts unseen cars best?

## Data

The analysis uses `Car details v3.csv`, a dataset of 8,128 used-car listings from the Indian market.

| Step | Rows |
| --- | ---: |
| Original listings | 8,128 |
| After removing exact duplicates | 6,926 |
| Main sample (distance up to 300,000 km, model years 1983–2020) | 6,908 |
| Extension sample (power known, diesel or petrol only) | 6,613 |

Data preparation:

- Mileage, engine size and maximum power are extracted from text (for example "74 bhp") and zero values are treated as missing.
- `car_age = 2020 − year`, and distance is scaled to units of 10,000 km (`km_10k`).
- Mileage mixes km/l and km/kg, so it is explored but not used as a predictor.

## Models

| Model | Specification | Sample |
| --- | --- | --- |
| 1 | `selling_price ~ km_10k` | Main (6,908) |
| 2 | `selling_price ~ km_10k + car_age` | Main (6,908) |
| 3 | `log(selling_price) ~ km_10k + car_age` | Extension (6,613) |
| 4 | `log(selling_price) ~ km_10k + car_age + power_10 + transmission + fuel` | Extension (6,613) |

`power_10` is engine power in units of 10 bhp. Transmission is Manual or Automatic (reference: Manual) and fuel is Diesel or Petrol (reference: Diesel).

## Key results

| Model | Distance effect | Other effects | R² |
| --- | --- | --- | ---: |
| 1 | −₹23,695 per 10,000 km | — | 0.04 |
| 2 | −₹1,563 per 10,000 km (p = 0.26) | Age −₹54,461 per year | 0.19 |
| 3 | +2.1% per 10,000 km | Age −13.8% per year | 0.52 |
| 4 | −0.5% per 10,000 km | Age −10.9% per year, power +12.0% per 10 bhp, automatic +16.5%, petrol −23.4% | 0.83 |

Models 3 and 4 use log price, so effects are percentage changes: `(exp(β) − 1) × 100`.

**Test-set evaluation** (80/20 split of the extension sample, `set.seed(123)`, 1,323 held-out cars; log-model predictions are `exp()` of the fitted log price, so they estimate the median):

| Model | Test RMSE | Test MAE |
| --- | ---: | ---: |
| 1 | ₹475,794 | ₹291,560 |
| 2 | ₹432,133 | ₹236,568 |
| 3 | ₹438,998 | ₹212,970 |
| **4** | **₹184,209** | **₹112,642** |

**Main findings**

- Car age is the strongest predictor: each extra year is associated with about 11% lower price in Model 4.
- Distance alone looks important (Model 1), but its effect largely disappears once car age is included (Model 2), because older cars have usually been driven further.
- After controlling for power, transmission and fuel type, each extra 10,000 km is associated with about 0.5% lower price.
- Higher power and automatic transmission are associated with higher prices, and petrol cars sell for less than comparable diesel cars.
- Model 4 has the lowest test RMSE and MAE.

## Shiny dashboard

The dashboard has seven sections:

1. **Overview** – project objective and dataset snapshot
2. **Data Preparation** – cleaning steps, missing values and a cleaned-data preview
3. **Exploratory Analysis** – price against distance, age, power, engine size and mileage, price distributions, and price by fuel type and transmission
4. **Regression Models** – summaries of Models 1–4
5. **Model Evaluation** – test RMSE and MAE, plus Model 4 diagnostic plots
6. **Price Prediction** – enter distance, age, power, transmission and fuel type to estimate a typical (median) price with Model 4
7. **Conclusion** – key findings and limitations

## Requirements

- R (4.x recommended) and RStudio
- Analysis packages: `tidyverse`, `broom`, `scales`, and optionally `patchwork`, `lmtest` and `car`
- Dashboard packages: `shiny`, `shinydashboard`, `tidyverse`, `scales`, `DT`

Install everything with:

```r
install.packages(c("tidyverse", "broom", "scales", "patchwork", "lmtest", "car",
                   "shiny", "shinydashboard", "DT"))
```

## How to run

1. Clone or download this repository and open the project folder in RStudio, so the working directory is the project root.
2. Put the dataset at `data/Car details v3.csv`.
3. Run the analysis script. Figures are saved to `plots/` and tables to `output/` (both folders are created automatically).
4. To run the dashboard locally, open the Shiny app file and click **Run App**, or run `shiny::runApp()` from the folder that contains it. The data file must be at `data/Car details v3.csv` relative to the app.

## Limitations

- The data are observational, so the results are associations, not causal effects.
- The data may not represent all used-car markets or current market prices.
- Car age uses 2020 as the reference year.
- Mileage may mix km/litre and km/kg units, so it is not used as a regression predictor.
- Model 4 assumes the same distance effect at every car age; the distance-by-age-group plot suggests it weakens as cars get older.
- Log-model predictions estimate the median rather than the mean price.
- Predictions are estimates and depend on the quality and coverage of the dataset.

## Authors

Kavvin Kathiresan and Peter Lee
MSc Data Science and Statistics, University of Limerick
