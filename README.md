# 2021HDBproject

An end-to-end data science project analysing and forecasting Singapore HDB resale prices using machine learning and statistical modeling.

## Project summary
This project explores how property characteristics such as floor area, remaining lease, distance to CBD, and nearby amenities influence HDB resale pricing. The goal is to quantify which factors matter most and build predictive models that can estimate resale values and classify whether an HDB unit is above or below the median market price.

## Why this project matters
Housing is one of the most important financial decisions for households in Singapore. By modelling resale prices, this project demonstrates how data science can turn large property datasets into actionable insights for buyers, sellers, and analysts.

## Core workflow
- Exploratory data analysis and feature understanding
- Unsupervised learning using clustering and PCA
- Regression modeling for price prediction
- Logistic regression and decision tree classification for high-price classification
- Model comparison using performance metrics and validation approaches

## Key results
- Full linear regression model achieved strong explanatory power with adjusted R-squared of 0.9266
- Feature importance analysis highlighted floor area, remaining lease, distance to CBD, and nearby amenities as major drivers
- Logistic regression achieved AUC = 0.94 for identifying high-value HDB flats
- Multiple analytical methods were compared to assess predictive performance and business relevance

## Technical stack
- R
- readxl, dplyr
- ks, corrplot
- pls, kknn, ROCR
- tree, rpart, rpart.plot

## Files
- `HDB_Data_Project.R` — complete project analysis in R
- `README.md` — project overview
- `dashboard.html` — recruiter-friendly visual summary

## Recruiter-ready dashboard
Open the dashboard here: `dashboard.html`

## Learnings
This work shows a practical data science workflow: cleaning and understanding data, building interpretable models, validating results, and communicating insights clearly. The project is well-suited for discussing statistical analysis, predictive modeling, and business interpretation in interviews.
