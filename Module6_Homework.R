# =====================================================
# Module 6 Homework - Multivariate Linear Regression
#                     Supervised Modeling Technique
# Mike Hankinson
# =====================================================
# Background
# ...........
# This assignment will extend on the data on individuals from the city provided for 
# assignment 5. 

# In addition to jewelry spend and salary, you are provided with three additional 
# independent variables that can potentially be added to the model. These are: 
    # 1. Real_Estate: Size of primary residence in square feet 
    # 2. Stock: Amount of assets in stock portfolio in thousands of dollars 
    # 3. Previous_Spend: Amount of money spent on jewelry last year 

# Questions
# ...........
# 1.  For each of the above predictors, create a multivariate linear models where you use that 
#     variable and Salary to predict jewelry spend (3 models total, 2 predictors in each model). 
#     For each model, comment if you think the new variable has been a helpful addition. 
#     Say why or why not you think this is the case. 
#     Interpret what you think this means for what metrics drive jewelry spend. 

# 2.	Create a model using all four predictors. Use the step function to find the iteration 
#     of the model with the lowest AIC. Do you agree that this is the best model? 
#     Why or why not is this the case? Suggest a final model and interpret your findings. 
 
# ********************************************************************    
# Question 1
# ********************************************************************
# Process
# __________________________________________
# 1. Load data
# 2. Correlation
# 3. Build 3 Models with 2 Predictors
# 4. Compare the slope estimates, standard errors, T-statistics, and P-values
# 5. Compute and compare confidence intervals for B1 - T-Test
# 6. Conclusions

# 1. Load and Plot Data
# ------------------------------------------
dat <- read.csv("Assignment6.csv")
tail(dat)

# ***Note: Units of Data Columns***
# Jewelry             $      
# Salary              $M  (Thousands of dollars)  
# Real_Estate         $M      
# Stock               $M 
# Previous_Spend      $



# 2. Correlation
# ------------------------------------------
cor(dat)

# Positive correlations are seen in 
# 1. Jewelry and Salary: 0.6269505
# 2. Jewelry and Real Estate: 0.6089253
# 3. Jewelry and Stock, minimally: 0.09441619
# 4. Jewelry and Previous Spend: 0.69961888



# 3. Build 3 Models with 2 Predictors
# ------------------------------------------

# Also, for comparision, build 4th model (from last week) with only 1 predictor (Salary)
# 3 Linear Models
Jewelry.m1 <- lm(Jewelry ~ Salary + Real_Estate, dat)
Jewelry.m2 <- lm(Jewelry ~ Salary + Stock, dat)
Jewelry.m3 <- lm(Jewelry ~ Salary + Previous_Spend, dat)
Jewelry.base <- lm(Jewelry ~ Salary, dat)

# 4. Compare the slope estimates, standard errors, T-statistics, and P-values
# ------------------------------------------

# Obtain and Format Data from 3 Models
Salary.Real_Estate <- round(summary(Jewelry.m1)$coef[2:3,],3) # round to 3 places
Salary.Stock <- round(summary(Jewelry.m2)$coef[2:3,],3)
Salary.Previous_Spend <- round(summary(Jewelry.m3)$coef[2:3,],3)
Base.Model.Salary.Only <- round(summary(Jewelry.base)$coef[2,],3)


# B1 Summary of 3 Individual Models
Two.Predictor.models <- rbind(Salary.Real_Estate, Salary.Stock, Salary.Previous_Spend, Base.Model.Salary.Only)
    #                     Estimate Std.   Error   t value     Pr(>|t|)
    # Salary                    0.053      0.007    7.109       0.000
    # Real_Estate               0.002      0.001    2.524       0.012
      
    # Salary                    0.071      0.003    27.555      0.000
    # Stock                    -0.001      0.001    -1.011      0.312
      
    # Salary                    0.042      0.002    18.112      0.000
    # Previous_Spend            0.480      0.019    24.819      0.000
      
    # Base.Model.Salary.Only    0.070      0.003    27.854      0.000



    # - Note the 3 models are compared to the Base.Model which was performed last week as well. 
    #   The base model, as shown above, only studies the influence of salary on annual jewelry 
    #   spend.  It shows a B1 coefficient of $0.070 spend/$M Salary (per 1,000 salary) 
    # - The B1s of Salary/Stock Model show a slight negative relationship (nearly no relationship)
    #   between Stock and Spend, with the balance tucked away under Salary ($0.071/$M Salary).
    #   This is also demonstrated in the large (> 0.001) Stock P-Value
    # - Interesting to note Previous_Spend accounts for the majority of the slope within the 
    #   Salary/Previous_SPend model. This, in fact, understates the difference, as units are different
    #   between the 2 predictors -- 
    #       * Salary in $spend/$M earned, 1:1,000
    #       * Previous_Spend in $spend/$spend, 1:1
    # - We have introduced additional error into the 3 two-feature models. 
    # - P-values for all but stock are < 0.001.  Meaning there appears to be a definitive relationship
    #   with the features in models 1 and 3 as well as the base model, 4.    


# 5. Compute and compare confidence intervals for B1 - T-Test
# ------------------------------------------
# Additional Information -->

two.predictor.models.confidence <- rbind(Salary.Real_Estate=confint(Jewelry.m1)[2,], Salary.Stock=confint(Jewelry.m2)[2,], 
                                      Salary.Previous_Spend =confint(Jewelry.m3)[2,], Base.Model.Salary.Only=confint(Jewelry.base)[2,])

    #                           2.5 %     97.5 %
    # Salary.Real_Estate     0.03804700 0.06705160      
    # Salary.Stock           0.06555450 0.07560510      
    # Salary.Previous_Spend  0.03767883 0.04683357      
    # Base.Model.Salary.Only 0.06516019 0.07503500



# 6. Conclusions
# ------------------------------------------
# Is addition of the new variable a helpful addition to the model? Why or Why Not?
 
# - Model 1 - Jewelry~Salary+Real_Estate: Maybe, Real Estate probably not a good variable to add to the model
#     + Good Positive Correlation, 0.6089253
#     + Not much to add in terms of slope B1, 0.002
#     + Large P-Value, 0.012
# - Model 2 - Jewelry~Salary+Stock: No, Stock is not a good variable to add to the model
#     + Minimal Correlation, 0.09441619
#     + Negative slope B1, -0.001 
#     + Large P-Value, 0.012
# - Model 3 - Jewelry~Salary+Previous_Spend: Yes, Previous Spend is a good variable to add to the model
#     + Good Positive Correlation, 0.69961888.  Highest individual correlation (> Salary)
#     + Provides much of the slope with B1, 0.480
#     + P-Value < 0.001
#     + Tight confidence level...above 0.  

# Interpret what this means for what metrics drive jewelry spend. 
# - Previous spend is a large predictor for future jewelry purchases.  This makes intuitive sense. 
#   People who like jewelry will spend money time-and-again for new purchases.  
# - In addition, people who have a larger income have larger disposable money in which to spend
#   on jewelry purchases.  
# - Amount of stock and/or real estate value is not a good predictor for expenditure on jewelry. 



# ********************************************************************    
# Question 2
# ********************************************************************
# Process
# __________________________________________
# 1. Build Model Using All Predictors Simultaneously
# 2. Use step() to discover the combination of predictors to produce the lowest AIC possible
# 3. Use step() to discover the combination of predictors to produce the lowest AIC possible 
# 4. Conclusions


# 1. Build model using all predictors simultaneously 
# ------------------------------------------
Jewelry.m4 <- lm(Jewelry ~ ., dat) # Joint model
    # Call:
    #   lm(formula = Jewelry ~ ., data = dat)
    # 
    # Residuals:
    #   Min      1Q  Median      3Q     Max 
    # -11.582  -2.142  -0.140   2.292  10.288 
    # 
    # Coefficients:
    #               Estimate Std. Error     t value   Pr(>|t|)    
    # (Intercept)       3.069e+02  7.712e+00  39.794  < 2e-16 ***
    #   Salary          2.425e-02  6.127e-03   3.958    8e-05 ***
    #   Real_Estate     1.626e-03  4.992e-04   3.258    0.00115 ** 
    #   Stock          -5.631e-04  5.519e-04  -1.020    0.30778    
    # Previous_Spend    4.806e-01  1.927e-02  24.933  < 2e-16 ***
    #   ---
    #   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    # 
    # Residual standard error: 3.282 on 1195 degrees of freedom
    # Multiple R-squared:  0.6032,	Adjusted R-squared:  0.6019 
    # F-statistic: 454.1 on 4 and 1195 DF,  p-value: < 2.2e-16


All.Features.Model <- round(summary(Jewelry.m4)$coef[2:5,],3)
    #               Estimate    Std. Error  t value   Pr(>|t|)
    # Salary            0.024      0.006    3.958     0.000
    # Real_Estate       0.002      0.000    3.258     0.001
    # Stock            -0.001      0.001    -1.020    0.308
    # Previous_Spend    0.481      0.019    24.933    0.000


# 2. Use step() to discover the combination of predictors to produce the lowest AIC possible
# ------------------------------------------
# - Given a data set, the AIC value measures the quality of a model relative 
#   to other models.
# - AIC is not measured on an absolute scale and the actual value of the metric 
#   has little to no meaning.
# - AIC is only designed to be used as a relative metric to evaluate quality when 
#   different fits are performed on the same response variable.
# - Do not rely on AIC to make determinations on which model is 
#   better fit for independent problem statements that utilize different 
#   data.
# - AIC values are not bounded by 0 and can be negative-models with 
#   negative AIC represent a higher quality fit than models with 
#   positive AIC.
# - Lower values are always preferred.


# 3. Use step() to discover the combination of predictors to produce the lowest AIC possible 
# ------------------------------------------
# - Avoids itterative approach of drop1() and add1()
# - At each iteration, the function will provide the AIC for the current model 
#   and the corresponding AIC if we were to remove any predictors or add a 
#   predictor not included back in.

step(Jewelry.m4, direction="both")

    # Start:  AIC=2856.97
    # Jewelry ~ Salary + Real_Estate + Stock + Previous_Spend
    # 
    # Df Sum of Sq   RSS    AIC
    # - Stock           1      11.2 12880 2856.0
    # <none>                        12869 2857.0
    # - Real_Estate     1     114.3 12983 2865.6
    # - Salary          1     168.7 13037 2870.6
    # - Previous_Spend  1    6694.7 19563 3357.6
    # 
    # Step:  AIC=2856.01
    # Jewelry ~ Salary + Real_Estate + Previous_Spend
    # 
    #                   Df Sum of Sq   RSS    AIC
    # <none>                        12880 2856.0
    # + Stock           1      11.2 12869 2857.0
    # - Real_Estate     1     115.5 12995 2864.7
    # - Salary          1     162.9 13043 2869.1
    # - Previous_Spend  1    6698.9 19579 3356.5
    # 
    # Call:
    #   lm(formula = Jewelry ~ Salary + Real_Estate + Previous_Spend, 
    #      data = dat)
    # 
    # Coefficients:
    #   (Intercept)       Salary     Real_Estate  Previous_Spend  
    # 3.068e+02       2.376e-02       1.635e-03       4.807e-01  

# 4. Conclusions
# ------------------------------------------
# Do you agree that this is the best model? Why or why not?
# Suggest a final model and interpret your findings.


# - The model with the lowest AIC uses Salary, Previous_Spend and 
#   Real_Estate as the best predictors for Jewelry Expenditure. 
# - These all seem like reasonable predictors for Jewelry expenditure.  
# - As described above, Real_Estate was a fringe option in the first models due to large P-Value (0.012) and it (B1) did 
#   not add much to the overall slope of the model  
# - However, in the full features model, P-value for real estate was low (0.001) added little error,
#   solidifying it for inclusion in the final model.  
# - Final Model Recommendation: lm(formula = Jewelry ~ Salary + Real_Estate + Previous_Spend, data = dat)






# rm(list = ls())      Removes global environment
                    