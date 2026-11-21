assignment 1
================
Dawson Conlon
2026-11-20

# Problem 1

``` r
#Load the data
water=read.csv("https://raw.githubusercontent.com/Thuntida/dataset/refs/heads/main/water.csv")

#Load into a data frame
water.df = data.frame(water)
head(water.df)
```

    ##    PROD TEMP HOUR USAGE DAYS
    ## 1 171.3 39.7  9.5  19.0   20
    ## 2  19.4 16.0 20.0   6.6   21
    ## 3  18.7 12.1 26.0   6.7   21
    ## 4  25.6 39.0 24.0   9.5   21
    ## 5  25.6 39.0 23.0   9.5   21
    ## 6 139.2 14.3 16.0  12.2   21

Y is the USAGE feature and the rest of the variables are the predictors

### 1.1

``` r
#Fit the model 
water.model = lm(USAGE ~ (PROD + TEMP + HOUR + DAYS), data = water.df)

#Get the Coefficients
coefficients(water.model)
```

    ## (Intercept)        PROD        TEMP        HOUR        DAYS 
    ##  5.89162697  0.04020739  0.16867306 -0.07099009 -0.02162304

Below is the estimate of the model

$$
\widehat{USAGE,i}=5.892+0.040𝑋_{PROD,𝑖}+0.169𝑋_{TEMP,𝑖}-0.070𝑋_{HOUR,𝑖}-0.0216𝑋_{DAYS,𝑖}
$$

Interpretation

When all predictor variables are held constant, monthly water usage is
at 5.8916 (gallons/minute)

For a given amount of TEMP and HOUR and DAYS (these are held constant)
an increase in 1 unit of amount of production (hundreds of cubic) leads
to an increase of 0.0402 gallons/minute of monthly water usage.

For a given amount of PROD and HOUR and DAYS( these are held constant)
an increase in 1 unit of average monthly temperature (degrees Celsius)
leads to an increase of 0.1687 gallons/minute of monthly water usage

For a given amount of PROD and TEMP and DAYS (these are held constant)
an increase in 1 unit of hours shut down for maintenance (hours) leads
to an decrease of 0.0710 gallons/minute of monthly water usage.

For a given amount of PROD, TEMP, and HOUR (these are held constant) an
increase in 1 unit of number of operating days in a month (days) leads
in a decrease of -0.0216 gallons/minute of monthly water usage.

### 1.2

``` r
options(scipen = 999)

#get the anova table
aov(water.model)
```

    ## Call:
    ##    aov(formula = water.model)
    ## 
    ## Terms:
    ##                     PROD     TEMP     HOUR     DAYS Residuals
    ## Sum of Squares  4210.334 1813.652   54.263    1.412   763.058
    ## Deg. of Freedom        1        1        1        1       244
    ## 
    ## Residual standard error: 1.768414
    ## Estimated effects may be unbalanced

``` r
#Calc the F-statistic
fob = (6079.661 / 4) / (763.058 / 244)
p = 4
df = 244
#calc the p value
1 - (pf(fob, p, df))
```

    ## [1] 0

$$
\begin{aligned}
H_0:\ \beta_1 =\beta_2=\beta_3=\beta_4 = 0: \\ \text{USAGE can not be expressed as a linear function of at least one of the four predictor variables} \\\\
H_a:\ \text{At least one}\ \beta_i \neq0: \\ \text{USAGE can be expressed as a linear function of at least oneof the four predictor variables}
\end{aligned}
$$

$$
\begin{aligned}
F_{obs} = 486.017 \\\\
P-value(F_{4,\ 244} \ge486.017) \approx0
\end{aligned}
$$

Based on the data above, with a p value that is less than 0.05 one can
reject the HO (Monthly water usage can not be expressed as a linear
function of at least one of the four predictor variables). This leads
one to conclude that monthly water usage can be expressed as a linear
function of at least one of the four predictor variables (TEMP, PROD,
DAYS, HOUR).

### 1.3

I would not suggest the full model until the predictors have been tested
through a individual T-test

Full Model:

$$
USAGE = \beta_0 + \beta_1X_{PROD,i} + \beta_2X_{TEMP,i}+ \beta_3X_{HOUR,i} + \beta_4X_{DAYS,i} + e_i
$$

Hypothesis to test if DAYS is statistically irreverent .

$$
\begin{aligned}
H0: 𝛽4=0\ (USAGE \ CAN \ NOT \  be \ expressed \ as \ a \ linear \ function \ of \ DAYS)\\
\\ HA: 𝛽4\neq\ 0\ (USAGE \ CAN  \  be \ expressed \ as \ a \ linear \ function \ of \ DAYS)\\
\end{aligned}
$$

``` r
#get coefficents for DAYS
summary(water.model)$coef
```

    ##                Estimate  Std. Error    t value
    ## (Intercept)  5.89162697 1.028794291  5.7267298
    ## PROD         0.04020739 0.001629089 24.6809092
    ## TEMP         0.16867306 0.008209366 20.5464176
    ## HOUR        -0.07099009 0.016992311 -4.1777776
    ## DAYS        -0.02162304 0.032182776 -0.6718825
    ##                                                                                 Pr(>|t|)
    ## (Intercept) 0.00000002998277458121093863721096442476188315140461781993508338928222656250
    ## PROD        0.00000000000000000000000000000000000000000000000000000000000000000002863363
    ## TEMP        0.00000000000000000000000000000000000000000000000000000039011120777274334881
    ## HOUR        0.00004102173294638998801145066130935390447120880708098411560058593750000000
    ## DAYS        0.50229414689401719051886630040826275944709777832031250000000000000000000000

``` r
#calc tobs
tob = (-0.02162304 - 0) / (0.032182776)

#calc p-value
df = 244
2 * (pt(tob, df)) #tob is negative so no 1 - pt
```

    ## [1] 0.5022942

``` r
#get supporting CI 
confint(water.model)
```

    ##                   2.5 %      97.5 %
    ## (Intercept)  3.86517588  7.91807806
    ## PROD         0.03699851  0.04341626
    ## TEMP         0.15250280  0.18484333
    ## HOUR        -0.10446043 -0.03751976
    ## DAYS        -0.08501455  0.04176846

Below is the P value and test statistic for DAYS

$$
\begin{aligned}
T_{obs} = -0.672  \\ 
P - value = P(F_{244}\le -0.672 ) * 2 = 0.502
\end{aligned}
$$

Based on the data above, with a P-value that is greater than 0.05 one
can fail to reject the HO (DAYS can not be expressed as a linear
function of USAGE). This leads one to conclude that DAYS is
statistically irreverent and should be excluded from the model.

To further support this conclusion the 95% confidence interval for
predictor DAYS is: -0.0850 to 0.0417, this interval captures 0
suggesting DAYS may be statistically irrelevant to the model.

``` r
#Make the reduced model
water.reduced = lm(USAGE ~ (PROD + TEMP + HOUR), water.df)

summary(water.reduced)
```

    ## 
    ## Call:
    ## lm(formula = USAGE ~ (PROD + TEMP + HOUR), data = water.df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.5066 -1.1356  0.0469  1.1519  5.3750 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value             Pr(>|t|)    
    ## (Intercept)  5.307511   0.549483   9.659 < 0.0000000000000002 ***
    ## PROD         0.040115   0.001621  24.741 < 0.0000000000000002 ***
    ## TEMP         0.169188   0.008164  20.723 < 0.0000000000000002 ***
    ## HOUR        -0.070769   0.016970  -4.170            0.0000423 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.766 on 245 degrees of freedom
    ## Multiple R-squared:  0.8883, Adjusted R-squared:  0.8869 
    ## F-statistic: 649.3 on 3 and 245 DF,  p-value: < 0.00000000000000022

``` r
confint(water.reduced)
```

    ##                   2.5 %      97.5 %
    ## (Intercept)  4.22519744  6.38982411
    ## PROD         0.03692098  0.04330837
    ## TEMP         0.15310634  0.18526907
    ## HOUR        -0.10419445 -0.03734272

I will now test the predictors statistical relivance through the
individual T test:

PROD: Individual T-test

$$
\begin{aligned}
H0: 𝛽1=0\ (USAGE \ CAN \ NOT \  be \ expressed \ as \ a \ linear \ function \ of \ PROD)\\
\\ HA: 𝛽1\neq\ 0\ (USAGE \ CAN  \  be \ expressed \ as \ a \ linear \ function \ of \ PROD)
\end{aligned}
$$

$$
\begin{aligned}
T_{obs} = 24.741 \\\\
P-value= P(T_{245}\ge24.741)*2 \approx0
\end{aligned}
$$

Based on the data above, with a p-value that is less than 0.05 one can
reject the HO (USAGE can not be expressed as a linear function of PROD).
This leads one to conlcude that USAGE can be expressed as a linear
function of PROD and PROD is statistically relevant to the model. To
further support this conclusion the 95% confidience interval of 0.0369 -
0.0433 does not capture 0.

TEMP Individual T-test:

$$
\begin{aligned}
H0: 𝛽2=0\ (USAGE \ CAN \ NOT \  be \ expressed \ as \ a \ linear \ function \ of \ TEMP)\\
\\ HA: 𝛽2\neq\ 0\ (USAGE \ CAN  \  be \ expressed \ as \ a \ linear \ function \ of \ TEMP)
\end{aligned}
$$

$$
\begin{aligned}
T_{obs} = 20.723 \\\\
P-value= P(T_{245}\ge20.723)*2 \approx0
\end{aligned}
$$

Based on the data above, with a p-value that is less than 0.05 one can
reject the HO (USAGE can not be expressed as a linear function of TEMP).
This leads one to conclude that USAGE can be expressed as a linear
function of TEMP and TEMP is statistically relevant to the model. To
further support this conclusion the 95% confidence interval of 0.153 -
0.185 does not capture 0.

HOUR Individual T-test:

$$
\begin{aligned}
H0: 𝛽3=0\ (USAGE \ CAN \ NOT \  be \ expressed \ as \ a \ linear \ function \ of \ HOUR)\\
\\ HA: 𝛽3\neq\ 0\ (USAGE \ CAN  \  be \ expressed \ as \ a \ linear \ function \ of \ HOUR)
\end{aligned}
$$

$$
\begin{aligned}
T_{obs} = -4.170   \\\\
P-value= P(T_{245}\le-4.170  )*2 =0.0000423
\end{aligned}
$$

Based on the data above, with a p-value that is less than 0.05 one can
reject the HO (USAGE can not be expressed as a linear function of HOUR).
This leads one to conclude that USAGE can be expressed as a linear
function of HOUR and HOUR is statistically relevant to the model. To
further support this conclusion the 95% confidence interval of -0.104 to
-0.0373 does not capture 0.

Suggested model:

$$
USAGE_i = B_0 + \beta_1X_{PROD,i} + \beta_2X_{TEMP,i} + \beta_3X_{HOUR,i} +e_i
$$

Estimates

$$
\widehat{USAGE,𝑖}=5.308+0.040𝑋_{PROD,i}+ 0.169X_{TEMP,𝑖} - 0.070𝑋_{HOUR,𝑖}
$$

$$
\begin{aligned}
S_e = 1.766 \\
Adj\ R_2 = 0.887
\end{aligned}
$$

Interpretation of the model estimates

When all predictor variables are held constant, monthly water usage is
at 5.3075 (gallons/minute)

For a given amount of TEMP and HOUR (these are held constant) an
increase in 1 unit of amount of production (hundreds of cubic) leads to
an increase in 0.0401 gallons/minute of monthly water usage.

For a given amount of PROD and HOUR( these are held constant) an
increase in 1 unit of average monthly temperature (degrees celsius)
leads to an increase of 0.169 gallons/minute of monthly water usage

For a given amount of PROD and TEMP (these are held constant) an
increase in 1 unit of hours shut down for maintenance (hours) leads to
an decrease of 0.070 gallons/minute of monthly water usage.

### 1.4

I am going to run a partial F test with DAYS feature not in the model

Hypothesis

$$
\begin{aligned}
H_0: \ \beta_4 = 0 \ (\text{In the model} \ {USAGE = \beta_0 + \beta_1X_{PROD,i} + \beta_2X_{TEMP,i}+ \beta_3X_{HOUR,i} + \beta_4X_{DAYS,i}}+e_i) \\\\
H_A: \ \beta_4 \neq 0 \ (\text{In the model} \ {USAGE = \beta_0 + \beta_1X_{PROD,i} + \beta_2X_{TEMP,i}+ \beta_3X_{HOUR,i} + \beta_4X_{DAYS,i}+e_i})
\end{aligned}
$$

This is tested against the reduced model below

$$
USAGE_i = B_0 + \beta_1X_{PROD,i} + \beta_2X_{TEMP,i} + \beta_3X_{HOUR,i} + e_i
$$

``` r
#AOV of the full model
aov(water.model)
```

    ## Call:
    ##    aov(formula = water.model)
    ## 
    ## Terms:
    ##                     PROD     TEMP     HOUR     DAYS Residuals
    ## Sum of Squares  4210.334 1813.652   54.263    1.412   763.058
    ## Deg. of Freedom        1        1        1        1       244
    ## 
    ## Residual standard error: 1.768414
    ## Estimated effects may be unbalanced

``` r
SSE.full = 763.058
df.full = 244

#AOV of the reduced model
aov(water.reduced)
```

    ## Call:
    ##    aov(formula = water.reduced)
    ## 
    ## Terms:
    ##                     PROD     TEMP     HOUR Residuals
    ## Sum of Squares  4210.334 1813.652   54.263   764.470
    ## Deg. of Freedom        1        1        1       245
    ## 
    ## Residual standard error: 1.766433
    ## Estimated effects may be unbalanced

``` r
SSE.red = 764.470
df.red = 245

#calc fobs
fobs = (SSE.red - SSE.full) / (df.red - df.full) / (SSE.full/df.full)

#Calc P-value
1 - pf(fobs, 1, df.full)
```

    ## [1] 0.5022547

$$
\begin{aligned}
F_{obs} = 0.4515\\\\
P - value = P(F_{1, \ 245} \ge 0.4515 ) = 0.5022
\end{aligned}
$$

Final suggested model

$$
USAGE_i = B_0 + \beta_1X_{PROD,i} + \beta_2X_{TEMP,i} + \beta_3X_{HOUR,i} 
$$

Based on the data above, with a P-value that is larger than 0.05 one can
fail to reject the HO $\beta_4 = 0$, this leads one to conclude that the
predictor DAYS is statistically irrelevant and should not be included in
the final model.

### 1.5

``` r
confint(water.reduced)
```

    ##                   2.5 %      97.5 %
    ## (Intercept)  4.22519744  6.38982411
    ## PROD         0.03692098  0.04330837
    ## TEMP         0.15310634  0.18526907
    ## HOUR        -0.10419445 -0.03734272

Based on the data above while PROD and HOUR are held constant, one can
say with 95% confidence that for 1 unit increase in average monthly
temperature (degrees Celsius) monthly water usage increases on average
somewhere between 0.153 - 0.185 gallons/minute.

### 1.6

``` r
#Full Model
summary(water.model)$adj.r.squared
```

    ## [1] 0.886658

``` r
summary(water.model)$sigma
```

    ## [1] 1.768414

``` r
#Reduced Model
summary(water.reduced)$adj.r.squared
```

    ## [1] 0.8869118

``` r
summary(water.reduced)$sigma
```

    ## [1] 1.766433

$$
\begin{aligned}
Full \ Model \\
R^{2}_{adj} = 0.8867 \ \ \ S_e = 1.768 \\\\
Reduced \ Model \\
R^{2}_{adj} = 0.8869 \ \ \ S_e = 1.766 \\
\end{aligned}
$$

Interpretation:

The reduced model using the predictors PROD, TEMP and HOUR has the
ability to explain 88.69% of the variation in monthly water usage.

The average difference between the observed monthly water usage and the
reduced models predicted monthly water usage is around 1.766
gallons/minute.

Based on the data above I would suggest using the reduced model for
predictive purposes, this is because the residual standard error is
smaller and the adjusted R squared is larger, this mean the reduced
model can **with less predictors** explain more of the variation in
monthly water usage with less deviation within predictions.

### 1.7

I am going to visualize the relationship between the variables with a 3d
plot to get a better understanding of if there is interactions
occurring.

``` r
#plot_ly(water.df, x = ~PROD, y = ~TEMP, z = ~USAGE, color = "purple", type = "scatter3d")

#plot_ly(water.df, x = ~PROD, y = ~USAGE, z = ~HOUR, color = "purple", type = "scatter3d")

#plot_ly(water.df, x = ~TEMP, y = ~USAGE, z = ~HOUR, color = "purple", type = "scatter3d")
```

There appears to be a contour to the 3d plot with PROD and TEMP, this
indicates that PROD and TEMP may have an interaction.

The 3d plot with HOUR and PROD has a less clear interaction based on the
shape of the plot, I will test if there is an interaction between these
variables still.

The 3d plot with TEMP and HOUR shows a relative flat plane and I would
suspect there is no interaction occurring between these variables, this
will still be worth investigating via a individual T-test

``` r
#create interaction model that test all interactions
int.model = lm(USAGE ~ (PROD + TEMP + HOUR)^2, data = water.df)
summary(int.model)
```

    ## 
    ## Call:
    ## lm(formula = USAGE ~ (PROD + TEMP + HOUR)^2, data = water.df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.1941 -0.3165 -0.0502  0.2755  7.0985 
    ## 
    ## Coefficients:
    ##                Estimate  Std. Error t value            Pr(>|t|)    
    ## (Intercept) 12.94151858  0.71132725  18.193 <0.0000000000000002 ***
    ## PROD        -0.00364190  0.00256487  -1.420               0.157    
    ## TEMP        -0.02388739  0.02128862  -1.122               0.263    
    ## HOUR        -0.23403414  0.02512308  -9.316 <0.0000000000000002 ***
    ## PROD:TEMP    0.00118911  0.00006932  17.154 <0.0000000000000002 ***
    ## PROD:HOUR    0.00077670  0.00007820   9.933 <0.0000000000000002 ***
    ## TEMP:HOUR    0.00076004  0.00076827   0.989               0.324    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9867 on 242 degrees of freedom
    ## Multiple R-squared:  0.9656, Adjusted R-squared:  0.9647 
    ## F-statistic:  1131 on 6 and 242 DF,  p-value: < 0.00000000000000022

``` r
#CI for full interaction model
confint(int.model)
```

    ##                     2.5 %        97.5 %
    ## (Intercept) 11.5403354057 14.3427017615
    ## PROD        -0.0086942159  0.0014104222
    ## TEMP        -0.0658220337  0.0180472461
    ## HOUR        -0.2835219697 -0.1845463106
    ## PROD:TEMP    0.0010525626  0.0013256584
    ## PROD:HOUR    0.0006226682  0.0009307386
    ## TEMP:HOUR   -0.0007532988  0.0022733871

$$
\begin{aligned}
Fulll \ Interaction \ Model \\
USAGE_i = \beta_0 + \beta_1X_{PROD,i} + \beta_2X_{TEMP,i} + \beta_3X_{HOUR,i} + \beta_4(X_{PROD,i}X_{TEMP_1}) + \\\beta_5(X_{PROD,i}X_{HOUR,i}) + \beta_6(X_{TEMP,i}X_{HOUR,i}) + e_i
\end{aligned}
$$

I will perform a fill model F test to see if USAGE can be expressed as a
linear function of at least one of the predictor features

$$
\begin{aligned}
H_0: \ \beta_1=\beta_2=\beta3 = \beta4=\beta5=\beta_6 = 0\ \\(\text{USAGE can not be expressed as a linear function of any of the predictor variables}) \\\\
H_A: \ \beta_1=\beta_2=\beta3 = \beta4=\beta5=\beta_6 \ne 0\ \\(\text{USAGE can be expressed as a linear function of any of at least one of the predictor variables}) 
\end{aligned}
$$

$$
\begin{aligned}
F_{obs} =  1131 \\\\
P-value = P(F_{6,\ 242}\ge1131) \approx0
\end{aligned}
$$

Based on the data above, with a p-value that is less than 0.05 one can
reject the HO (USAGE can not be expressed as a linear function of any of
the predictor variables). This leads one to conclude that USAGE can be
expressed as a linear function of at least one of the six predictor
variables.

I am going to perform a individual T-test on each of the interaction
terms to test if they are statistically relevant.

PROD:TEMP individual T-test

$$
\begin{aligned}
H_0: B_4 = 0\ (\text{USAGE can not be expressed as a linear function of the interaction between PROD * TEMP}) \\\\
H_A: B_4 \neq 0\ (\text{USAGE can be expressed as a linear function of the interaction between PROD * TEMP})
\end{aligned}
$$

$$
\begin{aligned}
T_{obs} = 17.154 \\
P-Value = P(T_{242}\ge17.154) *2 \ \approx0
\end{aligned}
$$

Based on the data above, with a P-value that is less than 0.05 one can
reject the HO (USAGE can not be expressed as a linear function of the
interaction between PROD \* TEMP), this leads one to conclude that USAGE
can be expressed as a linear function of the interaction of PROD and
TEMP and the interaction is statistically relevant and should stay
within the model. This conclusion is further supported with a confidence
interval of 0.0010 - 0.0013 that does not capture 0.

PROD:HOUR individual T-test

$$
\begin{aligned}
H_0: B_5 = 0\ (\text{USAGE can not be expressed as a linear function of the interaction between PROD * HOUR}) \\
H_A: B_5 \ \neq 0\ (\text{USAGE can be expressed as a linear function of the interaction between PROD * HOUR})
\end{aligned}
$$

$$
\begin{aligned}
T_{obs} = 9.933 \\
P-Value = P(T_{244}\ge9.933) *2 \ \approx0
\end{aligned}
$$

Based on the data above, with a P-value that is less than 0.05 one can
reject the HO (USAGE can not be expressed as a linear function of the
interaction between PROD \* HOUR), this leads one to conclude that USAGE
can be expressed as a linear function of the interaction PROD and HOUR
and the interaction is statistically relevant and should stay within the
model. This conclusion is further supported with a confidence interval
of 0.0006 - 0.0009 that does not capture 0.

TEMP:HOUR individual T-test

$$
\begin{aligned}
H_0: B_6 = 0\ (\text{USAGE can not be expressed as a linear function of the interaction between TEMP * HOUR}) \\
H_A: B_6 \ \neq 0\ (\text{USAGE can be expressed as a linear function of the interaction between TEMP * HOUR})
\end{aligned}
$$

$$
\begin{aligned}
T_{obs} = 0.989 \\
P-Value = P(T_{244}\geq0.989) *2 = 0.324
\end{aligned}
$$

Based on the data above, with a p-value that is greater than 0.05 one
can fail to reject the HO, this leads one to conclude that USAGE can not
be expressed as a linear function of the interaction between TEMP an
HOUR and the interaction is not statistically relevant and should be
dropped from the model. This conclusion is further supported with a
confidence interval of -0.0007 to 0.0022 that does capture 0.

Based on the models summary table; of note the individual predictor
terms of PROD and TEMP fail the individual T test and both show P values
that are larger than 0.05, but because there interactions are
significant these variables are kept in the model.

I will now make a reduced interaction model and see if the variables
pass the T-test.

``` r
#Make reduced interaction model
int.model2 = lm(USAGE ~ (PROD + TEMP + HOUR + (PROD * TEMP) + (PROD * HOUR)), water.df)
summary(int.model2)
```

    ## 
    ## Call:
    ## lm(formula = USAGE ~ (PROD + TEMP + HOUR + (PROD * TEMP) + (PROD * 
    ##     HOUR)), data = water.df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.1423 -0.3148 -0.0358  0.3029  7.2555 
    ## 
    ## Coefficients:
    ##                Estimate  Std. Error t value            Pr(>|t|)    
    ## (Intercept) 12.42573466  0.48388609  25.679 <0.0000000000000002 ***
    ## PROD        -0.00252883  0.00230482  -1.097               0.274    
    ## TEMP        -0.00473677  0.00885856  -0.535               0.593    
    ## HOUR        -0.21507266  0.01624154 -13.242 <0.0000000000000002 ***
    ## PROD:TEMP    0.00114170  0.00005009  22.795 <0.0000000000000002 ***
    ## PROD:HOUR    0.00078732  0.00007745  10.165 <0.0000000000000002 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9866 on 243 degrees of freedom
    ## Multiple R-squared:  0.9654, Adjusted R-squared:  0.9647 
    ## F-statistic:  1357 on 5 and 243 DF,  p-value: < 0.00000000000000022

``` r
#CI for reduced interaction model
confint(int.model2)
```

    ##                     2.5 %        97.5 %
    ## (Intercept) 11.4725882293 13.3788810907
    ## PROD        -0.0070688117  0.0020111570
    ## TEMP        -0.0221861377  0.0127125909
    ## HOUR        -0.2470648253 -0.1830805043
    ## PROD:TEMP    0.0010430438  0.0012403606
    ## PROD:HOUR    0.0006347554  0.0009398899

$$
\begin{aligned}
Reduced \ Interaction \ Model \\
USAGE_i = \beta_0 + \beta_1X_{PROD,i} + \beta_2X_{TEMP,i} + \beta_3X_{HOUR,i} + \beta_4(X_{PROD,i}X_{TEMP_1}) + \beta_5(X_{PROD,i}X_{HOUR,i}) + e_i
\end{aligned}
$$

PROD:TEMP individual T-test

$$
\begin{aligned}
H_0: B_4 = 0\ (\text{USAGE can not be expressed as a linear function of ther interaction between PROD * TEMP}) \\\\
H_A: B_4 \neq 0\ (\text{USAGE can be expressed as a linear function of ther interaction between PROD * TEMP})
\end{aligned}
$$

$$
\begin{aligned}
T_{obs} = 22.795 \\
P-Value = P(T_{243}>=22.795) *2 \approx0
\end{aligned}
$$

Based on the data above, with a P-value that is less than 0.05 one can
reject the HO (USAGE can not be expressed as a linear function of the
interaction between PROD \* TEMP), this leads one to conclude that USAGE
can be expressed as a linear function of the interaction between PROD
and TEMP and the interaction is statistically relevant and should stay
within the model. This conclusion is further supported with a confidence
interval of 0.0010 - 0.0012 that does not capture 0.

PROD:HOUR individual T-test

$$
\begin{aligned}
H_0: B_5 = 0\ (\text{USAGE can not be expressed as a linear function of the interaction between PROD * HOUR}) \\
H_A: B_5 \ \neq 0\ (\text{USAGE can be expressed as a linear function of the interaction between PROD * HOUR})
\end{aligned}
$$

$$
\begin{aligned}
T_{obs} = 10.165 \\
P-Value = P(T_{243}\ge10.165) *2 \ \approx0
\end{aligned}
$$

Based on the data above, with a P-value that is less than 0.05 one can
reject the HO (USAGE can not be expressed as a linear function of the
interaction between PROD \* HOUR), this leads one to conclude that USAGE
can be expressed as a linear function of the interaction between PROD
and HOUR and the interaction is statistically relevant and should stay
within the model. This conclusion is further supported with a confidence
interval of 0.0006 - 0.0009 that does not capture 0.

Because the interaction term of PROD \* HOUR and PROD \* TEMP all show
statistical relevance in the individual T-test they are justified to
stay within the model. The individual predictors of PROD and TEMP show a
P-value that is larger than 0.05 under the individual T-test but because
there interactions with other variables are statistically relevant they
are kept in the final model.

$$
\begin{aligned}
Reduced \ Model \\
R^{2}_{adj} = 0.8869 \ \ \ S_e = 1.766 \\\\
Reduced \ Interaction \ Model \\
R^{2}_{adj} = 0.9647 \ \ \ S_e = 0.9866 \\
\end{aligned}
$$

The reduced interaction model has the ability to explain 96.47% of the
variation in monthly water usage.

In the reduced interaction model, the average difference between the
observed monthly water usage and the models predicted monthly water
usage is around 0.9866 gallons/minute.

Based on the data above, I would suggest using the reduced interaction
model. This is because the models predictors all show statistical
relevance under the individual T-test and this model has a smaller
residual standard error and adjusted R squared compared to the reduced
model in 1.6. This means the reduced interaction model can explain more
variation in USAGE with less deviation between predicted and observed
values.

Final proposed model:

$$
\begin{aligned}
Reduced \ Interaction \ Model \\
USAGE_i = \beta_0 + \beta_1X_{PROD,i} + \beta_2X_{TEMP,i} + \beta_3X_{HOUR,i} + \beta_4(X_{PROD,i}X_{TEMP_1}) + \beta_5(X_{PROD,i}X_{HOUR,i}) + e_i
\end{aligned}
$$

# Problem 2

- 𝑋𝑡𝑦𝑝𝑒_𝑜𝑓_𝑠𝑐ℎ𝑜𝑜𝑙: the type of school (0 - public, 1 - private)

- 𝑋𝑎𝑣𝑒.𝑡𝑜𝑡𝑎𝑙.𝑠𝑎𝑡 - the mean/average SAT (a college/university entrance
  test) of all students enrolled in the school

- 𝑋𝑟𝑜𝑜𝑚.𝑎𝑛𝑑.𝑏𝑜𝑎𝑟𝑑 - the amount of money a student pays for room and
  board (in 1000s of USD) per year

- 𝑋𝑎𝑛𝑛.𝑡𝑜𝑡𝑎𝑙.𝑐𝑜𝑠𝑡 - the annual total cost of attending the school
  (tuition, living expenses, other associated educational costs)

- 𝑌 - the mean/average debt a student attending the school incurs *per
  year*

``` r
#load data
a1q2.df = read.csv("https://raw.githubusercontent.com/Statman44/Data602/refs/heads/main/univcol.csv")
head(a1q2.df, 4)
```

    ##                   School Type.of.Term Location Type.of.School Average.Total.SAT
    ## 1 ArizonaStateUniversity            1        2              0              1080
    ## 2    BallStateUniversity            1        1              0               985
    ## 3  Cal.StateUniv.-Fresno            1        1              0               955
    ## 4      ClemsonUniversity            1        3              0              1130
    ##   TOEFL.Score Room.and.Board Annual.Total.Cost Average.Indebtedness
    ## 1           0            4.3              12.7               12.900
    ## 2           1            4.0              12.5                8.210
    ## 3           0            5.4              13.1                8.761
    ## 4           1            3.9              12.4                9.983

``` r
#Put in a data frame 
student.df = data.frame(a1q2.df)
```

### 2.1

I will use the summary method to get the estimates

``` r
#Make model
student.model = lm(Average.Indebtedness ~ (Annual.Total.Cost +
                                             Room.and.Board + 
                                             Average.Total.SAT  ), student.df)


summary(student.model)$coef
```

    ##                       Estimate  Std. Error    t value    Pr(>|t|)
    ## (Intercept)       13.944059245 5.161607382  2.7014955 0.008508181
    ## Annual.Total.Cost  0.434913731 0.140015409  3.1061848 0.002664099
    ## Room.and.Board    -0.130058582 0.500192056 -0.2600173 0.795553570
    ## Average.Total.SAT -0.007646138 0.004924538 -1.5526611 0.124658722

$$
\widehat{Average.Indebtedness}_i = 13.944 + 0.434X_{Annual.Total.Cost,i} - 0.130X_{Room.and.Board,i} - 0.007X_{Average.Total.SAT} + e_i
$$

Interpretation

When all predictor variables are held constant, the mean debt a student
incurs per year is at 13.944 (no units reported in the assignment)

For a given amount of Room.and.Board and Average.Total.SAT (these are
held constant) an increase in 1 unit of the annual total cost of
attending the school (tuition, living expenses, other associated
educational costs), leads to an increase in 0.434 of mean debt a student
incurs per year.

For a given amount of Annual.Total.Cost and Average.Total.SAT (these are
held constant) an increase in 1 unit of the amount of money a student
pays for room and board (in \$1000s of USD) per year, leads to decrease
of 0.1300 of mean debt a student incurs per year.

For a given amount of Room.and.Board and Annual.Total.Cost (these are
held constant) an increase in 1 unit of mean/average SAT (a
college/university entrance test) of all students enrolled in the
school, leads to decrease of 0.0076 of mean debt a student incurs per
year.

### 2.2

``` r
aov(student.model)
```

    ## Call:
    ##    aov(formula = student.model)
    ## 
    ## Terms:
    ##                 Annual.Total.Cost Room.and.Board Average.Total.SAT Residuals
    ## Sum of Squares           256.9721         1.0568           36.0067 1135.1236
    ## Deg. of Freedom                 1              1                 1        76
    ## 
    ## Residual standard error: 3.864691
    ## Estimated effects may be unbalanced

$$
SSE = 1135.124
$$

The total variation that is not explained by the model is 1135.14

### 2.3

``` r
#Pull SE from summary
summary(student.model)$sigma
```

    ## [1] 3.864691

$$
S_e = 3.8647
$$

Interpretation:

The average difference between the observed mean debt a student
attending the school incurs per year and the student models predicted
mean debt a student attending the school incurs per year is around
3.8647.

### 2.4

``` r
summary(student.model)$adj.r.squared
```

    ## [1] 0.1743879

$$
R^2_{adj} = 0.1744
$$

Interpretation:

Based on the data above, the student model with its current predictors
(Annual.Total.Cost, Room.and.Board, Average.Total.SAT) has the ability
to explain 17.44% of the variation in Y (The mean/average debt a student
attending the school incurs *per year).*

### 2.5

``` r
#get aov table data
aov(student.model)
```

    ## Call:
    ##    aov(formula = student.model)
    ## 
    ## Terms:
    ##                 Annual.Total.Cost Room.and.Board Average.Total.SAT Residuals
    ## Sum of Squares           256.9721         1.0568           36.0067 1135.1236
    ## Deg. of Freedom                 1              1                 1        76
    ## 
    ## Residual standard error: 3.864691
    ## Estimated effects may be unbalanced

$$
\begin{array}{lccccc}
{\rm Source\:\:of\:\:Variation}  & \text{Sum of Squares}      &  df   &   \text{Mean Square}   & F_{Obs}  \\
{\rm Regression}                 &  294.0356                   &  3    & 98.01187               &  98.01187/14.93584 = 6.562193\\
{\rm Unexplained/Error}          &  1135.1236                   & 80 - 3 - 1 = 76    & 14.93584                &  \\
\hline
{\rm Total}                      &  1429.159                 & 80 - 1 = 79    &                        & 
\end{array}
$$

I will now perform a full model F-test to see if Average.Indebtness can
be expressed as a linear function of at least one of the predictor
variables in the current model.

$$
\begin{aligned}
H0: 𝛽1=𝛽2=𝛽3=0 \ (Average.Indebtedness \ CAN \ NOT \  be \ expressed \\\ as \ a \ linear \ function \ of \ at \ least \ ONE \ of \ the \ three \ predictors) \\\\
HA: \text{At least one}\ \beta_i \neq0: \ (Average.Indebtedness \ CAN \ be \ expressed \ \\\ as \ a \ linear \ function \ of \ at \ least \ ONE \ of \ the \ three \ predictors)
\end{aligned}
$$

``` r
#input variables from the anova table
fob = 6.562193
df = 76
p = 3

#calc p 
1 - (pf(fob, p, df))
```

    ## [1] 0.0005271545

$$
\begin{aligned}
F_{obs} = 6.562 \\ 
P - value = P(F_{3, \ 76} \ge 6.562) = 0.0005272
\end{aligned}
$$

Based on the data above, with a P value that is less than 0.05 one can
reject the HO ( Average.Indebtness can not be expressed as a linear
function of at least on of the three predictors). This leads one to
conclude that Average.Indebtness can be expressed as a linear function
of at least one of the predictors (ann.total.cost, room.and.board,
ave.total.sat).

### 2.6

$$
\begin{aligned}
H_0:\ B_1 = 0\ ( An \ increase\ in \ 1000 \ of \ X_{ann.total.cost} \ does\ not \ lead \ to \ an \ increase \ in \ X_{Average.Indebtness}) \\
H_A: \ B_1 \ >0\ (An \ increase\ in \ 1000 \ of \ X_{ann.total.cost} \ does \ lead \ to \ an \ increase \ in \ X_{Average.Indebtness})
\end{aligned}
$$

``` r
#grab test stat and df from summary table
t = 3.106
df = 76

#calc right tail p value
2 * (1 - pt(t, df))
```

    ## [1] 0.002665575

$$
\begin{aligned}
T_{obs} = 3.106 \\
P-value(T_{76} \ge3.106)*2 = 0.002665575
\end{aligned}
$$

Based on the data above ,with a P-value that is less than 0.05 one can
reject the HO (For a given Room.and.Board, and Average.Total.SAT, a 1000
dollar increase in Average.Total.Cost does not lead to an increase in
mean student debt ). This leads one to conclude that a 1000 dollar
increase in Average.Total.Cost does lead to an increase in mean student
debt.

### 2.7

I am going to want to perform a individual T test and then compare Se
and adjusted R squared between the interaction model and the original
model to see if this interaction should be maintained in the model.

Interaction model:

$$
\begin{aligned}
Average.Indebtedness,i = \beta_0 + \beta_1X{Annual.Total.Cost,i} + \\\beta_2X{Average.Total.SAT} + \beta_3(X_{Annual.Total.Cost,i}X_{Average.Total.SAT,i})
\end{aligned}
$$

$$
\begin{aligned}
H_0: B_3 = 0\ (\text{Average.Indebtedness can not be expressed as a LF of Annual.Total.Cost*Average.Total.SAT }) \\\\
H_A:  B_3 \ \neq 0\ (\text{Average.Indebtedness can be expressed as a LF of Annual.Total.Cost*Average.Total.SAT })
\end{aligned}
$$

``` r
#Make interaction model
student.int = lm(Average.Indebtedness ~ (Annual.Total.Cost + Average.Total.SAT + (Annual.Total.Cost * Average.Total.SAT)), student.df)

#get estimate and std error
summary(student.int)$coef
```

    ##                                          Estimate    Std. Error   t value
    ## (Intercept)                         -21.097958301 16.8518958958 -1.251963
    ## Annual.Total.Cost                     2.046624207  0.7833516500  2.612651
    ## Average.Total.SAT                     0.022662840  0.0148927371  1.521738
    ## Annual.Total.Cost:Average.Total.SAT  -0.001397495  0.0006634881 -2.106285
    ##                                       Pr(>|t|)
    ## (Intercept)                         0.21442255
    ## Annual.Total.Cost                   0.01082358
    ## Average.Total.SAT                   0.13222484
    ## Annual.Total.Cost:Average.Total.SAT 0.03848090

``` r
#calc tobs
tob = (-0.001397495 - 0) / (0.0006634881)
df = 76

#calc p-value
2 * (pt(tob, df))
```

    ## [1] 0.03848088

``` r
#get supporting confidence interval
confint(student.int)
```

    ##                                             2.5 %         97.5 %
    ## (Intercept)                         -54.661417778 12.46550117501
    ## Annual.Total.Cost                     0.486444182  3.60680423198
    ## Average.Total.SAT                    -0.006998616  0.05232429664
    ## Annual.Total.Cost:Average.Total.SAT  -0.002718946 -0.00007604379

$$
\begin{aligned}
T_{obs} = -2.106  \\
P-Value = P(T_{76}<=-2.106 ) *2 = < 0.0385
\end{aligned}
$$

Based on the data above, with a P-value that is less than 0.05 one can
reject the HO (Average.Indebtedness can not be expressed as a linear
function of the interaction between
Annual.Total.Cost\*Average.Total.SAT).

This leads one to conclude that Average.Indebtedness can be expressed as
a linear function of the interaction between
Annual.Total.Cost:Average.Total.SAT. To further support this conclusion,
the confidence interval for the interaction variables
Annual.Total.Cost:Average.Total.SAT does not capture 0 (-0.0027 to
-0.00007).

Compare key metrics

``` r
#full model
summary(student.model)$sigma
```

    ## [1] 3.864691

``` r
summary(student.model)$adj.r.squared
```

    ## [1] 0.1743879

``` r
#interaction model
summary(student.int)$sigma
```

    ## [1] 3.758272

``` r
summary(student.int)$adj.r.squared
```

    ## [1] 0.2192302

$$
\begin{aligned}
Original \ Model \\
S_e = 3.864691 \\
R^2_{adj} = 0.1743879 \\\\
Interaction \ Model \\
S_e = 3.758272 \\
R^2_{adj} = 0.2192302
\end{aligned}
$$

Based on the data above, The interaction model with its current
predictors (Annual.Total.Cost, average.Total.SAT,
Annual.Total.Cost:Average.Total.SAT) has the ability to explain 21.92%
of the variation in Average.Indebtedness. The difference between the
observed values in Average.Indebtedness and the interaction models
predictions of Average.Indebtedness is on average 3.76. Because of the
results of the individual T-test and because the interaction model can
explain more of the variation in Average.Indebtedness with less average
error in predictions, I would conclude that the interaction term should
be considered in the final model.

### 2.8

Because the interaction model has a larger adjusted R squared and
smaller RMSE I will use this model.

``` r
#make a new df for predictive data
newdata = data.frame(Annual.Total.Cost = 15.5,
                     Room.and.Board = 5.5,
                     Average.Total.SAT = 1310)

predict(student.int, newdata, interval = 'predict', level = 0.95)
```

    ##       fit      lwr      upr
    ## 1 11.9369 3.957384 19.91642

Based on the data above, one can say with 95% confidence that for the
given parameters (Annual.Total.Cost = 15500, Room.and.Board = 5500,
Average.Total.SAT = 1310) student debt is somewhere between 3957.38 -
19916.42 (USD).

### 2.9

``` r
#make a new df for predictive data
newdata = data.frame(Annual.Total.Cost = 14.5,
                     Room.and.Board = 4.7,
                     Average.Total.SAT = 1105)

predict(student.int, newdata, interval = 'conf', level = 0.95)
```

    ##        fit     lwr      upr
    ## 1 11.22917 10.0217 12.43664

Based on the data above, one can say with 95% confidence that for the
given parameters (Annual.Total.Cost = 14500, Room.and.Board = 4700,
Average.Total.SAT = 1105) average student debt is somewhere between
10021.7 - 12436.64 USD.

# Problem 3

### 3.1

``` r
#load data
turbine=read.csv("https://raw.githubusercontent.com/Thuntida/dataset/refs/heads/main/TURBINE.csv")

#make data frame
turbine.df = data.frame(turbine)

#make model to predict heatrate
turbine.model = lm( HEATRATE~ (RPM + 
                                 INLET.TEMP + 
                                 EXH.TEMP + 
                                 CPRATIO + 
                                 AIRFLOW ), turbine.df )

summary(turbine.model)$coef
```

    ##                   Estimate   Std. Error     t value
    ## (Intercept) 13614.46078214 870.01293672 15.64857281
    ## RPM             0.08878591   0.01391227  6.38184400
    ## INLET.TEMP     -9.20087316   1.49919571 -6.13720616
    ## EXH.TEMP       14.39385268   3.46095001  4.15893112
    ## CPRATIO         0.35190426  29.55567961  0.01190649
    ## AIRFLOW        -0.84795834   0.44211432 -1.91796171
    ##                                    Pr(>|t|)
    ## (Intercept) 0.00000000000000000000004982409
    ## RPM         0.00000002643092638462019771744
    ## INLET.TEMP  0.00000006864479550734743137242
    ## EXH.TEMP    0.00010164901885495401779278823
    ## CPRATIO     0.99053907955637054971731458863
    ## AIRFLOW     0.05980038140120977341318564413

$$
\begin{aligned}
HEATRATE_i=B_0+B_1𝑋_{1RPM,i}+ B_2X_{2\ INLET.TEMP,𝑖} + \\ B_3𝑋_{3\ EXH.TEMP,𝑖} + B_4X_{4CPRATIOi} \ B_5X_{5AIRFLOW}  + e_i \\\\
\widehat{HEATRATE_i}=13614.461+0.0888  𝑋_{1RPM,i}-9.200X_{2\ INLET.TEMP,𝑖} + \\14.394𝑋_{3\ EXH.TEMP,𝑖} + 0.352X_{4CPRATIOi} \ -0.848X_{5AIRFLOW}  + e_i
\end{aligned}
$$

### 3.2

Going to perform a full model F-test

$$
\begin{aligned}
H0: 𝛽1=𝛽2=𝛽3=𝛽4=𝛽5=0 \ (HEATRATE \ CAN \ NOT \  be \ \\expressed \ as \ a \ linear \ function \ of \ at \ least \ ONE \ of \ these \ five \ predictors) 
\\\\
HA: \text{At least one}\ \beta_i \neq0:\ (HEATRATE \ CAN \ be \ \\expressed \ as \ a \ linear \ function \ of \ at \ least \ ONE \ of \ these \ five \ predictors)
\end{aligned}
$$

``` r
aov(turbine.model)
```

    ## Call:
    ##    aov(formula = turbine.model)
    ## 
    ## Terms:
    ##                       RPM INLET.TEMP  EXH.TEMP   CPRATIO   AIRFLOW Residuals
    ## Sum of Squares  119598530   26893467   7784225      4623    774427  12841935
    ## Deg. of Freedom         1          1         1         1         1        61
    ## 
    ## Residual standard error: 458.8284
    ## Estimated effects may be unbalanced

``` r
#residual data
SSR = 119598530  +  26893467 +   7784225  +  4623   + 774427
MSR = SSR / 5

#unexplained data
SSE = 12841935
MSE = SSE / 61

#calc FOB
fob = MSR / MSE
df = 61
p = 5

#calc P value
1 - (pf(fob, p, df))
```

    ## [1] 0

$$
\begin{aligned}
F_{obs} = 147.30 \\ 
P - value = P(F_{5, \ 61} >= 147.30) \approx 0
\end{aligned}
$$

Based on the data above, with a P-value that is less than 0.01 one can
reject the HO (HEATRATE can not be expressed as a linear function of at
least one of the predictor variables). This leads one to conclude that
at least one of the predictors can be expressed as a linear function of
HEARTRATE.

### 3.3

I will run a individual T-test on CPRATIO and AIRFLOW and then if the
model is reduced I will perform a partial F-test.

CPRATIO:

$$
\begin{aligned}
H_0: B_{4} = 0\ (HEARTRATE\ can \ not \ be \ expressed \ as \ a \ linear \ function \ of \ CPRATIO) \\
H_A: B_{4} \neq\  0\ (HEARTRATE\ can \ be \ expressed \ as \ a \ linear \ function \ of \ CPRATIO)
\end{aligned}
$$

``` r
#get coefficents
summary(turbine.model)$coef
```

    ##                   Estimate   Std. Error     t value
    ## (Intercept) 13614.46078214 870.01293672 15.64857281
    ## RPM             0.08878591   0.01391227  6.38184400
    ## INLET.TEMP     -9.20087316   1.49919571 -6.13720616
    ## EXH.TEMP       14.39385268   3.46095001  4.15893112
    ## CPRATIO         0.35190426  29.55567961  0.01190649
    ## AIRFLOW        -0.84795834   0.44211432 -1.91796171
    ##                                    Pr(>|t|)
    ## (Intercept) 0.00000000000000000000004982409
    ## RPM         0.00000002643092638462019771744
    ## INLET.TEMP  0.00000006864479550734743137242
    ## EXH.TEMP    0.00010164901885495401779278823
    ## CPRATIO     0.99053907955637054971731458863
    ## AIRFLOW     0.05980038140120977341318564413

``` r
#calc tobs
tobs = (0.35190426 - 0) / (29.55567961)
df = 61

#calc p-value
2 * (1 - (pt(tobs, df)))
```

    ## [1] 0.9905391

$$
\begin{aligned}
T_{obs} = 0.012 \\ 
P - value = P(T_{61} >= 0.012)*2 = 0.990539
\end{aligned}
$$

Based on the data above, with a P-value that is larger than 0.06 one can
fail to reject the HO (HEARTRATE can not be expressed as a linear
function of CPRATIO). this leads one to conclude that HEATRATE cannot be
expressed as a linear function of CPRATIO and the predictor CPRATIO is
statistically irrelevant to the model.

I will Now make a reduced model without CPRATIO and perform a partial F
test

Full model:

$$
HEATRATE_i=B_0+B_1𝑋_{1RPM,i}+ B_2X_{2\ INLET.TEMP,𝑖} + B_3𝑋_{3\ EXH.TEMP,𝑖} + B_4X_{4CPRATIOi} \ B_5X_{5AIRFLOW}  + e_i
$$

Proposed reduced model:

$$
HEATRATE_i=B_0+B_1𝑋_{1RPM,i}+ B_2X_{2\ INLET.TEMP,𝑖} + B_3𝑋_{3\ EXH.TEMP,𝑖} +\ B_4X_{4AIRFLOW}  + e_i
$$

Partial F-test.

$$
\begin{aligned}
H_0: \beta_4 = 0\ (\text{In the model}\ HEATRATE_i=B_0+B_1𝑋_{1RPM,i}+ B_2X_{2\ INLET.TEMP,𝑖} + B_3𝑋_{3\ EXH.TEMP,𝑖} + \\
B_4X_{4CPRATIOi} \ B_5X_{5AIRFLOW}  + e_i \\\\
H_a: \beta_4 \neq 0\ (\text{In the model}\ HEATRATE_i=B_0+B_1𝑋_{1RPM,i}+ B_2X_{2\ INLET.TEMP,𝑖} + B_3𝑋_{3\ EXH.TEMP,𝑖} + \\ B_4X_{4CPRATIOi} \ B_5X_{5AIRFLOW}  + e_i 
\end{aligned}
$$

``` r
#make the reduced model
turbine.red.cpr = lm( HEATRATE~ (RPM + 
                                   INLET.TEMP + 
                                   EXH.TEMP + 
                                   AIRFLOW ), turbine.df )

#reduced model 
aov(turbine.red.cpr)
```

    ## Call:
    ##    aov(formula = turbine.red.cpr)
    ## 
    ## Terms:
    ##                       RPM INLET.TEMP  EXH.TEMP   AIRFLOW Residuals
    ## Sum of Squares  119598530   26893467   7784225    779021  12841965
    ## Deg. of Freedom         1          1         1         1        62
    ## 
    ## Residual standard error: 455.1137
    ## Estimated effects may be unbalanced

``` r
SSE.red = 12841965
df.red = 62

#full model
aov(turbine.model)
```

    ## Call:
    ##    aov(formula = turbine.model)
    ## 
    ## Terms:
    ##                       RPM INLET.TEMP  EXH.TEMP   CPRATIO   AIRFLOW Residuals
    ## Sum of Squares  119598530   26893467   7784225      4623    774427  12841935
    ## Deg. of Freedom         1          1         1         1         1        61
    ## 
    ## Residual standard error: 458.8284
    ## Estimated effects may be unbalanced

``` r
SSE.full = 12841935
df.full = 61

#calc fobs
fobs = (SSE.red - SSE.full) / (df.red - df.full) / (SSE.full/df.full) #0.0001425019

#calc p value
1 - (pf(fobs, 1, df.full))
```

    ## [1] 0.9905145

$$
\begin{aligned}
F_{obs} = 0.0001425 \\\\
P-value(F_{61,\ 62}\ge0.0001425) = 0.9905
\end{aligned}
$$

Based on the data above with a P value that is larger than 0.06 one can
fail to reject the HO $\beta_4 = 0$. This leads one to conclude that
$\beta_4 = 0$ and CPRATIO is statistically irrelevant, and should be
dropped from the model.

I will now check if the predictor variables in the reduced model are
statistically relevant.

$$
HEATRATE_i=B_0+B_1𝑋_{1RPM,i}+ B_2X_{2\ INLET.TEMP,𝑖} + B_3𝑋_{3\ EXH.TEMP,𝑖} +\ B_4X_{4AIRFLOW}  + e_i
$$

AIRFLOW individual T-test:

$$
\begin{aligned}
H_0: B_{4\ AIRFLOW} = 0\ (HEARTRATE\ can \ not \ be \ expressed \ as \ a \ linear \ function \ of \ AIRFLOW) \\
H_A: B_{4\ AIRFLOW} \ne\  0\ (HEARTRATE\ can \ be \ expressed \ as \ a \ linear \ function \ of \ AIRFLOW)
\end{aligned}
$$

``` r
#get coefficents
summary(turbine.red.cpr)$coef
```

    ##                   Estimate   Std. Error    t value
    ## (Intercept) 13617.92418593 813.30619025  16.743908
    ## RPM             0.08882334   0.01344256   6.607622
    ## INLET.TEMP     -9.18560522   0.77039672 -11.923214
    ## EXH.TEMP       14.36283051   2.25963132   6.356272
    ## AIRFLOW        -0.84752034   0.43701419  -1.939343
    ##                                     Pr(>|t|)
    ## (Intercept) 0.000000000000000000000001099304
    ## RPM         0.000000010222710217556675456960
    ## INLET.TEMP  0.000000000000000010827481435419
    ## EXH.TEMP    0.000000027582362871992372487779
    ## AIRFLOW     0.057013270531059638546178547358

``` r
#calc tobs
tobs = ( -0.84752034 - 0) / (0.43701419)
df = 62

#calc p-vlaue
2 * (pt(tobs, df))
```

    ## [1] 0.05701327

$$
\begin{aligned}
T_{obs} = -1.939  \\ \\
P - value\ = P(T_{62} \le -1.939 )*2 = 0.0570
\end{aligned}
$$

Based on the data above, with a P-value that is smaller than 0.06 (or
equal to if rounding) one can reject the HO (HEARTRATE can not be
expressed as a linear function of AIRFLOW). This leads one to conclude
that HEARTRATE can be expressed as a linear function of AIRFLOW and the
predictor AIRFLOW is statistically relevant to the model.

RPM individual T-test:

$$
\begin{aligned}
H_0: B_{1\ RPM} = 0\ (HEARTRATE\ can \ not \ be \ expressed \ as \ a \ linear \ function \ of \ RPM) \\
H_A:B_{1\ RPM} \ne\  0\ (HEARTRATE\ can \ be \ expressed \ as \ a \ linear \ function \ of \ RPM)
\end{aligned}
$$

``` r
#get coefficents
summary(turbine.red.cpr)$coef
```

    ##                   Estimate   Std. Error    t value
    ## (Intercept) 13617.92418593 813.30619025  16.743908
    ## RPM             0.08882334   0.01344256   6.607622
    ## INLET.TEMP     -9.18560522   0.77039672 -11.923214
    ## EXH.TEMP       14.36283051   2.25963132   6.356272
    ## AIRFLOW        -0.84752034   0.43701419  -1.939343
    ##                                     Pr(>|t|)
    ## (Intercept) 0.000000000000000000000001099304
    ## RPM         0.000000010222710217556675456960
    ## INLET.TEMP  0.000000000000000010827481435419
    ## EXH.TEMP    0.000000027582362871992372487779
    ## AIRFLOW     0.057013270531059638546178547358

``` r
#calc tobs
tobs = ( 0.08882334 - 0) / (0.01344256)
df = 62

#calc p-vlaue
2 * (1 - (pt(tobs, df)))
```

    ## [1] 0.00000001022275

$$
\begin{aligned}
T_{obs} = 6.608  \\ \\
P - value\ = P(T_{62} \ge 6.608 )*2 = 0.00000001
\end{aligned}
$$

Based on the data above, with a P-value that is smaller than 0.06, one
can reject the HO (HEARTRATE can not be expressed as a linear function
of RPM). This leads one to conclude that HEARTRATE can be expressed as a
linear function of RPM and the predictor RPM is statistically relevant
to the model.

INLET.TEMP individual T test:

$$
\begin{aligned}
H_0: B_{2\ INLET.TEMP } = 0\ (HEARTRATE\ can \ not \ be \ expressed \ as \ a \ linear \ function \ of \ INLET.TEMP ) \\
H_A: B_{2\ INLET.TEMP } \ne\  0\ (HEARTRATE\ can \ be \ expressed \ as \ a \ linear \ function \ of \ INLET.TEMP )
\end{aligned}
$$

``` r
#get coefficents
summary(turbine.red.cpr)$coef
```

    ##                   Estimate   Std. Error    t value
    ## (Intercept) 13617.92418593 813.30619025  16.743908
    ## RPM             0.08882334   0.01344256   6.607622
    ## INLET.TEMP     -9.18560522   0.77039672 -11.923214
    ## EXH.TEMP       14.36283051   2.25963132   6.356272
    ## AIRFLOW        -0.84752034   0.43701419  -1.939343
    ##                                     Pr(>|t|)
    ## (Intercept) 0.000000000000000000000001099304
    ## RPM         0.000000010222710217556675456960
    ## INLET.TEMP  0.000000000000000010827481435419
    ## EXH.TEMP    0.000000027582362871992372487779
    ## AIRFLOW     0.057013270531059638546178547358

``` r
#calc tobs
tobs = ( -9.18560522  - 0) / (0.77039672)
df = 62

#calc p-vlaue
2 * (pt(tobs, df))
```

    ## [1] 0.00000000000000001082748

$$
\begin{aligned}
T_{obs} = -11.923   \\ \\
P - value\ = P(T_{62} \le -11.923  )*2 = 0.00000000000000001
\end{aligned}
$$

Based on the data above, with a P-value that is smaller than 0.06, one
can reject the HO (HEARTRATE can not be expressed as a linear function
of INLET.TEMP). This leads one to conclude that HEARTRATE can be
expressed as a linear function of INLET.TEMP and the predictor
INLET.TEMP is statistically relevant to the model.

EXH.TEMP individual T-test:

$$
\begin{aligned}
H_0: B_{3\ EXH.TEMP   } = 0\ (HEARTRATE\ can \ not \ be \ expressed \ as \ a \ linear \ function \ of \ EXH.TEMP   ) \\
H_A: B_{3\ EXH.TEMP  .TEMP } \ne\  0\ (HEARTRATE\ can \ be \ expressed \ as \ a \ linear \ function \ of \ EXH.TEMP   )
\end{aligned}
$$

``` r
#get coefficents
summary(turbine.red.cpr)$coef
```

    ##                   Estimate   Std. Error    t value
    ## (Intercept) 13617.92418593 813.30619025  16.743908
    ## RPM             0.08882334   0.01344256   6.607622
    ## INLET.TEMP     -9.18560522   0.77039672 -11.923214
    ## EXH.TEMP       14.36283051   2.25963132   6.356272
    ## AIRFLOW        -0.84752034   0.43701419  -1.939343
    ##                                     Pr(>|t|)
    ## (Intercept) 0.000000000000000000000001099304
    ## RPM         0.000000010222710217556675456960
    ## INLET.TEMP  0.000000000000000010827481435419
    ## EXH.TEMP    0.000000027582362871992372487779
    ## AIRFLOW     0.057013270531059638546178547358

``` r
#calc tobs
tobs = ( 14.36283051  - 0) / (2.25963132)
df = 62

#calc p-vlaue
2 * (1-(pt(tobs, df)))
```

    ## [1] 0.00000002758236

$$
\begin{aligned}
T_{obs} = 6.356   \\ \\
P - value\ = P(T_{62} \ge 6.356  )*2 = 0.0000000275
\end{aligned}
$$

Based on the data above, with a P-value that is smaller than 0.06, one
can reject the HO (HEARTRATE can not be expressed as a linear function
of EXH.TEMP). This leads one to conclude that HEARTRATE can be expressed
as a linear function of EXH.TEMP and the predictor EXH.TEMP is
statistically relevant to the model.

Based on the results of the reduced models individual T test the final
proposed reduced model is below.

Final Proposed reduced model

$$
\begin{aligned}
HEATRATE_i=B_0+B_1𝑋_{1RPM,i}+ B_2X_{2\ INLET.TEMP,𝑖} + \\ B_3𝑋_{3\ EXH.TEMP,𝑖} +\ B_4X_{4AIRFLOW}  + e_i \\\\
\widehat{HEATRATE_i}=13617.924+0.089𝑋_{1RPM,i}-9.19 X_{2\ INLET.TEMP,𝑖} + 14.363 \\𝑋_{3\ EXH.TEMP,𝑖} +\ -0.84752 X_{4AIRFLOW}  + e_i
\end{aligned}
$$

### 3.4

I am going to perform backwards elimination to test all possible
interactions and there significance on the reduced model from 3.3

``` r
#make reduced model with all possible interactions 
back.int.turbine = lm(HEATRATE ~ (RPM + INLET.TEMP  + EXH.TEMP  + AIRFLOW)^2, turbine.df)

#set up test
step_mod.back.int = ols_step_backward_p(back.int.turbine, p_val = 0.06, details=TRUE) 
```

    ## Backward Elimination Method 
    ## ---------------------------
    ## 
    ## Candidate Terms: 
    ## 
    ## 1. RPM 
    ## 2. INLET.TEMP 
    ## 3. EXH.TEMP 
    ## 4. AIRFLOW 
    ## 5. RPM:INLET.TEMP 
    ## 6. RPM:EXH.TEMP 
    ## 7. RPM:AIRFLOW 
    ## 8. INLET.TEMP:EXH.TEMP 
    ## 9. INLET.TEMP:AIRFLOW 
    ## 10. EXH.TEMP:AIRFLOW 
    ## 
    ## 
    ## Step   => 0 
    ## Model  => HEATRATE ~ RPM + INLET.TEMP + EXH.TEMP + AIRFLOW + RPM:INLET.TEMP + RPM:EXH.TEMP + RPM:AIRFLOW + INLET.TEMP:EXH.TEMP + INLET.TEMP:AIRFLOW + EXH.TEMP:AIRFLOW 
    ## R2     => 0.948 
    ## 
    ## Initiating stepwise selection... 
    ## 
    ## Step     => 1 
    ## Removed  => RPM:EXH.TEMP 
    ## Model    => HEATRATE ~ RPM + INLET.TEMP + EXH.TEMP + AIRFLOW + RPM:INLET.TEMP + RPM:AIRFLOW + INLET.TEMP:EXH.TEMP + INLET.TEMP:AIRFLOW + EXH.TEMP:AIRFLOW 
    ## R2       => 0.94782 
    ## 
    ## Step     => 2 
    ## Removed  => RPM:INLET.TEMP 
    ## Model    => HEATRATE ~ RPM + INLET.TEMP + EXH.TEMP + AIRFLOW + RPM:AIRFLOW + INLET.TEMP:EXH.TEMP + INLET.TEMP:AIRFLOW + EXH.TEMP:AIRFLOW 
    ## R2       => 0.94651 
    ## 
    ## Step     => 3 
    ## Removed  => INLET.TEMP:EXH.TEMP 
    ## Model    => HEATRATE ~ RPM + INLET.TEMP + EXH.TEMP + AIRFLOW + RPM:AIRFLOW + INLET.TEMP:AIRFLOW + EXH.TEMP:AIRFLOW 
    ## R2       => 0.94437 
    ## 
    ## Step     => 4 
    ## Removed  => RPM:AIRFLOW 
    ## Model    => HEATRATE ~ RPM + INLET.TEMP + EXH.TEMP + AIRFLOW + INLET.TEMP:AIRFLOW + EXH.TEMP:AIRFLOW 
    ## R2       => 0.94244 
    ## 
    ## 
    ## No more variables to be removed.
    ## 
    ## Variables Removed: 
    ## 
    ## => RPM:EXH.TEMP 
    ## => RPM:INLET.TEMP 
    ## => INLET.TEMP:EXH.TEMP 
    ## => RPM:AIRFLOW

``` r
#get summary of model
summary(step_mod.back.int$model)
```

    ## 
    ## Call:
    ## lm(formula = paste(response, "~", paste(c(include, cterms), collapse = " + ")), 
    ##     data = l)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -787.68 -189.26  -22.34  145.15 1307.53 
    ## 
    ## Coefficients:
    ##                       Estimate  Std. Error t value             Pr(>|t|)    
    ## (Intercept)        13603.30921   993.04287  13.699 < 0.0000000000000002 ***
    ## RPM                    0.04578     0.01577   2.902             0.005174 ** 
    ## INLET.TEMP           -12.79883     1.09014 -11.741 < 0.0000000000000002 ***
    ## EXH.TEMP              23.27429     2.90057   8.024      0.0000000000446 ***
    ## AIRFLOW                1.34695     3.49629   0.385             0.701414    
    ## INLET.TEMP:AIRFLOW     0.01613     0.00364   4.432      0.0000402888796 ***
    ## EXH.TEMP:AIRFLOW      -0.04150     0.01087  -3.816             0.000323 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 401.4 on 60 degrees of freedom
    ## Multiple R-squared:  0.9424, Adjusted R-squared:  0.9367 
    ## F-statistic: 163.7 on 6 and 60 DF,  p-value: < 0.00000000000000022

``` r
#make proposed model
turbine.int = lm(HEATRATE ~ (RPM + INLET.TEMP + EXH.TEMP + AIRFLOW + (INLET.TEMP*AIRFLOW ) + (EXH.TEMP*AIRFLOW)), turbine.df)
```

Proposed interaction model

$$
\begin{aligned}
{HEATRATE}=\beta_0+\beta_1𝑋_{1RPM,𝑖} + \beta_2X_{2, INLET.TEMP ,𝑖}+ \\ beta_3X_{3, EXH.TEMP  ,𝑖}+ \beta_4𝑋_{4AIRFLOW,𝑖} + \beta_5X_{5INLET.TEMP:AIRFLOW ,i} +\beta_6X_{6EXH.TEMP:AIRFLOW,i} + e_i
\end{aligned}
$$

I will perform a full model F-test to see if HEATRATE can be expressed
as a linear function of at least one of the predictor variables.

$$
\begin{aligned}
H_0: \ \beta_1=\beta_2=\beta3 = \beta4=\beta5=\beta_6 = 0\ \\(\text{HEATRATE can not be expressed as a linear funtion of any of the predictor variables}) \\\\
H_A: \ \beta_1=\beta_2=\beta3 = \beta4=\beta5=\beta_6 \ne 0\ \\(\text{HEATRATE can be expressed as a linear funtion of at least one of the predictor variables}) 
\end{aligned}
$$

$$
\begin{aligned}
F_{obs} = 163.7 \\\\
P-value = P(F_{6,60}\ge163.7) \approx0
\end{aligned}
$$

With a P-value that is less than 0.06 one can reject the HO (HEATRATE
can not be expressed as a linear funtion of any of the predictor
variables). This leads one to conclude that HEATRATE can be expressed as
a linear function of at least one of the six predictor variables (RPM,
INLET.TEMP, EXH.TEMP, AIRFLOW, INLET.TEMP:AIRFLOW, EXH.TEMP:AIRFLOW ).

I am now going to perform individual T test to test if the individual
predictors are statistically relevant, after this I will be able to
propose a final model.

EXH.TEMP:AIRFLOW T-test:

$$
\begin{aligned}
H_0: \beta_6 =0\ (\text{HEATRATE can not be exppressed as a linear function of EXH.TEMP:AIRFLOW}) \\\\
H_A: \beta_6 \ne0\ (\text{HEATRATE can  be exppressed as a linear function of EXH.TEMP:AIRFLOW})
\end{aligned}
$$

$$
\begin{aligned}
T_{obs} = -3.816  \\\\
P-value = P(T_{60}\le-3.816) = 0.000323
\end{aligned}
$$

Based on the data above, with a P-value that is less than 0.06 one can
reject the HO (HEATRATE can not be expressed as a linear function of
EXH.TEMP:AIRFLOW). This leads one to conclude that HEATRATE can be
expressed as a linear function of EXH.TEMP:AIRFLOW and the interaction
term is statistically relevant.

INLET.TEMP:AIRFLOW T-test:

$$
\begin{aligned}
H_0: \beta_5 =0\ (\text{HEATRATE can not be exppressed as a linear function of INLET.TEMP:AIRFLOW }) \\\\
H_A: \beta_5 \ne0\ (\text{HEATRATE can  be exppressed as a linear function of INLET.TEMP:AIRFLOW })
\end{aligned}
$$

$$
\begin{aligned}
T_{obs} = 4.432  \\\\
P-value = P(T_{60}\ge4.432) = 0.0000402
\end{aligned}
$$

Based on the data above, with a P-value that is less than 0.06 one can
reject the HO (HEATRATE can not be expressed as a linear function of
INLET.TEMP:AIRFLOW). This leads one to conclude that HEATRATE can be
expressed as a linear function of INLET.TEMP:AIRFLOW and the interaction
term is statistically relevant.

RPM T-test:

$$
\begin{aligned}
H_0: \beta_1 =0\ (\text{HEATRATE can not be exppressed as a linear function of RPM }) \\\\
H_A: \beta_1 \ne0\ (\text{HEATRATE can  be exppressed as a linear function of RPM})
\end{aligned}
$$

$$
T_{obs} = 2.902  \\\\
P-value = P(T_{60}\ge2.902) = 0.005174
$$

Based on the data above, with a P-value that is less than 0.06 one can
reject the HO (HEATRATE can not be expressed as a linear function of
RPM). This leads one to conclude that HEATRATE can be expressed as a
linear function of RPM and the interaction term is statistically
relevant.

The individual predictors of INLET.TEMP, EXH.TEMP and AIRFLOW do not
have to be analyzed in the individual T test as there interaction terms
are deemed statistically relevant, so those individual predictors will
be maintained in the model.

What model I would suggest for predicting:

$$
\begin{aligned}
{HEATRATE}=\beta_0+\beta_1𝑋_{1RPM,𝑖} + \beta_2X_{2, INLET.TEMP ,𝑖}+ \beta_3X_{3, EXH.TEMP  ,𝑖}+ \\ \beta_4𝑋_{4AIRFLOW,𝑖} + \beta_5X_{5INLET.TEMP:AIRFLOW ,i} +\beta_6X_{6EXH.TEMP:AIRFLOW,i} + e_i 
\end{aligned}
$$

$$
\begin{aligned}
S_e = 401.4 \\\\
R^2_{asj} = 0.9367
\end{aligned}
$$

I would suggest the final interaction model above for predicting
purposes. What led me to this conclusion was as follows

The model shows that all the predictors are statistically relevant in
relation to HEATRATE. What finally led me to this conclusion is compared
to the reduced model, the interaction model has a smaller residual
standard error and a larger adjusted R squared. This means the
interaction model can explain more variation in HEATRATE with less
average deviation in predictions compared to the reduced model.

### 3.5

``` r
summary(turbine.int)$coef
```

    ##                          Estimate    Std. Error     t value
    ## (Intercept)        13603.30921159 993.042873307  13.6986122
    ## RPM                    0.04577613   0.015772142   2.9023411
    ## INLET.TEMP           -12.79882733   1.090136359 -11.7405747
    ## EXH.TEMP              23.27428987   2.900567894   8.0240459
    ## AIRFLOW                1.34694935   3.496287227   0.3852513
    ## INLET.TEMP:AIRFLOW     0.01613280   0.003639687   4.4324692
    ## EXH.TEMP:AIRFLOW      -0.04149806   0.010874531  -3.8160780
    ##                                        Pr(>|t|)
    ## (Intercept)        0.00000000000000000003966161
    ## RPM                0.00517373411212335241465832
    ## INLET.TEMP         0.00000000000000003479902672
    ## EXH.TEMP           0.00000000004458657466484375
    ## AIRFLOW            0.70141361124674095339770474
    ## INLET.TEMP:AIRFLOW 0.00004028887956533510071632
    ## EXH.TEMP:AIRFLOW   0.00032312898325406426902270

$$
\begin{aligned}
\widehat{HEATRATE}=13603.30+0.0452𝑋_{RPM,𝑖} - 12.79𝑋_{INLET.TEMP ,𝑖}+ 23.27𝑋_{EXH.TEMP,𝑖}+ 1.34𝑋_{AIRFLOW,𝑖} + \\\\0.016X_{INLET.TEMP:AIRFLOW ,i} - 0.041X_{EXH.TEMP:AIRFLOW,i} + e_i
\end{aligned}
$$

Interpretation

When all predictor variables are held constant, HEATRATE is at 13603.30
(kilojoules per kilowatt per hour)

When all other predictors are held constant, an increase in 1 unit of
RPM (revolutions per minute) leads to an increase of 0.045 kilojoules
per kilowatt per hour of HEATRATE

When all other predictors are held constant, an increase in 1 unit of
INLET.TEMP (degrees Celsius) leads to an decrease of 12.79 kilojoules
per kilowatt per hour of HEATRATE

When all other predictors are held constant, an increase in 1 unit of
EXH.TEMP (degrees Celsius) leads to an increase of 23.27 kilojoules per
kilowatt per hour of HEATRATE

When all other predictors are held constant, an increase in 1 unit of
AIRFLOW (kilograms per second)) leads to an increase of 1.34 kilojoules
per kilowatt per hour of HEATRATE

When all other predictors are held constant, an increase in 1 unit of
the interaction between INLET.TEMP (degrees Celsius) and AIRFLOW
(kilograms per second) leads to an increase of 0.016 kilojoules per
kilowatt per hour of HEATRATE

When all other predictors are held constant, an increase in 1 unit of
the interaction between EXH.TEMP (degrees Celsius) and AIRFLOW
(kilograms per second) leads to an decrease of 0.041 kilojoules per
kilowatt per hour of HEATRATE

### 3.6

``` r
summary(turbine.int)$sigma
```

    ## [1] 401.3508

$$
S_e = 401.35
$$

The average difference between the observed values of heat rate and what
the interaction model predicts for heat rate is 401.35 kilojoules per
kilowatt per hour.

### 3.7

``` r
summary(turbine.int)$adj.r.squared
```

    ## [1] 0.9366789

$$
R^2_{adj} = 0.9366
$$

Based on the data above, the interaction model with its current
predictor features (RPM, INLET.TEMP, EXH.TEMP, AIRFLOW,
INLET.TEMP:AIRFLOW, EXH.TEMP:AIRFLOW) has the ability to explain 93.66%
of the variation in HEATRATE.

### 3.8

``` r
#make df with parameters
newdata = data.frame(
  RPM = 273145,
  INLET.TEMP = 1240,
  EXH.TEMP = 920,
  CPRATIO = 10,
  AIRFLOW = 25
)

predict(turbine.int, newdata, interval = 'predict', level = 0.95 )
```

    ##        fit      lwr     upr
    ## 1 31227.97 24067.74 38388.2

Based on the data above, when the following predictors are RPM = 273145,
INLET.TEMP = 1240, EXH.TEMP = 920, CPRATIO = 10 and AIRFLOW = 25,
HEATRATE will be somewhere between 24067.74 kilojoules per kilowatt per
hour - 38388.2 kilojoules per kilowatt per hour.
