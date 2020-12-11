F- Test for Joint Significance 
# heteroskedasticity-robust F-test
linearHypothesis(model, c("size=0", "expenditure=0"), white.adjust = "hc1")

#Not robust to heteroskedacity 
summary(model)$fstatistic

#olsrr is the package 
# Conducts Breusch Pagan Test
model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
ols_test_breusch_pagan(model)

------------------------------------------------------------------------------------------

# compute robust standard errors
rob_se <- diag(vcovHC(model, type = "HC1"))^0.5

# compute robust 95% confidence intervals
rbind("lower" = coef(model) - qnorm(0.975) * rob_se,
      "upper" = coef(model) + qnorm(0.975) * rob_se)
-------------------------------------------------------------------------------------------------     
# draw the robust 95% confidence set for coefficients on size and expenditure 
confidenceEllipse(model, 
                  fill = T,
                  lwd = 0,
                  which.coef = c("size", "expenditure"),
                  main = "95% Confidence Sets",
                  vcov. = vcovHC(model, type = "HC1"),
                  col = "red")
------------------------------------------------------------------------------------------------------                  
 # fit the quadratic Model
quadratic_model <- lm(score ~ income + I(income^2), data = CASchools)
----------------------------------------------------------------------------------
# obtain the model summary
coeftest(quadratic_model, vcov. = vcovHC, type = "HC1")

# estimate a cubic model
cubic_model <- lm(score ~ poly(income, degree = 3, raw = TRUE), data = CASchools)
-----------------------------------------------------------------------------------






------------------------------------------------------------------------------------------------------
quadriatic_model <- lm(score ~ income + I(income^2), data = CASchools)

# set up data for prediction
new_data <- data.frame(income = c(10, 11))

# do the prediction
Y_hat <- predict(quadriatic_model, newdata = new_data)

# compute the difference
diff(Y_hat)

-------------------------------------------------------------------------------------------------------------

new_data <- data.frame(income = c(10, 11, 40, 41))

# predict the outcomes 
Y_hat <- predict(LinearLog_model, newdata = new_data)

# compute the expected difference
Y_hat_matrix <- matrix(Y_hat, nrow = 2, byrow = TRUE)
Y_hat_matrix[, 2] - Y_hat_matrix[, 1]


-------------------------------------------------------------------------------------------------------------------------

bi_model <- lm(score ~ HiSTR * HiEL, data = CASchools)

# print a robust summary of the coefficients
coeftest(bi_model, vcov. = vcovHC, type = "HC1")

-----------------------------------------------------------------------------------------------------------------------


# estimate the fixed effects regression with plm()
fatal_fe_mod <- plm(fatal_rate ~ beertax, 
                    data = Fatalities,
                    index = c("state", "year"), 
                    model = "within")

# print summary using robust standard errors
coeftest(fatal_fe_mod, vcov. = vcovHC, type = "HC1")

------------------------------------------------------------------------------------------------------------------------------

fatal_tefe_mod <- plm(fatal_rate ~ beertax, 
                      data = Fatalities,
                      index = c("state", "year"), 
                      model = "within", 
                      effect = "twoways")

coeftest(fatal_tefe_mod, vcov = vcovHC, type = "HC1")

--------------------------------------------------------------------------------------------------------------------------------

class(fatal_tefe_lm_mod)
coeftest(fatal_tefe_lm_mod, vcov = vcovHC, type = "HC1")[1, ]
class(fatal_tefe_mod)
coeftest(fatal_tefe_mod, vcov = vcovHC, type = "HC1")
--------------------------------------------------------------------------------------------------------------------------------------

Fatalities$drinkagec <- cut(Fatalities$drinkage,
                            breaks = 18:22, 
                            include.lowest = TRUE, 
                            right = FALSE)

# set minimum drinking age [21, 22] to be the baseline level
Fatalities$drinkagec <- relevel(Fatalities$drinkagec, "[21,22]")

# mandadory jail or community service?
Fatalities$punish <- with(Fatalities, factor(jail == "yes" | service == "yes", 
                                             labels = c("no", "yes")))

# the set of observations on all variables for 1982 and 1988
Fatalities_1982_1988 <- Fatalities[with(Fatalities, year == 1982 | year == 1988), ]

--------------------------------------------------------------------------------------------------------------------------------------------
# compute the null model
denyprobit_null <- glm(formula = deny ~ 1, 
                       family = binomial(link = "probit"), 
                       data = HMDA)

# compute the pseudo-R2 using 'logLik'
1 - logLik(denyprobit2)[1]/logLik(denyprobit_null)[1]
#> [1] 0.08594259

------------------------------------------------------------------------------------------------------------------------------------------
Calculate J Statistics Use F Test First to determine relavancy intitally 
cig_iv_OR <- lm(residuals(cig_ivreg_diff3) ~ incomediff + salestaxdiff + cigtaxdiff)

cig_OR_test <- linearHypothesis(cig_iv_OR, 
                               c("salestaxdiff = 0", "cigtaxdiff = 0"), 
                               test = "Chisq")

# compute correct p-value for J-statistic
pchisq(cig_OR_test[2, 5], df = 1, lower.tail = FALSE)
#> [1] 0.02636406

--------------------------------------------------------------------------------------------------------------------------------------
# estimate the model
cig_ivreg2 <- ivreg(log(packs) ~ log(rprice) + log(rincome) | log(rincome) + 
                    salestax, data = c1995)

coeftest(cig_ivreg2, vcov = vcovHC, type = "HC1")
#> 
#> t test of coefficients:
#> 
#>              Estimate Std. Error t value  Pr(>|t|)    
#> (Intercept)   9.43066    1.25939  7.4883 1.935e-09 ***
#> log(rprice)  -1.14338    0.37230 -3.0711  0.003611 ** 
#> log(rincome)  0.21452    0.31175  0.6881  0.494917    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

----------------------------------------------------------------------------------------------------------------------------------
# compute the DID estimator using a linear model
lm(I(y_post - y_pre) ~ TDummy)
