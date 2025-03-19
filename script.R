---
title: "Econometrics 1 - Assignment 2"
author: "15161536 & 15141284"
date: "2025-03-16"
output:
  html_document: default
  pdf_document: default
---

Part 1:

Taking the natural logarithm on both sides:

\[ \ln y = \ln(\beta_1 L^{\beta_2} K^{\beta_3} e^{\varepsilon}) \]

Using logarithm properties:

\[ \ln y = \ln \beta_1 + \beta_2 \ln L + \beta_3 \ln K + \ln e^{\varepsilon} \]

Since $\ln e^{\varepsilon} = \varepsilon$, we get:

\[ \ln y = \ln \beta_1 + \beta_2 \ln L + \beta_3 \ln K + \varepsilon \]

\[ Y^* = \alpha + \beta_2 X_1 + \beta_3 X_2 + \varepsilon \]

First we can define our variables, and then regress on the model above.
```{r}
#a.) 
df<-read.csv("Textile.csv")
L=df$l
Y=df$y
K=df$k
{model=lm(log(Y) ~ log(L) + log(K), df)}
summary(model)
```
#HERE WE INTERPRET THE COEFFICIENTS
Using price elasticity. When Labour is higher by 1%, then, on average, Y is higher by 0.90782%. When K is higher by 1%, then, on average, Y is higher by 0.20703%. Both variables, L and K are significant, given their large t-values and low p-values. 
Coefficients:

To test whether our model is constant returns to scale, we use the null hypothesis:

\[\beta_{2}+\beta_{3}=1 \]

We first define the unrestricted model and calcute the SSR
```{r} 
UR_Model=lm(log(Y) ~ log(L) + log(K), df)
ssrUR <- sum(residuals(UR_Model)^2)
```

Here we define the restricted model, which takes the following form, and calculate the SSR
Implementing the restriction \[\beta_{2}+\beta_{3}=1 \], and rewriting as:
\[\beta_{2}=1-\beta_{3} \], we get the following restricted model:

\[ \ln y = \ln \beta_1 + \beta_2 \ln L + (1-\beta_2) \ln K + \varepsilon \]

\[ \ln y = \ln \beta_1 + \beta_2 \ln L + \ln K - \beta_2 \ln K + \varepsilon \]

\[ \ln y = \ln \beta_1 + \beta_2 (\ln L - \ln K) + \ln K  + \varepsilon \]

```{r}
R_Model=lm(log(Y)-log(K) ~ I(log(L)-log(K)), df)
ssrR <- sum(residuals(R_Model)^2)
```

Now with the information calculated we can form an F-statistic

```{r}
n<-nrow(df)
k <- 3
F_stat<- ((ssrR - ssrUR)/1)/(ssrUR/(n-k))

cat("F Statistic:", F_stat, "\n")
```

Define the quantiles"
```{r}
sl1 <- qf(1 - 0.01, df1 = 1, df2 = n - k)
sl5 <- qf(1 - 0.05, df1 = 1, df2 = n - k)
sl10<- qf(1 - 0.10, df1 = 1, df2 = n - k)

cat("Critical F-value at 1% level:", sl1, "\n")
cat("Critical F-value at 5% level:", sl5, "\n")
cat("Critical F-value at 10% level:", sl10, "\n")
```
Now we can test, to reject or not reject the null hypothesis:
```{r}
if (F_stat > sl1) {
  cat("Reject the null hypothesis at 1% significance level.\n")
} else {
  cat("Fail to reject the null hypothesis at 1% significance level.\n")
}

if (F_stat > sl5) {
  cat("Reject the null hypothesis at 5% significance level.\n")
} else {
  cat("Fail to reject the null hypothesis at 5% significance level.\n")
}

if (F_stat > sl10) {
  cat("Reject the null hypothesis at 10% significance level.\n")
} else {
  cat("Fail to reject the null hypothesis at 10% significance level.\n")
}
```

The feasible Wald test statistic for $H_0: R\beta = r$ is:

\[ W_{R\beta=r} = n (R\hat{\beta} - r)' \left( s^2 R (n^{-1} X'X)^{-1} R' \right)^{-1} (R\hat{\beta} - r) \]

which is asymptotically equivalent to the infeasible statistic under $H_0$:

\[ W_{R\beta=r} \overset{d}{\to} \chi^2(J). \]

Using the restriction \[\beta_{2}+\beta_{3}=1 \] can we define the following vectors, under the null hypothesis

```{r}
R<-matrix(c(0,1,1),nrow=1)
r<-1 
beta_hat <- coef(model)
vcov_matrix <- vcov(model)
```

Now we can plug this into the equation to calculate the statistic:

```{r}
restriction_value <- sum(R * beta_hat)  # R * beta_hat
variance_restriction <- as.numeric(R %*% vcov_matrix %*% t(R))
wald_stat <- (restriction_value - r)^2 / variance_restriction

# Print Wald statistic
cat("Wald Statistic:", wald_stat, "\n")
```

Both the Wald-Statistic and F-Statistic have the same value. Given that the F-statistic and Wald-statisitic are related as follows:

\[ W_{R\beta=r} = \[F_{R\beta=r}]*J

Because the number of restrictions is equal to 1 in this case, it is no surprise these statistics are equivalent.

Part 3: 
```{r}
vcov_matrix <- summary(UR_Model)$cov.unscaled
var_b2 <- vcov_matrix[2, 2]  # Variance of beta2 (log(L))
var_b3 <- vcov_matrix[3, 3]  # Variance of beta3 (log(K))
cov_b2_b3 <- vcov_matrix[2, 3]  # Covariance between beta2 and beta3
corr_b2_b3 <- cov_b2_b3 / sqrt(var_b2 * var_b3)

# Print the correlation
cat("Correlation between beta2 and beta3:", corr_b2_b3, "\n")
```

Part 4:
Using the steps from the instructions, to calculate the statistic for Breusch-Pagan:

```{r}
residuals_squared <- residuals(model)^2

auxilary_model<-lm(residuals_squared ~ log(L) + log(K))
R2 <- summary(auxilary_model)$r.squared

LM_stat <- n*R2

cat("LM Statistic", LM_stat, "\n")
#Here test at 1%,5, and 10%
```

Part 5:

```{r}
aux_model_sq <- lm(residuals_squared ~ log(L) + log(K) + I(log(L)^2) + I(log(K)^2), data=df)
R2_sq <- summary(aux_model_sq)$r.squared
LM_stat_sq <- n * R2_sq
cat("LM Statistic Squared", LM_stat_sq, "\n")
```



Question 2:

Part 1:
