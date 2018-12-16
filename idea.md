Predicting White Wine Quality - Classification
========================================================
author: Nathania
date: 12/14/2018
autosize: true

Objective
========================================================
The app was designed to determine what physicochemical properties affect white wine quality. These properties are:



```r
str(white)
```

```
'data.frame':	4898 obs. of  12 variables:
 $ fixed.acidity       : num  7 6.3 8.1 7.2 7.2 8.1 6.2 7 6.3 8.1 ...
 $ volatile.acidity    : num  0.27 0.3 0.28 0.23 0.23 0.28 0.32 0.27 0.3 0.22 ...
 $ citric.acid         : num  0.36 0.34 0.4 0.32 0.32 0.4 0.16 0.36 0.34 0.43 ...
 $ residual.sugar      : num  20.7 1.6 6.9 8.5 8.5 6.9 7 20.7 1.6 1.5 ...
 $ chlorides           : num  0.045 0.049 0.05 0.058 0.058 0.05 0.045 0.045 0.049 0.044 ...
 $ free.sulfur.dioxide : num  45 14 30 47 47 30 30 45 14 28 ...
 $ total.sulfur.dioxide: num  170 132 97 186 186 97 136 170 132 129 ...
 $ density             : num  1.001 0.994 0.995 0.996 0.996 ...
 $ pH                  : num  3 3.3 3.26 3.19 3.19 3.26 3.18 3 3.3 3.22 ...
 $ sulphates           : num  0.45 0.49 0.44 0.4 0.4 0.44 0.47 0.45 0.49 0.45 ...
 $ alcohol             : num  8.8 9.5 10.1 9.9 9.9 10.1 9.6 8.8 9.5 11 ...
 $ quality             : int  6 6 6 6 6 6 6 6 6 6 ...
```


Data Exploration
========================================================
Do correlation analysis between variables including it's distribution among the data,outlier. Some of the result as follows:
- Volatile acid seems to have a negative impact on the quality of the wine. As volatile acid level goes up, the quality of the wine degrades.Citric acid seems to have a positive correlation with Wine Quality. Better wines have higher Citric Acid.
- Even though weakly correlated, from the decrease in median values of the Chlorides with increase in quality, it seems that lower percent of Chloride seems to produce better wines.
- Low concentration of Free Sulphur Dioxide produces better wine and too high concentration results in lower quality of wine. Better wines seems to have lower densities and sugar and high alcohol.

Correlation Plot 
========================================================
Wine Density usually contain alcohol and sugar, hence we examine the relationship among them




```r
tmp <- white[,c(4,8,11:12)];tmp %>% ggpairs( aes(colour=quality), lower = list(combo=wrap("facethist", binwidth=5)))
```

![plot of chunk unnamed-chunk-4](idea-figure/unnamed-chunk-4-1.png)
Choosing Model
========================================================
From data exploration above, even though some of the variable have linear relationship, some of them aren't. We conduct multiple experiment, at the end, we end up using GLM and Naive Bayes as the classifier. Hence we convert the regression problem into classification problem. During the training:
- GLM, we group the quality as follows: 3-6 (Bad), (>7) Good
- Naive Bayes, we group as follows: 3-4(Bad), 5-6(Average), (7-9)Good

We partition 80% as training, 20% as testing set.
For Naive Bayes, we got 88%, while for GLM 90%. But there might be some overfitting there which is cause by the lack of expertise in this industry.
