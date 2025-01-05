# Data-Science-on-HMEQ_Loss-with-R

File name: **Box plot.R**

This R script preprocesses the **HMEQ_Loss dataset** through systematic **data cleaning** and **preparation**. It begins with exploratory steps like examining **structure**, **summary statistics**, and **sample rows**, followed by visualizations using **boxplots** and **histograms** to analyze **variability** and **distributions**. **Missing values** in **numeric columns** are imputed using **medians** or **logical replacements**, with imputed values stored in new columns prefixed with **IMP_** and indicators for **missingness** prefixed with **M_**. Original columns are dropped post-imputation. The **categorical REASON column** is one-hot encoded into **binary flags** (**FLAG.R.HomeImp** and **FLAG.R.DebtCon**), and the original column is removed. These steps ensure **missing data** and **categorical variables** are effectively handled, creating a clean, **analysis-ready dataset**.

File name: **R part.R**

This R script demonstrates a sophisticated two-stage machine learning approach for predicting loan defaults and potential loss amounts using **decision trees**. The analysis focuses on creating **classification** and **regression models** that predict the probability of default (**TARGET_BAD_FLAG**) and the potential loss amount (**TARGET_LOSS_AMT**) using various predictive techniques like **Gini impurity**, **entropy**, **ANOVA**, and **Poisson methods**. Key predictive features include **debt-to-income ratio**, **delinquency count**, and **credit history age**, which are crucial in determining default risk. The models compare different **splitting criteria** and evaluation metrics like **ROC curves** and **Root Mean Square Error (RMSE)** to assess predictive performance. Ultimately, the two-step modeling approach provides a more nuanced method for understanding and predicting **financial risk** by separately modeling loss probability and potential loss magnitude.

