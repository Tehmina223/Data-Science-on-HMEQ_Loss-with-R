# Data-Science-on-HMEQ_Loss-with-R

File name: **Box plot.R**

This R script preprocesses the **HMEQ_Loss dataset** through systematic **data cleaning** and **preparation**. It begins with exploratory steps like examining **structure**, **summary statistics**, and **sample rows**, followed by visualizations using **boxplots** and **histograms** to analyze **variability** and **distributions**. **Missing values** in **numeric columns** are imputed using **medians** or **logical replacements**, with imputed values stored in new columns prefixed with **IMP_** and indicators for **missingness** prefixed with **M_**. Original columns are dropped post-imputation. The **categorical REASON column** is one-hot encoded into **binary flags** (**FLAG.R.HomeImp** and **FLAG.R.DebtCon**), and the original column is removed. These steps ensure **missing data** and **categorical variables** are effectively handled, creating a clean, **analysis-ready dataset**.

File name: **R part.R**

This R script demonstrates a sophisticated two-stage machine learning approach for predicting loan defaults and potential loss amounts using **decision trees**. The analysis focuses on creating **classification** and **regression models** that predict the probability of default (**TARGET_BAD_FLAG**) and the potential loss amount (**TARGET_LOSS_AMT**) using various predictive techniques like **Gini impurity**, **entropy**, **ANOVA**, and **Poisson methods**. Key predictive features include **debt-to-income ratio**, **delinquency count**, and **credit history age**, which are crucial in determining default risk. The models compare different **splitting criteria** and evaluation metrics like **ROC curves** and **Root Mean Square Error (RMSE)** to assess predictive performance. Ultimately, the two-step modeling approach provides a more nuanced method for understanding and predicting **financial risk** by separately modeling loss probability and potential loss magnitude.


File name: **Model Validation**

The R script demonstrates a sophisticated two-stage machine learning approach for predicting loan defaults and potential loss amounts using **decision trees**. The analysis employs **classification** and **regression models** to predict default probability (**TARGET_BAD_FLAG**) and loss amount (**TARGET_LOSS_AMT**) using techniques like **Gini impurity** and **entropy**. Key predictive features include **debt-to-income ratio**, **delinquency count**, and **credit history age**, which are crucial in determining financial risk. The models compare different splitting criteria and evaluation metrics like **ROC curves** and **Root Mean Square Error (RMSE)** to assess performance. Ultimately, the Gini split decision tree is recommended due to its consistently higher AUC and more stable predictive capabilities across various datasets.


File name: **Decisiontree&RF&Gradientboosting**

The code implements a comprehensive **machine learning analysis** using three classification and regression models (Decision Tree, Random Forest, and Gradient Boosting) on a financial dataset. The primary objective was to predict **loan default risk** and **loss amount** by comparing model performances through metrics like **Area Under the Curve (AUC)** and **Root Mean Squared Error (RMSE)**. **Random Forest** emerged as the most balanced model, achieving the best overall performance with an AUC of 0.951 and demonstrating robust predictive capabilities across different data splits. Key predictive variables included **LOAN**, **M_DEBTINC**, and **IMP_DELINQ**, which consistently showed significant importance across all model iterations. The analysis provides a nuanced approach to understanding credit risk prediction by leveraging multiple advanced machine learning techniques.


