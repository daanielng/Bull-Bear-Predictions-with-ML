Files included:

DATA
y_bb.RData            : Data used to generate models
compiled_pred.RDa     : Models generated by running R Script (model_pred_code.R)

MODELING AND EVALUATION
model_pred_code.R     : R Script to generate the models and predictions
lift_roc_pr-rec_code.R: R Script to calculate Lift and ROC and plot the relevant graphs
portfolio_calculator.R: R Script to model the stock market performance of the different models

func-boost-mod.R      : Helper script to run rolling window Boosted Trees
func-dp.R             : Helper script to run rolling window Dynamic Probit
func-rf-mod.R         : Helper script to run rolling window Random Forests
func-rlasslogit.R     : Helper script to run rolling window Lasso Logit