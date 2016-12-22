# Santander Product Recommendation competition on Kaggle
Please download the datasets from the [Kaggle competition page](https://www.kaggle.com/c/santander-product-recommendation) before running the codes.

Edit the path to the files in *model_xgb.R* as well as *prepare_submission_file.py*

**model_xgb.R** imports the raw data, creates the features, trains the model and generates the predictions for the test data.   
**prepare_submission_file.py** creates the final submission file in Kaggle's required format, after removing products already owned by customers.

This model scores ~ 0.03102 on the private LB, ranked 11th.
