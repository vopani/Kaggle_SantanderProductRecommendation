# file paths (edit the paths before running)
path_train = 'train.csv'
path_preds = 'preds.csv'
path_submission = 'submit.csv'

# loading libraries
import numpy as np
import pandas as pd

# preparing submission file
target_cols = ['ind_cco_fin_ult1','ind_cder_fin_ult1','ind_cno_fin_ult1','ind_ctju_fin_ult1',
			   'ind_ctma_fin_ult1','ind_ctop_fin_ult1','ind_ctpp_fin_ult1','ind_dela_fin_ult1',
			   'ind_ecue_fin_ult1','ind_fond_fin_ult1','ind_hip_fin_ult1','ind_plan_fin_ult1',
			   'ind_pres_fin_ult1','ind_reca_fin_ult1','ind_tjcr_fin_ult1','ind_valo_fin_ult1',
               'ind_nomina_ult1','ind_nom_pens_ult1','ind_recibo_ult1']

last_instance_df = pd.read_csv(path_train, usecols=['ncodpers'] + target_cols)
last_instance_df = last_instance_df.drop_duplicates('ncodpers', keep='last')

cust_dict = {}
target_cols = np.array(target_cols)

for ind, row in last_instance_df.iterrows():
    cust = row['ncodpers']
    used_products = set(target_cols[np.array(row[1:])==1])
    cust_dict[cust] = used_products

del last_instance_df

preds = pd.read_csv(path_preds)

test_id = np.array(preds['ncodpers'])

preds.drop(['ncodpers'], axis=1, inplace=True)
preds = np.argsort(preds, axis=1)
preds = np.fliplr(preds)

final_preds = []

for ind, pred in enumerate(preds):
    cust = test_id[ind]
    top_products = target_cols[pred]
    used_products = cust_dict.get(cust,[])
    new_top_products = []
    for product in top_products:
        if product not in used_products:
            new_top_products.append(product)
        if len(new_top_products) == 7:
            break
    final_preds.append(" ".join(new_top_products))
    
submission = pd.DataFrame({'ncodpers':test_id, 'added_products':final_preds})
submission.to_csv(path_submission, index=False)
