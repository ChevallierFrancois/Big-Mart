import pandas as pd
train = pd.read_csv("train.csv")
test = pd.read_csv("test.csv")


# Determine mean of the output column:
mean_item_sales = train.pivot_table(values='Item_Outlet_Sales',index='Item_Identifier')
#Initialize submission dataframe with ID varaibles
base = test[['Item_Identifier','Outlet_Identifier']]
#Assign outcome variable to mean value by product:
base['Item_Outlet_Sales'] = base.apply(lambda x: mean_item_sales[x['Item_Identifier']],axis=1)
#Export submission:
base.to_csv("submission_baseline2.csv",index=False)