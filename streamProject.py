import streamlit as st
import pickle 
import pandas as pd

st.write("Please note that for fbs true has been replaced by fbs>120 mg/dl and false by fbs<120 mg/dl")
st.write("Please note that for exang yes has been replaced by exang yes and no by exang no")

st.write("for more information please check the database")

user_input = st.text_input("Please enter antecedent separated by a comma(with no space before or after the comma", "exang yes,male")
st.title(user_input)

# Load frequent itemsets and rules from the pickle file
with open('as_rule_apriori_model.pkl', 'rb') as f:
    frequent_itemsets, rules = pickle.load(f)

#inputData="atypical angina,fbs<120 mg/dl"
inputData=user_input
#convert user input to{'',''}
items = {word.strip() for word in inputData.split(",")} 
#st.title(items)

# Create a pandas Series from the dictionary
#new_transaction_series = pd.Series(new_transaction_dict)

# Check which rules apply to this new transaction
appl_rules = []
for index, rule in rules.iterrows():
    antecedents = rule['antecedents']
    #st.title(antecedents)
    #if {'atypical angina','fbs<120 mg/dl'}.issubset(antecedents):
    if items.issubset(antecedents):
        appl_rules.append(rule)

# Display applicable rules
if appl_rules:
    applicable_rules_df = pd.DataFrame(appl_rules)
    st.title("\nApplicable rules for the new symptoms:")
    #st.title(applicable_rules_df[['antecedents', 'consequents', 'support', 'confidence', 'lift']])
   
    st.dataframe(applicable_rules_df[['antecedents', 'consequents', 'support', 'confidence', 'lift']])
else:
    st.title("No applicable rules found for the new symptoms.")

    #streamlit run streamProject.py
