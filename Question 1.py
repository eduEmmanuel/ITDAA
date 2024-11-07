import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import re
import nltk
from nltk.tokenize import word_tokenize
from nltk.corpus import stopwords
import math
from sklearn.utils import resample
from sklearn.feature_extraction.text import CountVectorizer
from wordcloud import WordCloud
from sklearn.model_selection import train_test_split
import numpy as np

#nltk.download('punkt')
#nltk.download('stopwords')

#!pip install tensorflow
import tensorflow as tf
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, Flatten
from tensorflow.keras.optimizers import Adam
from tensorflow.keras.losses import SparseCategoricalCrossentropy
from tensorflow.keras.metrics import SparseCategoricalAccuracy
from sklearn.metrics import classification_report
from tensorflow.keras.layers import Dense, Dropout

df = pd.read_csv("C:/Users/Emmanuel/Documents/Data Science/ITNPA4-34 Natural Language Processing with Python/Project 1/mbti_datasets.csv",header = 0,sep=",")
df.tail()
#df= df.head(20)# limited df to remove

df.shape

# increased_df = pd.DataFrame(columns=['type', 'posts']) 
# print(increased_df)

### 1.1. 

#1.1. 
#for row in df.itertuples():#loop through df
#     # print(f"type: {row.type}, posts: {row.posts}")
#    #print("in for")
#    posts = row.posts
#    split_posts = posts.split('|||')
#    for post in split_posts:
#        increased_df.loc[len(increased_df)] = [row.type, post]
        
#print(increased_df)

data = [
    {"type": row.type, "posts": post} 
    for row in df.itertuples() 
    for post in row.posts.split('|||')
]

increased_df = pd.DataFrame(data)
print(increased_df)

increased_df.shape

### 1.2.

#1.2.
def perRegexLow(post):
    post = re.sub(r'http[s]?://\S+', '', post)  # Remove URLs

    post = re.sub(r'[^\w\s]', '', post)  # Remove punctuation
    
    #remove special characters and digits
    post = re.sub(r'[^a-zA-Z\s]', '', post) 

    post = re.sub(r'\s+', ' ', post).strip()  # Replace multiple spaces with a single space and strip leading/trailing spaces

    post = post.lower() # to lowercase
    
    return post



c_df = pd.DataFrame(columns=['type', 'posts']) #clean data to be stored here

for row in increased_df.itertuples():#loop through increased_df
    post = row.posts
    c_post = perRegexLow(post)
    if c_post: #post may be empty after url is removed
        c_df.loc[len(c_df)] = [row.type, c_post]
    
#print(c_df)

#expanded_df = expanded_df[~expanded_df['posts'].str.contains(r'http', case=False, na=False)]
#print()

#print(c_df['posts'][4])

print(c_df)

### 1.3.


df_t = pd.DataFrame(columns=['type', 'posts','tokens']) #clean data with tokens column to be stored here

stop_w = set(stopwords.words('english'))#get stop words

for row in c_df.itertuples():#loop through c_df
    post_token = word_tokenize(row.posts)#tokenise
    post_stop_r = [word for word in post_token if word not in stop_w] #remove stop words
   # print(post_token)
    #print(post_stop_r)
    df_t.loc[len(df_t)] = [row.type, row.posts,post_stop_r]

print(df_t)

#1.4.

# Create count plot with grouping by 'Group'
sns.countplot(x='type', data=df_t)
plt.xticks(rotation=90)
plt.title('Count Plot Of All Records Per Personality Type')
# Display the plot
plt.show()

print("here")

### Question 2
### 2.1.


type_counts= df_t['type'].value_counts() # count the no. of times each type appears
print(type_counts)
resample_sz= math.ceil((type_counts.max()/2))
print(resample_sz)
# df_majority = df_t[df_t['type'] == type_counts.idxmax()]
# df_majority.tail()

balanced_df_temp= []
for type_label, type_df in df_t.groupby('type'): #type_df will be subset of dt_t corresponding to the types
    
    if len(type_df) < resample_sz:  # Upsample
       
        type_resampled = resample(type_df, #subset type
                                   replace=True,  # Sample with replacement(sampling method in which, after selecting an item from the dataset, the item is put back and is available to be chosen again in subsequent draws)
                                   n_samples=resample_sz,  # Upsample
                                   random_state=42)
        
    elif len(type_df) > resample_sz:  # Downsample
       
        type_resampled = resample(type_df, 
                                   replace=False,  # Sample without replacement
                                   n_samples=resample_sz,  # Downsample 
                                   random_state=42)
    else:
        # keep as is
        type_resampled = type_df
    
    balanced_df_temp.append(type_resampled)
    
# Combine types
df_balance = pd.concat(balanced_df_temp)

print(df_balance['type'].value_counts())

df_balance.tail(50)

### 2.2.

sns.countplot(x='type', data=df_balance, palette='viridis')
plt.xticks(rotation=90)
plt.title('Count Plot Of All Records Per Personality Type')
plt.show()

### 2.3

#https://www.ibm.com/reference/python/countvectorizer#:~:text=CountVectorizer%20is%20a%20class%20in,of%20word%20or%20token%20counts.
#CountVectorizer is a class in scikit-learn that transforms a collection of text documents into a 
#numerical matrix of word or token counts.

# Join tokens to form sentences
df_balance['to_vectorize'] = df_balance['tokens'].apply(lambda x: ' '.join(x))
df_balance.tail()

# Initialize 
vect = CountVectorizer(
stop_words='english',
lowercase=True,
max_features=50   
)

vect_result=vect.fit_transform(df_balance['to_vectorize'])# captures how often a word occurs(understood by computer)
#print(vect_result)

print(vect_result.toarray())
print(vect.get_feature_names_out())

vect_df = pd.DataFrame(vect_result.toarray(), columns=vect.get_feature_names_out())

# Display the DataFrame
print(vect_df)

# # Sum up the frequency of each word
# word_freq = vect_df.sum(axis=0).to_dict()

# wc = WordCloud(width=800, height=400, background_color='white',max_words=50,colormap='inferno').generate_from_frequencies(word_freq)

# plt.figure(figsize=(10, 6))
# plt.imshow(wc, interpolation='bilinear')
# plt.axis('off')
# plt.show()

### 2.4.

# Split the dataset into training and testing sets
df_train, df_test = train_test_split(vect_df, test_size=0.2, random_state=1234)


### 2.5

#The layers are fully connected(each nueron gets an imput from every prebious neuron)
model = Sequential()#data will flow sequentially .acts as a container for the layers you will add to the model.

model.add(Dense(64, input_shape=(df_train.shape[1],), activation='relu')) #64 neurons. input_shape shows the features (50)
model.add(Dropout(0.3)) 
model.add(Dense(128, activation='relu'))  
model.add(Dense(64, activation='relu')) 
model.add(Dense(32, activation='relu')) 

# Decoding layers (reconstruct data)
model.add(Dense(64, activation='relu'))
model.add(Dense(df_train.shape[1], activation='sigmoid'))  # Output matches input shape

# Compile
model.compile(optimizer='adam', loss='binary_crossentropy', metrics=['mse'])

m_fit = model.fit(df_train, df_train, epochs=20, batch_size=2, validation_data=(df_test, df_test))

# Evaluate the model on reconstruction loss
test_loss, test_mse = model.evaluate(df_test, df_test) #Mean Squared Error (MSE)
print(f"Reconstruction loss: {test_mse:.4f}")

### 2.6. 

pred = model.predict(df_test)
y_pred = np.argmax(pred, axis=1) # Get class with highest probability
y_test_labels = np.argmax(df_test, axis=1) 
report = classification_report(y_test_labels, y_pred)
print(report)

#0.0060
#target_names = vect_df['target'].unique()
#print(target_names)  # Output: ['A' 'B']

