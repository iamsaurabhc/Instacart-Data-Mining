import pandas as pd
import numpy as np
import gensim
import matplotlib.pyplot as plt
from sklearn.decomposition import PCA


#Let's take a look at the orders
orders = pd.read_csv("../Input/orders.csv")
#Lets look at the details of these orders, provided in the Prior and Train Datasets
train_orders = pd.read_csv("../Input/order_products__train.csv")
prior_orders = pd.read_csv("../Input/order_products__prior.csv")
products = pd.read_csv("../Input/products.csv").set_index('product_id')

print("Orders Head")
print(orders.head())
#print(orders.shape)
#print(orders.head())
print(train_orders.head())
print(prior_orders.head())

'''
In a scenario like this algorithm like Word2Vec would come in handy since it’s most concerned
with words that come together in the same context then we can use it to find products
that are usually bought together or products that are similar to each other. To do this
we need to interpret every order as a sentence and every product in an order as a word.
'''

#Transforming Product ID into a string instead of an integer
train_orders["product_id"] = train_orders["product_id"].astype(str)
prior_orders["product_id"] = prior_orders["product_id"].astype(str)
print("Transformation to string Done!")

#let’s group orders into a list of products
train_products = train_orders.groupby("order_id").apply(lambda order: order['product_id'].tolist())
prior_products = prior_orders.groupby("order_id").apply(lambda order: order['product_id'].tolist())
print("Grouping Done!\n")

#Each order is now a list of products and each product is represented by its ID string
print(train_products.head())

#Let’s merge the two dataframes together then find the longest order
sentences = prior_products.append(train_products)
print(sentences)
longest = np.max(sentences.apply(len))
print(longest)

#OrderID having highest number of products
print(sentences[sentences.apply(len) == 145])
print("Merging Done!\n")

#transform the sentences into a numpy array
sentences = sentences.values
print("Sentences to Array Done!")
print(sentences)

#Finally, train the Word2Vec model. We will be using Gensim’s implementation
model = gensim.models.Word2Vec(sentences, size=100, window=longest, min_count=2, workers=4)
print("Training the Model done!")

#Organize Data for visualization
vocab = list(model.wv.vocab.keys())
#print(vocab.head())
print(vocab)

'''
Notice the usage of window = longest in the training of the model.
Since there is no sequence characteristics of the products in an order
-Because each product in an order is independent on the orders that were
chosen before it in the cart- we should have a training window huge enough
to accommodate all the products together or else we imply that products that
are far apart even though they’re in the same cart are dissimilar which is not true.
'''

##VISUALIZATION

'''
The model has now learnt vector representations of each product
(Except for those below min_count) so let’s see what has it learnt
'''

'''
First of all we need to project our vectors onto 2 dimensions so we
can visualize them. We do the projection by using PCA.
'''

#PCA transform the vectors into 2d
pca = PCA(n_components=2)
pca.fit(model.wv.syn0)

#Next we will need two helper functions for visualization, here they are
def get_batch(vocab, model, n_batches):
    output = list()
    for i in range(0, n_batches):
        rand_int = np.random.randint(len(vocab), size=1)[0]
        suggestions = model.most_similar(positive=[vocab[rand_int]], topn=5)
        suggest = list()
        for i in suggestions:
            suggest.append(i[0])
        output += suggest
        output.append(vocab[rand_int])
    return output

def plot_with_labels(low_dim_embs, labels, filename='7Oct_3.png'):
    """From Tensorflow's tutorial."""
    assert low_dim_embs.shape[0] >= len(labels), "More labels than embeddings"
    plt.figure(figsize=(18, 18))  #in inches
    for i, label in enumerate(labels):
        x, y = low_dim_embs[i,:]
        plt.scatter(x, y)
        plt.annotate(label,
                     xy=(x, y),
                     xytext=(5, 2),
                     textcoords='offset points',
                     ha='right',
                     va='bottom')
    plt.savefig(filename)
    plt.show()

'''
Finally we need to random sample our products and visualize these
products and their neighbors. This is the job of the get_batch function
as it will extract the nearest 5 products for each product n_batches products
it will sample. let’s call these functions and visualize their output using Matplotlib
'''

#Visualize a random sample
embeds = []
labels = []
for item in get_batch(vocab, model, n_batches=3):
    embeds.append(model[item])
    labels.append(products.loc[int(item)]['product_name'])

embeds = np.array(embeds)
embeds = pca.fit_transform(embeds)
plot_with_labels(embeds, labels)
