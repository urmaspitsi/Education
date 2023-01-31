import os, random
import numpy as np

from imageio import imread
import matplotlib.pyplot as plt
import matplotlib.cm as cm
from PIL import Image
import PIL.ImageOps

from sklearn import metrics
from sklearn.metrics import accuracy_score, classification_report, confusion_matrix
from sklearn.neural_network import MLPClassifier

from fashion_mnist.utils import mnist_reader

SEED = 0
random.seed(SEED)

X_train, y_train = mnist_reader.load_mnist('fashion_mnist/data/fashion', kind='train')
X_test, y_test = mnist_reader.load_mnist('fashion_mnist/data/fashion', kind='t10k')

X_train = X_train / 255.0
X_test = X_test / 255.0

labels = ['t_shirt_top', 'trouser', 'pullover', 'dress', 'coat', 'sandal', 'shirt', 'sneaker', 'bag', 'ankle_boots']
img_width,img_height = 28,28

clf = MLPClassifier(max_iter=50, hidden_layer_sizes=(10,), random_state=SEED, verbose=True)
clf.fit(X_train, y_train)

preds = clf.predict(X_test)
print(clf.score(X_test, y_test))

accuracy_score(y_test, preds)
conf_matrix = confusion_matrix(y_test, preds)
report = classification_report(y_test, preds)


def read_images_from_folder(full_path):
  valid_extensions = ['.jpg','.gif','.png', '.bmp']
  imgs, names = [], []
  for f in os.listdir(full_path):
    ext = os.path.splitext(f)[1]
    if ext.lower() in valid_extensions:
      im = Image.open(os.path.join(full_path, f)).convert('L')
      im = PIL.ImageOps.invert(im)
      imgs.append(im)
      names.append(f)
  return imgs, names

def predict_external():
  imgs, names = read_images_from_folder('pildituvastus_test')
  for im, n in zip(imgs, names):
    img1 = im.resize((img_width, img_height), Image.ANTIALIAS)
    a = np.reshape(np.asarray(img1), (1, img_width * img_height)) / 255.0
    predicted_class = clf.predict(a)[0]
    probas = np.ravel(clf.predict_proba(a))
    print(f'predicted label: {labels[predicted_class]}, probability: {round(100 * probas[predicted_class], 1)}%, filename: {n}')

predict_external()

imgs, names = read_images_from_folder('pildituvastus_test')

def plot_data(x, y, labels=[]):
  n = int(np.sqrt(len(x)))
  fig, axs = plt.subplots(nrows=n, ncols=n, sharex=True, sharey=True, figsize=(8, 8))
  for i in range(n**2):
    ax = axs[i // n, i % n]
    ax.imshow(x[i], cmap=cm.gray)
    ax.set_title(f'{labels[y[i]]}', fontdict={'fontsize': 12})
    ax.axis('off')
  plt.tight_layout()
  #fig.savefig('test.png', dpi=300)
  plt.show();

#-----------------------------------------------------------------------------------------
# Plot train samples.
#-----------------------------------------------------------------------------------------
plot_data(images, preds, labels)


#-----------------------------------------------------------------------------------------
# Plot external images.
#-----------------------------------------------------------------------------------------
images = np.asarray([np.array(i.resize((img_width, img_height), Image.ANTIALIAS), dtype=np.uint8) for i in imgs])
preds = [clf.predict(np.reshape(images[i], (1, img_width * img_height)) / 255.0)[0] for i in range(images.shape[0])]

plot_data(images, preds, labels)



img1 = imgs[0].resize((img_width, img_height), Image.ANTIALIAS)
img1.show()

a = np.reshape(np.asarray(img1), (1, img_width * img_height)) / 255.0
predicted_class = clf.predict(a)[0]
probas = np.ravel(clf.predict_proba(a))
probas[predicted_class]
labels[predicted_class[0]]


a = np.array(img1, dtype=np.uint8)
plt.imshow(a, cmap=cm.gray)






















