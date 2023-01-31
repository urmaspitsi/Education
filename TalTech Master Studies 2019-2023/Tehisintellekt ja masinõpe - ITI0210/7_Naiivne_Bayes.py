from collections import Counter
from itertools import chain
import math
import os

def stopword(wstr):
  w = wstr.strip()
  if len(w) < 4:
    return True
  elif w.isdigit():
    return True
  else:
    return False

def read_file(file_name):
  res = []
  with open(file_name, encoding="latin-1") as f:
    res = [w.strip() for w in f.read().replace("\n", " ").split(" ") if not stopword(w)]
  return res

def read_dir(dirn):
  res = []
  for fn in os.listdir(dirn):
    res.append(read_file(os.path.join(dirn, fn)))
  return res

def to_flat_list(list_of_lists):
  return list(chain(*list_of_lists))

def unique_words(x):
  return sorted(set(to_flat_list(ham)))

def most_common_words_from_counters(counters, n):
  return sorted(set(list(chain(*[[w for w, _ in c.most_common(n)] for c in counters]))))

ham = read_dir('naive_bayes/enron6/ham')
spam = read_dir('naive_bayes/enron6/spam')

ham_counter = Counter(to_flat_list(ham))
spam_counter = Counter(to_flat_list(spam))

# 1. Dokumentide koguarv: ham/spam.
# 2. Loenda sõnade esinemised eraldi rämpspostis ja tavalistes kirjades.
# 3. Loenda, palju sõnu üldse on ham/spam.
# 4. Loenda mitu unikaalset sõna on (|V|)

num_ham = len(ham)
num_spam = len(spam)
num_all_letters = num_ham + num_spam
num_words_ham = len(to_flat_list(ham))
num_words_spam = len(to_flat_list(spam))
P_ham = num_ham / num_all_letters
P_spam = num_spam / num_all_letters
num_unique_words = len(unique_words(to_flat_list(ham) + to_flat_list(spam)))

def is_spam(words):
  freq_ham = P_ham + sum([math.log((ham_counter[w] + 1) / num_words_ham) for w in words])
  freq_spam = P_spam + sum([math.log((spam_counter[w] + 1) / num_words_spam) for w in words])
  return freq_spam > freq_ham, freq_ham, freq_spam


Kiri_1 = read_file('naive_bayes/test/kiri1.txt')
Kiri_2 = read_file('naive_bayes/test/kiri2.txt')

is_spam1, prob_ham1, prob_spam1 = is_spam(Kiri_1)
is_spam2, prob_ham2, prob_spam2 = is_spam(Kiri_2)

#--------------------------------------------------------------------------------------
# Test on unseen data.
#--------------------------------------------------------------------------------------
ham_test = read_dir('naive_bayes/enron6/ham')
spam_test = read_dir('naive_bayes/enron6/spam')
x_test = ham_test + spam_test
truth = [False for _ in range(len(ham_test))] + [True for _ in range(len(spam_test))]

from sklearn.metrics import accuracy_score, classification_report, confusion_matrix

def predictions(x):
  return [i for i,_,_ in [is_spam(letter) for letter in x]]

preds = predictions(x_test)
accuracy_score(truth, preds)
confusion_matrix5 = confusion_matrix(truth, preds)
report5 = classification_report(truth, preds)

confusion_matrix4 = confusion_matrix(truth, preds)
report4 = classification_report(truth, preds)

# train data.
confusion_matrix6 = confusion_matrix(truth, preds)
report6 = classification_report(truth, preds)
tn, fp, fn, tp = confusion_matrix6.ravel()

#most_common_words = most_common_words_from_counters([ham_counter, spam_counter], 100)
#freq_ham = P_ham + sum([math.log((ham_counter[w] + 1) / num_words_ham) for w in most_common_words])
#freq_spam = P_spam + sum([math.log((spam_counter[w] + 1) / num_words_spam) for w in most_common_words])
