from collections import Counter
from itertools import chain
import random
from timeit import default_timer as timer
from typing import Dict, List, Tuple
from nqueens_explicit import nqueens

random.seed(0)

def create_initial_configuration(n:int) -> List[int]:
  return random.sample(range(n), n)

def print_board(configuration:List[int], empty=' . ', non_empty=' x ') -> List[List[str]]:
  n = len(configuration)
  return [configuration[i] * empty + non_empty + (n - configuration[i] - 1) * empty for i in range(n)]

def is_same_diagonal(row1:int, col1:int, row2:int, col2:int) -> bool:
  return (row1 - row2) == (col1 - col2) or (row1 + col1) == (row2 + col2)

def num_conflicts(conf:List[int]) -> int:
  n:int = len(conf)
  return 2 * n - len(set([i - v for i,v in enumerate(conf)])) - len(set([i + v for i,v in enumerate(conf)]))

def is_valid_board(conf:List[int]) -> bool:
  return num_conflicts(conf) == 0

def num_conflicts_after_swap(conf:List[int], i:int, j:int) -> int:
  conf[i], conf[j] = conf[j], conf[i] # Swap poistions.
  res = num_conflicts(conf)           # Calculate conflicts.
  conf[i], conf[j] = conf[j], conf[i] # Swap back.
  return res

def next_move_greedy_sgd(conf: List[int], batch_size:int=10, min_num_improvements:int=10) -> Tuple[int, int]:
  n = len(conf)
#  cost = 2 * n - len(diagonal1) - len(diagonal2)
  min_cost:int = 9999999999
  res:Tuple[int,int] = [(-1, min_cost)]
  num_improvements: int = 0
  rows, cols = random.sample(range(n), batch_size), random.sample(range(n), batch_size)
  for i in rows:
    for j in cols:
#      i_minus_j = i - conf[j]
#      i_plus_j = i + conf[j]
#      j_minus_i = j - conf[i]
#      j_plus_i = j + conf[i]
#
#      if (i_minus_j in diagonal1):
#        cost += 1
#      else:
#        cost -= 1
#
#      if (j_minus_i in diagonal1):
#        cost += 1
#      else:
#        cost -= 1
#
#      if (i_plus_j in diagonal2):
#        cost += 1
#      else:
#        cost -= 1
#
#      if (j_plus_i in diagonal2):
#        cost += 1
#      else:
#        cost -= 1
#
#      c = cost
      c = num_conflicts_after_swap(conf, i, j)
      if (c < min_cost):
        if (num_improvements >= min_num_improvements):
          return (i, j, c) # Greedy: Exit immediately if better solution.
        num_improvements += 1
        min_cost = c
        res[0] = (i, j, c)
      elif (c == min_cost):
        res.append((i, j, c))
  if (len(res) > 1):
    random.shuffle(res)
  return res[0]

def solve_nqueens(conf: List[int], num_iterations:int=20, batch_size:int=10, min_num_improvements:int=10,
                  verbose=False):
  res = conf.copy()
  start_time = timer()
  found_solution: bool = False
#  cost = 2 * len(conf) - len(diagonal1) - len(diagonal2)
  for i in range(1, num_iterations + 1):
    start_iteration_time = timer()
    r, c, num_confl = next_move_greedy_sgd(res, batch_size=batch_size, min_num_improvements=min_num_improvements)
    res[r], res[c] = res[c], res[r] # Swap pair.

#    i_minus_j = r - conf[c]
#    i_plus_j = r + conf[c]
#    j_minus_i = c - conf[r]
#    j_plus_i = c + conf[r]
#
#    if (i_minus_j in diagonal1):
#      cost += 1
#    else:
#      cost -= 1
#
#    if (j_minus_i in diagonal1):
#      cost += 1
#    else:
#      cost -= 1
#
#    if (i_plus_j in diagonal2):
#      cost += 1
#    else:
#      cost -= 1
#
#    if (j_plus_i in diagonal2):
#      cost += 1
#    else:
#      cost -= 1

#    num_confl = num_conflicts(res)

    end_iteration_time = timer()
    if (verbose):
      print(i, ': conflicts=', num_confl, ',', round(end_iteration_time - start_iteration_time, 1), 'sec')
    if (num_confl < 1):
      if (verbose):
        print('found solution in steps:', i, ', total time:', round(end_iteration_time - start_time, 1), 'sec')
      found_solution = True
      break
  return res, found_solution

num_queens = 100
initial_conf = create_initial_configuration(num_queens)
solution, is_solution = solve_nqueens(initial_conf, num_iterations=10000, verbose=True,
                                      batch_size=min(100, int(0.5 * len(initial_conf))),
                                      min_num_improvements=3)

is_valid = is_valid_board(solution)
num_conflicts_solution = num_conflicts(solution)
solution_board = print_board(solution)
solution_board

is_valid_initial = is_valid_board(initial_conf)
num_conflicts_initial = num_conflicts(initial_conf)
initial_board = print_board(initial_conf)
initial_board



solution = nqueens(1_000_000)
is_valid = is_valid_board(solution)
num_conflicts_solution = num_conflicts(solution)


diagonal1: Dict[int,int] = {}
diagonal2: Dict[int,int] = {}

def diagonal_conflicts(conf:List[int]) -> Tuple[Dict[int,int], Dict[int,int]]:
  d1 = {k: v for k,v in Counter([i - v for i,v in enumerate(conf)]).most_common()}
  d2 = {k: v for k,v in Counter([i + v for i,v in enumerate(conf)]).most_common()}
  return d1, d2

diagonal1, diagonal2 = diagonal_conflicts(initial_conf)
num_d1_conflicts = len(initial_conf) - len(diagonal1)
num_d2_conflicts = len(initial_conf) - len(diagonal2)

###################################################################################
# Workbench
###################################################################################

def offsets(conf:List[int]):
  res = dict()
  for j, (a, b) in enumerate([(i - v, i + v) for i,v in enumerate(conf)]):
    if a in res:
      res[a].append(j)
    else:
      res[a] = [j]

    if b in res:
      res[b].append(j)
    else:
      res[b] = [j]
  res = {k: sorted(set(v)) for k,v in res.items()}
  return res

def common_offsets(conf:List[int]):
  return Counter([i - v for i,v in enumerate(conf)]).most_common(), Counter([i + v for i,v in enumerate(conf)]).most_common()

def most_conflicted_idx(conf:List[int]) -> List[int]:
  d1, d2 = common_offsets(conf)
  offset_values = offsets(conf)
  most_conflicted_offsets = list(tuple(zip(*d1[:5]))[0]) + list(tuple(zip(*d2[:5]))[0])
  return list(chain(*[offset_values[m] for m in most_conflicted_offsets]))


d1, d2 = common_offsets(initial_conf)
offset_values = offsets(initial_conf)
most_confl_offsets = list(tuple(zip(*d1[:5]))[0]) + list(tuple(zip(*d2[:5]))[0])
most_confl_idx = list(chain(*[offset_values[m] for m in most_confl_offsets]))


a = list(chain(*[random.sample(range(100), 10) for i in range(10)]))
b = Counter(a).most_common()
c = tuple(zip(*b[:5]))[0]

list(set.intersection(set([i for i,_ in d1]), set([i for i,_ in d2])))
len(set([i for i,_ in d2]))

###################################################################################
# Crapyard
###################################################################################

def swap(i:int, j:int, conf:List[int]) -> List[int]:
  conf[i], conf[j] = conf[j], conf[i]
  return conf

