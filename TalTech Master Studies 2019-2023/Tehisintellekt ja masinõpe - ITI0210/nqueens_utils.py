import random
from timeit import default_timer as timer
from typing import List, Tuple

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

def solve_greedy_sgd(conf: List[int], num_iterations:int=20, batch_size:int=10, min_num_improvements:int=10,
                  verbose=False):
  res = conf.copy()
  start_time = timer()
  found_solution: bool = False
  for i in range(1, num_iterations + 1):
    start_iteration_time = timer()
    r, c, num_confl = next_move_greedy_sgd(res, batch_size=batch_size, min_num_improvements=min_num_improvements)
    res[r], res[c] = res[c], res[r] # Swap pair.
    end_iteration_time = timer()
    if (verbose):
      print(i, ': conflicts=', num_confl, ',', round(end_iteration_time - start_iteration_time, 1), 'sec')
    if (num_confl < 1):
      if (verbose):
        print('found solution in steps:', i, ', total time:', round(end_iteration_time - start_time, 1), 'sec')
      found_solution = True
      break
  return res, found_solution


