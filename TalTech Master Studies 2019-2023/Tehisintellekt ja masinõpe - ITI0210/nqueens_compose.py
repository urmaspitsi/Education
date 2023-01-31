from collections import Counter
from itertools import chain
import random
from timeit import default_timer as timer
from typing import List, Tuple
from nqueens_utils import create_initial_configuration, is_valid_board, num_conflicts, print_board

random.seed(0)

# https://arxiv.org/pdf/1805.07329.pdf
# https://en.wikipedia.org/wiki/Eight_queens_puzzle
# https://rosettacode.org/wiki/N-queens_problem

def swap(i:int, j:int, conf:List[int], k:int) -> List[int]:
  ''' Swap blocks of size: k. '''
  conf[i], conf[j] = conf[j], conf[i]
  return conf

def solve_n_plus_1(conf: List[int]) -> List[int]:
  n = len(conf) + 1
  res = [0] + [i + 1 for i in conf]         # top-left
  if (num_conflicts(res) != 0):
    res = [n - 1] + conf                    # top-right
    if (num_conflicts(res) != 0):
      res = [i + 1 for i in conf] + [0]     # bottom-left
      if (num_conflicts(res) != 0):
        res = conf + [n - 1]                # bottom-right
  return res

def solve_nqueens(n:int, verbose=False):
  start_time = timer()
  res = [2, 0, 3, 1]
  for i in range(5, n + 1):
    start_iteration_time = timer()
    res = solve_n_plus_1(res)
    end_iteration_time = timer()
    if (verbose):
      print(i, ',', round(end_iteration_time - start_iteration_time, 1), 'sec')
  print('total time:', round(end_iteration_time - start_time, 1), 'sec')
  return res

#----------------------------------------------------------------------------------------------
#
#----------------------------------------------------------------------------------------------

def num_conflicts_after_swap(conf:List[int], i:int, j:int, block_size:int=1) -> int:
  i_start = (i - 1) * block_size
  i_end = i_start + block_size
  j_start = (j  - 1) * block_size
  j_end = j_start + block_size
  conf[i_start:i_end], conf[j_start:j_end] = conf[j_start:j_end], conf[i_start:i_end] # Swap blocks.
  res = num_conflicts(conf)           # Calculate conflicts.
  conf[i_start:i_end], conf[j_start:j_end] = conf[j_start:j_end], conf[i_start:i_end] # Swap back.
  return res

def next_move_greedy_sgd(conf: List[int], batch_size:int=10, min_num_improvements:int=10, block_size:int=1) -> Tuple[int, int]:
  n = len(conf) // block_size
  min_cost:int = 9999999999
  res:Tuple[int,int] = [(-1, min_cost)]
  num_improvements: int = 0
  rows, cols = random.sample(range(n), batch_size), random.sample(range(n), batch_size)
  for i in rows:
    for j in cols:
      c = num_conflicts_after_swap(conf, i, j, block_size)
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
                  verbose=False, block_size:int=1):
  res = conf.copy()
  start_time = timer()
  found_solution: bool = False
  for i in range(1, num_iterations + 1):
    start_iteration_time = timer()
    r, c, num_confl = next_move_greedy_sgd(res, batch_size=batch_size,
                                           min_num_improvements=min_num_improvements,
                                           block_size=block_size)
    r_start = (r - 1) * block_size
    r_end = r_start + block_size
    c_start = (c - 1) * block_size
    c_end = c_start + block_size
    res[r_start:r_end], res[c_start:c_end] = res[c_start:c_end], res[r_start:r_end] # Swap pair of blocks.

    end_iteration_time = timer()
    if (verbose):
      print(i, ': conflicts=', num_confl, ',', round(end_iteration_time - start_iteration_time, 1), 'sec')
    if (num_confl < 1):
      if (verbose):
        print('found solution in steps:', i, ', total time:', round(end_iteration_time - start_time, 1), 'sec')
      found_solution = True
      break
  return res, found_solution

#----------------------------------------------------------------------------------------------
#
#----------------------------------------------------------------------------------------------

def compose(conf: List[int], order: List[int]) -> List[int]:
  '''
    Result size is len(order) * len(conf). Composes initial conf into k*k blocks.
    Where k=len(order).
  '''
  n = len(conf)
  return list(chain(*[[conf[j] + i * n for j in range(n)] for i in order]))

#----------------------------------------------------------------------------------------------
#
#----------------------------------------------------------------------------------------------
#n4 = [2, 0, 3, 1]
num_queens = 10
initial_conf = create_initial_configuration(num_queens)
solution0, is_solution = solve_greedy_sgd(initial_conf, num_iterations=10000, verbose=False,
                                      batch_size=min(100, int(0.5 * len(initial_conf))),
                                      min_num_improvements=3, block_size=1)

solution0 = [1, 3, 0, 2]
is_valid = is_valid_board(solution0)
num_conflicts_solution0 = num_conflicts(solution0)
solution_board = print_board(solution0)
solution_board

num_blocks = 4
solution = compose([4, 1, 3, 0, 2], [1, 3, 0, 2]) # 4*4 blocks of 5*5 board.
solution = compose(solution, [1, 3, 0, 2]) # 4*4 blocks of 20*20 board.

idx = [4, 1, 3, 0, 2]
idx = random.sample(range(num_blocks), num_blocks)
solution = compose(solution0, idx)
#solution = compose(solution, idx)
solution, is_solution = solve_greedy_sgd(solution, num_iterations=10000, verbose=True,
                                      batch_size=min(100, int(0.5 * num_blocks)),
                                      min_num_improvements=3, block_size=num_queens)


is_valid = is_valid_board(solution)
num_conflicts_solution = num_conflicts(solution)
solution_board = print_board(solution)
solution_board


solution = solve_n_plus_1(n4)
solution = solve_nqueens(n=6, verbose=True)

###################################################################################
# Workbench
###################################################################################
a = list(chain(*[random.sample(range(100), 10) for i in range(10)]))
b = Counter(a).most_common()
c = tuple(zip(*b[:5]))[0]
