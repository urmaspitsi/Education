import random
from timeit import default_timer as timer
from typing import List, Tuple

def create_initial_configuration(n:int) -> List[int]:
  return random.sample(range(n), n)

def print_board(configuration:List[int], empty=' . ', non_empty=' x ') -> List[List[str]]:
  n = len(configuration)
  return [configuration[i] * empty + non_empty + (n - configuration[i] - 1) * empty for i in range(n)]

def num_conflicts(conf:List[int]) -> int:
  n:int = len(conf)
  return n - len(set([i - v for i,v in enumerate(conf)])) + n - len(set([i + v for i,v in enumerate(conf)]))

def is_valid_board(conf:List[int]) -> bool:
  return num_conflicts(conf) == 0

def swap(i:int, j:int, conf:List[int]) -> List[int]:
  conf[i], conf[j] = conf[j], conf[i]
  return conf

def next_move_greedy_sgd(conf: List[int], batch_size:int=10, min_num_improvements:int=10) -> Tuple[int, int]:
  n = len(conf)
  min_cost:int = 9999999999
  res:Tuple[int,int] = [(-1, min_cost)]
  num_improvements: int = 0
  rows, cols = random.sample(range(n), batch_size), random.sample(range(n), batch_size)
  for i in rows:
    for j in cols:
      c = num_conflicts(swap(i, j, conf.copy()))
      if (c < min_cost):
        if (num_improvements >= min_num_improvements):
          return (i, j) # Greedy: Exit immediately if better solution.
        num_improvements += 1
        min_cost = c
        res[0] = (i, j)
      elif (c == min_cost):
        res.append((i, j))
  if (len(res) > 1):
    random.shuffle(res)
  return res[0]

def solve_nqueens(conf: List[int], num_iterations:int=20, verbose=False):
  res = conf.copy()
  start_time = timer()
  found_solution: bool = False
  for i in range(1, num_iterations + 1):
    start_iteration_time = timer()
    r, c = next_move_greedy_sgd(res, batch_size=min(100, int(0.5 * len(res))), min_num_improvements=3)
    res = swap(r, c, res)
    num_confl = num_conflicts(res)
    end_iteration_time = timer()
    if (verbose):
      print(i, ': conflicts=', num_confl, ',', round(end_iteration_time - start_iteration_time, 1), 'sec')
    if (num_confl < 1):
      if (verbose):
        print('found solution in steps:', i, ', total time:', round(end_iteration_time - start_time, 1), 'sec')
      found_solution = True
      break
  return res, found_solution

# Solve n-queens problem.
num_queens = 20
initial_conf = create_initial_configuration(num_queens)
solution, is_solution = solve_nqueens(initial_conf, num_iterations=1000, verbose=True)

# Check solution.
is_valid = is_valid_board(solution)
num_conflicts_solution = num_conflicts(solution)
solution_board = print_board(solution)
solution_board

# Check initial configuration.
is_valid_initial = is_valid_board(initial_conf)
num_conflicts_initial = num_conflicts(initial_conf)
initial_board = print_board(initial_conf)
initial_board



