from queue import PriorityQueue
import math, random
from timeit import default_timer as timer

def invert_dict(dict_to_invert):
  return {v: k for k, v in dict_to_invert.items()}

def draw_path(kaart, path):
  res = kaart.copy()
  for r, c in path[1:-1]:
    res[r] = res[r][:c] + '.' + res[r][c + 1:]
  return res

def manhattan(a, b):
  return abs(a[0] - b[0]) + abs(a[1] - b[1])

def h2(a, b):
  return max(abs(a[0] - b[0]), abs(a[1] - b[1]))  # suurim koordinaadi nihe

def euclidean(a, b):
  return math.sqrt((a[0] - b[0])**2 + (a[1] - b[1])**2)

def load_map(file_name):
  with open(file_name) as f:
    return [l.strip() for l in f.readlines() if len(l) > 1]

class Rajaleidja():
  def __init__(self, kaart, start, end, allow_diagonal_move=False):
    self.allow_diagonal_move = allow_diagonal_move
    self.debug = False
    self.destination_char = 'D'
    self.end = end
    self.kaart = kaart
    self.lava_char = '*'
    self.path = []
    self.start = start

  def __call__(self): return self.shortest_path_breath_first()

  def __len__(self): return len(self.kaart)

  def width(self): return len(self.kaart[0])

  def get(self, point): return self.kaart[point[0]][point[1]]

  def is_destination(self, point): return self.get(point) == self.destination_char

  def is_lava(self, point): return self.get(point) == self.lava_char

  def is_valid(self, point):
    if point == self.start:
      return False
    elif point[0] < 0 or point[0] >= len(self) or point[1] < 0 or point[1] >= self.width():
      return False
    elif self.is_lava(point):
      return False
    else:
      return True

  def neighbors(self, point, randomize_order=False):
    up, down = (point[0] - 1, point[1]), (point[0] + 1, point[1])
    right, left = (point[0], point[1] + 1), (point[0], point[1] - 1)
    res = []
    if self.is_valid(up): res.append(up)
    if self.is_valid(right): res.append(right)
    if self.is_valid(down): res.append(down)
    if self.is_valid(left): res.append(left)
    if self.allow_diagonal_move:
      upleft, downleft = (point[0] - 1, point[1] - 1), (point[0] + 1, point[1] - 1)
      upright, downright = (point[0] - 1, point[1] + 1), (point[0] + 1, point[1] + 1)
      if self.is_valid(upleft): res.append(upleft)
      if self.is_valid(downleft): res.append(downleft)
      if self.is_valid(upright): res.append(upright)
      if self.is_valid(downright): res.append(downright)

    if randomize_order:
      random.shuffle(res)
    return res

  def create_path(self, dict_of_points, start):
    res, current = [start], start
    counter, counter_limit = 0, self.__len__() * self.width()
    while current != self.start and counter < counter_limit: # just in case to avoid infinite loop.
      current = dict_of_points[current]
      res.append(current)
      counter += 1
    return list(reversed(res))

  def draw_shortest_path(self, heuristic_func, method='greedy'):
    if self.path == []:
      return draw_path(self.kaart, self.greedy_path(heuristic_func) if method=='greedy' else self.astar_path(heuristic_func))
    else:
      return draw_path(self.kaart, self.path)

  def shortest_path(self, method='greedy', cost_func=manhattan, heuristic_func=manhattan):
    if method == 'greedy':
      return self.greedy_path(heuristic_func)
    else:
      return self.astar_path(cost_func, heuristic_func)

  def greedy_path(self, heuristic_func):
    frontier = PriorityQueue()
    frontier.put(self.start, 0)
    came_from = {}
    came_from[self.start] = None
    found_destination = self.is_destination(self.start)
    while not frontier.empty():
      current = frontier.get()
      found_destination = self.is_destination(current)
      if found_destination:
        break

      for next in self.neighbors(current):
        if next not in came_from:
          priority = heuristic_func(next, self.end) # greedy
          if self.debug: print(f'{current} -> {next}')
          frontier.put(next, priority)
          came_from[next] = current

    res = self.create_path(came_from, current) if found_destination else []
    self.path = res
    return res

  def astar_path(self, cost_func, heuristic_func, verbose=False):
    start_time = timer()
    frontier = PriorityQueue()
    frontier.put(self.start, 0)
    came_from = {}
    cost_so_far = {}
    came_from[self.start] = None
    cost_so_far[self.start] = 0
    found_destination = self.is_destination(self.start)
    get_count = 0
    push_count = 0
    while not frontier.empty():
      current = frontier.get()
      if (verbose):
        get_count += 1
      found_destination = self.is_destination(current)
      if found_destination:
        break

      for next in self.neighbors(current):
        new_cost = cost_so_far[current] + cost_func(current, next)
        if (next not in came_from) or (new_cost < cost_so_far[next]):
          cost_so_far[next] = new_cost
          priority = new_cost + heuristic_func(next, self.end) # A*
          if self.debug: print(f'{current} -> {next}')
          frontier.put(next, priority)
          if (verbose):
            push_count += 1

          came_from[next] = current

    res = self.create_path(came_from, current) if found_destination else []
    self.path = res
    end_time = timer()
    time_elapsed = end_time - start_time
    if (verbose):
      print(time_elapsed, get_count + push_count, get_count, push_count)
    return res


cave300 = load_map('ITI0210_caves/cave300x300') # greedy: 1069, A* diag: 386
cave600 = load_map('ITI0210_caves/cave600x600') # greedy: 2174, A* diag: 882
cave900 = load_map('ITI0210_caves/cave900x900') # greedy: 3938, A* diag: 1254

# kaart rida veerg
# 300x300 start 2 2 goal 295 257
# 600x600 start 2 2 goal 598 595
# 900x900 start 2 2 goal 898 895

# Note: Shortest path result includes start and end points!
# Usage 1: initialize Rajaleidja, calculate shortest path, draw shortest path.
rl = Rajaleidja(cave300, (2, 2), (295, 257), allow_diagonal_move=False)
rl = Rajaleidja(cave600, (2, 2), (598, 595), allow_diagonal_move=False)
rl = Rajaleidja(cave900, (2, 2), (898, 895), allow_diagonal_move=False)

greedy = rl.greedy_path(manhattan)
a_star = rl.astar_path(manhattan, manhattan, verbose=True)


result = rl.draw_shortest_path(manhattan, method='astar')
result = rl.draw_shortest_path(manhattan, method='greedy')

# Output to text file.
with open('ITI0210_caves/cave900_astar_diagonal.txt', 'w') as text_file:
    text_file.write('\n'.join(result))

with open('ITI0210_caves/cave900_greedy.txt', 'w') as text_file:
    text_file.write('\n'.join(result))


# Calculate all combinations.
def calculate_all():
  start = (2, 2)
  destinations = [(295, 257), (598, 595), (898, 895)]
  cave_names = ['300', '600', '900']
  cost_func = manhattan
  res =dict()
  for method in ['astar', 'greedy']:
    for heuristic in [manhattan, h2]:
      for i, cave in enumerate([cave300, cave600, cave900]):
        for allow_diagonal in [False, True]:
          rl = Rajaleidja(cave, start, destinations[i], allow_diagonal_move=allow_diagonal)
          shortest = rl.shortest_path(method, cost_func, heuristic)
          res[(method, str(heuristic).split(' ')[1], cave_names[i], allow_diagonal)] = (len(shortest) - 1, shortest, rl)
          print(method, str(heuristic).split(' ')[1], cave_names[i], allow_diagonal)

  return res


# Output all combinations results into text file.
all_results = calculate_all()
ls = [(k, v[0], v[1]) for k,v in all_results.items()]

# Compact result, 2 columns: choices and cost.
items_str = [str(k) + '; ' + str(l) for k,l,_ in ls]
with open('ITI0210_caves/all_results.txt', 'w') as text_file:
    text_file.write('\n'.join(items_str))


# Full result, 3 columns: choices, cost, shortest path.
items_str = [str(k) + '; ' + str(l) + '; ' + str(p) for k,l,p in ls]
with open('ITI0210_caves/all_paths.txt', 'w') as text_file:
    text_file.write('\n'.join(items_str))


