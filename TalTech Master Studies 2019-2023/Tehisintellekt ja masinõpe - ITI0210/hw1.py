from queue import Queue
import random

class Rajaleidja():
  def __init__(self, kaart, start, save_steps=False):
    self.debug = False
    self.destination_char = 'D'
    self.kaart = kaart
    self.lava_char = '*'
    self.path = []
    self.save_steps = save_steps
    self.start = start
    self.steps = [] # stores intermediate steps in same format as input map

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

  def draw_shortest_path(self, randomize_order=False):
    if randomize_order or self.path == []:
      return draw_path(self.kaart, self.shortest_path_breath_first(randomize_order))
    else:
      return draw_path(self.kaart, self.path)

  def shortest_path_breath_first(self, randomize_order=False):
    frontier = Queue()
    frontier.put(self.start)

    came_from = {}
    came_from[self.start] = None
    found_destination = self.is_destination(self.start)

    while not frontier.empty() and not found_destination:
      current = frontier.get()
      for next in self.neighbors(current, randomize_order):
        if next not in came_from:
          if self.debug: print(f'{current} -> {next}')
          frontier.put(next)
          came_from[next] = current

        found_destination = self.is_destination(next)
        if found_destination:
          current = next
          break

    res = self.create_path(came_from, current) if found_destination else []
    self.path = res
    return res

def invert_dict(dict_to_invert):
  return {v: k for k, v in dict_to_invert.items()}

def draw_path(kaart, path):
  res = kaart.copy()
  for r, c in path[1:-1]:
    res[r] = res[r][:c] + '.' + res[r][c + 1:]
  return res


lava_map1 = [
    "      **               **      ",
    "     ***     D        ***      ",
    "     ***                       ",
    "                      *****    ",
    "           ****      ********  ",
    "           ***          *******",
    " **                      ******",
    "*****             ****     *** ",
    "*****              **          ",
    "***                            ",
    "              **         ******",
    "**            ***       *******",
    "***                      ***** ",
    "                               ",
    "                s              ",
]

lava_map2 = [
    "     **********************    ",
    "   *******   D    **********   ",
    "   *******                     ",
    " ****************    **********",
    "***********          ********  ",
    "            *******************",
    " ********    ******************",
    "********                   ****",
    "*****       ************       ",
    "***               *********    ",
    "*      ******      ************",
    "*****************       *******",
    "***      ****            ***** ",
    "                               ",
    "                s              ",
]


start_row = 14
start_col = 16

# Note: Shortest path result includes start and end points!
# Usage 1: initialize Rajaleidja, calculate shortest path, draw shortest path.
rl = Rajaleidja(lava_map1, (start_row, start_col))
shortest_path = rl.shortest_path_breath_first(randomize_order=False)
rl.draw_shortest_path(randomize_order=False)

# Usage 2: Call Rajaleidja directly. Returns shortest path as list of points.
shortest_path = Rajaleidja(lava_map2, (start_row, start_col))()
len(shortest_path) - 1, shortest_path

