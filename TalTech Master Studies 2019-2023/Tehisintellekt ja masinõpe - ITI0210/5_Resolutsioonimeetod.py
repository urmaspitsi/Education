from itertools import combinations

'''
  Ülesanded:

  1 1 0       0 0 0 .      ? . . . 0
  ? 1 ?       1 2 1 1      . 4 2 1 .
  1 1 0       . . ? .      . 2 0 0 .

  ? : question
  . : not investigated
'''
#---------------------------------------------------------------------------------
# Minesweeper setup. Parsing input, helpers etc.
#---------------------------------------------------------------------------------
def board_indexes(num_rows, num_columns):
  a = [i for i in range(1, num_rows * num_columns + 1)]
  return [a[i*num_columns:(i+1) * num_columns] for i in range(num_rows)]

def parse_task(task: str, num_rows):
  num_columns = len(task) // num_rows
  res = board_indexes(num_rows, num_columns)
  for r,row in enumerate(res):
    for c,col in enumerate(row):
      res[r][c] = task[col-1:col]
  return res

def number_of_mines(board, r, c):
  res = board[r][c]
  return int(res) if res.isdigit() else -1

#---------------------------------------------------------------------------------
# Logic engine. Knowledge base creation, resolution etc.
#---------------------------------------------------------------------------------
def all_combs(ls, counter, limit):
  if counter > limit:
    return ls
  else:
    q = [i1 + i2 for i2 in ls[1] for i1 in ls[0]]
    return all_combs([q] + ls[2:], counter + 1, limit)

def is_valid_rule(ls):
  if ls[0] >= 0: return True
  s = set([i for i in ls if i >= 0])
  for i in ls:
    if i >= 0: return True
    if -i in s: return False
  return True

def kbase_to_cnf(kb, remove_contradictions=True):
  '''
    Generates all combinations of disjunction elements.
    Result is list Conjunctions that are possible. If remove_contradictions=True, then only those conjunctions
    are kept that are valid.
  '''
  a = all_combs(kb, 1, len(kb) - 1)[0]
  res = [sorted(set(ls)) for ls in a]
  if remove_contradictions:
    return [ls for ls in res if is_valid_rule(ls)]
  else:
    return res

def knowlwdge_base_with_alpha(kb, alpha):
  return [sorted(set(ls + [alpha])) for ls in kb]

def resolution(kb, alpha):
  '''
    Returns 2-tuple of (boolean, knowledgebase).
    alpha : 1-based index (usual left-right, top-down order) or 2-tuple of 0-based row and column indices.
    Negative alpha means there is NO mine on that location. E.g. 'alpha=-4' means we ask if cell 4 is mine-free.
    Positive alpha means there IS mine on that location.
    kb - teadmusbaas CNF kujul.
    alpha - literaal, mida tahame kontrollida.
  '''
  kb_new = [ls for ls in knowlwdge_base_with_alpha(kb, alpha) if is_valid_rule(ls)]
  is_ok = len(kb_new) > 0
  return is_ok, kb_new

#---------------------------------------------------------------------------------
# Minesweeper Game class.
#---------------------------------------------------------------------------------
class MineSweeper():
  ''' Minesweeper Game class. '''
  def __init__(self, task:str, num_rows:int=3):
    self.task = task
    self.num_rows = num_rows
    self.num_columns = len(task) // num_rows
    self.indexes = self.board_indexes()
    self.board = self.parse_task()

  def board_indexes(self):
    return board_indexes(self.num_rows, self.num_columns)

  def parse_task(self):
    return parse_task(self.task, self.num_rows)

  def num_mines(self, r, c):
    return number_of_mines(self.board, r, c)

  def idx_from_row_col(self, row, col):
    return row * self.num_columns + col + 1

  def free_neighbors(self, r, c):
    a = self.board
    idx = self.indexes
    max_r = len(a) - 1
    max_c = len(a[0]) - 1
    res = []
    if c + 1 <= max_c:
      if a[r][c + 1] == '.':
        res.append(idx[r][c + 1])
    if r + 1 <= max_r and c + 1 <= max_c:
      if a[r + 1][c + 1] == '.':
        res.append(idx[r + 1][c + 1])
    if r + 1 <= max_r:
      if a[r + 1][c] == '.':
        res.append(idx[r + 1][c])
    if r + 1 <= max_r and c - 1 >= 0:
      if a[r + 1][c - 1] == '.':
        res.append(idx[r + 1][c - 1])
    if c - 1 >= 0:
      if a[r][c - 1] == '.':
        res.append(idx[r][c - 1])
    if r - 1 >= 0 and c - 1 >= 0:
      if a[r - 1][c - 1] == '.':
        res.append(idx[r - 1][c - 1])
    if r - 1 >= 0:
      if a[r - 1][c] == '.':
        res.append(idx[r - 1][c])
    if r - 1 >= 0 and c + 1 <= max_c:
      if a[r - 1][c + 1] == '.':
        res.append(idx[r - 1][c + 1])
    return res

  def create_facts(self, r, c):
    ''' Returns list of lists: list of conjunctions. Positive numbers indicate mine, negative numbers not mine. '''
    res = []
    num_mines = self.num_mines(r, c)
    if num_mines >= 0:
      neighbors = self.free_neighbors(r, c)
      if num_mines == 0:
        res = [[-i for i in neighbors]]
      elif num_mines == 1:
        res = [[n if i == j else -n for i,n in enumerate(neighbors)] for j in range(len(neighbors))]
      else:
        combs = list(combinations(range(len(neighbors)), num_mines))
        for ids in combs:
          fact = [-i for i in neighbors]
          for i in ids:
            fact[i] = -fact[i]
          res.append(fact)
    return res

  def knowledge_base_dnf(self):
    ''' List of List of Lists: each element is list of Conjunctions: each conjunction a list of intgers. '''
    res = []
    for r,rows in enumerate(self.board):
      for c,cols in enumerate(rows):
        conjunctions = self.create_facts(r, c)
        if len(conjunctions) > 0 and conjunctions != [[]]:
          res.append(conjunctions)
    return res

  def knowledge_base_cnf(self, remove_contradictions=True):
    return kbase_to_cnf(self.knowledge_base_dnf(), remove_contradictions=remove_contradictions)

  def is_safe_move(self, address):
    '''
      Address is 1-based index or 2-tuple of 0-based row and column indices.
      Returns 2-tuple of (boolean, knowledgebase).
      If knowledge base is empty then there is conflict.
      True, if it is OK to open the cell, ie there is no mine on that address.
    '''
    kbase = self.knowledge_base_cnf()
    if type(address) is tuple:
      row, col = address
      idx = self.idx_from_row_col(row, col)
    else:
      idx = address
    if len(kbase) == 0:
      return False, kbase
    else:
      res = [v for v in kbase if idx not in set(v)]
      if len(res) == 0:
        return False, kbase
      else:
        return True, res

#---------------------------------------------------------------------------------
# Testing results.
#---------------------------------------------------------------------------------

task1 = '110.1.110' #'110?1?110'
task2 = '000.1211....' #'000.1211..?.'
task3 = '....0.421..200.' #'?...0.421..200.'

MS = MineSweeper(task2, num_rows=3)
kbase_DNF = MS.knowledge_base_dnf()
kbase_CNF_raw = MS.knowledge_base_cnf(remove_contradictions=False)
kbase_CNF_without_contradictions = MS.knowledge_base_cnf(remove_contradictions=True)

kbase_CNF_with_alpha = knowlwdge_base_with_alpha(kbase_CNF_without_contradictions, -11)
is_ok, kbase = resolution(kbase_CNF_with_alpha, -11)

is_mine_free, kbase = MS.is_safe_move(11)

'''
  Ülesanded:

  1 1 0       0 0 0 .      ? . . . 0
  ? 1 ?       1 2 1 1      . 4 2 1 .
  1 1 0       . . ? .      . 2 0 0 .

  ? : question
  . : not investigated
'''

#---------------------------------------------------------------------------------
# Junkjard.
#---------------------------------------------------------------------------------

#  def resolution(self, alpha):
#    '''
#      Returns 2-tuple of (boolean, knowledgebase).
#      alpha : 1-based index (usual left-right, top-down order) or 2-tuple of 0-based row and column indices.
#      Negative alpha means there is NO mine on that location. E.g. 'alpha=-4' means we ask if cell 4 is mine-free.
#      Positive alpha means there IS mine on that location.
#    '''
#    # kb - teadmusbaas CNF kujul
#    # alpha - literaal, mida tahame kontrollida.
#    address = alpha
#    kbase = self.knowledge_base_cnf()
#    if type(address) is tuple:
#      row, col = address
#      idx = self.idx_from_row_col(row, col)
#    else:
#      idx = address
#    if len(kbase) == 0:
#      return False, kbase
#    else:
#      res = [v for v in kbase if idx in set(v)]
#      if len(res) == 0:
#        return False, kbase
#      else:
#        return True, res


#  def board_correct(self, board) -> bool:
#    for r,rows in enumerate(board):
#      for c, cols in enumerate(rows):
#        num_mines = number_of_mines(board, r, c)
#        if num_mines >= 0:
#          if not self.num_mines_correct_one(r, c, board, num_mines):
#            return False
#    return True
#
#  def num_mines_correct_one(self, r, c, board, num_mines) -> bool:
#    if num_mines < 0:
#      return True
#
#    a = board
#    max_r = len(a) - 1
#    max_c = len(a[0]) - 1
#    mine_counter = 0
#    if c + 1 <= max_c:
#      if a[r][c + 1] == 'x':
#        mine_counter += 1
#    if mine_counter > num_mines: return False
#    if r + 1 <= max_r and c + 1 <= max_c:
#      if a[r + 1][c + 1] == 'x':
#        mine_counter += 1
#    if mine_counter > num_mines: return False
#    if r + 1 <= max_r:
#      if a[r + 1][c] == 'x':
#        mine_counter += 1
#    if mine_counter > num_mines: return False
#    if r + 1 <= max_r and c - 1 >= 0:
#      if a[r + 1][c - 1] == 'x':
#        mine_counter += 1
#    if mine_counter > num_mines: return False
#    if c - 1 >= 0:
#      if a[r][c - 1] == 'x':
#        mine_counter += 1
#    if mine_counter > num_mines: return False
#    if r - 1 >= 0 and c - 1 >= 0:
#      if a[r - 1][c - 1] == 'x':
#        mine_counter += 1
#    if mine_counter > num_mines: return False
#    if r - 1 >= 0:
#      if a[r - 1][c] == 'x':
#        mine_counter += 1
#    if mine_counter > num_mines: return False
#    if r - 1 >= 0 and c + 1 <= max_c:
#      if a[r - 1][c + 1] == 'x':
#        mine_counter += 1
#    return mine_counter == num_mines
#


#def generate_tasks(task):
#  # idx of '.'
#  # for i = 1 to len(idx) -> combinations of i from idx
#  idx = range(10)
#  res = [[i] for i in idx]
#  if len(idx) > 1:
#    a = list(chain(*[combinations(idx, i) for i in range(2, 5)]))
#    res.extend(a)
#  return res


