from copy import deepcopy
from typing import Tuple
import numpy as np
import random

random.seed(0)
np.random.seed(0)

def contains_n_items(x:np.ndarray, n:int) -> bool:
  ''' Checks whether array x contains at least n True values. Assumes x is boolean or contains values 0 or 1. '''
  l = len(x)
  if l < n: return False
  elif l == n: return sum(x) == n
  else:
    for i in range(l - n + 1):
      if (sum(x[i:(i + n)])==n):
        return True
    return False

def diagonal1(x:np.ndarray, row_idx, col_idx): return np.diagonal(x, col_idx - row_idx)

def diagonal2(x:np.ndarray, row_idx, col_idx): return np.diagonal(np.flipud(x), row_idx + col_idx + 1 - len(x))

def is_win(x:np.ndarray, last_move, n):
  ''' Checks if the move has produced a win. '''
  r,c = last_move
  if contains_n_items(x[r,:], n): return True
  elif contains_n_items(x[:,c], n): return True
  elif contains_n_items(diagonal1(x, r, c), n): return True
  elif contains_n_items(diagonal2(x, r, c), n): return True
  else: return False

class Game():
  ''' ConnectFour game.'''
  def __init__(self, height:int=6, width:int=7, num_connect:int=4):
    self.height, self.width, self.num_connect = height, width, num_connect
    self.board:np.ndarray = np.zeros((height, width), dtype=np.uint8)   # Board state storage matrix. Each cell contains either 0 (not marked), 1 (player1 has marked this) or 2 (player 2 has marked this)
    self.player1:np.ndarray = np.zeros((height, width), dtype=np.bool)  # Player 1 moves.
    self.player2:np.ndarray = np.zeros((height, width), dtype=np.bool)  # Player 2 moves.
    self.free_row:np.ndarray = np.zeros(width, dtype=np.uint8)          # Array of free row idx for each column.
    self.num_moves, self.max_num_moves = 0, height * width
    self.winner = 0                                                     # 0=Draw, 1=Player1 won, 2=Player2 won.
    self.game_over = False
    self.win_value, self.lose_value, self.draw_value = 1., 0., 0.5      # Numeric score values for game results.

  def clone(self):
    return deepcopy(self)

  def print_board(self, start_from_bottom:bool=True):
    ''' Print the board. Start marking from the top/bottom. '''
    return np.flipud(self.board) if start_from_bottom else self.board

  def board_is_full(self) -> bool: return self.num_moves >= self.max_num_moves

  def make_move(self, player_id:int, row_col_idx:Tuple[int,int]) -> bool:
    ''' Makes a move into specified row and col idx as specified player. Returns True or False whether game is over after the move. '''
    r,c = row_col_idx
    if player_id == 1:
      self.player1[r, c] = True
    else:
      self.player2[r, c] = True
    self.board[r, c] = player_id
    self.free_row[c] += 1
    self.num_moves += 1
    win = is_win(self.player1 if player_id == 1 else self.player2, row_col_idx, self.num_connect)
    if win:
      self.winner = player_id
    self.game_over = win or self.board_is_full()
    return self.game_over

  def make_move_in_column(self, player_id:int, col_idx:int) -> bool:
    ''' Makes a move into specified col idx as specified player. Returns True or False whether game is over after the move. '''
    assert (self.free_row[col_idx] < self.height), 'This column is full! Choose another.'
    return self.make_move(player_id, (self.free_row[col_idx], col_idx))

  def possible_moves(self) -> np.ndarray:
    ''' Returns array with column indexes of possible moves. '''
    return np.where(self.free_row < self.height)[0]

  def random_move(self) -> Tuple[int,int]:
    c = np.random.choice(self.possible_moves(), 1)
    return (self.free_row[c], c)

  def best_move(self) -> Tuple[int,int]:
    return self.random_move()

  def create_next_move(self, player_id:int) -> Tuple[int,int]:
    return self.random_move()

  def score(self, player_id:int=1) -> float:
    ''' Returns the score of the current game from the player_id's perspective. '''
    if player_id == 1:
      return self.win_value if self.winner == 1 else self.draw_value if self.winner == 0 else self.lose_value
    else:
      return self.win_value if self.winner == 2 else self.draw_value if self.winner == 0 else self.lose_value

  def play(self, player1_moves_first: bool=True) -> int:
    ''' Plays until game is over. Returns the winner (1 or 2) or zero if draw. '''
    player1_id, player2_id, player1_moves = 1, 2, player1_moves_first
    while not self.game_over:
      player_id = player1_id if player1_moves else player2_id
      next_move = self.create_next_move(player_id)
      win = self.make_move(player_id, next_move)
      player1_moves = not player1_moves
    return self.winner

  def play_by_random_moves(self, player_id:int=1) -> float:
    win = self.make_move(player_id, self.create_next_move(player_id))
    return self.score(player_id=player_id)


def best_move_monte_carlo(game_state:Game, player_id:int=1, num_plays:int=50) -> Tuple[int,int]:
  ''' Returns best move (row idx, col idx) according to Monte Carlo tree search. '''
  moves = game_state.possible_moves()
  scores: np.ndarray = np.zeros(len(moves))
  num_wins: np.ndarray = np.zeros(len(moves), dtype=np.int)
  for i in range(len(scores)):
    for p in range(num_plays):
      new_game = game_state.clone()
      win = new_game.make_move_in_column(player_id, moves[i])            # Player 1 moves.
      if win:
        scores[i] += new_game.score(player_id=player_id)
        num_wins[i] += 1
      else:
        winner = new_game.play(player1_moves_first=(player_id!=1))   # Player 2 moves.
        scores[i] += new_game.score(player_id=player_id)
        if new_game.winner == player_id:
          num_wins[i] += 1

  col_idx = moves[np.argmax(scores)]
  print('')
  print(f'player: {player_id}, best column: {col_idx}')
  print(f'scores: {scores}')
  print(f'num_wins: {num_wins}')
  print(f'win %: {np.round(100 * num_wins / num_plays,1)}')
  return (game_state.free_row[col_idx], col_idx)

def play_against_monte_carlo_as_first(game: Game, move_in_column_idx:int):
  win = game.make_move_in_column(player_id=1, col_idx=move_in_column_idx)
  if win:
    print('Player1: I won!')
  else:
    r, c = best_move_monte_carlo(game, player_id=2)
    win = game.make_move_in_column(player_id=2, col_idx=c)
    if win:
      print('Player2: Monte Carlo has won!')
  print(game.print_board())

def play_against_monte_carlo_as_second(game: Game, move_in_column_idx:int):
  r, c = best_move_monte_carlo(game, player_id=1)
  win = game.make_move_in_column(player_id=1, col_idx=c)
  if win:
    print('Player1: Monte Carlo has won!!!')
  else:
    win = game.make_move_in_column(player_id=2, col_idx=move_in_column_idx)
    if win:
      print('Player2: I won!!!')
  print(game.print_board())

def play_monte_carlo_vs_monte_carlo(game: Game):
  r, c = best_move_monte_carlo(game, player_id=1)
  win = game.make_move_in_column(player_id=1, col_idx=c)
  if win:
      print('Player1: Monte Carlo 1 has won!')
  else:
    r, c = best_move_monte_carlo(game, player_id=2)
    win = game.make_move_in_column(player_id=2, col_idx=c)
    if win:
      print('Player2: Monte Carlo 2 has won!')
  print(game.print_board())

def random_play_against_monte_carlo(g:Game, as_first_player=True):
  while not g.game_over:
    if as_first_player:
      play_against_monte_carlo_as_first(g, move_in_column_idx=g.random_move()[1])
    else:
      play_against_monte_carlo_as_second(g, move_in_column_idx=g.random_move()[1])
  print(f'num moves: {np.sum(g.board!=0)} ({np.sum(g.board==1)}/{np.sum(g.board==2)})')

def monte_carlo_vs_monte_carlo(g:Game):
  while not g.game_over:
      play_monte_carlo_vs_monte_carlo(g)
  print(f'num moves: {np.sum(g.board!=0)} ({np.sum(g.board==1)}/{np.sum(g.board==2)})')

#-----------------------------------------------------------------------------------------
# Initialize new game.
#-----------------------------------------------------------------------------------------
g = Game(height=6, width=7, num_connect=4)   # Initialize new game.
free = g.free_row                            # Array containing free row idx for each column.
g.print_board()                              # Print initial board.

#-----------------------------------------------------------------------------------------
# Start playing.
#-----------------------------------------------------------------------------------------
monte_carlo_vs_monte_carlo(g)                                 # Monte Carlo vs Monte Carlo.

random_play_against_monte_carlo(g, as_first_player=True)      # Random play vs Monte Carlo.

play_against_monte_carlo_as_first(g, move_in_column_idx=3)    # Manual input as player 1 vs Monte Carlo.

play_against_monte_carlo_as_second(g, move_in_column_idx=3)   # Manual input as player 2 vs Monte Carlo.


#-----------------------------------------------------------------------------------------
# Other stuff.
#-----------------------------------------------------------------------------------------
r, c = best_move_monte_carlo(g, player_id=2)             # Find best move according to Monte Carlo: row and col idx.
g.make_move_in_column(player_id=2, col_idx=c)            # Makes move in the specified column as particular player.
free = g.free_row                                        # Show free row indexes for each column.
g.print_board()                                          # Print current game state.

win = g.make_move_in_column(player_id=1, col_idx=0)
free = g.free_row
g.print_board()
g.possible_moves()

win = g.make_move_in_column(player_id=2, col_idx=2)
free = g.free_row
g.print_board()



