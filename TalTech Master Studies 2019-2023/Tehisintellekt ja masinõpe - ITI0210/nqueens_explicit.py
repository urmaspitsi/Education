from typing import List

def nqueens(n:int) -> List[int]:
  '''
  N-Queens problem generic solution for arbitrary n.
  Source: https://en.wikipedia.org/wiki/Eight_queens_puzzle
  If the remainder from dividing n by 6 is not 2 or 3 then the list is simply all even numbers followed by all odd numbers not greater than n.
  Otherwise, write separate lists of even and odd numbers (2, 4, 6, 8 – 1, 3, 5, 7).
  If the remainder is 2, swap 1 and 3 in odd list and move 5 to the end (3, 1, 7, 5).
  If the remainder is 3, move 2 to the end of even list and 1,3 to the end of odd list (4, 6, 8, 2 – 5, 7, 1, 3).
  Append odd list to the even list and place queens in the rows given by these numbers, from left to right (a2, b4, c6, d8, e3, f1, g7, h5).
 '''
  odd = list(range(0, n, 2)) # odd for 1 based count
  even = list(range(1, n, 2)) # even for 1 based count
  remainder = n % 6
  if (remainder == 2):
    odd = [2, 0] + list(range(4, n - 2, 2)) + [5]
  elif (remainder == 3):
    even = list(range(3, n - 2, 2)) + [2]
    odd = list(range(2, n - 2, 2)) + [0, 2]
  return even + odd
