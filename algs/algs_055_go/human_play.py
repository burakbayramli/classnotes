import os, glob, pickle, go
import json, re, util
import numpy as np
from shutil import copy
from mcts import MCTSPlayer
from util import flatten_idx, pprint_board
from util import flatten_idx, random_transform, idx_transformations
from tensorflow.contrib.keras import backend as K
from tensorflow.contrib.keras import models as M
import numpy as np
import random, simplenet

class Human(object):
    def __init__(self,board_size):
        self.is_human = True
        self.board_size = board_size

    def get_move(self, state):
        while True:
            query = raw_input("Your move: ")
            if len(query)==0:
                return go.PASS_MOVE
            else:
                try:
                    alphabet, number = re.match(r"([a-z]+)([0-9]+)", query, re.I).groups()
                    y = ord(alphabet.upper()) - ord('A')
                    x = self.board_size - int(number)
                    return ((x,y))
                except:
                    print("The input should have the form like 'a1' or 'A1'.")
                    continue

def run_a_game(alphago_player, human_player, boardsize):
    '''Run num_games games to completion, keeping track of each position and move of the new_player.
    And return the win ratio

    '''

    board_size = boardsize
    state = go.GameState(size=board_size, komi=0)

    # Start all odd games with moves by 'old_player'. Even games will have 'new_player' black.
    human_color = np.random.choice([go.BLACK, go.WHITE])
    if human_color == go.BLACK:
        current = human_player
        other = alphago_player
        print("Your color is black.")
    else:
        current = alphago_player
        other = human_player
        print("Your color is white.")

    pprint_board(state.board)
    while not state.is_end_of_game:
        move = current.get_move(state)
        try:
            state.do_move(move)
        except:
            print("Illegal move!")
            continue
        if other == alphago_player:
            other.mcts.update_with_move(move)
        current, other = other, current

        pprint_board(state.board)
    winner = state.get_winner()
    if winner == human_color:
        print("You won.")
    elif winner == 0:
        print("Tie.")
    else:
        print("AlphagoZero won")

def run_play(cmd_line_args=None):

    # Set initial conditions
    policy = simplenet.PolicyValue(simplenet.PolicyValue.create_network())
    policy.load()

    boardsize = policy.model.input_shape[-1]
    best_player = MCTSPlayer(policy.eval_value_state, policy.eval_policy_state, n_playout=10, evaluating=True)
    human_player = Human(boardsize)
    run_a_game(best_player, human_player, boardsize)

if __name__ == '__main__':
    run_play()
