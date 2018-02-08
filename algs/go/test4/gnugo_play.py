import os, glob, pickle, go
import json, re, util
import numpy as np
from shutil import copy
from ai import MCTSPlayer
from util import flatten_idx, pprint_board
from util import flatten_idx, random_transform, idx_transformations
from tensorflow.contrib.keras import backend as K
from tensorflow.contrib.keras import models as M
import numpy as np, gtp
import random, simplenet

# Dis dunyadan bir Go programiyla oynamak icin arayuz
class GnuGo(object):
    def __init__(self,board_size):
        self.is_human = True
        self.board_size = board_size
        self.gnugo = gtp.GTPFacade("white", ["gnugo", "--mode", "gtp", "--level", "10"])
        self.gnugo.boardsize(9)
        self.gnugo.komi(5.5)
        self.gnugo.clear_board()

    def set_others_move(self, coord):
        self.gnugo.play(gtp.BLACK, coord)
        
    def get_move(self, state):
        (x,y) = self.gnugo.genmove(gtp.WHITE)
        if (x,y)==(0,0): return go.PASS_MOVE
        return (x-1,y-1)

def run_a_game(alphago_player, human_player, boardsize):
    '''Run num_games games to completion, keeping track of each position and move of the new_player.
    And return the win ratio

    '''

    board_size = boardsize
    state = go.GameState(size=board_size, komi=0)

    # Start all odd games with moves by 'old_player'. Even games will have 'new_player' black.
    human_color = go.WHITE
    current = human_player
    other = alphago_player

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
            print 'my move', move
            current.set_others_move(move)
                
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
    human_player = GnuGo(boardsize)
    run_a_game(best_player, human_player, boardsize)

if __name__ == '__main__':
    run_play()
