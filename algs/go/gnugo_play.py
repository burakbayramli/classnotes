import os, glob, pickle, go
import json, re, util, gtp
import numpy as np, sys
from shutil import copy
from ai import MCTSPlayer
from util import flatten_idx, pprint_board
from util import flatten_idx, random_transform, idx_transformations
from tensorflow.contrib.keras import backend as K
from tensorflow.contrib.keras import models as M
import random, simplenet

# Dis dunyadan bir Go programiyla oynamak icin arayuz
class GnuGo(object):
    def __init__(self,board_size, level):
        self.is_human = True
        self.board_size = board_size
        self.gnugo = gtp.GTPFacade("white", ["gnugo", "--mode", "gtp", "--level", str(level)])
        self.gnugo.boardsize(9)
        self.gnugo.komi(0.0)
        self.gnugo.clear_board()

    def set_others_move(self, coord):
        if coord:
            self.gnugo.play(gtp.BLACK, (coord[0]+1,coord[1]+1))
        else:
            self.gnugo.play(gtp.BLACK, (0,0))
        
    def get_move(self):
        (x,y) = self.gnugo.genmove(gtp.WHITE)
        if (x,y)==(0,0): return go.PASS_MOVE
        return (x-1,y-1)

    def showboard(self):
        self.gnugo.showboard()        

def run_a_game(alphago_player, gnugo_player, boardsize):
    '''Run num_games games to completion, keeping track of each position and move of the new_player.
    And return the win ratio

    '''

    board_size = boardsize
    state = go.GameState(size=board_size, komi=0)

    pprint_board(state.board)
    while not state.is_end_of_game:
        try:
            move = alphago_player.get_move(state)            
            state.do_move(move)
            alphago_player.mcts.update_with_move(move)            
            print 'alphago move', move
            gnugo_player.set_others_move(move)
            #pprint_board(state.board)
            gnugo_player.showboard()

            move = gnugo_player.get_move()
            print 'gnugo move', move
            state.do_move(move)
            #pprint_board(state.board)
            gnugo_player.showboard()
            
        except Exception as e:
            print(e)
            exit()
            
    winner = state.get_winner()
    print 'winner', winner
if __name__ == '__main__':
    # Arguments
    # gnugo_play.py [num of recursive calls] [gnugo difficulty (between 1-10)]
    policy = simplenet.PolicyValue(simplenet.PolicyValue.create_network())
    policy.load()
    alphago_player = MCTSPlayer(policy.eval_value_state, policy.eval_policy_state, n_playout=40, evaluating=True)
    gnugo_player = GnuGo(9, 10)
    run_a_game(alphago_player, gnugo_player, 9)
