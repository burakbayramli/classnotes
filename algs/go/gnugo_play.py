#
# GnuGo ile iletisim kurar, gnugo programinin ayri kurulmus olmasi lazim
# Ubuntu'da sudo apt-get install gnugo
#
import os, glob, pickle, go
import json, re, util, gtp
import numpy as np, sys
from shutil import copy
from mcts import MCTSPlayer
from util import flatten_idx, pprint_board
from util import flatten_idx, random_transform, idx_transformations
from tensorflow.contrib.keras import backend as K
from tensorflow.contrib.keras import models as M
import random, simplenet

def gnu_to_agz(action):
    if action==(0,0): return None
    (x,y) = action
    return (9-y,(x-1))

def agz_to_gnu(action):
    if not action: return (0,0)
    (x,y) = action
    return (y+1,9-x)

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
        self.gnugo.play(gtp.BLACK, (agz_to_gnu(coord)))
        
    def get_move(self):
        (x,y) = self.gnugo.genmove(gtp.WHITE)
        if (x,y)==(0,0): return go.PASS_MOVE
        return gnu_to_agz((x,y))

    def showboard(self):
        self.gnugo.showboard()        

def run_a_game(alphago_player, gnugo_player):
    '''Run num_games games to completion, keeping track of each position and move of the new_player.
    And return the win ratio

    '''

    state = go.GameState(size=9, komi=0)

    pprint_board(state.board)
    while not state.is_end_of_game:
        try:
            move = alphago_player.get_move(state)            
            print 'alphago move', move
            state.do_move(move, go.BLACK)
            alphago_player.mcts.update_with_move(move)            
            gnugo_player.set_others_move(move)
            #pprint_board(state.board)
            gnugo_player.showboard()

            move = gnugo_player.get_move()
            print 'gnugo move', move
            state.do_move(move, go.WHITE)
            #alphago_player.mcts.update_with_move(move)
            #pprint_board(state.board)
            gnugo_player.showboard()

            #exit()
        except Exception as e:
            print('exception')
            print (e)
            exit()
            
    winner = state.get_winner()
    print 'winner', winner
if __name__ == '__main__':
    # Arguments
    # gnugo_play.py [num of recursive calls] [gnugo difficulty (between 1-10)]
    policy = simplenet.PolicyValue(simplenet.PolicyValue.create_network())
    policy.load()
    alphago_player = MCTSPlayer(policy.eval_value_state, policy.eval_policy_state, n_playout=50, evaluating=True)
    gnugo_player = GnuGo(board_size=9,level=5)
    run_a_game(alphago_player, gnugo_player)
