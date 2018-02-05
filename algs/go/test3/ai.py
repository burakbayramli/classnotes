"""Policy players"""
import numpy as np
import go
import mcts
from operator import itemgetter

class MCTSPlayer(object):
    def __init__(self, value_function, policy_function, c_puct=5, n_playout=1600, evaluating=True, self_play=False):
        self.mcts = mcts.MCTS(value_function, policy_function, c_puct, n_playout)
        self.move_count = 0
        self.evaluating = evaluating
        self.self_play = self_play
        if self.evaluating:
            temperature = 0.
        else:
            temperature = 1.
        self.temperature = temperature
        self.is_human = False # for playing a game in play.py

    def get_move(self, state, self_play=False):
        sensible_moves = [move for move in state.get_legal_moves()]
        if len(sensible_moves) > 0:
            move = self.mcts.get_move(state, self.temperature, self.self_play)
            if not self_play:
                self.mcts.update_with_move(move)
            self.move_count += 1
            if not self.evaluating :
                if self.move_count == 2:
                    self.temperature = 0.
            return move
        # No 'sensible' moves available, so do pass move
        self.move_count += 1
        if not self.evaluating:
            if self.move_count == 2:
                self.temperature = 0.
        return go.PASS_MOVE

