import os, glob, pickle, go
import json, re, util
import numpy as np
from shutil import copy
from ai import MCTSPlayer
from util import flatten_idx, pprint_board
from util import flatten_idx, random_transform, idx_transformations
from tensorflow.contrib.keras import backend as K
from tensorflow.contrib.keras import models as M
import numpy as np
import random


class NeuralNetBase(object):
    """Base class for neural network classes handling feature processing, construction
    of a 'forward' function, etc.
    """

    # keep track of subclasses to make generic saving/loading cleaner.
    # subclasses can be 'registered' with the @neuralnet decorator
    subclasses = {}

    def __init__(self):
        pass

def neuralnet(cls):
    """Class decorator for registering subclasses of NeuralNetBase
    """
    NeuralNetBase.subclasses[cls.__name__] = cls
    return cls


@neuralnet
class MockPolicyValue(NeuralNetBase):
    """uses a convolutional neural network to evaluate the state of the game
    and compute a probability distribution over the next action
    and value of the current state.
    """

    def _select_moves_and_normalize(self, nn_output, moves, size, transform="noop"):
        """helper function to normalize a distribution over the given list of moves
        and return a list of (move, prob) tuples
        """
        if len(moves) == 0:
            return []
        move_indices = [flatten_idx(idx_transformations(m, size, transform), size) for m in moves]
        # get network activations at legal move locations
        distribution = nn_output[move_indices]
        distribution = distribution / distribution.sum()
        return zip(moves, distribution)

    def batch_eval_policy_state(self, states, moves_lists=None):
        """Given a list of states, evaluates them all at once to make best use of GPU
        batching capabilities.

        Analogous to [eval_policy_state(s) for s in states]

        Returns: a parallel list of move distributions as in eval_policy_state
        """

        #print 'batch_eval_policy_state', states
        exit()
        n_states = len(states)
        if n_states == 0:
            return []
        state_size = states[0].size
        if not all([st.size == state_size for st in states]):
            raise ValueError("all states must have the same size")
        # concatenate together all one-hot encoded states along the 'batch' dimension
        nn_input = np.concatenate([self.preprocessor.state_to_tensor(s) for s in states], axis=0)
        # pass all input through the network at once (backend makes use of
        # batches if len(states) is large)
        network_policy, network_value = self.forward(nn_input)
        # default move lists to all legal moves
        moves_lists = moves_lists or [st.get_legal_moves() for st in states]
        results = [None] * n_states
        for i in range(n_states):
            results[i] = self._select_moves_and_normalize(network_policy[i], moves_lists[i],
                                                          state_size)
        return results

    def batch_eval_value_state(self, states):
        return np.array([random.random() for x in states])        

    def eval_policy_state(self, state, moves=None):
        return [(action, random.random()) for action in state.get_legal_moves()]

    def eval_value_state(self, state):
        return random.random()

    @staticmethod
    def create_network(**kwargs):
        pass

def self_play_and_save(player, opp_player, boardsize, mock_state=[]):    
    '''Run num_games games to completion, keeping track of each position
    and move of the new_player.  And save the game data

    '''
    state_list = []
    pi_list = []
    player_list = []

    board_size = boardsize
    state = go.GameState(size=board_size, komi=0)

    # Allowing injection of a mock state object for testing purposes
    if mock_state:
        state = mock_state

    player_color = go.BLACK
    current = player
    other = opp_player

    step = 0
    while not state.is_end_of_game:
        move = current.get_move(state, self_play=True)
        #print(move)
        childrens = current.mcts._root._children.items()
        #print(childrens)
        actions, next_states = map(list, zip(*childrens))
        _n_visits = [next_state._n_visits for next_state in next_states]
        if not move == go.PASS_MOVE:
            if step < 25: # temperature is considered to be 1
                distribution = np.divide(_n_visits, np.sum(_n_visits))
            else:
                max_visit_idx = np.argmax(_n_visits)
                distribution = np.zeros(np.shape(_n_visits))
                distribution[max_visit_idx] = 1.0
        else: # to prevent the model from overfitting to PASS_MOVE
            distribution = np.zeros(np.shape(_n_visits))
        pi = zip(actions, distribution)
        #print(zip(actions, _n_visits))
        state_list.append(state.copy())
        #pprint_board(state.board)
        pi_list.append(pi)

        current.mcts.update_with_move(move)
        state.do_move(move)
        other.mcts.update_with_move(move)
        current, other = other, current
        step += 1

    winner = state.get_winner()
    print 'winner', winner
    if winner == go.BLACK:
        reward_list = [(-1.)**j for j in range(len(state_list))]
    else : # winner == go.WHITE:
        reward_list = [(-1.)**(j+1) for j in range(len(state_list))]
    return state_list, pi_list, reward_list

def run_self_play(cmd_line_args=None):
    while True:
        # Set initial conditions
        policy = MockPolicyValue()

        boardsize = 9
        # different opponents come from simply changing the weights of 'opponent.policy.model'. That
        # is, only 'opp_policy' needs to be changed, and 'opponent' will change.
        opp_policy = MockPolicyValue()

        for i in range(10):
            print(str(i) + "th self playing game")
            player = MCTSPlayer(policy.eval_value_state, policy.eval_policy_state, n_playout=10, evaluating=False, self_play=True)
            opp_player= MCTSPlayer(opp_policy.eval_value_state, opp_policy.eval_policy_state, n_playout=10, evaluating=False, self_play=True)
            state_list, pi_list, reward_list = self_play_and_save(opp_player, player, boardsize)
            print len(state_list)
            #print state_list[0]

            b = util.get_board(state_list[20])
            print type(b)
            print b.shape
            
            exit()
            #data_to_save["state"] = state_list
            #data_to_save["pi"] = pi_list
            #data_to_save["reward"] = reward_list
            del player
            del opp_player
        #metadata["self_play_model"] += [best_weight_path]
        #save_metadata()
        del policy
        del opp_policy

if __name__ == '__main__':
    run_self_play()
