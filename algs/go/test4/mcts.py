"""
Monte Carlo Tree Search, as described in Silver et al 2017.

This is a "pure" implementation of the AlphaGo MCTS algorithm in that
it is not specific to the game of Go; everything in this file is
implemented generically with respect to some state, actions, policy
function, and value function.
"""
import numpy as np, resource, sys
from operator import itemgetter

sys.setrecursionlimit(1500)
resource.setrlimit(resource.RLIMIT_STACK, [0x10000000, resource.RLIM_INFINITY])
sys.setrecursionlimit(0x100000)

class TreeNode(object):
    """A node in the MCTS tree. Each node keeps track of its own value Q,
    prior probability P, and its visit-count-adjusted prior score u.
    """

    def __init__(self, parent, prior_p):
        self._parent = parent
        self._children = {}  # a map from action to TreeNode
        self._n_visits = 0
        self._W = 0 # This was not in the original code.
        self._Q = 0
        # This value for u will be overwritten in the first call to
        # update(), but is useful for choosing the first action from
        # this node.
        self._u = prior_p
        self._P = prior_p

    def printing(self):
        print(self._children)
        for _, child in self._children.iteritems():
            child.printing()

    def expand(self, action_priors):
        """Expand tree by creating new children.

        Arguments:
        action_priors -- output from policy function - a list of tuples of actions and their prior
            probability according to the policy function.

        Returns:
        None
        """
        for action, prob in action_priors:
            if action not in self._children:
                self._children[action] = TreeNode(self, prob)

    def select(self):
        """Select action among children that gives maximum action value, Q plus bonus u(P).

        Returns:
        A tuple of (action, next_node)
        """
        return max(self._children.iteritems(), key=lambda act_node: act_node[1].get_value())

    def update(self, leaf_value, c_puct):
        """Update node values from leaf evaluation.

        Arguments:
        leaf_value -- the value of subtree evaluation from the current player's perspective.
        c_puct -- a number in (0, inf) controlling the relative impact of values, Q, and
            prior probability, P, on this node's score.

        Returns:
        None
        """
        # Count visit.
        self._n_visits += 1
        # Update W
        self._W += leaf_value
        # Update Q, a running average of values for all visits.
        self._Q = self._W / self._n_visits
        # Update u, the prior weighted by an exploration
        # hyperparameter c_puct and the number of visits. Note that u
        # is not normalized to be a distribution.
        if not self.is_root():
            self._u = c_puct * self._P * np.sqrt(self._parent._n_visits) / (1 + self._n_visits)

    def update_recursive(self, leaf_value, c_puct):
        """Like a call to update(), but applied recursively for all ancestors.

        Note: it is important that this happens from the root downward
        so that 'parent' visit counts are correct.

        """
        # If it is not root, this node's parent should be updated first.
        if self._parent:
            self._parent.update_recursive(leaf_value, c_puct)
        self.update(leaf_value, c_puct)

    def get_value(self):
        """Calculate and return the value for this node: a combination of leaf
        evaluations, Q, and this node's prior adjusted for its visit
        count, u

        """
        return self._Q + self._u

    def is_leaf(self):
        """Check if leaf node (i.e. no nodes below this have been expanded).
        """
        return self._children == {}

    def is_root(self):
        return self._parent is None


class MCTS(object):
    """A simple (and slow) single-threaded implementation of Monte Carlo Tree Search.

    Search works by exploring moves randomly according to the given
    policy up to a certain depth, which is relatively small given the
    search space. "Leaves" at this depth are assigned a value by the
    value function evaluated at that leaf.The probability of
    revisiting a node changes over the course of the many playouts
    according to its estimated value.  Ultimately the node which is
    chosen based on the number of visits is returned as the next
    action.

    The term "playout" refers to a single search from the root.

    """

    def __init__(self, value_fn, policy_fn, c_puct=5, n_playout=1600):
        """Arguments:
        value_fn -- a function that takes in a state and ouputs a score in [-1, 1], i.e. the
            expected value of the end game score from the current player's perspective.
        policy_fn -- a function that takes in a state and outputs a list of (action, probability)
            tuples for the current player.
        c_puct -- a number in (0, inf) that controls how quickly exploration converges to the
            maximum-value policy, where a higher value means relying on the prior more, and
            should be used only in conjunction with a large value for n_playout.
        """
        self._root = TreeNode(None, 1.0)
        self._value = value_fn
        self._policy = policy_fn
        self._c_puct = c_puct
        self._n_playout = n_playout

    def _playout(self, state, self_play):
        """Run a single playout from the root to the given depth, getting a
        value at the leaf and propagating it back through its
        parents. State is modified in-place, so a copy must be
        provided.

        Arguments:
        state -- a copy of the state.
        self_play -- whether this is on self_play or not

        Returns:
        None

        """
        node = self._root
        if not node.is_leaf() and self_play:
            etas = np.random.dirichlet([0.03 for _ in range(len(node._children.items()))],1)[0]
            j = 0
            for action, child_node in node._children.iteritems():
                child_node._P = 0.75*child_node._P + 0.25*etas[j]
                j += 1

        while True:
            # Only expand node if it has not already been
            # done. Existing nodes already know their prior.
            if node.is_leaf():
                action_probs = self._policy(state)
                # Check for end of game.
                if len(action_probs) == 0:
                    break
                if node.is_root() and self_play:
                    etas = np.random.dirichlet([0.03 for _ in range(len(action_probs))],1)[0]
                    j = 0
                    new_action_probs = []
                    for action, prob in action_probs:
                        prob = 0.75*prob + 0.25*etas[j]
                        new_action_probs.append((action, prob))
                        j += 1
                    action_probs = new_action_probs
                node.expand(action_probs)
                break
            # Greedily select next move.
            action, node = node.select()
            state.do_move(action)

        # Evaluate the leaf using value function which is the
        # subnetwork of policy_value network
        leaf_value = self._value(state)

        # Update value and visit count of nodes in this traversal.
        node.update_recursive(leaf_value, self._c_puct)


    def get_move(self, state, temperature, self_play):
        """Runs all playouts sequentially and returns the action based on
           exponentiated visit count.

        Arguments:
        state -- the current state, including both game state and the current player.

        Returns:
        the selected action

        """
        for n in range(self._n_playout):
            state_copy = state.copy()
            self._playout(state_copy, self_play)

        # action is chosen proportional to its exponentiated visit count
        if temperature > 0:
            childrens = self._root._children.items()
            actions, next_states = map(list, zip(*childrens))
            exponentiated_n_visits = np.power([next_state._n_visits for next_state in next_states],1./temperature)
            pi = np.divide(exponentiated_n_visits, np.sum(exponentiated_n_visits))
            child_idx = range(len(childrens))
            child_idx = np.random.choice(child_idx, p = pi)
            return actions[child_idx]
        else : # when temperature is infinitesimal
            return max(self._root._children.iteritems(), key=lambda act_node: act_node[1]._n_visits)[0]

    def update_with_move(self, last_move):
        """Step forward in the tree, keeping everything we already know about
        the subtree, assuming that get_move() has been called
        already. Siblings of the new root will be garbage-collected.

        """
        if last_move in self._root._children:
            self._root = self._root._children[last_move]
            self._root._parent = None
        else:
            self._root = TreeNode(None, 1.0)


class ParallelMCTS(MCTS):
    pass

class MCTSPlayer(object):
    def __init__(self, value_function, policy_function, c_puct=5, n_playout=1600, evaluating=True, self_play=False):
        self.mcts = MCTS(value_function, policy_function, c_puct, n_playout)
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

