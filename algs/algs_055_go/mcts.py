import numpy as np, resource, sys
from operator import itemgetter

sys.setrecursionlimit(1500)
resource.setrlimit(resource.RLIMIT_STACK, [0x10000000, resource.RLIM_INFINITY])
sys.setrecursionlimit(0x100000)

class TreeNode(object):
    """MCTS agacindaki bir dugum. Her dugum kendi Q degerini, onceki
    olasiligi P'yi, ve kac kez ziyaret edildigi sayisiyla duzeltilmis
    onsel skoru u'yu biliyor.
    """
    def __init__(self, parent, prior_p):
        self._parent = parent
        self._children = {} 
        self._n_visits = 0
        self._W = 0 
        self._Q = 0
        self._u = prior_p
        self._P = prior_p

    def printing(self):
        print(self._children)
        for _, child in self._children.iteritems():
            child.printing()

    def expand(self, action_priors):
        for action, prob in action_priors:
            if action not in self._children:
                self._children[action] = TreeNode(self, prob)

    def select(self):        
        """Cocuklar arasinda maksimum aksiyon Q + u(P)'yi vereni sec
        """
        return max(self._children.iteritems(),
                   key=lambda act_node: act_node[1].get_value())

    def update(self, leaf_value, c_puct):
        """Dugumu altindaki cocuklardan gelen irdeleme uzerinden guncelle

        Arguments:
        
        leaf_value -- mevcut oyuncu perspektifinden alt agacin degeri
        
        c_puct -- (0, inf) arasinda bir sayi, bu dugumun skoru
        uzerinde Q ve P'nun etkisini izafi olarak ayarlar

        Returns:
        None

        """
        self._n_visits += 1
        self._W += leaf_value
        self._Q = self._W / self._n_visits
        if not self.is_root():
            self._u = c_puct * self._P * np.sqrt(self._parent._n_visits) / (1 + self._n_visits)

    def update_recursive(self, leaf_value, c_puct):
        if self._parent:
            self._parent.update_recursive(leaf_value, c_puct)
        self.update(leaf_value, c_puct)

    def get_value(self):
        return self._Q + self._u

    def is_leaf(self):
        return self._children == {}

    def is_root(self):
        return self._parent is None


class MCTS(object):
    def __init__(self, value_fn, policy_fn, c_puct=5, n_playout=1600):
        self._root = TreeNode(None, 1.0)
        self._value = value_fn
        self._policy = policy_fn
        self._c_puct = c_puct
        self._n_playout = n_playout

    def _playout(self, state, self_play):
        node = self._root
        if not node.is_leaf() and self_play:
            tmp = [0.03 for _ in range(len(node._children.items()))]
            etas = np.random.dirichlet(tmp,1)[0]
            j = 0
            for action, child_node in node._children.iteritems():
                child_node._P = 0.75*child_node._P + 0.25*etas[j]
                j += 1

        while True:
            if node.is_leaf():
                action_probs = self._policy(state)
                # Check for end of game.
                if len(action_probs) == 0:
                    break
                if node.is_root() and self_play:
                    tmp = [0.03 for _ in range(len(action_probs))]
                    etas = np.random.dirichlet(tmp,1)[0]
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

        # Alt dugumunun degerini YSA'yi kullanarak hesapla
        leaf_value = self._value(state)

        node.update_recursive(leaf_value, self._c_puct)


    def get_move(self, state, temperature, self_play):
        for n in range(self._n_playout):
            state_copy = state.copy()
            self._playout(state_copy, self_play)

        if temperature > 0:
            childrens = self._root._children.items()
            actions, next_states = map(list, zip(*childrens))
            tmp = [next_state._n_visits for next_state in next_states]
            exponentiated_n_visits = np.power(tmp,1./temperature)
            pi = np.divide(exponentiated_n_visits, np.sum(exponentiated_n_visits))
            child_idx = range(len(childrens))
            child_idx = np.random.choice(child_idx, p = pi)
            return actions[child_idx]
        else : # when temperature is infinitesimal
            return max(self._root._children.iteritems(),
                       key=lambda act_node: act_node[1]._n_visits)[0]

    def update_with_move(self, last_move):
        if last_move in self._root._children:
            self._root = self._root._children[last_move]
            self._root._parent = None
        else:
            self._root = TreeNode(None, 1.0)

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

        self.move_count += 1
        if not self.evaluating:
            if self.move_count == 2:
                self.temperature = 0.
        return go.PASS_MOVE

