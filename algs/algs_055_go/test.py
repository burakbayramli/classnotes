import unittest, pickle, util, simplenet, resnet
import numpy as np, gtp 

class BasicTest(unittest.TestCase):

    def testGnuGo(self):
        gnugo = gtp.GTPFacade("white", ["gnugo", "--mode", "gtp", "--level", "10"])
        gnugo.boardsize(9)
        gnugo.komi(5.5)
        gnugo.clear_board()

        for i in range(10):
            res = gnugo.genmove(gtp.WHITE)
            print 'res', res
            gnugo.showboard()        
    
    def testBasic(self):
        state, pi, reward = pickle.load(open("testdata.pkl"))
        state._create_neighbors_cache()

        lm = state.get_legal_moves()
        print len(lm) > 10
        
        print util.pprint_board(state.board)
        
        res = util.to_pi_mat(pi)
        
        self.assertTrue(res[0]==0)

        net = simplenet.PolicyValue(simplenet.PolicyValue.create_network())
        
        res = net.eval_value_state(state)

        self.assertTrue(np.abs(res) >= 0.0 and np.abs(res) < 1.0)

        net = resnet.PolicyValue(resnet.PolicyValue.create_network())

        self.assertTrue(np.abs(res) >= 0.0 and np.abs(res) < 1.0)

        b = util.get_board(state)
        self.assertTrue(b.shape == (17, 9, 9))
        
if __name__ == "__main__": 

    unittest.main()
    
