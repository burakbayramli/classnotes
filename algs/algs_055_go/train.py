import os, glob, pickle, go
import json, re, util
import numpy as np
from shutil import copy
from mcts import MCTSPlayer
from util import flatten_idx, pprint_board
from tensorflow.contrib.keras import optimizers as O
from tensorflow.contrib.keras import callbacks as C
from tensorflow.contrib.keras import backend as K
import resnet
import simplenet

def self_play_and_save(player, opp_player):
    
    state_list = []
    pi_list = []
    player_list = []

    state = go.GameState(size=9, komi=0)

    player_color = go.BLACK
    current = player
    other = opp_player

    step = 0
    while not state.is_end_of_game:
        move = current.get_move(state, self_play=True)

        childrens = current.mcts._root._children.items()

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
        pi_list.append(pi)

        state_list.append(state.copy())

        current.mcts.update_with_move(move)
        state.do_move(move)
        other.mcts.update_with_move(move)
        current, other = other, current
        step += 1

    winner = state.get_winner()
    print 'winner', winner
    # oyun bitti kimin kazandigini biliyoruz, mesela siyah kazandiysa
    # odulleri hamle bazinda +1,-1,+1,.. olacak sekilde ata, beyaz
    # kazandiysa -1,+1,-1 seklinde. Siyah olunca +1 cunku oyuna hep siyah
    # basliyor.
    if winner == go.BLACK:
        reward_list = [(-1.)**j for j in range(len(state_list))]
    else : # winner == go.WHITE:
        reward_list = [(-1.)**(j+1) for j in range(len(state_list))]
    return state_list, pi_list, reward_list

def self_play_and_train(cmd_line_args=None):

    # iki farkli ag yarat, egitim bunlardan ilkini gunceller.
    # belli bir sure sonra oteki YSA ilkinin kaydettigi veriden guncellenir,
    # ve ikisi tekrar esit hale gelir, bu boyle devam eder.
    policy = simplenet.PolicyValue(simplenet.PolicyValue.create_network())
    opp_policy = simplenet.PolicyValue(simplenet.PolicyValue.create_network())

    def lr_scheduler(epoch):
        if epoch == 5000:
            K.set_value(model.optimizer.lr, .001)
        elif epoch == 7000:
            K.set_value(model.optimizer.lr, .0001)
        return K.get_value(model.optimizer.lr)

    change_lr = C.LearningRateScheduler(lr_scheduler)
    sgd = O.SGD(lr=.01, momentum=0.9)
    policy.model.compile(loss=['categorical_crossentropy','mean_squared_error'],
                         optimizer=sgd)        

    batch_size = 50
    n_pick = 10 # her oyundan kac veri noktasi alalim
        
    for epoch in range(1000):

        state_list2 = []
        pi_list2 = []
        reward_list2 = []        
        
        while True: # batch_size kadar veri toplayincaya kadar oyna
            try:
                player = MCTSPlayer(policy.eval_value_state,
                                    policy.eval_policy_state,
                                    n_playout=50, evaluating=False,
                                    self_play=True)
                
                opp_player= MCTSPlayer(opp_policy.eval_value_state,
                                       opp_policy.eval_policy_state,
                                       n_playout=50, evaluating=False,
                                       self_play=True)
                
                state_list, pi_list, reward_list = self_play_and_save(
                    opp_player, player
                )

                # oyunda atilan tum adimlar, sonuclar state_list,
                # pi_list, reward_list listesi icinde. Simdi rasgele
                # n_pick tane veri noktasi her oyun kaydindan cekip
                # cikartilir.
                idxs = [np.random.choice(range(10,len(state_list)),replace=False) \
                        for i in range(n_pick)]
                
                print 'picked results', idxs
                
                for idx in idxs:
                    state_list2.append(state_list[idx])
                    pi_list2.append(pi_list[idx])
                    reward_list2.append(reward_list[idx])
                    
                if len(state_list2) >= batch_size: break
            except:
                print 'exception'
                continue

        pout = np.zeros((batch_size, 9*9+1))
        vout = np.zeros((batch_size, 1))
        Y = [pout, vout]
        X = np.zeros((batch_size, 17, 9, 9))

        for i in range(len(state_list2)):
            vout[i,:] = reward_list2[i]
            X[i, :] = util.get_board(state_list2[i])
            pout[i,:] = util.to_pi_mat(pi_list2[i])
                                                
        policy.model.fit(X, Y)

        if epoch % 5 == 0:
            print 'saving'
            policy.save()

        if epoch % 50 == 0:
            print 'birincinin en son kayitli agirliklarindan bu agi guncelle'
            opp_policy.load()

if __name__ == '__main__':
    self_play_and_train()
