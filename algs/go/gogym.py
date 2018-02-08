#!/usr/bin/env python
from six.moves import input as raw_input
import argparse
import pachi_py
import gym
from gym import spaces, envs
from gym.envs.board_game import go

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--raw_actions', action='store_true')
    args = parser.parse_args()

    env = envs.make('Go9x9-v0')
    env.reset()
    while True:
        s = env._state
        print 's', s
        env._render()

        colorstr = pachi_py.color_to_str(s.color)
        if args.raw_actions:
            a = int(raw_input('{} (raw)> '.format(colorstr)))
            print 'a 1', a
            print 'coordstr 1', coordstr
        else:
            coordstr = raw_input('{}> '.format(colorstr))
            print 'coordstr 2', coordstr
            a = go.str_to_action(s.board, coordstr)
            print 'a 2', a
        _, r, done, _ = env.step(a)
        if done:
            break

    print
    print('You win!' if r > 0 else 'Opponent wins!')
    print('Final score:', env._state.board.official_score)

if __name__ == '__main__':
    main()
