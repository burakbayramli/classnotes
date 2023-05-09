import collections
import random
import math
import random
import itertools
import pandas as pd
import numpy as np

def _new_board():
    return ((0, 0, 0),
            (0, 0, 0),
            (0, 0, 0))

def apply_move(board_state, move, side):
    move_x, move_y = move

    def get_tuples():
        for x in range(3):
            if move_x == x:
                temp = list(board_state[x])
                temp[move_y] = side
                yield tuple(temp)
            else:
                yield board_state[x]

    return tuple(get_tuples())


def available_moves(board_state):
    for x, y in itertools.product(range(3), range(3)):
        if board_state[x][y] == 0:
            yield (x, y)


def _has_3_in_a_line(line):
    return all(x == -1 for x in line) | all(x == 1 for x in line)

def has_winner(board_state):
    # check rows
    for x in range(3):
        if _has_3_in_a_line(board_state[x]):
            return board_state[x][0]
    # check columns
    for y in range(3):
        if _has_3_in_a_line([i[y] for i in board_state]):
            return board_state[0][y]

    # check diagonals
    if _has_3_in_a_line([board_state[i][i] for i in range(3)]):
        return board_state[0][0]
    if _has_3_in_a_line([board_state[2 - i][i] for i in range(3)]):
        return board_state[0][2]

    return 0  # no one has won, return 0 for a draw

def _upper_confidence_bounds(payout, samples_for_this_machine, log_total_samples):
    return payout / samples_for_this_machine + \
        math.sqrt((2 * log_total_samples) / samples_for_this_machine)


def monte_carlo_tree_search_uct(board_state, side, number_of_samples):
    state_results = collections.defaultdict(float)
    state_samples = collections.defaultdict(float)

    for _ in range(number_of_samples):
        current_side = side
        current_board_state = board_state
        first_unvisited_node = True
        rollout_path = []
        result = 0

        while result == 0:
            move_states = {move: apply_move(current_board_state, move, current_side)
                           for move in available_moves(current_board_state)}

            if not move_states:
                result = 0
                break

            if all((state in state_samples) for _, state in move_states):
                log_total_samples = math.log(sum(state_samples[s] \
                                                 for s in move_states.values()))
                l = lambda _, s: _upper_confidence_bounds(state_results[s],
                                                          state_samples[s],
                                                          log_total_samples)
                move, state = max(move_states, key=l)
            else:
                move = random.choice(list(move_states.keys()))

            current_board_state = move_states[move]

            if first_unvisited_node:
                rollout_path.append((current_board_state, current_side))
                if current_board_state not in state_samples:
                    first_unvisited_node = False

            current_side = -current_side

            result = has_winner(current_board_state)

        for path_board_state, path_side in rollout_path:
            state_samples[path_board_state] += 1.
            result *= path_side
            result /= 2.
            result += .5
            state_results[path_board_state] += result

    move_states = {move: apply_move(board_state, move, side) \
                   for move in available_moves(board_state)}

    l2 = lambda x: state_results[move_states[x]] / state_samples[move_states[x]]
    move = max(move_states, key=l2)

    return state_results[move_states[move]] / state_samples[move_states[move]], move


