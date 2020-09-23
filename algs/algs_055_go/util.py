import os
import itertools
import numpy as np
import go

BOARD_TRANSFORMATIONS = {
    "noop": lambda feature: feature,
    "rot90": lambda feature: np.rot90(feature, 1), # counter clock wise 90
    "rot180": lambda feature: np.rot90(feature, 2),
    "rot270": lambda feature: np.rot90(feature, 3),
    "fliplr": lambda feature: np.fliplr(feature),
    "flipud": lambda feature: np.flipud(feature),
    "diag1": lambda feature: np.transpose(feature),
    "diag2": lambda feature: np.fliplr(np.rot90(feature, 1))
}

# for board location indexing
LETTERS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

def idx_transformations(position, size, transform):
    if position == go.PASS_MOVE:
        return go.PASS_MOVE
    else:
        tmp = np.zeros((size, size))
        tmp[position] = 1.
        tmp = BOARD_TRANSFORMATIONS[transform](tmp)
        idx = np.where(tmp == 1.)
        return (int(idx[0]),int(idx[1]))

#IDX_TRANSFORMATIONS = {
#    "noop": lambda feature: feature[0],
#    "rot90": lambda feature: (-feature[0][1]+feature[1]-1, feature[0][0]),
#    "rot180": lambda feature: (-feature[0][0]+feature[1]-1, -feature[0][1]+feature[1]-1),
#    "rot270": lambda feature: (feature[0][1], -feature[0][0]+feature[1]-1),
#    "flipud": lambda feature: (-feature[0][0]+feature[1]-1, feature[0][1]),
#    "fliplr": lambda feature: (feature[0][0], -feature[0][1]+feature[1]-1),
#    "diag1": lambda feature: (-feature[0][1]+feature[1]-1, -feature[0][0]+feature[1]-1),
#    "diag2": lambda feature: (feature[0][1], feature[0][0])
#}

def random_transform():
    return np.random.choice(["noop", "rot90", "rot180", "rot270", "fliplr", "flipud", "diag1", "diag2"])

#def idx_transformations(position, size, transform):
#    if position == go.PASS_MOVE:
#        return go.PASS_MOVE
#    else:
#        return IDX_TRANSFORMATIONS[transform]((position, size))

def flatten_idx(position, size):
    if position == go.PASS_MOVE:
        return 0
    else:
        (x, y) = position
        return x * size + y + 1


def unflatten_idx(idx, size):
    if idx == 0:
        return go.PASS_MOVE
    else:
        x, y = divmod(idx-1, size)
        return (x, y)

def pprint_board(board):
    board_size = np.shape(board)[0]
    out0 = '   '
    for j in range(board_size):
        out0 += LETTERS[j] +' '
    print(out0)
    for j in range(board_size):
        if j + 1 < 10:
            out = ' ' + str(board_size-j) + ' '
        else :
            out = str(board_size-j) + ' '
        line = list(board[j])
        for pt in line:
            if pt == go.EMPTY:
                out += '. '
            elif pt == go.BLACK:
                out += 'X '
            else:
                out += 'O '
        out += str(board_size-j)
        print(out)
    print(out0)


def get_board(state):
    """A feature encoding WHITE BLACK and EMPTY on separate planes, but plane 0
    always refers to the current player and plane 1 to the opponent
    """
    planes = np.zeros((17, state.size, state.size))
    board_history = state.board_history
    for i in range(8):
        planes[2*i,:,:] = board_history[7-i] == state.current_player
        planes[2*i+1,:,:] = board_history[7-i] == -state.current_player
    if state.current_player == go.BLACK:
        planes[16,:,:] = 1
    else:
        planes[16,:,:] = 0
    planes = np.array([BOARD_TRANSFORMATIONS["noop"](plane) for plane in planes])
    return planes
    
def to_pi_mat(pi):    
    pi_mat = np.zeros((9,9))
    none_val = False
    for idx,val in pi:
        if idx: pi_mat[idx[0],idx[1]] = val
    	elif val > 0: none_val = True
    pi_mat2 = np.zeros(9*9+1)
    if none_val: pi_mat2[0] = 1.0
    pi_mat2[1:] = pi_mat.reshape(9*9)
    return pi_mat2
