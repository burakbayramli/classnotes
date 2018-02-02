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
                out += 'O '
            else:
                out += 'X '
        out += str(board_size-j)
        print(out)
    print(out0)


def plot_board(board, history, out_directory, output_file, should_plot=False, western_column_notation=True):
    try:
        import matplotlib
        matplotlib.use('Agg')
        import matplotlib.pyplot as plt
        import matplotlib.cm as cm
    except ImportError as e:
        print('Failed to import matplotlib.')
        raise e

    from distutils.version import StrictVersion
    matplotlib_version = matplotlib.__version__
    if StrictVersion(matplotlib_version) < StrictVersion('1.5.1'):
        print('Your version of matplotlib might not support our use of it')

    # Initial matplotlib setup
    fig, ax = plt.subplots(figsize=(10, 10))
    plt.xlim([0, board.shape[0] + 1])
    plt.ylim([0, board.shape[1] + 1])

    # Wooden background color
    ax.set_axis_bgcolor('#fec97b')
    plt.gca().invert_yaxis()

    # Setup ticks
    ax.tick_params(axis='both', length=0, width=0)
    # Western notation has the origin at the lower-left
    if western_column_notation:
        plt.xticks(range(1, board.shape[0] + 1), range(1, board.shape[0] + 1))
        plt.yticks(range(1, board.shape[1] + 1), reversed(range(1, board.shape[1] + 1)))
    # Traditional has the origin at the upper-left and uses letters minus 'I' along the top
    else:
        ax.xaxis.tick_top()
        plt.xticks(range(1, board.shape[0] + 1), [x for x in LETTERS[:board.shape[0] + 1] if x != 'I'])
        plt.yticks(range(1, board.shape[1] + 1), range(1, board.shape[1] + 1))

    # Draw grid
    for i in range(board.shape[0]):
        plt.plot([1, board.shape[0]], [i + 1, i + 1], lw=1, color='k', zorder=0)
    for i in range(board.shape[1]):
        plt.plot([i + 1, i + 1], [1, board.shape[1]], lw=1, color='k', zorder=0)

    # Display stones already played
    stone_x_coords = []
    stone_y_coords = []
    stone_colors = []
    for i in range(board.shape[0]):
        for j in range(board.shape[1]):
            if board[i][j] != go.EMPTY:
                stone_x_coords.append(i + 1)
                stone_y_coords.append(j + 1)
                if board[i][j] == go.BLACK:
                    stone_colors.append('black')
                else:
                    stone_colors.append('w')
    plt.scatter(stone_x_coords, stone_y_coords, marker='o', edgecolors='k',
                s=700, c=stone_colors, zorder=3)

    # Display numbers on stones
    for i, action in enumerate(history):
        if action != go.PASS_MOVE:
            if i%2 == 0:
                plt.text(action[0]+1, action[1]+1, str(i+1), ha='center', va='center', color='w')
            else:
                plt.text(action[0]+1, action[1]+1, str(i+1), ha='center', va='center', color='black')

    if output_file is not None:
        plt.savefig(os.path.join(out_directory, output_file), bbox_inches='tight')
    if should_plot:
        plt.show()
    plt.close()
