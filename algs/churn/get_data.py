import numpy as np
from sklearn.preprocessing import normalize

# Configurable observation look-back period for each engine/day
max_time = 100



"""Load and parse engine data files into:

   - an (engine/day, observed history, sensor readings) x tensor,
     where observed history is 100 days, zero-padded for days that
     don't have a full 100 days of observed history (e.g., first
     observed day for an engine)

   - an (engine/day, 2) tensor containing time-to-event and 1 (since
     all engines failed)

There are probably MUCH better ways of doing this, but I don't use
Numpy that much, and the data parsing isn't the point of this demo
anyway.

"""
def load_file(name):
    with open(name, 'r') as file:
        return np.loadtxt(file, delimiter=',')

np.set_printoptions(suppress=True, threshold=10000)

def build_data(engine, time, x, max_time, is_test):
    # y[0] will be days remaining, y[1] will be event indicator, always 1 for this data
    out_y = np.empty((0, 2), dtype=np.float32)

    # A full history of sensor readings to date for each x
    out_x = np.empty((0, max_time, 24), dtype=np.float32)

    for i in range(100):
        print("Engine: " + str(i))
        # When did the engine fail? (Last day + 1 for train data, irrelevant for test.)
        max_engine_time = int(np.max(time[engine == i])) + 1

        if is_test:
            start = max_engine_time - 1
        else:
            start = 0

        this_x = np.empty((0, max_time, 24), dtype=np.float32)

        for j in range(start, max_engine_time):
            engine_x = x[engine == i]

            out_y = np.append(out_y, np.array((max_engine_time - j, 1), ndmin=2), axis=0)

            xtemp = np.zeros((1, max_time, 24))
            xtemp[:, max_time-min(j, 99)-1:max_time, :] = engine_x[max(0, j-max_time+1):j+1, :]
            this_x = np.concatenate((this_x, xtemp))

        out_x = np.concatenate((out_x, this_x))

    return out_x, out_y

if __name__ == "__main__": 
 
    train = load_file('train.csv')
    test_x = load_file('test_x.csv')
    test_y = load_file('test_y.csv')

    # Combine the X values to normalize them, then split them back out
    all_x = np.concatenate((train[:, 2:26], test_x[:, 2:26]))
    all_x = normalize(all_x, axis=0)

    train[:, 2:26] = all_x[0:train.shape[0], :]
    test_x[:, 2:26] = all_x[train.shape[0]:, :]

    # Make engine numbers and days zero-indexed, for everybody's sanity
    train[:, 0:2] -= 1
    test_x[:, 0:2] -= 1


    train_x, train_y = build_data(train[:, 0], train[:, 1], train[:, 2:26], max_time, False)
    test_x = build_data(test_x[:, 0], test_x[:, 1], test_x[:, 2:26], max_time, True)[0]

    train_u = np.zeros((100, 1), dtype=np.float32)
    train_u += 1
    test_y = np.append(np.reshape(test_y, (100, 1)), train_u, axis=1)

    f = '%1.3f'
    np.save('data/train_x',train_x)
    np.save('data/train_y',train_y)
    np.save('data/test_x',test_x)
    np.save('data/test_y',test_y)

    
