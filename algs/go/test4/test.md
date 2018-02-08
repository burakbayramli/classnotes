
```python
from six.moves import input as raw_input
import argparse
import pachi_py
import gym
from gym import spaces, envs
from gym.envs.board_game import go

env = envs.make('Go9x9-v0')
env.reset()
s = env._state
a = go.str_to_action(s.board, "c3")
xx, r, done, xxx = env.step(a)
env._render()
```

```text
To play: black
Move:   2  Komi: 0.0  Handicap: 0  Captures B: 0 W: 0
      A B C D E F G H J  
    +-------------------+
  9 | . . . . . . . . . |
  8 | . . . . . . . . . |
  7 | . . . . . O). . . |
  6 | . . . . . . . . . |
  5 | . . . . . . . . . |
  4 | . . . . . . . . . |
  3 | . . X . . . . . . |
  2 | . . . . . . . . . |
  1 | . . . . . . . . . |
    +-------------------+
Out[1]: <StringIO.StringIO instance at 0x7f2e27f51ef0>
```

```python
print xx[1]
```

```text
[[0 0 0 0 0 0 0 0 0]
 [0 0 0 0 0 0 0 0 0]
 [0 0 0 0 0 1 0 0 0]
 [0 0 0 0 0 0 0 0 0]
 [0 0 0 0 0 0 0 0 0]
 [0 0 0 0 0 0 0 0 0]
 [0 0 0 0 0 0 0 0 0]
 [0 0 0 0 0 0 0 0 0]
 [0 0 0 0 0 0 0 0 0]]
```











