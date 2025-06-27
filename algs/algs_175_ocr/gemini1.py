import numpy as np
import matplotlib.pyplot as plt
import tensorflow as tf

train_inputs_0 = np.asarray(
    [[0.633766, 0.221185, 0.0917319, 0.0129757, 0.0142857, 0.0260553],
     [0.111121, 0.588392, 0.278779, 0.0055756, 0.00569609, 0.010436],
     [0.0357786, 0.633813, 0.321418, 0.00249248, 0.00272882, 0.0037688],
     [0.0663296, 0.643849, 0.280111, 0.00283995, 0.0035545, 0.00331533],
     [0.458235, 0.396634, 0.123377, 0.00648837, 0.00903441, 0.00623107]],
    dtype=np.float32)

def sparse_tuple_from(sequences, dtype=np.int32):
    indices = []
    values = []
    lengths = []
    for n, seq in enumerate(sequences):
        indices.extend(zip([n] * len(seq), range(len(seq))))
        values.extend(seq)
        lengths.append(len(seq))
    indices = np.asarray(indices, dtype=np.int64)
    values = np.asarray(values, dtype=dtype)
    if indices.size == 0:
        shape = np.asarray([len(sequences), 0], dtype=np.int64)
    else:
        shape = np.asarray([len(sequences), np.asarray(indices).max(0)[1] + 1], dtype=np.int64)
    return indices, values, shape, np.asarray(lengths, dtype=np.int32)


single_dummy_target = [[1, 2, 3]]
target_indices, target_values, target_shape, target_lengths = sparse_tuple_from(single_dummy_target)

num_features = 6 # This represents the number of classes in your output (plus one for blank if blank is not a separate feature)

@tf.function
def ctc_model(logits_input_batch, target_indices, target_values, target_shape, sequence_lengths, label_lengths):
    logits_transposed = tf.transpose(logits_input_batch, (1, 0, 2)) # [max_time, batch_size, num_features]

    targets = tf.SparseTensor(indices=target_indices, values=target_values, dense_shape=target_shape)

    blank_idx = num_features - 1 # Assuming num_features already includes the blank class as the last one

    loss = tf.nn.ctc_loss(
        targets,
        logits_transposed,
        tf.cast(label_lengths, tf.int32),
        tf.cast(sequence_lengths, tf.int32),
        blank_index=blank_idx # Explicitly provide blank_index
    )

    decoded, log_prob = tf.nn.ctc_greedy_decoder(
        inputs=logits_transposed,
        sequence_length=tf.cast(sequence_lengths, tf.int32),
        blank_index=blank_idx # Also provide blank_index here for consistency
    )

    return loss, decoded, log_prob

batch_logits = tf.expand_dims(tf.constant(train_inputs_0, dtype=tf.float32), axis=0) # Shape becomes (1, 5, 6)
batch_seq_len = tf.constant([5], dtype=tf.int32)

loss_val, decoded_val, log_prob_val = ctc_model(
    logits_input_batch=batch_logits,
    target_indices=tf.constant(target_indices, dtype=tf.int64),
    target_values=tf.constant(target_values, dtype=tf.int32),
    target_shape=tf.constant(target_shape, dtype=tf.int64),
    sequence_lengths=batch_seq_len,
    label_lengths=tf.constant(target_lengths, dtype=tf.int32)
)

print("Loss:", loss_val.numpy())
for i, d in enumerate(decoded_val):
    print(f"Decoded sequence {i}:", tf.sparse.to_dense(d).numpy())
print("Log Probabilities:", log_prob_val.numpy())
