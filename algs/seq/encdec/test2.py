# -*- coding: utf-8 -*-
import data_utils

en_token_ids, en_seq_lens, en_vocab_dict, en_rev_vocab_dict = data_utils.process_data('data/en.p', max_vocab_size=10000, target_lang=False)
sp_token_ids, sp_seq_lens, sp_vocab_dict, sp_rev_vocab_dict = data_utils.process_data('data/sp.p', max_vocab_size=10000, target_lang=True)

print 'vocab en'
print en_vocab_dict
print 'vocab sp'
for i,x in enumerate(sp_vocab_dict):
    if i==10: break
    print x.decode("utf-8", "strict"), sp_vocab_dict[x]
