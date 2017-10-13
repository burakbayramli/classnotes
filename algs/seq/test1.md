
```python
import data_utils

en_token_ids, en_seq_lens, en_vocab_dict, en_rev_vocab_dict = data_utils.process_data('data/my_en.txt', max_vocab_size=5000, target_lang=False)
sp_token_ids, sp_seq_lens, sp_vocab_dict, sp_rev_vocab_dict = data_utils.process_data('data/my_sp.txt', max_vocab_size=5000, target_lang=True)

print 'vocab en'
print en_vocab_dict
print 'vocab sp'
print sp_vocab_dict

train_encoder_inputs,\
train_decoder_inputs,\
train_targets, \
train_en_seq_lens,\
train_sp_seq_len, \
valid_encoder_inputs,\
valid_decoder_inputs,\
valid_targets, \
valid_en_seq_lens,\
valid_sp_seq_len = data_utils.split_data(en_token_ids, sp_token_ids, en_seq_lens, sp_seq_lens,train_ratio=0.8)
```

```text
vocab en
{'all': 97, 'four': 98, 'causes': 99, 'seemed': 100, 'synthetic': 53, 'uphold': 101, 'tremors': 102, 'concerned': 103, 'father': 54, 'young': 104, 'languages': 55, 'to': 7, 'sent': 106, 'activities': 107, 'very': 108, 'rise': 109, 'translations': 110, 'Richter': 111, 'try': 112, 'limitations': 350, '_GO': 1, 'says': 114, 'even': 115, 'what': 116, 'weren': 117, 'melt': 118, 'spokesman': 119, 'scientists': 120, 'body': 121, 'never': 122, 'desired': 123, 'English': 124, 'healthy': 125, 'economics': 126, 'amount': 127, 'makes': 128, 'Constitution': 129, 'manage': 130, 'visible': 131, 'Tom': 29, 'from': 56, 'takes': 133, 'working': 134, '0': 105, 'E-cigarettes': 135, 'few': 136, 'live': 137, 'recommend': 138, 'until': 140, 'today': 141, 'more': 57, 'diamond': 142, 'Erick': 143, 'chocolate': 58, 'Roger': 144, 'must': 145, 'this': 146, 'can': 38, 'learn': 147, 'growing': 59, 'example': 148, 'high': 61, 'something': 149, 'want': 62, 'phrase': 150, 'native': 24, 'huge': 151, 'end': 152, 'get': 251, 'damage': 154, 'how': 155, 'instead': 156, 'economy': 63, 'A': 39, 'tension': 157, 'may': 64, 'after': 159, 'wrong': 160, 'produce': 161, 'fits': 162, 'a': 10, 'cigarettes': 163, 'so': 65, 'pulled': 164, 'longest': 165, 'breaking': 153, 'over': 30, 'soon': 166, 'ended': 167, 'looks': 168, 'still': 169, 'its': 66, 'll': 170, ',': 5, 'him': 77, 'window': 67, 'permanent': 172, 'Using': 173, 'main': 295, 'then': 175, 'them': 68, 'good': 40, 'practice': 69, 'impossible': 209, 'effects': 177, 'they': 41, 'front': 178, 'hearing': 208, 'realize': 179, 'easily': 180, 'In': 181, 'settings': 182, 'rocks': 183, 'magnitude': 184, 'found': 185, 'everyone': 186, 'year': 187, 'our': 31, 'ring': 188, 'really': 96, 'large': 190, "'": 12, 'dialects': 191, 'since': 71, 'health': 72, 'dioxide': 192, 'got': 193, 'cause': 194, 'announced': 195, 'colorless': 196, 'tempo': 197, '_UNK': 3, 'quite': 198, 'offhandedly': 199, 'put': 42, 'org': 200, 'language': 32, 'Though': 364, 'times': 202, 'place': 203, 'first': 204, 'spoken': 205, 'Oklahoma': 206, 'There': 207, 'yourself': 132, 'ironing': 60, 'wages': 210, 'system': 212, 'their': 211, 'too': 214, 'which': 88, 'wrapped': 215, 'countries': 259, 'Association': 216, 'that': 11, 'shelf': 217, 'cops': 218, 'than': 73, 'kind': 219, 'require': 220, 'matter': 221, 'speaking': 222, 'iron': 33, 'result': 223, 'and': 8, '_EOS': 2, 'have': 74, 'sentences': 75, 'need': 224, 'seem': 225, 'saw': 226, 'speakers': 227, 'After': 228, 'sound': 34, 'ashore': 229, 'online': 230, 'banjo': 231, 'play': 232, 'though': 233, 'who': 234, 'The': 43, '\xe2\x80\x94': 389, 'don': 235, 'clear': 236, 'face': 237, 'Tatoeba': 238, 'laws': 239, 'saying': 240, 'businesses': 241, 'one': 70, 'We': 76, 'proposed': 322, 'Top-down': 243, 'justice': 244, 'sentence': 245, 'folded': 246, 'surroundings': 247, 'only': 248, 'meant': 249, 'his': 17, 'hit': 250, 'means': 139, 'pocket': 252, 'unimportant': 253, 'fastest': 254, 'words': 255, 'fabrics': 171, 'Swimming': 257, 'tobacco': 258, 'hiring': 176, 'she': 260, 'where': 261, 'iPad': 262, 'computer': 263, 'are': 18, 'ability': 264, 'knees': 265, 'said': 266, 'closet': 267, 'That': 268, 'away': 269, 'difficulties': 270, 'various': 271, 'correctly': 272, 'probably': 273, 'we': 44, 'jobs': 274, 'creating': 78, 'attention': 275, 'news': 277, 'restaurant': 278, 'many': 79, 'barely': 279, 'against': 280, 'players': 281, 's': 19, 'became': 80, 'long-term': 282, 'comment': 283, 'had': 81, 'excess': 284, 'alcohol-related': 285, 'learning': 286, 'carbon': 82, 'swimmer': 287, 'discrimination': 288, 'quickly': 289, 'willing': 276, 'expected': 291, 'threw': 292, 'photos': 293, 'present': 294, 'And': 213, 'criminal': 174, 'vanilla': 83, 'choices': 296, 'will': 297, 'pace': 298, 'while': 299, 'warning': 377, 'ceilings': 301, 'country': 302, 'promoted': 303, 'mistake': 84, 'is': 15, 'it': 14, 'middle': 304, 'someone': 305, 'in': 9, 'You': 306, 'fibers': 308, 'make': 85, 'same': 35, 'speaker': 25, 'underestimate': 309, 'largest': 310, 'week': 311, 'I': 312, 'moment': 313, 'footprint': 314, 'off': 315, 'database': 316, 'Miller': 317, 'well': 318, 'aftershock': 319, 'contact': 320, 'mother': 321, 'the': 4, 'If': 86, 'just': 242, 'less': 87, 'being': 323, 'shape': 324, 'rooms': 325, 'alternative': 326, 'Nicholas': 327, 'yet': 328, 'adding': 329, 'death': 330, 'seems': 331, 'board': 332, 'electrical': 333, 'has': 334, 'swim': 335, 'around': 307, 'Project': 336, 'know': 89, 'background': 337, 'world': 338, 'like': 26, 'offices': 339, 't': 27, 'night': 340, 'translated': 341, 'works': 342, 'cooled': 343, 'old': 344, 'often': 45, 'people': 90, 'some': 345, 'back': 91, 'ideals': 346, 'strongest': 347, '5': 380, 'Mary': 36, 'Channel': 348, 'scale': 349, '_PAD': 0, 'cord': 113, 'be': 28, 'shaken': 351, 'on': 21, 'about': 46, 'of': 13, 'practical': 352, 'Japan': 353, 'shiny': 354, 'block': 355, 'pollution': 356, 'own': 357, 'No': 358, 'into': 359, 'down': 360, 'tatoeba': 361, '1990s': 362, '.': 6, 'your': 47, 'fabric': 363, 'her': 92, 'area': 201, 'housing': 365, 'noticed': 366, 'overestimate': 367, 'start': 368, 'much': 290, 'way': 369, 'was': 48, 'Obama': 370, 'but': 22, 'heat': 371, 'uncle': 372, 'highest': 373, 'with': 37, 'he': 23, 'made': 49, 'up': 93, 'convince': 50, 'accustomed': 374, 'called': 375, 'doesn': 94, 'an': 376, 'as': 51, 'too-hot': 300, 'at': 16, 'education': 378, 'again': 95, 'when': 52, 'other': 379, 'Five': 158, 'sick': 381, 'you': 20, 'out': 189, 'users': 382, 'important': 383, 'Cindy': 384, 'authorities': 385, 'died': 386, 'Dover': 387, 'building': 388, 'applause': 256, 'exhausting': 390, 'At': 391, 'time': 392, 'starting': 393, 'others': 394}
vocab sp
{'kullan\xc4\xb1c\xc4\xb1lar': 62, 'masas\xc4\xb1n\xc4\xb1': 63, 'Gen\xc3\xa7lerin': 64, 'hata': 32, 'durumda': 65, '0': 66, '\xc3\xa7\xc3\xbcnk\xc3\xbc': 67, '\xc3\xa7evrimi\xc3\xa7i': 68, 'g\xc3\xb6r\xc3\xbcnsede': 69, '\xc3\xbcretti\xc4\x9fimiz': 70, 'onun': 10, 'sonunda': 71, 'konusunda': 72, 'sentetik': 73, 'ideallerimiz': 74, 'Richter': 75, 'senin': 20, 'yasalar\xc4\xb1': 76, 'dil': 77, 'alma': 78, 'olan': 79, 'haber': 80, 'kablosunu': 81, '\xc4\xb0nsanlar\xc4\xb1': 82, 'imkans\xc4\xb1zd\xc4\xb1r': 83, '_GO': 1, 'leh\xc3\xa7eler': 85, 'konu\xc5\x9fmak': 86, 'olacakt\xc4\xb1r': 87, 'teklif': 373, '\xc3\xa7ekmek': 362, 'temas': 89, 'hasara': 90, '\xc4\xb1s\xc4\xb1': 91, 'biri': 251, 'pratik': 21, ';': 93, '\xc5\x9fu': 94, 'daha': 33, '\xc3\xa7\xc3\xb6kt\xc3\xbc': 95, 'd\xc3\xbcnya': 363, 'Y\xc3\xbcksek': 98, 'konu\xc5\x9fulursa': 99, 'i\xc5\x9fletmelerimiz': 100, 'se\xc3\xa7imler': 101, 'olabilir': 102, 'yerli': 12, 's\xc3\xb6zc\xc3\xbc': 103, 'Tom': 14, 've': 7, 'e\xc4\x9fitim': 104, 'kal\xc4\xb1c\xc4\xb1': 105, 'a\xc5\x9fa\xc4\x9f\xc4\xb1ya': 106, 'b\xc3\xbcy\xc3\xbcyor': 160, 'g\xc3\xb6r\xc3\xbcnm\xc3\xbcyor': 107, 'daki': 108, '\xc3\xbct\xc3\xbclerken': 109, 'kuma\xc5\x9fla': 97, 'bilgisayardan': 111, 'sistemini': 113, 'Roger': 114, '\xc3\xbclkede': 115, 'olu\xc5\x9fturma': 116, 'babas\xc4\xb1': 35, '1990': 117, 'ettiren': 118, 'ancak': 119, 'alk\xc4\xb1\xc5\x9flarken': 120, 'nedenle': 121, 'yapt\xc4\xb1klar\xc4\xb1': 122, 'etti\xc4\x9fi': 36, 'bunun': 37, 's\xc4\xb1n\xc4\xb1rlar\xc4\xb1n\xc4\xb1': 123, '\xc3\xa7ekiyor': 124, 'etti': 125, 'sonucu': 126, 'g\xc3\xbc\xc3\xa7l\xc3\xbc': 127, 'Ekonomimizin': 128, 'beklenen': 129, 'budur': 130, '\xc3\xa7alabilinceye': 131, 'renksiz': 132, 'E\xc4\x9fer': 133, 'dikkat': 134, 'kadar': 13, 'uyar\xc4\xb1yorlar': 135, 'nedeni': 136, 'konu\xc5\x9fmac\xc4\xb1': 22, 'hafta': 137, '\xc4\xb0ngilizcesi': 138, 'Sentetik': 139, '\xc5\x9fey': 140, 'ya\xc5\x9famaya': 186, 'fark': 38, '\xc3\xb6l\xc3\xbcm\xc3\xbcn': 142, '\xc3\xb6rnek': 143, 'bir\xc3\xa7ok': 39, 'konu\xc5\x9fanlar\xc4\xb1': 144, 'bina': 145, 'ediliyor': 146, 'ayakizi': 147, 'hasta': 230, 'zorlukla': 149, 'te\xc5\x9fvik': 239, 'y\xc3\xbczmenin': 151, 'koyamazs\xc4\xb1n\xc4\xb1z': 152, 'g\xc3\xb6re': 153, 'otoriteleri': 154, 'bulunabilen': 155, 'eritecek': 156, 'vanilya': 40, '\xc3\xbczerine': 157, 'le': 158, '\xc3\xbct\xc3\xbcy\xc3\xbc': 159, 'nin': 41, ',': 5, 'Projesi': 161, '\xc3\xbct\xc3\xbc': 162, 'lardan': 163, 'yorum': 164, 'Derne\xc4\x9finden': 165, 'ye': 166, 'hala': 167, 'karbon': 168, 'gerektirdi\xc4\x9fi': 169, 'muhtemelen': 170, 'alternatif': 171, 'olduk\xc3\xa7a': 172, 'al\xc4\xb1p': 173, 'etmeyi': 174, 'istekli': 175, 'kar\xc5\x9f\xc4\xb1': 42, 'y\xc3\xbcz\xc3\xbcndeki': 176, '\xc3\xa7\xc4\xb1kard\xc4\xb1': 177, 'etmeye': 178, 'ikna': 23, 'en': 15, 'fazla': 179, 'sard\xc4\xb1': 180, "'": 8, 'odalar\xc4\xb1': 181, '\xc3\xa7evrilmi\xc5\x9f': 182, '\xc3\xb6l\xc3\xa7e\xc4\x9fine': 183, 'desteklemeliyiz': 184, '\xc3\xb6nemli': 185, 'hemen': 141, 'yarat\xc4\xb1yorlar': 187, 'c\xc3\xbcmlelerden': 188, '_UNK': 3, 'olmak': 189, 'yapt\xc4\xb1\xc4\x9f\xc4\xb1nda': 190, 'org': 191, 'En': 192, 'sonra': 43, 'i\xc3\xa7in': 44, '\xc3\xb6\xc4\x9frenmeye': 193, 'yeteneklerini': 194, 'vadeli': 195, '\xc3\xa7\xc4\xb1karmak': 196, 's\xc3\xb6ylemeyi': 197, 'ta\xc5\x9flar': 198, 'do\xc4\x9fru': 199, 'ceza': 200, 'bloklar\xc4\xb1ndan': 201, 'Oklahoma': 202, 'konu\xc5\x9fman': 84, 'edinme': 203, 'onlara': 204, 'yine': 205, 'onu': 24, 'ayarlar\xc4\xb1': 206, 'arad\xc4\xb1': 207, 'oldu': 208, 'g\xc3\xb6r\xc3\xbcn\xc3\xbcr': 209, '\xc3\xb6n\xc3\xbcnde': 210, 'g\xc3\xb6r\xc3\xbcn\xc3\xbcyordu': 211, 'zorunday\xc4\xb1z': 212, 'baz\xc4\xb1': 213, 'd\xc3\xbc\xc5\x9f\xc3\xbcncesizce': 214, '\xc3\xb6yle': 215, 'g\xc3\xb6r\xc3\xbclebilir': 216, 'sa\xc4\x9fl\xc4\xb1kl\xc4\xb1': 217, 'alan': 45, 'ayn\xc4\xb1': 25, 'b\xc3\xbcy\xc3\xbckl\xc3\xbc\xc4\x9f\xc3\xbc': 218, '\xc4\xb1n': 219, 'oldu\xc4\x9fu': 220, 'duyurdu': 221, 'tavanl\xc4\xb1': 222, 'yapmak': 223, 'sa\xc4\x9flar': 224, 'sigaralar': 225, 'uyum': 226, 'konu\xc5\x9ftu\xc4\x9funu': 227, '\xc3\xa7evresi': 228, 'ana': 229, 'olarak': 46, 'yukar\xc4\xb1dan': 148, 'deneyimini': 231, 'anlam\xc4\xb1na': 232, 'dolaba': 233, 'sadece': 234, 'yapt\xc4\xb1\xc4\x9f\xc4\xb1': 235, 'ofis': 236, 'dilindeki': 47, 'ba\xc5\x9flang\xc4\xb1\xc3\xa7ta': 237, 'vurmad\xc4\xb1\xc4\x9f\xc4\xb1': 238, 'katlad\xc4\xb1': 150, 'h\xc4\xb1zl\xc4\xb1': 240, 'yerde': 241, 'ki': 48, '\xc3\xa7abucak': 242, 'att\xc4\xb1': 243, 'Tatoeba': 49, 'annesi': 244, 'Alkolle': 245, 'Y\xc3\xbcz\xc3\xbcc\xc3\xbc': 246, 'restoranda': 247, 'kendi': 248, '\xc3\xbccretler': 249, 'ge\xc3\xa7mi\xc5\x9f': 250, 'i\xc5\x9fler': 92, 'Birden': 252, 'Y\xc3\xbczme': 253, 'cam\xc4\xb1n\xc4\xb1': 254, 'kald\xc4\xb1rd\xc4\xb1': 255, 'Bir': 50, 'evlenme': 256, 'de': 16, 'beden': 257, 'da': 51, 'a\xc5\x9fan': 258, 'kuma\xc5\x9flar\xc4\xb1': 52, '\xc3\x9ct\xc3\xbc': 259, '\xc3\xa7ikolata': 260, 'tekrar': 26, '\xc3\xbczerinde': 261, 'istiyorsan': 262, 'dizlerinin': 263, 'ortas\xc4\xb1nda': 264, 'ban\xc3\xa7o': 265, 'olmad\xc4\xb1\xc4\x9f\xc4\xb1n\xc4\xb1': 266, 'iPad': 267, 'ses': 268, 'foto\xc4\x9fraf': 269, 'kullanmak': 270, 'sa\xc4\x9fl\xc4\xb1k': 271, 'ya\xc5\x9f\xc4\xb1ndayken': 272, '\xc3\xa7ikolatan\xc4\xb1n': 273, 'kolayl\xc4\xb1kla': 274, 'y\xc3\xbcz\xc3\xbck': 275, 'faaliyetlerimizin': 276, 'ekonominin': 277, 'gecenin': 278, 'istenilen': 279, 'g\xc3\xb6zlerinde': 280, 'de\xc4\x9fi\xc5\x9fik': 281, 'tempoda': 53, 'ile': 282, 'd\xc3\xbczey': 283, 'yapmaya': 284, 't\xc3\xbcr\xc3\xbc': 285, 'gerilimi': 286, 'so\xc4\x9fuduktan': 287, '_EOS': 2, 'adalet': 288, 's\xc4\xb1cak': 289, 'kirlenmesinin': 290, 'elektrik': 291, '.': 4, 'etkilerine': 390, 'Senin': 293, 'sebep': 294, 'vard\xc4\xb1': 295, 'Yani': 296, 'ba\xc5\x9flamalar\xc4\xb1': 297, '\xc3\xa7evirileri': 298, 'iyi': 17, '\xc3\xb6nemlidir': 299, '\xc3\xbcst': 300, 'par\xc3\xa7ay\xc4\xb1': 301, 'oldu\xc4\x9funu': 302, '\xc3\xa7o\xc4\x9funlukla': 303, 'sonu\xc3\xa7land\xc4\xb1': 304, 'koydu': 305, 'cebinden': 306, 'oldu\xc4\x9funa': 27, 'olmal\xc4\xb1s\xc4\xb1n': 307, 'biliyor': 308, 'uzun': 54, '\xc3\xa7al\xc4\xb1\xc5\x9f\xc4\xb1yor': 345, 'Ayr\xc4\xb1mc\xc4\xb1l\xc4\xb1\xc4\x9fa': 309, 'rafa': 371, 'd\xc3\xb6rt': 310, 'al\xc4\xb1\xc5\x9fk\xc4\xb1nd\xc4\xb1r': 311, 'birden': 312, 'yerine': 313, 'defalarca': 314, 'yerini': 315, 'Elektronik': 316, 'herkes': 317, '\xc3\xb6nemsiz': 318, 'konu\xc5\x9fan\xc4\xb1': 319, 'Daver': 320, 'k\xc4\xb1rarak': 321, 'Ve': 322, 'sarst\xc4\xb1': 348, 'ard\xc4\xb1ndan': 324, 'Miller': 325, 'c\xc3\xbcmleleri': 326, '\xc3\xbct\xc3\xbcye': 394, 'ilgili': 328, 'hatal\xc4\xb1': 329, '\xc3\xb6neririz': 330, 'genellikle': 331, '\xc3\xbct\xc3\xbcn\xc3\xbcn': 332, '\xc4\xb1': 333, 'hakk\xc4\xb1nda': 56, 'zaman': 57, 'parlak': 334, '\xc3\xa7al\xc4\xb1\xc5\x9f': 335, 'elmas': 336, 'polisi': 337, 'yorucu': 338, 'Nicholas': 339, 'hen\xc3\xbcz': 340, 'konu\xc5\x9fmad\xc4\xb1\xc4\x9f\xc4\xb1n\xc4\xb1': 341, 'b\xc3\xbcy\xc3\xbct\xc3\xbcrek': 397, 'vard\xc4\xb1r': 398, 'b\xc3\xbcy\xc3\xbcd\xc3\xbc\xc4\x9f\xc3\xbc': 344, 'ama': 55, 'sars\xc4\xb1nt\xc4\xb1': 346, 'birilerini': 347, 'i\xc5\x9fe': 58, 'karbondioksit': 323, 'ba\xc5\x9fka': 349, 'bir': 6, '\xc3\xa7\xc4\xb1kt\xc4\xb1': 350, '\xc3\xb6ld\xc3\xbc': 351, 'g\xc3\xb6rd\xc3\xbcm': 352, 'dikkatini': 353, '\xc5\x9fekilde': 354, 'g\xc3\xb6nderildi': 355, 'amcas\xc4\xb1': 356, 'Mary': 18, 'penceresine': 357, 'miktar\xc4\xb1d\xc4\xb1r': 358, '_PAD': 0, 'Anayasa': 359, 'gelir': 360, 't\xc3\xbct\xc3\xbcne': 361, 'art\xc3\xa7\xc4\xb1n\xc4\xb1n': 88, 'ger\xc3\xa7ekten': 59, 'bilmeyen': 96, 'bu': 19, 'bilim': 364, 'y\xc3\xbckselmeye': 365, 'dilin': 366, 'o': 11, '\xc3\xbclke': 367, 'konut': 368, 'be\xc5\x9f': 369, '\xc3\xa7ok': 28, 's\xc3\xb6yledi': 370, 'anda': 34, 'eklemeni': 372, 'kendini': 112, 'ba\xc5\x9fl\xc4\xb1yor': 374, 'zorluklar\xc4\xb1n\xc4\xb1': 375, '\xc3\xa7al\xc4\xb1\xc5\x9f\xc4\xb1rsan': 376, 'becerebilsen': 377, 'Obama': 378, 'yaramad\xc4\xb1\xc4\x9f\xc4\xb1n\xc4\xb1': 379, 'y\xc3\xbcksek': 380, 'gibi': 9, 'Hen\xc3\xbcz': 381, 'adamlar\xc4\xb1': 382, 'beri': 383, 's\xc4\xb1k': 384, 'ortaya': 385, 'ekonomi': 386, 'istedi\xc4\x9fimiz': 387, 'bizim': 29, 'un': 388, 'fakat': 30, 'dili': 389, 'dile': 292, 'yapt\xc4\xb1': 391, 'olu\xc5\x9fan': 392, 'k\xc4\xb1y\xc4\xb1ya': 393, 'az': 60, 'Erick': 110, 'hafife': 327, '\xc3\xbczerindeki': 395, 'ne': 61, 'etmek': 396, '5': 342, 'herkesin': 343, 'duymaya': 399, '\xc3\xa7alanlar\xc4\xb1n': 400, 'veritaban\xc4\xb1': 401, 'Cindy': 402, 's\xc3\xb6ylerse': 403, 'e': 404, 'Kanal': 405, 'asla': 406, 'b\xc3\xbcy\xc3\xbck': 31, 'Japonya': 407}
20 training samples and 5 validations samples
```

```python
print len(train_encoder_inputs)
print 'encoder---------------'
print train_encoder_inputs
print 'decoder---------------'
print train_decoder_inputs
print 'target-------------'
print train_targets

```

```text
20
encoder---------------
[[144 317  12 ...,   0   0   0]
 [ 43 238 336 ...,   0   0   0]
 [ 29 193 360 ...,   0   0   0]
 ..., 
 [ 29  49   4 ...,   0   0   0]
 [243 126 122 ...,   0   0   0]
 [ 39  82 314 ...,   0   0   0]]
decoder---------------
[[  1 381   6 272 114 325   8 219  35 351   7 141 324 244 230 208   5  11
   19 121 202   8 108 356 110   8 158 186 355   4   2   0   0   0   0   0
    0   0   0   0   0   0   0]
 [  1  49   4 191  51  68 155   5  49 161  39 292 182 143 188 392  31   6
  401 116 261 345   4   2   0   0   0   0   0   0   0   0   0   0   0   0
    0   0   0   0   0   0   0]
 [  1  14 247 343 210 263 157  95   5 306   6 336 275 177   7 317 120  18
    8 166 256 373 125   4   2   0   0   0   0   0   0   0   0   0   0   0
    0   0   0   0   0   0   0]
 [  1  14  10 353 362  44 278 264  18   8  41 357 198 243  30  37 313  10
  254 321 304   7  18   8  41  35 337 207   4   2   0   0   0   0   0   0
    0   0   0   0   0   0   0]
 [  1 322  17  80  94  48 386  26 160   4   2   0   0   0   0   0   0   0
    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    0   0   0   0   0   0   0]
 [  1 316 225   5 361  42 217   6 171  46 239 146  55 271 154   5  62 395
   54 195 390 134 124   4   2   0   0   0   0   0   0   0   0   0   0   0
    0   0   0   0   0   0   0]
 [  1  14   8 388 138  57  57 172  17  69   5  11 123 308   9 107   7  11
    6  32 190  24 329  27  23 396  83   4   2   0   0   0   0   0   0   0
    0   0   0   0   0   0   0]
 [  1 252 179 111 267   8 404 274 269 152   4   2   0   0   0   0   0   0
    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    0   0   0   0   0   0   0]
 [  1  98 222   7  31 181  79   6 145  10 315  45 132 236 201  33  60  21
  102   5  55 331 228 282  17 226 224   4   2   0   0   0   0   0   0   0
    0   0   0   0   0   0   0]
 [  1  75 183 153 218  66   4  66   8 333 258 369 346 234  19 137 407 348
    5 119 364 382 129  15  31  88 340 238  72 135   4   2   0   0   0   0
    0   0   0   0   0   0   0]
 [  1  50  77  61  13  28 115  99   5  12 319   9 268 196  11  13  33  60
  299   5  67  11 366 144 281  85 399 311   4   2   0   0   0   0   0   0
    0   0   0   0   0   0   0]
 [  1 259 287  43  18 291  81 394 180   5 159  26 371 305   5 162  63 150
    7  24 205 233 255   4   2   0   0   0   0   0   0   0   0   0   0   0
    0   0   0   0   0   0   0]
 [  1  64 384 235   6  32  51  93 375 327 173   5 248 194  16 280 397  25
   34  39 389 312 193 297   4   2   0   0   0   0   0   0   0   0   0   0
    0   0   0   0   0   0   0]
 [  1  82 273  40  27  61  13  23 178 376 335   5  11 167 260   5 112   7
  349 347  40  27  23 174 377  16   4   2   0   0   0   0   0   0   0   0
    0   0   0   0   0   0   0]
 [  1 309  42  76   5  58  78   5 368 203   5 104   7 200 288 113  16 184
    4 359   7  15 300 283  74 169 130   4   2   0   0   0   0   0   0   0
    0   0   0   0   0   0   0]
 [  1 246   5 402 339   5 338   6 151  71 149 320   8  16 393 295  30 405
  253 165   6 103  10  28  17   6  65 302 221   4   2   0   0   0   0   0
    0   0   0   0   0   0   0]
 [  1 245 328 142 310 229 136 398   4   2   0   0   0   0   0   0   0   0
    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    0   0   0   0   0   0   0]
 [  1  14 215 214 164 391  48 237 318   9 211  30  43  10 176   7 257  47
  286 352   7  37  10  44  59 363  13 185 220 242 385 350   4   2   0   0
    0   0   0   0   0   0   0]
 [  1 378 148 106 277 406  58 379 370   4   2   0   0   0   0   0   0   0
    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    0   0   0   0   0   0   0]
 [  1  50 168 147  29 276   6 126  46  70 323 290 358   4   2   0   0   0
    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    0   0   0   0   0   0   0]]
target-------------
[[381   6 272 114 325   8 219  35 351   7 141 324 244 230 208   5  11  19
  121 202   8 108 356 110   8 158 186 355   4   2   0   0   0   0   0   0
    0   0   0   0   0   0   0]
 [ 49   4 191  51  68 155   5  49 161  39 292 182 143 188 392  31   6 401
  116 261 345   4   2   0   0   0   0   0   0   0   0   0   0   0   0   0
    0   0   0   0   0   0   0]
 [ 14 247 343 210 263 157  95   5 306   6 336 275 177   7 317 120  18   8
  166 256 373 125   4   2   0   0   0   0   0   0   0   0   0   0   0   0
    0   0   0   0   0   0   0]
 [ 14  10 353 362  44 278 264  18   8  41 357 198 243  30  37 313  10 254
  321 304   7  18   8  41  35 337 207   4   2   0   0   0   0   0   0   0
    0   0   0   0   0   0   0]
 [322  17  80  94  48 386  26 160   4   2   0   0   0   0   0   0   0   0
    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    0   0   0   0   0   0   0]
 [316 225   5 361  42 217   6 171  46 239 146  55 271 154   5  62 395  54
  195 390 134 124   4   2   0   0   0   0   0   0   0   0   0   0   0   0
    0   0   0   0   0   0   0]
 [ 14   8 388 138  57  57 172  17  69   5  11 123 308   9 107   7  11   6
   32 190  24 329  27  23 396  83   4   2   0   0   0   0   0   0   0   0
    0   0   0   0   0   0   0]
 [252 179 111 267   8 404 274 269 152   4   2   0   0   0   0   0   0   0
    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    0   0   0   0   0   0   0]
 [ 98 222   7  31 181  79   6 145  10 315  45 132 236 201  33  60  21 102
    5  55 331 228 282  17 226 224   4   2   0   0   0   0   0   0   0   0
    0   0   0   0   0   0   0]
 [ 75 183 153 218  66   4  66   8 333 258 369 346 234  19 137 407 348   5
  119 364 382 129  15  31  88 340 238  72 135   4   2   0   0   0   0   0
    0   0   0   0   0   0   0]
 [ 50  77  61  13  28 115  99   5  12 319   9 268 196  11  13  33  60 299
    5  67  11 366 144 281  85 399 311   4   2   0   0   0   0   0   0   0
    0   0   0   0   0   0   0]
 [259 287  43  18 291  81 394 180   5 159  26 371 305   5 162  63 150   7
   24 205 233 255   4   2   0   0   0   0   0   0   0   0   0   0   0   0
    0   0   0   0   0   0   0]
 [ 64 384 235   6  32  51  93 375 327 173   5 248 194  16 280 397  25  34
   39 389 312 193 297   4   2   0   0   0   0   0   0   0   0   0   0   0
    0   0   0   0   0   0   0]
 [ 82 273  40  27  61  13  23 178 376 335   5  11 167 260   5 112   7 349
  347  40  27  23 174 377  16   4   2   0   0   0   0   0   0   0   0   0
    0   0   0   0   0   0   0]
 [309  42  76   5  58  78   5 368 203   5 104   7 200 288 113  16 184   4
  359   7  15 300 283  74 169 130   4   2   0   0   0   0   0   0   0   0
    0   0   0   0   0   0   0]
 [246   5 402 339   5 338   6 151  71 149 320   8  16 393 295  30 405 253
  165   6 103  10  28  17   6  65 302 221   4   2   0   0   0   0   0   0
    0   0   0   0   0   0   0]
 [245 328 142 310 229 136 398   4   2   0   0   0   0   0   0   0   0   0
    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    0   0   0   0   0   0   0]
 [ 14 215 214 164 391  48 237 318   9 211  30  43  10 176   7 257  47 286
  352   7  37  10  44  59 363  13 185 220 242 385 350   4   2   0   0   0
    0   0   0   0   0   0   0]
 [378 148 106 277 406  58 379 370   4   2   0   0   0   0   0   0   0   0
    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    0   0   0   0   0   0   0]
 [ 50 168 147  29 276   6 126  46  70 323 290 358   4   2   0   0   0   0
    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    0   0   0   0   0   0   0]]
```











