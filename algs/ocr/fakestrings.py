# -*- coding: utf8 -*-

import random

#simple weighted choice implementation from https://stackoverflow.com/questions/3679694/a-weighted-version-of-random-choice
def weighted_choice(choices):
   total = sum(w for c, w in choices)
   r = random.uniform(0, total)
   upto = 0
   for c, w in choices:
      if upto + w >= r:
         return c
      upto += w
   assert False, "Shouldn't get here"


def ICO():
    ico = ""
    weight = 8
    weight_sum = 0
    while len(ico) < 7:
        num = random.randint(0, 9)
        ico = ico + str(num)
        weight_sum = weight_sum + weight * num
        weight -= 1
    last_num = (11 - (weight_sum % 11)) % 10
    ico = ico + str(last_num)
    # dic = "CZ" + ico
    return ico


def DIC():
    dic = "CZ" + ICO()
    return dic


def phone_num():
    type = weighted_choice([(1, 0.4), (2, 0.3), (3, 0.1), (4, 0.2)])
    stuffs = []
    for i in range(0, 9):
        stuffs.append(random.randint(0, 9))
    if type is 1:
        return "+420 %d%d%d %d%d%d %d%d%d" % tuple(stuffs)
    if type is 2:
        return "+420-%d%d%d%d%d%d%d%d%d" % tuple(stuffs)
    if type is 3:
        return "+420%d%d%d%d%d%d%d%d%d" % tuple(stuffs)
    if type is 4:
        return "%d%d%d%d%d%d%d%d%d" % tuple(stuffs)


def data():
    month = random.randint(1, 12)
    long_month = "1,3,5,7,8,10,12,"
    if (str(month) + ",") in long_month:
        day = random.randint(1, 31)
    else:
        if month == 2:
            day = random.randint(1, 29)
        else:
            day = random.randint(1, 30)
    year = random.randint(1980, 2020)
    type = weighted_choice([(1,0.4),(2,0.4),(3,0.2)])
    if type is 1:
        date = str(day) + "." + str(month) + "." + str(year)
    if type is 2:
        date = str(day) + ". " + str(month) + ". " + str(year)
    if type is 3:
        date = str(day) + "-" + str(month) + "-" + str(year)
    return date


def account_num():
    prefix = ""
    while len(prefix) < 6:
        num = random.randint(0, 9)
        prefix = prefix + str(num)
    main_num = ""
    while len(main_num) < 10:
        num = random.randint(0, 9)
        main_num = main_num + str(num)
    i = random.randint(0, 1)
    if (i == 0):
        account = prefix + "-" + main_num
    if (i == 1):
        account = main_num
    # account = main_num + "/" + bank_code
    return account


def bank_num():
    bank_list = ['0100', '0300', '0600', '0710', '0730', '0800', '2010', '2020', '2030', '2040', '2050', '2060', '2070', '2100', '2200', '2210', '2220', '2230',
                 '2240', '2250', '2310', '2600', '2700', '3030', '3500', '4000', '4300', '5000', '5400', '5500', '5800', '6000', '6100', '6200', '6210', '6300',
                 '6700', '6800', '7910', '7940', '7950', '7960', '7970', '7980', '7990', '8030', '8040', '8060', '8090', '8150', '8200', '9870', '9890', '9910',
                 '9960', '9980']
    bank_code = bank_list[random.randint(0, len(bank_list) - 1)]
    return bank_code


def IBAN():
    account = account_num()
    code_bank = account[len(account) - 4:]
    main = account[:len(account) - 5]
    main = main.replace("-", "")
    num_1 = str(random.randint(0, 9))
    num_2 = str(random.randint(0, 9))
    iban = "CZ" + num_1 + num_2 + code_bank + "0000" + main
    return iban


def const_symbol():
    # use to generate specific sym., constant sym.
    sym = ""
    i = random.randint(1, 4)
    while len(sym) < i:
        num = random.randint(0, 9)
        sym = sym + str(num)
    return sym


def var_symbol():
    # use to generate specific sym., constant sym.
    sym = ""
    i = random.randint(1, 10)
    while len(sym) < i:
        num = random.randint(0, 9)
        sym = sym + str(num)
    return sym


def amount():
    thousand_sep, dec_sep = random.choice([(' ',','),('',','),('.',','),(' ','.')])
    length = random.randint(1,7)
    amount = []
    
    if random.randint(0,10):
        if random.randint(0,1):
            if random.randint(0,5):
                amount.append('00')
            else:
                amount.append('-')
                dec_sep = ','
        else:
            amount.append(str(random.randint(0,9)))
            amount.append(str(random.randint(0,9)))
        amount.append(dec_sep)
    else:
        thousand_sep = ''
    
    if length == 1:
        if random.randint(0,1):
            amount.append('0')
            return ''.join(reversed(amount))
              
    for i in range(length):
        if i == length -1:
            amount.append(str(random.randint(1,9)))
            break
        else:
            amount.append(str(random.randint(0,9)))
        if (i+1) % 3 == 0:
            amount.append(thousand_sep)
    return ''.join(reversed(amount))
    
    
def invoice_id():
    large = random.randint(1, 9)
    invid = ""
    while len(invid) < large:
        num = random.randint(0, 9)
        invid = invid + str(num)
    return invid 


def swift_code():
    bic_list = ['KOMBCZPP', 'CEKOCZPP', 'AGBACZPP', 'CNBACZPP', 'GIBACZPX', 'FIOBCZPP', 'BOTKCZPP', 'CITFCZPP', 'MPUBCZPP', 'FICHCZPP', 'ARTTCZPP', 'POBNCZPP',
                'CTASCZ22', 'ZUNOCZPP', 'CITICZPX', 'BACXCZPP', 'AIRACZPP', 'INGBCZPP', 'SOLACZPP', 'CMZRCZP1', 'ABNACZPP', 'RZBCCZPP', 'JTBPCZPP', 'PMBPCZPP',
                'EQBKCZPP', 'COBACZPX', 'BREXCZPP', 'GEBACZPP', 'SUBACZPP', 'VBOECZ2X', 'DEUTCZPX', 'SPWTCZ21', 'GENOCZ21', 'OBKLCZ2X', 'CZEECZPP', 'MIDLCZPP']
    num = random.randint(0, len(bic_list) - 1)
    return bic_list[num]


def randomstring(wantedlabels=None):
    if wantedlabels is None:
        wantedlabels = ['account_num','amount_due','amount_paid','amount_rounding','amount_total','bank_num', 'bic',
             'const_sym','customer_id','date_due','date_issue','date_uzp','iban','invoice_id',
             'order_id','phone_num','recipient_dic','recipient_ic','sender_dic','sender_ic','spec_sym','var_sym']
    lab = random.choice(wantedlabels)
    if lab is 'ico' or lab is 'sender_ic' or lab is 'recipient_ic':
        return ICO() , lab
    if lab is 'dic' or lab is 'sender_dic' or lab is 'recipient_dic':
        return DIC() , lab
    if lab is 'phone_num':
        return phone_num() , lab
    if lab is 'bic':
        return swift_code() , lab
    if lab is 'data' or lab is 'date_due' or lab is 'date_issue' or lab is 'date_uzp':
        return data() , lab
    if lab is 'account_num':
        return account_num() , lab
    if lab is 'bank_num':
        return bank_num() , lab
    if lab is 'iban':
        return IBAN() , lab
    if lab is 'const_sym' or lab is 'customer_id' or lab is 'order_id' or lab is 'spec_sym':
        return const_symbol() , lab
    if lab is 'var_sym':
        return var_symbol() , lab
    if lab is 'amount' or lab is 'amount_rounding' or lab is 'amount_due' or lab is 'amount_total' or lab is 'amount_paid':
        return amount() , lab
    if lab is 'invoice_id':
        return invoice_id() , lab
    else:
        raise ValueError("No such label: %s" % lab)
