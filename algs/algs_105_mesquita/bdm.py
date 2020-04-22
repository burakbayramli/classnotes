from __future__ import division
import csv

class Actor(object):
    def __init__(self, name, model, x, c, s):
        self.name = name
        self.x = x
        self.c = c
        self.s = s
        self.r = 1

        self.basic_utilities = {}
        self.probs = {}
        self.expected_utilities = {}
        self.offers = [] 


    def calculate_utilities(self, alter):
        dx = self.model.xmax - self.model.xmin
        mu = self.model.mu

        Us = 2 - 4*(0.5 - 0.5*abs((self.x - alter.x)/(dx)))**int(self.r)
        Uf = 2 - 4*(0.5 + 0.5*abs((self.x - alter.x)/(dx)))**int(self.r)
        Ub = 2 - 4*(0.5 - 0.25*((abs(self.x - mu)+abs(self.x-alter.x))/dx)) \
             **int(self.r)
        Uw = 2 - 4*(0.5 + 0.25*((abs(self.x - mu)+abs(self.x-alter.x))/dx)) \
             **int(self.r)
        Usq = 2 - 4*0.5**self.r

        self.basic_utilities[alter.name] = {"Us": Us,
                                            "Uf": Uf,
                                            "Ub": Ub,
                                            "Uw": Uw,
                                            "Usq": Usq}
    def calculate_prob(self, alter):
        if self.x == alter.x:
            self.probs[alter.name] = 0
            return

        top = 0
        bottom = 0
        for agent in self.model.Actors:
            d = abs(agent.x - alter.x) - abs(agent.x - self.x)
            top += agent.c * agent.s * d
            bottom += agent.c * agent.s * abs(d)
        top = max(top, 0)
        self.probs[alter.name] = top/bottom

    def calculate_expected_utility(self, alter):
        p = self.probs[alter.name]
        utils = self.basic_utilities[alter.name]
        Q = self.model.Q
        T = self.model.T
        s = alter.s
        EU = s * (p * utils["Us"] + (1-p)*utils["Uf"]) + (1-s)*utils["Us"]
        EU -= Q*utils["Usq"] - (1-Q)*(T*utils["Ub"] + (1-T)*utils["Uw"])
        self.expected_utilities[alter.name] = EU

    def calculate_r(self):
        attack_utils = [alter.expected_utilities[self.name] 
            for alter in self.model.Actors if alter is not self]

        security_levels = []
        for actor in self.model.Actors:
            security = sum([alter.expected_utilities[actor.name] 
                    for alter in self.model.Actors if alter is not actor])
            security_levels.append(security)

        max_security = max(security_levels)
        min_security = min(security_levels)
        R = (2 * sum(attack_utils) - max_security - min_security) / \
            max(0.001,(max_security - min_security))
        self.r = (1 - R/3)/(1+R/3)

    def send_offers(self):
        for actor in self.model.Actors:
            if actor.name in self.expected_utilities:
                if self.expected_utilities[actor.name] > 0:
                    # Send a challenge!                    
                    offer = {"Sender": self.name,
                            "x": self.x, # Target position
                            "EU": self.expected_utilities[actor.name]
                            }
                    actor.offers.append(offer)

    def choose_offer(self):
        if len(self.offers) == 0: return

        max_util = max([offer["EU"] for offer in self.offers])
        self.offers = [offer for offer in self.offers if offer["EU"] == max_util]
        offer = min(self.offers, key=lambda x: abs(self.x-x["x"]))

        # Resolve offer
        Uj = offer["EU"]
        Ui = self.expected_utilities[offer["Sender"]]
        if Ui > 0 and Ui < Uj:
            # There was a conflict, and this actor lost
            print self.name + " kaybediyor " + offer["Sender"]
            self.x = offer["x"]
            # If the actor won the conflict, action will be taken on the other end
        elif Ui < 0 and abs(Ui) < Uj:
            # Compromise
            print self.name + " orta noktada anlasiyor " + offer["Sender"]
            self.x += (offer["x"] - self.x) * abs(Ui/Uj)
        elif Ui < 0 and abs(Ui) > Uj:
            # Capituate
            print self.name + " tarafina geciyor " + offer["Sender"]
            self.x = offer["x"]

        self.offers = [] # Reset offers


class Model(object):
    def __init__(self, Actors, xmax, xmin, Q=1.0, T=1.0):
        self.Actors = Actors
        for actor in self.Actors:
            actor.model = self
        self.xmax = xmax
        self.xmin = xmin
        self.dx = xmax - xmin
        self.mu = 0 # Current median position
        self.Q =  Q
        self.T = T

        self.actor = {actor.name: actor for actor in self.Actors}

    def vote(self, verbose=False):
        pairwise_contests = {}
        for j in self.Actors:
            for k in self.Actors:
                votes = 0
                for i in self.Actors:
                    votes += i.c*i.s*((abs(i.x - k.x) - abs(i.x - j.x))/(self.dx))
                pairwise_contests[(j.x, k.x)] = votes
        
        if verbose:
            for key, val in pairwise_contests.items():
                print key, val
        return max(pairwise_contests, key=lambda x: pairwise_contests[x])[0]

    def find_mean(self):
        t = 0 # Running weighted total
        w = 0 # Running total weight
        for actor in self.Actors:
            w += actor.s * actor.c
            t += actor.s * actor.c * actor.x
        return t/w

    def calculate_basic_utilities(self):
        for actor in self.Actors:
            for alter in self.Actors:
                if actor is not alter:
                    actor.calculate_utilities(alter)

    def calculate_win_probabilities(self):
        for actor in self.Actors:
            for alter in self.Actors:
                if actor is not alter:
                    actor.calculate_prob(alter)
    
    def calculate_expected_utilities(self):
        for actor in self.Actors:
            for alter in self.Actors:
                if actor is not alter:
                    actor.calculate_expected_utility(alter)

    def calculate_r(self):
        for actor in self.Actors:
            actor.calculate_r()


    def make_offers(self):
        for actor in self.Actors:
            actor.send_offers()

    def resolve_offers(self):
        for actor in self.Actors:
            actor.choose_offer()

    def run_model(self):
        for actor in self.Actors:
            actor.r = 1
        self.mu = self.vote(False)
        self.calculate_basic_utilities()
        self.calculate_win_probabilities()
        self.calculate_expected_utilities()
        self.calculate_r()
        self.mu = self.find_mean()
        self.calculate_basic_utilities()
        self.calculate_win_probabilities()
        self.calculate_expected_utilities()
        self.make_offers()
        self.resolve_offers()

def run(csv_file, iter=10, xmin=1, xmax=10,T=1.0,Q=1.0):
    with open(csv_file, 'rU') as f:
        reader = csv.DictReader(f, )
        actors = [actor for actor in reader]
    for actor in actors:
        for key in actor:
            try:
                actor[key] = float(actor[key])
            except:
                continue
    Actors = []
    for actor in actors:
        new_actor = Actor(actor["Actor"],
                          None,
                          actor["Position"],
                          actor["Capability"],
                          actor["Salience"]/100.0)
        Actors.append(new_actor)

    model = Model(Actors, xmax, xmin)
    for actor in model.Actors:
        actor.model = model

    model.T = T
    model.Q = Q
    model.vote()
    model.find_mean()
    [actor.x for actor in model.Actors]
    for i in range(iter):
        model.run_model()
        print model.vote(False)
