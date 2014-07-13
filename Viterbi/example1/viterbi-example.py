#!/usr/bin/python
# --*-- coding: utf-8 --*--
# See http://en.wikipedia.org/wiki/Viterbi_algorithm
#
def viterbi(obs, states, start_p, trans_p, emit_p):
    V = [{}]
    path = {}

    # --------------
    # V[0]["t"] = 0
    # --------------
 
    # Initialize base cases (t == 0)
    for y in states:
        V[0][y] = start_p[y] * emit_p[y][obs[0]]
        path[y] = [y]

    # Run Viterbi for t > 0
    for t in range(1, len(obs)):
        V.append({})
        newpath = {}

        # --------------
        # V[t]["t"] = t
        # --------------
 
        for y in states:

            (prob, state) = max(((V[t-1][y0] * trans_p[y0][y] * emit_p[y][obs[t]], y0) for y0 in states))

            V[t][y] = prob
            newpath[y] = path[state] + [y]
 
        # Don't need to remember the old paths
        path = newpath

    # --------------
    # print ("")
    # for x in V:
    #     print x
    # print ("")
    # for p in path:
    #     print (p, path[p])
    # print ("")
    # --------------

    n = 0           # if only one element is observed max is sought in the initialization values
    if len(obs)!=1:
        n = t
    print_dptable(V)
    (prob, state) = max((V[n][y], y) for y in states)
    return (prob, path[state])
 
# prints a table of the steps.
def print_dptable(V):
    s = "    " + " ".join(("%7d" % i) for i in range(len(V))) + "\n"
    for y in V[0]:
        s += "%.5s: " % y
        s += " ".join("%.7s" % ("%f" % v[y]) for v in V)
        s += "\n"
    print(s)

def example():

    states = ('Healthy', 'Fever')

    observations = ('normal', 'cold', 'dizzy')

    start_probability = {'Healthy': 0.6, 'Fever': 0.4}

    transition_probability = {
        'Healthy' : {'Healthy': 0.7, 'Fever': 0.3},
        'Fever' : {'Healthy': 0.4, 'Fever': 0.6}
    }
 
    emission_probability = {
        'Healthy' : {'normal': 0.5, 'cold': 0.4, 'dizzy': 0.1},
        'Fever' : {'normal': 0.1, 'cold': 0.3, 'dizzy': 0.6}
    }

    return viterbi(observations,
                   states,
                   start_probability,
                   transition_probability,
                   emission_probability)

def main():
    print(example())

if __name__ == "__main__":
    main()
