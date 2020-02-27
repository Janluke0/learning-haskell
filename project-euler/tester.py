#ghci pXX.hs -e 'res'
import os
import time
import subprocess 

TIMEOUT = 10 #s


def parse_solutions(file="solutions.md"):
    sol = {}
    def _parse(s): 
        i = s.index('.')
        k = int(s[:i])
        sol[k] = s[i+1:].strip()
    with open(file) as f: 
        list(map(_parse, f.readlines()[4:])) ## we know that solution starts at the 5th line

    return sol


solutions = parse_solutions()

def do_test(sol_n,solutions = solutions,timeout=None):
    if os.path.isfile("p{0:03d}.hs".format(sol_n)):
        t = time.time()
        cmd = ["ghci","p{0:03d}.hs".format(sol_n),"-e","res"]
        out = subprocess.run(cmd,stdout=subprocess.PIPE,stderr=subprocess.PIPE,timeout=timeout)\
                .stdout.decode('utf-8').strip()
        #print(out,solutions[sol_n])
        return time.time() - t, out.strip()==solutions[sol_n].strip()
    
    return None,None

for i in range(1,50):
    try:
        t, res = do_test(i,timeout=TIMEOUT)
        if t != None : # an implementation of the solution exist 
            print("{2:03d}:\t{0} [{1:.02f}s]".format("OK" if res else "KO", t,i))
    except subprocess.TimeoutExpired:
        print("{0:03d}:\tKO [TIMEOUT]".format(i))