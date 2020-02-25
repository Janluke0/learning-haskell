#ghci pXX.hs -e 'res'
import os
import time
import subprocess 
def parse_solutions(file="solutions.md"):
    sol = {}
    def _parse(s): 
        i = s.index('.')
        k = int(s[:i])
        sol[k] = s[i+1:].strip()
    with open(file) as f: list(map(_parse, f.readlines()[4:]))

    return sol


solutions = parse_solutions()

def do_test(sol_n,solutions = solutions,timeout=None):
    if os.path.isfile("p{0:02d}.hs".format(sol_n)):
        t = time.time()
        cmd = ["ghci","p{0:02d}.hs".format(sol_n),"-e","res"]
        out = subprocess.run(cmd,stdout=subprocess.PIPE,stderr=subprocess.PIPE,timeout=timeout)\
                .stdout.decode('utf-8').strip()
        #print(out,solutions[sol_n])
        return time.time() - t, out.strip()==solutions[sol_n].strip()
    
    return None,None

for i in range(1,26):
    try:
        t, res = do_test(i,timeout=10)
        if t != None :
            print("{2:02d}:\t{0} [{1:.02f}s]".format("OK" if res else "KO", t,i))
    except subprocess.TimeoutExpired:
        print("{0:02d}:\tKO [TIMEOUT]".format(i))