import sys

def strip(s):
    return s.strip()

def check_exp_split(l):
    parts = []
    rules = []
    if len(l.split("<=>")) == 2:
        parts = list(map(strip, l.split("<=>")))
        r1 = parts[0] + " " + parts[1]
        r2 = parts[1] + " " + parts[0]
        rules.append(r1)
        rules.append(r2)
    elif len(l.split("=>")) == 2:
        # Rules from Ruler already avoid only-constant-as-lhs one directional rules and bidirectional
        # rules where rhs is a constant.
        # Rules with one side only a constant are always unidirectional with the rhs as constant.
        # So a <=> split will never lead to an 0 => ... form.
        # We therefore need not worry that splitting will lead to
        # an expansive rule with a constant on lhs which we again have to remove.
        parts = list(map(strip, l.split("=>")))
        rules.append(parts[0] + " " + parts[1])
    else:
        print("Not a valid rule. Must be split by => or <=>")
    expansive = (len(parts[0].strip()) <= 2) and (len(parts[1].strip()) > 2)
    (rules, unique_vars) = modulo_alpha_rename(rules)
    return (expansive, rules, unique_vars)

def modulo_alpha_rename(rules):
    ret = []
    unique_vars = set()
    alpha = "abcdefghijklmnopqrstuvwxyz"
    for r in rules:
        r = add_space_between_parens(r)
        mappings = dict()
        splits = r.split(" ")
        for i in range(len(splits)):
            e = splits[i]
            if "?" in e:
                if e in mappings:
                    splits[i] = mappings[e]
                else:
                    new_e = "@" + alpha[len(mappings)]
                    splits[i] = new_e
                    mappings[e] = new_e
        new_rule = remove_space_between_parens(' '.join(splits))
        for k in mappings:
            unique_vars.add(k)
        ret.append(new_rule)
    if len(ret) == 1:
        return (ret, unique_vars)
    else:
        if ret[0] == ret[1]:
            return ([ret[0]], unique_vars)
        else:
            return (ret, unique_vars)

def add_space_between_parens(r):
    r = r.replace('(', ' ( ')
    r = r.replace(')', ' ) ')
    return r

def remove_space_between_parens(r):
    r = r.replace(' ( ', '(')
    r = r.replace(' ) ', ')')
    return r

def rename_ops(rules):
    ret = []
    for r in rules:
        ret.append(r.replace('~', 'neg'))
    return ret

def rename_var(rules):
    ret = []
    for r in rules:
        ret.append(r.replace('@', ''))
    return ret

def process_rules(inf, outf):
    print("Using Ruler output rule file at: " + inf)
    all_rules = []
    unique_vars = set()
    with open(inf, 'r') as f:
        lines = f.readlines()
        for l in lines:
            (exp, rules, vs) = check_exp_split(l)
            for v in vs:
                unique_vars.add(v)
            if (not exp):
                rules = rename_var(rules)
                rules = rename_ops(rules)
                all_rules.extend(rules)
    ctr = 1
    with open(outf, 'w') as f:
        for a in all_rules:
            f.write("[ " + "rule" + str(ctr)  + " " + a + " ] \n")
            ctr = ctr + 1

if __name__ == "__main__":
    process_rules(sys.argv[1], sys.argv[2])