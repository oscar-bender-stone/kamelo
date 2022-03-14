#!/usr/bin/env python
import sys

def filename():
    try:
        return sys.argv[1]
    except IndexError:
        print("Please, give me a filename to do my job on it.")
        print("usage: python3 rewrite.py yourfilename b")
        print("  where the optional parameter b must be False")
        print("        if you need to see the implicit arguments")
        sys.exit()

def tab():
    return "  "

def is_cell(s):
    return s[0] == '<' or s[:9] == "Lbl'-LT-'"
    
def next_symbol(line):
    def aux(n):
        if line[n] == " " or line[n] == "(":
            return line[:n]
        else:
            return aux(n+1)
    return aux(0)

def next_param(raw_param):
    def aux(n, nested):
        if raw_param[n] == ')':
            if nested <= 0:
                return n+1, raw_param[:n+1]
            else:
                return aux(n+1, nested-1)
        if raw_param[n] == '(':
            return aux(n+1, nested+1)
        else:
            return aux(n+1, nested)
    return aux(0, 0)

def cut(line):
    init_tab = tab() * 5 # Pourrait se déduire de la 1ere ligne
    def aux(res_cut, line, n, nb_cur_tab, nb_next_tab, carac):
        if line[n] == ';': return res_cut + [carac + line]
        if line[n] == ')':
            return aux(res_cut, line, n+1, nb_cur_tab, nb_next_tab-1, carac)
        if line[n] == '(':
            # Si le paramètre suivant peut rester sur la même ligne
            len_param, param = next_param(line[n+1:])
            if not(is_cell(param)) and len_param <= 15: # Adapter cette valeur 15
                return aux(res_cut, line, n+1+len_param, nb_cur_tab, nb_next_tab, carac)
            # Si le symbole suivant est "inj"
            if next_symbol(line[n+1:]) == "inj":
                return aux(res_cut, line, n+1+3, nb_cur_tab, nb_next_tab+1, carac)
            else:
                res_cut += [tab() * nb_cur_tab + carac + line[:n] + '\n']
                return aux(res_cut, line[n+1:], 0, nb_next_tab, nb_next_tab+1, init_tab + '(')
        else:
            return aux(res_cut, line, n+1, nb_cur_tab, nb_next_tab, carac)
    return aux([], line, 0, 0, 0, "")

def delete_implicit_arg(line_list):
    res = []
    for line in line_list:
        new_line = ""
        to_skip = False
        to_delete = False
        delete_space = False
        for c in line:
            if to_skip:
                to_skip = False
                continue
            if c == '[':
                to_delete = True
            if c == ']':
                to_delete = False
                to_skip = True # Pour supprimer un espace
            else:
                if not(to_delete): new_line += c
        res += [new_line]
    return res

##################
# STEP A : Récupération d'une option
##################
if len(sys.argv) > 2:
    no_implicit_arg = (sys.argv[2] == "True")
else:
    no_implicit_arg = True
##################
# STEP B : Récupération et modification du fichier
##################
res = []
with open(filename(), 'r', encoding="utf-8") as fichier:
    # Récupération du contenu du fichier dans une liste
    # Une ligne = un élément de la liste
    contenu = fichier.readlines()
    # Suppression des arguments implicites
    if no_implicit_arg:
        contenu = delete_implicit_arg(contenu)
    # Ajout de retours à la ligne "intelligents"
    for line in contenu:
        if len(line.strip()) <= 80: # Adapter cette valeur 80
            res += [line]
        else:
            res += cut(line)
##################
# STEP C : Ecrasement de l'ancien fichier
##################
with open(filename(), "w", encoding="utf-8") as fichier:
    s = ""
    for item in res:
        s += item
    fichier.write(s)
